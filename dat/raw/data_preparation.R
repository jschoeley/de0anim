# Changes in total life-expectancy 1950-2020

# Init ------------------------------------------------------------

library(here); setwd(here())
library(tidyverse)

# Input -----------------------------------------------------------

region_meta <- read_csv('dat/raw/region_metadata.csv')
pclm_deaths <- read_rds('dat/raw/lt_input_85.rds')
hmd_lifetab <- read_rds('dat/raw/lt-1x1.rds')

# Create skeleton -------------------------------------------------

skeleton <- expand_grid(
    region_meta %>%
    select(region_code = region_code_iso3166_2, region_name),
  sex = c('Female', 'Male', 'Total'),
  year = 1950:2020
)

# Calculate e0 from PCLM ungrouped data ---------------------------

# simple piecewise-exponential life-table
CalculateLifeTable <-
  function (df, x, nx, Dx, Ex) {
    
    require(dplyr)
    
    df %>%
      transmute(
        x = {{x}},
        nx = {{nx}},
        mx = {{Dx}}/{{Ex}},
        px = exp(-mx*{{nx}}),
        qx = 1-px,
        lx = head(cumprod(c(1, px)), -1),
        dx = c(-diff(lx), tail(lx, 1)),
        Lx = ifelse(mx==0, lx*nx, dx/mx),
        Tx = rev(cumsum(rev(Lx))),
        ex = Tx/lx
      )
    
  }

# sum female and male deaths and population exposures to a total column
pclm_with_total <-
  pclm_deaths %>%
  select(
    region_code = region_iso, sex, year, age_start, age_width,
    death_total, population_py
  ) %>%
  pivot_wider(
    id_cols = c(region_code, year, age_start, age_width),
    names_from = c(sex),
    values_from = c(death_total, population_py),
    names_sep = '.'
  ) %>%
  mutate(
    death_total.Total = death_total.Female + death_total.Male,
    population_py.Total = population_py.Female + population_py.Male,
  ) %>%
  pivot_longer(
    cols = starts_with(c('death_total', 'population_py')),
    names_to = c('.value', 'sex'), names_sep = '\\.'
  )

pclm_gb <-
  pclm_with_total %>%
  mutate(
    region_code =
      ifelse(region_code %in% c('GB-EAW', 'GB-NIR', 'GB-SCT'),
             'GB', region_code),
  ) %>%
  group_by(region_code, year, age_start, age_width, sex) %>%
  summarise(
    death_total = sum(death_total),
    population_py = sum(population_py)
  ) %>%
  ungroup()

pclm_lifetab <-
  pclm_gb %>%
  arrange(region_code, sex, year, age_start) %>%
  group_by(region_code, sex, year) %>%
  group_modify(~{
    CalculateLifeTable(.x, age_start, age_width, death_total, population_py)
  }) %>%
  ungroup()

pclm_harmonized <-
  pclm_lifetab %>%
  filter(x == 0) %>%
  group_by(region_code, sex) %>%
  ungroup() %>%
  select(region_code, sex, year, e0_pclm = ex)

# Harmonize HMD data ----------------------------------------------

hmd_harmonized <-
  hmd_lifetab %>%
  as_tibble() %>%
  filter(age == 0) %>%
  mutate(
    sex = case_when(
      sex == 'b' ~ 'Total', sex == 'f' ~ 'Female', sex == 'm' ~ 'Male')
  ) %>%
  right_join(region_meta, by = c('country' = 'region_code_hmd')) %>%
  select(region_code = region_code_iso3166_2, sex, year, e0_hmd = ex) %>%
  arrange(region_code, sex, year) %>%
  group_by(region_code, sex) %>%
  ungroup()

de0 <-
  skeleton %>%
  left_join(pclm_harmonized) %>%
  left_join(hmd_harmonized) %>%
  mutate(
    e0 = ifelse(is.na(e0_hmd), e0_pclm, e0_hmd)
  ) %>%
  group_by(region_code, sex) %>%
  mutate(de0 = e0 - lag(e0)) %>%
  ungroup()

# Export ----------------------------------------------------------

de0 %>%
  mutate(across(.cols = c(e0_pclm, e0_hmd, e0, de0), ~round(., digits = 2))) %>%
  write_csv('dat/de0.csv')
