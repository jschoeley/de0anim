# Animate international annual life-expectancy changes 1970 to 2020

# Init ------------------------------------------------------------

library(here)
library(tidyverse)
library(gganimate)

# countries to exclude
exclude <- c( 
  'AU', # coarse age groups, missing weeks
  'CA', # coarse age groups, missing weeks
  'IL', # coarse age groups
  'KR', # coarse age group
  'LU', # adolescent hazard profiles in 2019 very different from 2020
  'LV', # adolescent hazard profiles in 2019 very different from 2020
  'NZ', # coarse age groups
  'TW', # death registration for 2020 not complete
  # we show the whole of GB
  'GB-EAW',
  'GB-SCT',
  'GB-NIR'
)

# add label to animated plot
AddLabel <- function (label, x, y, size = 6, family = 'Roboto Condensed',
                      color = 'grey', fontface = 'plain',
                      hjust = 0, vjust = 0, parse = FALSE) {
  .label <- enquo(label)
  geom_text(
    aes(group = year, label = !!.label),
    x = x, y = y,
    color = color, hjust = hjust, vjust = vjust, size = size,
    family = family, fontface = fontface, check_overlap = TRUE,
    parse = parse
  )
}

colors <- c(
  past = 'grey40',
  covid = '#7AF4D3',
  soviet = '#F491DE',
  flu = '#F4E479'
)

# Prepare data for animation --------------------------------------

# data on annual life-expectancy changes
de0 <- read_csv('dat/de0.csv')

# filter data
de0total <-
  de0 %>%
  filter(sex == 'Total', !(region_code %in% exclude)) %>%
  mutate(
    region_code =
      fct_reorder(region_code, de0, last)
  )

# annotate data with labels for animation
data_for_anim <-
  de0total %>%
  filter(year >= 1970) %>% 
  mutate(
    lab_1 =
      ifelse(
        year == 1970,
        '"Life-expectancy is a measure of"~bold("current population health")',
        NA
      ),
    lab_2 =
      ifelse(
        year == 1977,
        '"it tends to"~bold("increase")~"from year to year..."',
        NA
      ),
    lab_3 =
      ifelse(
        year == 1984,
        '"...but sometimes it"~bold("declines")',
        NA
      ),
    de0_4a =
      ifelse(
        year == 1991 &
          region_code %in% c('BG', 'LT', 'PL', 'SI', 'HR', 'SK',
                             'HU', 'LU', 'EE', 'LV', 'RU') &
          de0 < 0,
        de0, NA
      ),
    de0_4b =
      ifelse(
        year == 1992 &
          region_code %in% c('BG', 'LT', 'PL', 'SI', 'HR', 'SK',
                             'HU', 'LU', 'EE', 'LV', 'RU') &
          de0 < 0,
        de0, NA
      ),
    de0_4c =
      ifelse(
        year == 1993 &
          region_code %in% c('BG', 'LT', 'PL', 'SI', 'HR', 'SK',
                             'HU', 'LU', 'EE', 'LV', 'RU') &
          de0 < 0,
        de0, NA
      ),
    de0_4d =
      ifelse(
        year == 1994 &
          region_code %in% c('BG', 'LT', 'PL', 'SI', 'HR', 'SK',
                             'HU', 'LU', 'EE', 'LV', 'RU') &
          de0 < 0,
        de0, NA
      ),
    lab_4 =
      ifelse(
        year == 1992,
        'like in Eastern Europe after the fall of the Soviet Union',
        NA
      ),
    de0_5 =
      ifelse(year == 2015 & de0 < 0, de0, NA),
    lab_5 = ifelse(
      year == 2016,
      'or during the harsh flu-season 2014/15',
      NA
    ),
    lab_6 = ifelse(
      year == 2020,
      'or during the COVID-19 pandemic',
      NA
    )
  ) %>%
  group_by(region_code) %>%
  mutate(
    # add timings
    length = case_when(
      year == 2019 ~ 10,
      year %in% 1996:2013 ~ 0.5,
      TRUE ~ 1
    ),
    end = cumsum(length),
    start = end-length,
    enter = 0, exit = 0
  ) %>%
  fill(lab_1, lab_2, lab_3,
       lab_4, de0_4a, de0_4b, de0_4c, de0_4d,
       lab_5, de0_5,
       lab_6) %>%
  ungroup()

# Create animation ------------------------------------------------

anim <-
  data_for_anim %>%
  # create animation
  ggplot(aes(x = region_code, y = de0)) +
  # 0 line
  geom_hline(yintercept = 0, color = 'white') +
  # year
  AddLabel(year, x = 'DE', y = 2.5, size = 10, family = 'Roboto', fontface = 'bold') +
  # increase decrease
  AddLabel('1 year increase', x = 'US', y = 1, size = 5, family = 'Roboto') +
  AddLabel('1 year decrease', x = 'US', y = -1, size = 5, family = 'Roboto') +
  # a general increase
  AddLabel(lab_1, x = 'US', y = 2.6, parse = TRUE) +
  AddLabel(lab_2, x = 'BG', y = 2.3, parse = TRUE) +
  AddLabel(lab_3, x = 'BE', y = -1.5, parse = TRUE) +
  # COVID-19
  AddLabel(lab_6, x = 'GB-EAW', y = -2.4, color = colors['covid']) +
  # changes across all the years
  geom_point(color = colors['covid'], size = 4) +
  # fall of soviet union
  AddLabel(lab_4, x = 'IT', y = -1.8, color = colors['soviet']) +
  geom_point(aes(x = region_code, y = de0_4a), color = colors['soviet'], size = 4) +
  geom_point(aes(x = region_code, y = de0_4b), color = colors['soviet'], size = 4) +
  geom_point(aes(x = region_code, y = de0_4c), color = colors['soviet'], size = 4) +
  geom_point(aes(x = region_code, y = de0_4d), color = colors['soviet'], size = 4) +
  # 2015 flu epidemic
  geom_point(aes(x = region_code, y = de0_5), color = colors['flu'], size = 4) +
  AddLabel(lab_5, x = 'SI', y = -2.1, color = colors['flu']) +
  # covid
  AddLabel(lab_6, x = 'SE', y = -2.4, color = colors['covid'], fontface = 'bold') +
  # source
  AddLabel('@jschoeley', x = 'NO', y = -2.9, size = 5, color = 'grey40', hjust = 1) +
  scale_y_continuous(
    breaks = seq(-2, 2, 1), labels = rep('', 5), limits = c(-2.9, 2.9)
  ) +
  hrbrthemes::theme_ft_rc(base_family = 'Roboto Condensed') +
  labs(
    title = 'Annual changes in life-expectancy 1970 to 2020',
    y = NULL, x = NULL#,
    #caption = 'Data derived from: mortality.org Animation @jschoeley'
   ) +
  theme(
    plot.margin = unit(c(0.1,0.1,0.1,0.1), units = 'cm'),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text('Roboto', size = 13, colour = 'grey')
  ) +
  # animation
  transition_time(start) +
  enter_fade() +
  shadow_mark(
    exclude_layer = c(1:8, 10:18), past = TRUE,
    future = FALSE, color = 'grey50', alpha = 0.5, size = 2
  )

animate(
  anim, height = 500, width = 800,
  nframes = 400, duration = 30,
  rewind = FALSE, end_pause = 100
)
