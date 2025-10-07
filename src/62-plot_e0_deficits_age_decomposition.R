# Plot age decomposition of e0 deficit

# Init ------------------------------------------------------------

library(yaml)
library(readr); library(dplyr)
library(ggplot2); library(ggflagsplus)

# Constants -------------------------------------------------------

# input and output paths
setwd('.')
paths <- list()
paths$input <- list(
  config.yaml = './cfg/config.yaml',
  global_functions.R = './src/_global_functions.R',
  region_metadata.csv = './cfg/region_metadata.csv',
  deficits_and_excesses.rds = './out/50-deficits_and_excesses.rds'
)
paths$output <- list(
  e0deficitage.pdf = './out/62-e0deficitage.pdf',
  e0deficitage.csv = './out/62-e0deficitage.csv'
)

# global configuration
config <- read_yaml(paths$input$config.yaml)

# global objects and functions
source(paths$input$global_functions.R)

# constants specific to this analysis
cnst <- within(list(), {
  region = filter(
    read_csv(paths$input$region_metadata.csv),
    region_code_iso3166_2 %in% config$skeleton$regions
  )
})

dat <- list()

# Load data -------------------------------------------------------

dat$lt <-
  readRDS(paths$input$deficits_and_excesses.rds) |>
  filter(region_iso %in% config$showinoutput, !region_iso %in% config$excludefromagedecomposition)

# Plot e0 deficits by age -----------------------------------------

e0deficitage <- list()

e0deficitage$cnst <- list(
  colors = c('2020' = '#CCD800', '2021' = '#D89E00', '2022' = '#C05B00',
             '2023' = '#C02C00', '2024' = '#640099', '2020-2024' = 'grey50')
)

e0deficitage$data <-
  dat$lt |>
  filter(sex == 'Total') |>
  left_join(cnst$region, by = c('region_iso' = 'region_code_iso3166_2')) |>
  filter(year %in% c('2020', '2021', '2022', '2023', '2024', '2020-2024')) |>
  mutate(
    age = as.integer(age), age_group = (age %/% 20)*20
  ) |>
  ungroup() |>
  select(region_iso, region_name_en, year, age_group, e0_cntrb_t_mean) |>
  group_by(region_iso, region_name_en, year, age_group) |>
  summarise(
    e0_cntrb_t_mean = sum(e0_cntrb_t_mean)
  ) |>
  mutate(
    region_ggflag = tolower(region_iso)
  )

e0deficitage$fig <-
  e0deficitage$data |>
  ggplot() +
  aes(x = e0_cntrb_t_mean, y = age_group) +
  geom_vline(xintercept = 0, color = 'grey') +
  geom_point(
    x = -2, y = 100, size = 5.5
  ) +
  geom_flag(
    aes(x = -2, y = 100, country = region_ggflag),
    size = 5
  ) +
  geom_segment(
    aes(color = year, x = 0, xend = e0_cntrb_t_mean,
        y = age_group, yend = age_group, group = age_group),
    data = . %>% filter(year == '2020-2024'),
    size = 2
  ) +
  geom_path(aes(color = year), data = . %>% filter(year != '2020-2024')) +
  facet_wrap(~region_name_en, ncol = 4) +
  scale_color_manual(breaks = names(e0deficitage$cnst$colors),
                     values = e0deficitage$cnst$colors) +
  scale_y_continuous(
    breaks = c(0, 20, 40, 60, 80, 100),
    labels = c('0-19', '20-39', '40-59', '60-79', '80-99', '100+')
  ) +
  scale_x_continuous(breaks = seq(-2, 0.5, 0.5),
                     labels = c('-2', '-1.5', '-1', '-0.5', '0', '+0.5')) +
  labs(
    x = 'Years of contribution to annual LE deficit',
    y = 'Age group', color = 'Year'
    #title = 'Age-specific contributions to total LE deficit since 2020 by year'
  ) +
  MyGGplotTheme(grid = 'y', axis = 'x', panel_border = FALSE) +
  coord_cartesian(clip = 'off')

e0deficitage$fig

# Export ----------------------------------------------------------

ggsave(
  paths$output$e0deficitage.pdf, e0deficitage$fig,
  units = 'mm', width = 170, height = 190, device = 'pdf', scale = 1.4
)
e0deficitage$data |>
  mutate(across(.cols = where(is.numeric), .fns = ~round(.x,6))) |>
  write_csv(paths$output$e0deficitage.csv)
