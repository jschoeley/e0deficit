# Compare our adjusted e0 estimates against HMD and Eurostat

# Init ------------------------------------------------------------

library(yaml)
library(readr); library(dplyr); library(openxlsx)
library(purrr)
library(ggplot2)

# Constants -------------------------------------------------------

# input and output paths
setwd('.')
paths <- list()
paths$input <- list(
  tmpdir = './tmp',
  config = './cfg/config.yaml',
  global = './src/_global_functions.R',
  region = './cfg/region_metadata.csv',
  analysisinput_bias_corrected.rds = './out/31-analysisinput_bias_corrected.rds',
  lifetables = './out/50-lifetables.rds'
)
paths$output <- list(
  tmpdir = paths$input$tmpdir,
  xlsx_e0 = './out/91-e0benchmark.xlsx',
  e0benchmark.svg = './out/91-e0benchmark.svg'
)

# global configuration
config <- read_yaml(paths$input$config)

# global objects and functions
global <- source(paths$input$global)

# constants specific to this analysis
cnst <- within(list(), {
  region = filter(
    read_csv(paths$input$region),
    region_code_iso3166_2 %in% config$skeleton$regions
  )
})

dat <- list()

# Load data -------------------------------------------------------

dat$analysisinput <- readRDS(paths$input$analysisinput_bias_corrected.rds)
dat$lifetables <- readRDS(paths$input$lifetables)

# Calculate and plot e0 -------------------------------------------

e0benchmark <- list()

e0benchmark$data <-
  left_join(
    select(dat$analysisinput, region, sex, year, age = age_start, e0_hmd = lifeexpectancy_hmd, e0_eurostat = lifeexpectancy_eurostat),
    select(filter(dat$lifetables, scenario == 'actual'), region = region_iso, sex, year = year_int, age, e0_js = ex_q0.5)
  ) |>
  filter(age == 0) |>
  left_join(cnst$region, by = c('region' = 'region_code_iso3166_2')) |>
  filter(year %in% 2010:2024)


e0benchmark$padding <-
  e0benchmark$data |>
  group_by(region, region_name_en) |>
  summarise(
    ymin = min(c(e0_hmd, e0_eurostat, e0_js), na.rm = TRUE),
    ymax = max(c(e0_hmd, e0_eurostat, e0_js), na.rm = TRUE),
    yrange = ymax-ymin,
    ypadding = (14.32-yrange)/2,
    ymin_padded = ymin-ypadding,
    ymax_padded = ymax+ypadding
  ) |>
  ungroup()

e0benchmark$fig <-
  e0benchmark$data |>
  ggplot(aes(x = year, group = sex, color = sex, fill = sex)) +
  geom_vline(xintercept = 2020, color = 'grey') +
  geom_point(aes(y = e0_js), shape = 21, fill = 'white') +
  geom_point(aes(y = e0_eurostat), shape = 3) +
  geom_point(aes(y = e0_hmd), shape = 4) +
  # fake data to make each panel have equal range but shifted
  geom_linerange(
    aes(x = NA_real_, ymin = ymin_padded, ymax = ymax_padded),
    data = e0benchmark$padding, inherit.aes = FALSE
  ) +
  scale_x_continuous(
    breaks = seq(2010, 2024, 1),
    labels = c('2010', rep('', 13), '2024'),
    limits = c(2010, 2024),
  ) +
  scale_y_continuous(breaks = seq(70, 90, 2)) +
  scale_color_manual(values = unlist(config$figspec$colors$sex)) +
  scale_fill_manual(values = unlist(config$figspec$colors$sex)) +
  MyGGplotTheme(grid = 'y', axis = 'x', panel_border = FALSE,
                show_legend = FALSE) +
  labs(
    y = 'Period life expectancy', x = NULL, color = NULL, fill = NULL,
    #title = 'Life expectancy estimates versus HMD (angled cross) and Eurostat estimates (upright cross)'
  ) +
  facet_wrap(~region_name_en, scales = 'free_y', ncol = 5)

e0benchmark$fig

# e0benchmark$data |>
#   mutate(delta = e0_js - e0_hmd) |>
#   group_by(sex, region) |>
#   summarise(
#     delta_pre = mean(delta[year < 2020], na.rm = TRUE),
#     delta_post = mean(delta[year >= 2020], na.rm = TRUE)
#   ) |>
#   arrange(-delta_pre) |>
#   ungroup() |>
#   View()

# Export ----------------------------------------------------------

ExportSVG(
  e0benchmark$fig, filename = paths$output$e0benchmark.svg,
  width = 170, height = 220, dpi = 300
)

write.xlsx(
  e0benchmark$data, file = paths$output$xlsx_e0,
  keepNA = TRUE, na.string = '.',
  firstRow = TRUE, firstCol = TRUE, overwrite = TRUE
)
