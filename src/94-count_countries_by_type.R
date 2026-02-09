# Compare our adjusted e0 estimates against HMD and Eurostat

# Init ------------------------------------------------------------

library(yaml)
library(readr); library(dplyr)

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
  lifetables = './out/50-lifetables.rds',
  e0trends.csv = './out/60-e0trends.csv',
  pval.rds = './out/50-pval.rds',
  deficits_and_excesses.csv = './out/50-deficits_and_excesses.csv'
)
paths$output <- list(
  tmpdir = paths$input$tmpdir
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

# Input -----------------------------------------------------------

e0trends <- read_csv(paths$input$e0trends.csv)
analysisinput <- readRDS(paths$input$analysisinput_bias_corrected.rds)
pval <- readRDS(paths$input$pval.rds)
deficits <- read_csv(paths$input$deficits_and_excesses.csv)

# Functions ---------------------------------------------------------------

FormatNumber <- function (x) {
  formatC(x, digits = 2, format = 'f')
}
FormatPValue <- function (x) {
  x_ <- paste0('=', formatC(x, digits = 3, format = 'f'))
  x_ <- ifelse(x < 0.001, '<0.001', x_)
  return(x_)
}

# Count -----------------------------------------------------------

regioncount <- list()

# countries in e0 trend analysis
regioncount$regions_in_e0_trend_analysis <-
  unique(e0trends$region_name_en)
regioncount$regions_in_e0_trend_analysis
# number of countries in e0 trend analysis
regioncount$n_regions_in_e0_trend_analysis <-
  length(regioncount$regions_in_e0_trend_analysis)
regioncount$n_regions_in_e0_trend_analysis

# countries with deficits in 2024
regioncount$regions_with_e0_deficits_in_2024 <-
  e0trends |>
  filter(year == 2024, e0_deficit_avg < 0) |>
  pull(region_name_en)
regioncount$regions_with_e0_deficits_in_2024
# number of countries with deficits in 2024
regioncount$n_regions_with_e0_deficits_in_2024 <-
  length(regioncount$regions_with_e0_deficits_in_2024)
regioncount$n_regions_with_e0_deficits_in_2024

# countries with significant deficits in 2024
regioncount$regions_with_sign_e0_deficits_in_2024 <-
  pval |>
  filter(year == 2024, sex == 'Total', e0_deficit_pval <= 0.05) |>
  left_join(cnst$region, by = c('region_iso' = 'region_code_iso3166_2')) |>
  pull(region_name_en)
regioncount$regions_with_sign_e0_deficits_in_2024
# number of countries with significant deficits in 2024
regioncount$n_regions_with_sign_e0_deficits_in_2024 <-
  length(regioncount$regions_with_sign_e0_deficits_in_2024)
regioncount$n_regions_with_sign_e0_deficits_in_2024
# sentence of countries with significant deficits 2024
regioncount$sentence_regions_with_sign_e0_deficits_in_2024 <-
  deficits |>
    filter(sex == 'Total', year == 2024, age == 0) |>
    left_join(pval, by = c('sex', 'region_iso', 'year')) |>
    filter(e0_deficit_pval <= 0.05) |>
    select(region_iso, ex_actual_minus_expected_mean, e0_deficit_pval) |>
    left_join(cnst$region, by = c('region_iso' = 'region_code_iso3166_2')) |>
    mutate(
      label = paste0(
        region_name_en,
        ' (', FormatNumber(ex_actual_minus_expected_mean), ', p', FormatPValue(e0_deficit_pval), ')')
    ) |>
    arrange(region_name_en) |>
    pull(label) |>
    paste0(collapse = ', ')

# countries with significant deficits 2020-2024
regioncount$regions_with_sign_e0_deficits_in_2020_2024 <-
  pval |>
  filter(year %in% c('2020-2024'), sex == 'Total', e0_deficit_pval <= 0.05) |>
  left_join(cnst$region, by = c('region_iso' = 'region_code_iso3166_2')) |>
  pull(region_name_en)
regioncount$regions_with_sign_e0_deficits_in_2020_2024

# countries without significant deficits 2020-2024
regioncount$regions_without_sign_e0_deficits_in_2020_2024 <-
  pval |>
  filter(year %in% c('2020-2024'), sex == 'Total', e0_deficit_pval > 0.05) |>
  left_join(cnst$region, by = c('region_iso' = 'region_code_iso3166_2')) |>
  pull(region_name_en)
regioncount$regions_without_sign_e0_deficits_in_2020_2024

# type C countries without significant deficits 2024
pval |>
  left_join(cnst$region, by = c('region_iso' = 'region_code_iso3166_2')) |>
  filter(
    region_iso %in% config$groups$`C Late peak`,
    year %in% c('2024'), sex == 'Total', e0_deficit_pval > 0.05
  ) |>
  pull(region_name_en)
# type C countries with significant deficits 2024
pval |>
  left_join(cnst$region, by = c('region_iso' = 'region_code_iso3166_2')) |>
  filter(
    region_iso %in% config$groups$`C Late peak`,
    year %in% c('2024'), sex == 'Total', e0_deficit_pval <= 0.05
  ) |>
  pull(region_name_en)


# countries with full data series in HMD
regioncount$regions_with_full_hmd_deaths <-
  analysisinput |>
  group_by(region) |>
  summarise(all_hmd = all(death_source == 'hmd')) |>
  filter(all_hmd) |>
  pull(region)
# number of countries with full data series in HMD
regioncount$n_region_with_full_hmd_deaths <-
  length(regioncount$regions_with_full_hmd_deaths)

# countries with full HMD exposures
regioncount$regions_with_full_hmd_exposures <-
  analysisinput |>
  group_by(region) |>
  summarise(all_hmd = all(population_py == population_py_hmd)) |>
  filter(all_hmd) |>
  pull(region)
# number of countries with full data series in HMD
regioncount$n_region_with_full_hmd_data <-
  length(regioncount$regions_with_full_hmd_data)
