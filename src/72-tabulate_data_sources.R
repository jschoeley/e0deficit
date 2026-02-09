# Tabulate life expectancy deficits

# Init ------------------------------------------------------------

library(yaml)
library(readr); library(dplyr); library(tidyr)
library(gt)

# Constants -------------------------------------------------------

# input and output paths
setwd('.')
paths <- list()
paths$input <- list(
  config.yaml = './cfg/config.yaml',
  global_functions.R = './src/_global_functions.R',
  region_metadata.csv = './cfg/region_metadata.csv',
  analysisinput_bias_corrected.rds = './out/31-analysisinput_bias_corrected.rds'
)
paths$output <- list(
  datasources.tex = './out/72-datasources.tex'
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


# Functions -------------------------------------------------------

PrintRange <- function (x) {
  r <- range(x)
  if (diff(r) == 0) {
    as.character(r[1])
  } else {
    paste0(r, collapse = '-')
  }
}

# Load data -------------------------------------------------------

dat$analysisinput <-
  readRDS(paths$input$analysisinput_bias_corrected.rds)

# Create Table ----------------------------------------------------

datasources <- list()

datasources$data <-
  dat$analysisinput |>
  select(
    region, year,
    death_source, population_jan1st_source
  ) |>
  group_by(
    region, death_source,
    population_jan1st_source
  ) |>
  summarise(
    range = PrintRange(year)
  ) |>
  ungroup() |>
  arrange(region, range) |>
  mutate(
    death_source = case_when(
      death_source == 'stmf' ~ 'HMD-STMF',
      death_source == 'ons' ~ 'ONS',
      death_source == 'japan' ~ 'SBJ',
      death_source == 'cdc' ~ 'CDC',
      death_source == 'hmd' ~ 'HMD',
      is.na(death_source) ~ '-'
    ),
    population_jan1st_source = case_when(
      population_jan1st_source == 'wpp24' ~ 'WPP-2024',
      population_jan1st_source == 'hmd_estimates' ~ 'HMD-EST',
      population_jan1st_source == 'hmd_projections' ~ 'HMD-PRJ',
      is.na(population_jan1st_source) ~ '-'
    )
  ) |>
  right_join(cnst$region, by = c('region' = 'region_code_iso3166_2')) |>
  select(region = region_name_en, range, death_source, population_jan1st_source)

datasources$table <-
  datasources$data |>
  gt() |>
  cols_label(
    region = 'Country',
    range = 'Period',
    death_source = 'Death counts',
    population_jan1st_source = 'Jan 1st population'
  )

datasources$table

# Export ----------------------------------------------------------

gtsave(
  datasources$table,
  filename = paths$output$datasources.tex
)
