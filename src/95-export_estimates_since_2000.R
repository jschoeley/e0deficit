# Export e0 actual vs. expected since 2000

# Init ------------------------------------------------------------

library(yaml)
library(readr); library(dplyr); library(tidyr)

# Constants -------------------------------------------------------

# input and output paths
setwd('.')
paths <- list()
paths$input <- list(
  config.yaml = './cfg/config.yaml',
  region_metadata.R = './cfg/region_metadata.csv',
  lifetables.rds = './out/50-lifetables.rds'
)
paths$output <- list(
  estimates_since_2000.csv = './out/95-estimates_since_2000.csv'
)

# global configuration
config <- read_yaml(paths$input$config.yaml)

# constants specific to this analysis
cnst <- within(list(), {
  region = read_csv(paths$input$region_metadata.R)
})

dat <- list()

# Load data -------------------------------------------------------

dat$e0observed <- readRDS(paths$input$lifetables.rds)

# Select and subset -------------------------------------------------------

dat$estimates_since_2000 <-
  dat$e0observed |>
  filter(region_iso %in% config$showinoutput) |>
  filter(age == 0,
         year_int >= 2000) |>
  select(region_iso, sex, year = year_int, ex_mean, scenario) |>
  pivot_wider(names_from = scenario, values_from = ex_mean) |>
  rename(e0_actual = actual, e0_expected_lc = projected)

# Export ----------------------------------------------------------

dat$estimates_since_2000 |>
  mutate(across(.cols = where(is.numeric), .fns = ~round(.x,6))) |>
  write_csv(paths$output$estimates_since_2000.csv)
