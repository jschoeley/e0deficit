# Harmonize data on life-expectancy

# Init ------------------------------------------------------------

library(readr)
library(dplyr); library(lubridate); library(stringr); library(tidyr)

# Constants -------------------------------------------------------

# input and output paths
setwd('.')
paths <- list()
paths$input <- list(
  global_functions.R = './src/_global_functions.R',
  lifeexpectancy_eurostat.rds = 'dat/eurostat/14-lifeexpectancy_eurostat.rds',
  lifeexpectancy_hmd.rds = 'dat/hmdhfd/14-lifeexpectancy_hmd.rds',
  harmonized_skeleton.rds = './tmp/10-harmonized_skeleton.rds',
  region_metadata.csv = './cfg/region_metadata.csv'
)
paths$output <- list(
  harmonized_lifeexpectancy.rds = 'tmp/23-harmonized_lifeexpectancy.rds'
)

# meta data on regions
region_meta <- read_csv(paths$input$region_metadata.csv, na = '.')

lifeexpectancy <- list()

# Functions -------------------------------------------------------

source(paths$input$global_functions.R)

# Load data -------------------------------------------------------

skeleton <- readRDS(paths$input$harmonized_skeleton.rds)

lifeexpectancy$eurostat$raw <- readRDS(paths$input$lifeexpectancy_eurostat.rds)
lifeexpectancy$hmd$raw <- readRDS(paths$input$lifeexpectancy_hmd.rds)

# Harmonize e0 eurostat -------------------------------------------

lifeexpectancy$eurostat$clean <-
  lifeexpectancy$eurostat$raw |>
  mutate(
    sex = factor(
      sex,
      levels = c('F', 'M', 'T'),
      labels = c('Female', 'Male', 'Total')
    ),
    age = case_when(
      age == 'Y_LT1' ~ 'Y00',
      age == 'Y_GE85' ~ 'Y85',
      TRUE ~ age
    )
  ) |>
  filter(
    grepl('^Y[[:digit:]]+$', age)
  ) |>
  transmute(
    sex = sex,
    region = geo,
    year = year(TIME_PERIOD),
    age = as.integer(str_sub(age, 2, 4)),
    lifeexpectancy_eurostat = values,
    id = GenerateRowID(region, sex, year, age)
  ) |>
  select(id, lifeexpectancy_eurostat)

# Harmonize e0 hmd ------------------------------------------------

lifeexpectancy$hmd$clean <-
  lifeexpectancy$hmd$raw |>
  pivot_longer(
    c(Female, Male, Total),
    names_to = 'sex',
    values_to = 'lifeexpectancy_hmd'
  ) |>
  mutate(
    sex = factor(
      sex,
      levels = c('Female', 'Male', 'Total'),
      labels = c('Female', 'Male', 'Total')
    ),
    year = as.integer(Year),
    age = 0L,
    region = as.character(factor(
      region_hmd,
      region_meta$region_code_hmd,
      region_meta$region_code_iso3166_2
    )),
    id = GenerateRowID(region, sex, year, age)
  ) |>
  select(id, lifeexpectancy_hmd)

# Join with skeleton ----------------------------------------------

lifeexpectancy$ready_for_export <-
  skeleton |>
  left_join(lifeexpectancy$eurostat$clean, by = 'id') |>
  left_join(lifeexpectancy$hmd$clean, by = 'id') |>
  select(id, lifeexpectancy_eurostat, lifeexpectancy_hmd)

# Export ----------------------------------------------------------

saveRDS(lifeexpectancy$ready_for_export, file = paths$output$harmonized_lifeexpectancy.rds)
