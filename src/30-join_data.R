# Assemble data basis analysis

# Init ------------------------------------------------------------

library(dplyr); library(readr); library(openxlsx)

# Constants -------------------------------------------------------

# input and output paths
setwd('.')
paths <- list()
paths$input <- list(
  config.yaml = './cfg/config.yaml',
  harmonized_skeleton.rds = './tmp/10-harmonized_skeleton.rds',
  global_functions.R = './src/_global_functions.R',
  harmonized_population.rds = './tmp/20-harmonized_population.rds',
  harmonized_death.rds = './tmp/21-harmonized_death.rds',
  harmonized_netmigration.rds = './tmp/22-harmonized_netmigration.rds',
  harmonized_lifeexpectancy.rds = './tmp/23-harmonized_lifeexpectancy.rds'
)
paths$output <- list(
  analysisinput.rds = './out/30-analysisinput.rds',
  analysisinput.csv = './out/30-analysisinput.csv',
  analysisinput.xlsx = './out/30-analysisinput.xlsx'
)

# list containers for analysis artifacts
dat <- list()

# Functions -------------------------------------------------------

source(paths$input$global_functions.R)

# Data ------------------------------------------------------------

dat$skeleton <- readRDS(paths$input$harmonized_skeleton.rds)
dat$population <- readRDS(paths$input$harmonized_population.rds)
dat$death <- readRDS(paths$input$harmonized_death.rds)
dat$netmigration <- readRDS(paths$input$harmonized_netmigration.rds)
dat$lifeexpectancy <- readRDS(paths$input$harmonized_lifeexpectancy.rds)

# Join ------------------------------------------------------------

dat$analysisinput <-
  dat$skeleton |>
  left_join(dat$death, by = 'id') |>
  left_join(dat$population, by = 'id') |>
  left_join(dat$netmigration, by = 'id') |>
  left_join(dat$lifeexpectancy, by = 'id') |>
  mutate(nweeks_year = case_when(
    YearHasIsoWeek53(year) ~ 53L,
    death_source == 'hmd' ~ 52L,
    TRUE ~ 52L
  ))


# Export ----------------------------------------------------------

saveRDS(dat$analysisinput, file = paths$output$analysisinput.rds)

write_csv(dat$analysisinput, file = paths$output$analysisinput.csv)

write.xlsx(dat$analysisinput, file = paths$output$analysisinput.xlsx,
           keepNA = TRUE, na.string = '.',
           firstRow = TRUE, firstCol = TRUE, overwrite = TRUE)
