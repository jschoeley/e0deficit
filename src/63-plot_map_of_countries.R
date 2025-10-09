# Plot age decomposition of e0 deficit

# Init ------------------------------------------------------------

library(yaml)
library(readr); library(dplyr)
library(ggplot2); library(sf)

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
  countrycount = './out/63-countrycount.csv',
  mapcountrycount.pdf = './out/63-mapcountrycount.pdf'

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
  filter(region_iso %in% config$showinoutput)

# Determine binary country features -------------------------------

dat$lt |>
  filter(sex == 'Total')
