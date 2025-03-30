# Download data on death counts
#
# (1) Download weekly death count data by age, sex, and region from
#     STMF input file
# (2) Download annual death counts for E&W pre 2020 from ONS
# (3) Download annual death counts for US pre 2020 from CDC

# Init ------------------------------------------------------------

library(yaml)
library(httr)
library(purrr); library(dplyr); library(readr)
library(readxl)

# Constants -------------------------------------------------------

# input and output paths
setwd('.')
paths <- list()
paths$input <- list(
  config.yaml = './cfg/config.yaml',
  global_functions.R = './src/_global_functions.R',
  # STMF input data files
  url_stmf = 'https://www.mortality.org/File/GetDocument/Public/STMF/Inputs/STMFinput.zip',
  # ONS annual death counts pre 2020 for England and Wales
  url_ons = 'https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/birthsdeathsandmarriages/deaths/datasets/deathsregisteredinenglandandwalesseriesdrreferencetables/2019/finalreftables2019.xlsx',
  ons_raw = './dat/ons/12-ons_annual_deaths.xlsx'
)
paths$output <- list(
  stmf.rds = './dat/stmf/12-stmf.rds',
  stmf.zip = './dat/stmf/12-stmf.zip',
  ons_annual_deaths.xlsx = './dat/ons/12-ons_annual_deaths.xlsx',
  ons_annual_deaths.rds = './dat/ons/12-ons_annual_deaths.rds'
)

# global configuration
config <- read_yaml(paths$input$config.yaml)

# list containers for analysis artifacts
stmf <- list()
ons <- list()

# Download STMF data ----------------------------------------------

# download international weekly death counts by age and sex from STMF
stmf$zip <-
  GET(url = paths$input$url_stmf, progress())

# save downloaded zip to file
writeBin(
  object = content(stmf$zip, 'raw'),
  con = paths$output$stmf.zip
)

# Download ONS data -----------------------------------------------

# download GB-EAW annual death count by age and sex counts from ONS
ons$raw <-
  GET(url = paths$input$url_ons, progress())

# save downloaded excel table to file
writeBin(
  object = content(ons$raw, 'raw'),
  con = paste0(paths$output$ons_annual_deaths.xlsx)
)

# Download CDC data -----------------------------------------------

# manually via
# Centers for Disease Control and Prevention,
# National Center for Health Statistics.
# Underlying Cause of Death 1999-2019 on CDC WONDER Online Database.
# Accessed at http://wonder.cdc.gov/ucd-icd10.html on Jul 29, 2024
#
# ## Query Criteria:
#
# - Gender: Female; Male
# - Single-Year Ages: < 1 year; 1 year; 2 years; 3 years; 4 years; 5 years; 6 years; 7 years; 8 years; 9 years; 10 years; 11 years; 12 years; 13 years; 14 years; 15 years; 16 years; 17 years; 18 years; 19 years; 20 years; 21 years; 22 years; 23 years; 24 years; 25 years; 26 years; 27 years; 28 years; 29 years; 30 years; 31 years; 32 years; 33 years; 34 years; 35 years; 36 years; 37 years; 38 years; 39 years; 40 years; 41 years; 42 years; 43 years; 44 years; 45 years; 46 years; 47 years; 48 years; 49 years; 50 years; 51 years; 52 years; 53 years; 54 years; 55 years; 56 years; 57 years; 58 years; 59 years; 60 years; 61 years; 62 years; 63 years; 64 years; 65 years; 66 years; 67 years; 68 years; 69 years; 70 years; 71 years; 72 years; 73 years; 74 years; 75 years; 76 years; 77 years; 78 years; 79 years; 80 years; 81 years; 82 years; 83 years; 84 years; 85 years; 86 years; 87 years; 88 years; 89 years; 90 years; 91 years; 92 years; 93 years; 94 years; 95 years; 96 years; 97 years; 98 years; 99 years; 100+ years
# - Year/Month: 2015; 2016; 2017; 2018; 2019
# - Group By: Year; Gender; Single-Year Ages
# - Show Totals: False
# - Show Zero Values: False
# - Show Suppressed: False
# - Calculate Rates Per: 100,000
# - Rate Options: Default intercensal populations for years 2001-2009 (except Infant Age Groups)

# Preliminary format STMF data ------------------------------------

# list all files in archive
stmf$filenames <- unzip(
  paths$output$stmf.zip,
  list = TRUE
)[['Name']]

# bind all .csv files in stmf zip archive into single file
stmf$export <-
  stmf$filenames |>
  map(~{
    unz(paths$output$stmf.zip, filename = .x) |>
      read_csv(
        col_names = c('PopCode', 'Area', 'Year', 'Week', 'Sex', 'Age',
                      'AgeInterval', 'Deaths', 'Type', 'Access'),
        col_types = 'ciiccccdcc',
        skip = 1,
        na = '.'
      )
  }) |>
  bind_rows()

# Preliminary format ONS data -------------------------------------

# subset to data of interest and bind the tables
# for males and females together
ons$export <-
  bind_rows(
    male = read_xlsx(
      path = paths$input$ons_raw,
      sheet = 8, range = 'A9:K115'
    ),
    female = read_xlsx(
      path = paths$input$ons_raw,
      sheet = 9, range = 'A9:K115'
    ),
    .id = 'sex'
  )

# Export ----------------------------------------------------------

saveRDS(stmf$export, file = paths$output$stmf.rds)
saveRDS(ons$export, file = paths$output$ons_annual_deaths.rds)
