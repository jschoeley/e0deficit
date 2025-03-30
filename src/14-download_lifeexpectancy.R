# Download data on life-expectancy
#
# (1) Download life expectancy estimates by age, sex, and region from
#     Eurostat
# (2) Download life expectancy estimates by age, sex, and region from
#     HMD

# Init ------------------------------------------------------------

library(yaml)
library(readr)
library(dplyr)
library(eurostat)
library(HMDHFDplus)

# Constants -------------------------------------------------------

# input and output paths
setwd('.')
paths <- list()
paths$input <- list(
  config.yaml = './cfg/config.yaml',
  lifeexpectancy_eurostat = 'demo_mlexpec',
  skeleton.rds = './tmp/10-harmonized_skeleton.rds',
  region_metadata.csv = './cfg/region_metadata.csv'
)
paths$output <- list(
  lifeexpectancy_eurostat.rds = './dat/eurostat/14-lifeexpectancy_eurostat.rds',
  lifeexpectancy_hmd.rds = './dat/hmdhfd/14-lifeexpectancy_hmd.rds'
)

# global configuration
config <- read_yaml(paths$input$config.yaml)

# meta data on regions
region_meta <- read_csv(paths$input$region_metadata.csv, na = '.')

# constants specific to this analysis
cnst <- list(); cnst <- within(cnst, {
  # translation of hmdhfd sex code to harmonized sex code
  code_sex_hmdhfd =
    c(`Male` = config$skeleton$sex$Male,
      `Female` = config$skeleton$sex$Female)
  region_lookup_hmd =
    region_meta |>
    filter(region_code_iso3166_2 %in% config$skeleton$region) |>
    select(region_code_iso3166_2, region_code_hmd)
})

# Download eurostat life expectancy -------------------------------

lifeexpectancy_eurostat <-
  get_eurostat(paths$input$lifeexpectancy_eurostat, type = 'code')

# Download HMD life expectancy ------------------------------------

lifeexpectancy_hmd <-
  vector('list', length(cnst$region_lookup_hmd$region_code_hmd))
names(lifeexpectancy_hmd) <- cnst$region_lookup_hmd$region_code_hmd

for (country in names(lifeexpectancy_hmd)) {
  cat('Download HMD e0', country, '\n')
  lifeexpectancy_hmd[[country]] <- HMDHFDplus::readHMDweb(
    country, item = 'E0per',
    username = config$credentials$hmd_usr,
    password = config$credentials$hmd_pwd
  )
}
lifeexpectancy_hmd <- bind_rows(lifeexpectancy_hmd, .id = 'region_hmd')

# Export ----------------------------------------------------------

saveRDS(lifeexpectancy_eurostat, file = paths$output$lifeexpectancy_eurostat.rds)
saveRDS(lifeexpectancy_hmd, file = paths$output$lifeexpectancy_hmd.rds)
