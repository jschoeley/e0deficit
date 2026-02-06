# Download population counts and mortality/fertility rate estimates and projections
#
# We download yearly population midyear, January 1st population, and
# fertility rate estimates and projections from WPP by sex, age and
# region. We do so for both WPP 22 and WPP 24 to perform sensitivity
# analyses.
#
# Additionally we download, for all countries, HMD/HFD
#  - midyear population estimates by age and sex
#  - mortality rates by age and sex
#  - fertility rates by age
# and then (in the harmonization step) project the last available
# HMD midyear population using stable population projection.
#
# In case of SSL error run script from console with
# > export OPENSSL_CONF=~/sci/2023-04-ex2023/cfg/openssl.cnf
# > Rscript ~/sci/2023-04-ex2023/src/11-download_population_with_fertility.R

# Init ------------------------------------------------------------

library(httr); library(yaml); library(readr)
library(HMDHFDplus)
library(dplyr); library(tidyr); library(purrr); library(glue)

# Constants -------------------------------------------------------

# input and output paths
setwd('.')
paths <- list()
paths$input <- list(
  tmpdir = './tmp',
  config.yaml = './cfg/config.yaml',
  region_metadata.csv = './cfg/region_metadata.csv',

  wpp22 = './dat/wpp22',
  wpp24 = './dat/wpp24',

  # WPP 22 january 1st population estimates
  url_wpp22_jan1st_estimates = 'https://population.un.org/wpp/Download/Files/1_Indicators%20(Standard)/CSV_FILES/WPP2022_Population1JanuaryBySingleAgeSex_Medium_1950-2021.zip',
  url_wpp22_jan1st_projections = 'https://population.un.org/wpp/Download/Files/1_Indicators%20(Standard)/CSV_FILES/WPP2022_Population1JanuaryBySingleAgeSex_Medium_2022-2100.zip',
  zipfilename_wpp22_jan1st_estimates = 'WPP2022_Population1JanuaryBySingleAgeSex_Medium_1950-2021.csv',
  zipfilename_wpp22_jan1st_projections = 'WPP2022_Population1JanuaryBySingleAgeSex_Medium_2022-2100.csv',

  # WPP 22 midyear population estimates
  url_wpp22_midyear_estimates = 'https://population.un.org/wpp/Download/Files/1_Indicators%20(Standard)/CSV_FILES/WPP2022_PopulationBySingleAgeSex_Medium_1950-2021.zip',
  url_wpp22_midyear_projections = 'https://population.un.org/wpp/Download/Files/1_Indicators%20(Standard)/CSV_FILES/WPP2022_PopulationBySingleAgeSex_Medium_2022-2100.zip',
  zipfilename_wpp22_midyear_estimates = 'WPP2022_PopulationBySingleAgeSex_Medium_1950-2021.csv',
  zipfilename_wpp22_midyear_projections = 'WPP2022_PopulationBySingleAgeSex_Medium_2022-2100.csv',

  # WPP 24 january 1st population estimates
  url_wpp24_jan1st_estimates = 'https://population.un.org/wpp/Download/Files/1_Indicator%20(Standard)/CSV_FILES/WPP2024_Population1JanuaryBySingleAgeSex_Medium_1950-2023.csv.gz',
  url_wpp24_jan1st_projections = 'https://population.un.org/wpp/Download/Files/1_Indicator%20(Standard)/CSV_FILES/WPP2024_Population1JanuaryBySingleAgeSex_Medium_2024-2100.csv.gz',

  # WPP 24 midyear population estimates
  url_wpp24_midyear_estimates = 'https://population.un.org/wpp/Download/Files/1_Indicator%20(Standard)/CSV_FILES/WPP2024_PopulationBySingleAgeSex_Medium_1950-2023.csv.gz',
  url_wpp24_midyear_projections = 'https://population.un.org/wpp/Download/Files/1_Indicator%20(Standard)/CSV_FILES/WPP2024_PopulationBySingleAgeSex_Medium_2024-2100.csv.gz',

  # WPP 24 age specific fertility rates
  url_wpp24_fertility_projections = 'https://population.un.org/wpp/Download/Files/1_Indicator%20(Standard)/CSV_FILES/WPP2024_Fertility_by_Age1.csv.gz'
)
paths$output <- list(
  tmpdir = paths$input$tmpdir,
  wpp22 = './dat/wpp22',
  wpp24 = './dat/wpp24',
  wppjoint.rds = './dat/wppjoint/11-wppjoint.rds',
  hmdhfd.rds = './dat/hmdhfd/11-hmdhfd.rds'
)

# global configuration
config <- read_yaml(paths$input$config.yaml)

# meta data on regions
region_meta <- read_csv(paths$input$region_metadata.csv, na = '.')

# constants specific to this analysis
cnst <- list(); cnst <- within(cnst, {
  # lookup table for region codes
  # only countries defined in skeleton
  region_lookup_hmd =
    region_meta |>
    filter(region_code_iso3166_2 %in% config$skeleton$region) |>
    select(region_code_iso3166_2, region_code_hmd) |>
    drop_na()
  region_lookup_wpp =
    region_meta |>
    filter(region_code_iso3166_2 %in% config$skeleton$region) |>
    select(region_code_iso3166_2, region_code_wpp) |>
    drop_na()
  # first year in harmonized data set
  skeleton_first_year = config$skeleton$year$start
  # last year in harmonized data set
  skeleton_final_year = config$skeleton$year$end
  # hmd region codes
  region_codes_hmd = region_lookup_hmd$region_code_hmd
  names(region_codes_hmd) = region_lookup_hmd$region_code_hmd
})

# list containers for analysis artifacts
wpp22 <- list()
wpp24 <- list()
wppjoint <- list()
hmdhfd <- list()

# Download WPP 22 data --------------------------------------------

# Files downloaded May 31 2024. Saved in ./dat/wpp22.
# Not available online anymore.

# download World Population Prospects 2022 as sensitivity check
# - single year-age population estimates 1950-2022
#   (january 1st & midyear)
# - single year-age population projections 2023-2100
#   (january 1st & midyear)
# wpp22$wpp_get <- map(
#   c(
#     wpp22_popjan1st_estimates =
#       paths$input$url_wpp22_jan1st_estimates,
#     wpp22_popjan1st_projections =
#       paths$input$url_wpp22_jan1st_projections,
#     wpp22_popmidyear_estimates =
#       paths$input$url_wpp22_midyear_estimates,
#     wpp22_popmidyear_projections =
#       paths$input$url_wpp22_midyear_projections,
#     wpp22_popmidyearcnstmo_projections =
#       paths$input$url_wpp22_midyear_projectionsconstantmortality
#   ), ~{
#     GET(url = .x, progress())
#   })

# save downloaded zip to file
# iwalk(wpp22$wpp_get, ~{
#   writeBin(
#     object = content(.x, 'raw'),
#     con = glue('{paths$input$tmpdir}/11-{.y}.zip')
#   )
# })

wpp22$wpp_get <-
  c(
    wpp22_popjan1st_estimates =
      paths$input$url_wpp22_jan1st_estimates,
    wpp22_popjan1st_projections =
      paths$input$url_wpp22_jan1st_projections,
    wpp22_popmidyear_estimates =
      paths$input$url_wpp22_midyear_estimates,
    wpp22_popmidyear_projections =
      paths$input$url_wpp22_midyear_projections
  )

# Download WPP 24 data --------------------------------------------

# download World Population Prospects 2024
# - single year-age population estimates
#   (january 1st & midyear)
# - single year-age population projections
#   (january 1st & midyear)
# - single year-age fertility rate projections
wpp24$wpp_get <- map(
  c(
    wpp24_popjan1st_estimates =
      paths$input$url_wpp24_jan1st_estimates,
    wpp24_popjan1st_projections =
      paths$input$url_wpp24_jan1st_projections,
    wpp24_popmidyear_estimates =
      paths$input$url_wpp24_midyear_estimates,
    wpp24_popmidyear_projections =
      paths$input$url_wpp24_midyear_projections,
    wpp24_fertility_projections =
      paths$input$url_wpp24_fertility_projections
  ), ~{
    GET(url = .x, progress())
  })

# save downloaded zip to file
iwalk(wpp24$wpp_get, ~{
  writeBin(
    object = content(.x, 'raw'),
    con = paste0(paths$output$wpp24, '/11-', .y, '.gz')
  )
})

# Download HMDHFD data --------------------------------------------

# download hmd midyear population for all regions
hmdhfd$hmd_midyear_pop <-
  map(cnst$region_codes_hmd, ~{
    cat('Download HMD midyear pop for', .x, '\n')
    readHMDweb(
      CNTRY = .x, item = 'Exposures_1x1',
      username = config$credentials$hmd_usr, password = config$credentials$hmd_pwd,
      fixup = TRUE
    )
  })

# download hmd Jan 1st population for all regions
hmdhfd$hmd_jan1st_pop <-
  map(cnst$region_codes_hmd, ~{
    cat('Download HMD Jan 1st pop for', .x, '\n')
    readHMDweb(
      CNTRY = .x, item = 'Population',
      username = config$credentials$hmd_usr, password = config$credentials$hmd_pwd,
      fixup = TRUE
    )
  })

# download hmd death rates
hmdhfd$hmd_mort <-
  map(cnst$region_codes_hmd, ~{
    cat('Download HMD death rates for', .x, '\n')
    readHMDweb(
      CNTRY = .x, item = 'Mx_1x1',
      username = config$credentials$hmd_usr, password = config$credentials$hmd_pwd,
      fixup = TRUE
    )
  })

# download hfd fertility rates
hmdhfd$hfd_fert <-
  map(cnst$region_codes_hmd, ~{
    cat('Download HFD fertility rates for', .x, '\n')
    readHFDweb(
      CNTRY = .x, item = 'asfrRR',
      username = config$credentials$hfd_usr, password = config$credentials$hfd_pwd,
      fixup = TRUE
    )
  })

# Preliminary format WPP 22 data ----------------------------------

# long format data frame of WPP population data
wpp22$wpp_unzip <-
  map2(
    .x = names(wpp22$wpp_get),
    .y = c(paths$input$zipfilename_wpp22_jan1st_estimates,
           paths$input$zipfilename_wpp22_jan1st_projections,
           paths$input$zipfilename_wpp22_midyear_estimates,
           paths$input$zipfilename_wpp22_midyear_projections),
    ~{
      pth <- paste0(paths$input$wpp22, '/11-', .x, '.zip')
      # unzip
      unz(pth, .y) |>
        read_csv() |>
        filter(
          # subset to regions of interest
          LocID %in% cnst$region_lookup_wpp$region_code_wpp,
          # subset to years of interest
          Time %in% seq(cnst$skeleton_first_year, cnst$skeleton_final_year, 1)
        ) |>
        mutate(population_source = .x)
    })
names(wpp22$wpp_unzip) <- names(wpp22$wpp_get)

# Preliminary format WPP 24 data ----------------------------------

# long format data frame of WPP population data
wpp24$wpp_unzip <-
  map(
    .x = names(wpp24$wpp_get), ~{
      pth <- paste0(paths$input$wpp24, '/11-', .x, '.gz')
      # unzip
      gzfile(pth) |>
        read_csv() |>
        filter(
          # subset to regions of interest
          LocID %in% cnst$region_lookup_wpp$region_code_wpp,
          # subset to years of interest
          Time %in% seq(cnst$skeleton_first_year, cnst$skeleton_final_year, 1)
        ) |>
        mutate(population_source = .x)
    })
names(wpp24$wpp_unzip) <- names(wpp24$wpp_get)

# Join WPP 22 and 24 data -----------------------------------------

# bind all WPP data in a long dataframe
wppjoint <- bind_rows(
  # wpp 22
  wpp22$wpp_unzip$wpp22_popjan1st_estimates |>
    filter(Variant %in% c('Medium')) |>
    select(ISO2_code, Time, AgeGrpStart, population_source,
           Female = PopFemale,
           Male = PopMale),
  wpp22$wpp_unzip$wpp22_popjan1st_projections |>
    filter(Variant %in% c('Medium')) |>
    select(ISO2_code, Time, AgeGrpStart, population_source,
           Female = PopFemale,
           Male = PopMale),
  wpp22$wpp_unzip$wpp22_popmidyear_estimates |>
    filter(Variant %in% c('Medium')) |>
    select(ISO2_code, Time, AgeGrpStart, population_source,
           Female = PopFemale,
           Male = PopMale),
  wpp22$wpp_unzip$wpp22_popmidyear_projections |>
    filter(Variant %in% c('Medium')) |>
    select(ISO2_code, Time, AgeGrpStart, population_source,
           Female = PopFemale,
           Male = PopMale),
  # wpp 24
  wpp24$wpp_unzip$wpp24_popjan1st_estimates |>
    filter(Variant %in% c('Medium')) |>
    select(ISO2_code, Time, AgeGrpStart, population_source,
           Female = PopFemale,
           Male = PopMale),
  wpp24$wpp_unzip$wpp24_popjan1st_projections |>
    filter(Variant %in% c('Medium')) |>
    select(ISO2_code, Time, AgeGrpStart, population_source,
           Female = PopFemale,
           Male = PopMale),
  wpp24$wpp_unzip$wpp24_popmidyear_estimates |>
    filter(Variant %in% c('Medium')) |>
    select(ISO2_code, Time, AgeGrpStart, population_source,
           Female = PopFemale,
           Male = PopMale),
  wpp24$wpp_unzip$wpp24_popmidyear_projections |>
    filter(Variant %in% c('Medium')) |>
    select(ISO2_code, Time, AgeGrpStart, population_source,
           Female = PopFemale,
           Male = PopMale),
  wpp24$wpp_unzip$wpp24_fertility_projections |>
    filter(Variant %in% c('Medium')) |>
    select(ISO2_code, Time, AgeGrpStart, population_source,
           Female = ASFR) |>
    mutate(Male = NA)
)

# Preliminary format of HMD data ----------------------------------

# subset to year range of interest and
# bundle all preliminaries for a projection
# (mortality, fertility, population) into a list for export

hmdhfd$hmd_jan1st_pop <-
  hmdhfd$hmd_jan1st_pop |>
  map(~{
    .x |>
      select(
        Year, Age,
        # Jan 1st
        Female1, Male1
      ) |>
      filter(Year >= cnst$skeleton_first_year)
  }) |>
  bind_rows(.id = 'region_code_hmd')

hmdhfd$hmd_midyear_pop <-
  hmdhfd$hmd_midyear_pop |>
  map(~{
    .x |>
      select(Year, Age, Female, Male) |>
      filter(Year >= cnst$skeleton_first_year)
  }) |>
  bind_rows(.id = 'region_code_hmd')

hmdhfd$hmd_mort <-
  hmdhfd$hmd_mort |>
  map(~{
    .x |>
      select(Year, Age, Female, Male) |>
      filter(Year >= cnst$skeleton_first_year)
  }) |>
  bind_rows(.id = 'region_code_hmd')

hmdhfd$hfd_fert <-
  hmdhfd$hfd_fert |>
  map(~{
    .x |>
      select(Year, Age, ASFR) |>
      filter(Year >= cnst$skeleton_first_year)
  }) |>
  bind_rows(.id = 'region_code_hmd')

# Export ----------------------------------------------------------

saveRDS(wppjoint, file = paths$output$wppjoint.rds)
saveRDS(hmdhfd, file = paths$output$hmdhfd.rds)
