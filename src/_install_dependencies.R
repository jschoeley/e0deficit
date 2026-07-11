# Install required packages to local renv repository

# initialize project specific repository
renv::scaffold(
  # set remote repository
  repos = "https://packagemanager.posit.co/cran/2026-07-01"
)
renv::activate()

# if installing V8 on Linux
Sys.setenv(DOWNLOAD_STATIC_LIBV8 = 1)

# list required packages
# dput(unique(renv::dependencies()$Package))
packages <- c(
  "ragg",
  "svglite",
  "renv",
  "qs2",
  "ggplot2",
  "ISOweek",
  "purrr",
  "dplyr",
  "tidyr",
  "yaml",
  "glue",
  "HMDHFDplus",
  "httr",
  "readr",
  "readxl",
  "eurostat",
  "openxlsx",
  "scales",
  "gridExtra",
  "stringr",
  "ungroup",
  "lubridate",
  "abind",
  "StMoMo",
  "gt",
  "patchwork",
  "jschoeley/ggflagsplus@d799ac56a4365540b627e5fb6b0fa40a7eacf83e"
)

# install required packages
renv::install(packages, dependencies = "strong")
renv::snapshot()
