# Install required packages to local renv repository

# if installing V8 on Linux
Sys.setenv(DOWNLOAD_STATIC_LIBV8 = 1)

# set remote repository
options(
  repos = c(
    # install packages from CRAN as it was on November 1st 2025
    "CRAN" = "https://packagemanager.posit.co/cran/2025-11-01"
  )
)
# tell renv to use the posit package manager
renv::settings$ppm.enabled(value = TRUE)

# list required packages
# dput(unique(renv::dependencies()$Package))
packages <- c(
  "svglite",
  "renv",
  "qs",
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
renv::snapshot(type = 'all')
