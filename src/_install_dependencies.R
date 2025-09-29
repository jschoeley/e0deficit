# Install required packages to local renv repository

# set remote repository
options(
  repos = c(
    # install packages from CRAN as it was on Sept 1st 2025
    "CRAN" = "https://packagemanager.posit.co/cran/2025-09-01"
  )
)
# tell renv to use the posit package manager
renv::settings$ppm.enabled(value = TRUE)

# list required packages
# dput(unique(renv::dependencies()$Package))
packages <- c(
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
  "jschoeley/ggflagsplus@d799ac56a4365540b627e5fb6b0fa40a7eacf83e",
  "cowplot"
)

# install required packages
renv::install(packages, dependencies = "strong")
