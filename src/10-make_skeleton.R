# Create data base skeleton
#
# Here we define the "skeleton" of the data base used
# for analysis. It's a definition of years, ages, sexes, and
# regions that we wish to acquire data for. The order of the rows
# in the skeleton is crucial and must not be changed as the matrix
# operations employed later on depend on the row-order pre-specified
# here.

# Init ------------------------------------------------------------

library(yaml)
library(dplyr); library(tidyr)

# Constants -------------------------------------------------------

# input and output paths
setwd('.')
paths <- list()
paths$input <- list(
  tmpdir = './tmp',
  global_functions.R = './src/_global_functions.R',
  config.yaml = './cfg/config.yaml'
)
paths$output <- list(
  harmonized_skeleton.rds = './tmp/10-harmonized_skeleton.rds'
)

# global configuration
config <- read_yaml(paths$input$config.yaml)

# list containers for analysis artifacts
dat <- list()

# Functions -------------------------------------------------------

source(paths$input$global_functions.R)

# Generate skeleton -----------------------------------------------

# don't sort, don't change order of variables
# the conversion of these vectors to matrices later on depends
# on the row-order specified here
dat$skeleton <-
  expand_grid(
    region =
      config$skeleton$region,
    sex =
      unlist(config$skeleton$sex),
    year =
      seq(config$skeleton$year$start, config$skeleton$year$end, 1) %>%
      as.integer(),
    tibble(
      age_start = seq(config$skeleton$age$start,
                      config$skeleton$age$end, 1),
      age_width = c(diff(age_start), Inf)
    )
  )

# Add unique row id -----------------------------------------------

dat$skeleton <-
  dat$skeleton %>%
  mutate(
    id = GenerateRowID(region, sex, year, age_start)
  )

# Define order of rows and columns --------------------------------

col_order <- quos(id, region, sex, year, age_start, age_width)
dat$skeleton <-
  dat$skeleton %>%
  select(!!!col_order)

# Export ---------------------------------------------------------

saveRDS(dat$skeleton, file = paths$output$harmonized_skeleton.rds)
