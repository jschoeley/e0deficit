# Tabulate life expectancy deficits

# Init ------------------------------------------------------------

library(yaml)
library(readr); library(dplyr); library(tidyr)
library(gt)

# Constants -------------------------------------------------------

# input and output paths
setwd('.')
paths <- list()
paths$input <- list(
  config.yaml = './cfg/config.yaml',
  global_functions.R = './src/_global_functions.R',
  region_metadata.csv = './cfg/region_metadata.csv',
  lifetables.rds = './out/50-lifetables.rds',
  deficits_and_excesses.rds = './out/50-deficits_and_excesses.rds',
  pval.rds = './out/50-pval.rds'
)
paths$output <- list(
  e0deficits_total.html = './out/71-e0deficits_total.html',
  e0deficits_male.html = './out/71-e0deficits_male.html',
  e0deficits_female.html = './out/71-e0deficits_female.html',
  e0deficits.csv = './out/71-e0deficits.csv'
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

dat$e0observed <-
  readRDS(paths$input$lifetables.rds) |>
  filter(region_iso %in% config$showinoutput)
dat$e0expected <-
  readRDS(paths$input$deficits_and_excesses.rds) |>
  filter(region_iso %in% config$showinoutput)
dat$pval <- readRDS(paths$input$pval.rds)

#dat$e0expected_sims <- readRDS(paths$input$e0expected_sims)
#dat$e0expected_sims <- dat$e0expected_sims[,,,config$showinoutput,,]

groups <- config$groups

# Create Table ----------------------------------------------------

e0deficits <- list()

e0deficits$data$e0observed <-
  dat$e0observed |>
  mutate(year = ifelse(is.na(year), '2020-2024', year)) |>
  filter(scenario == 'actual', age == 0, year %in% c(2010:2024, '2020-2024')) |>
  select(year, sex, region = region_iso, e0_actual = ex_mean)

e0deficits$data$e0expected <-
  dat$e0expected |> filter(age == '0') |>
  select(
    region = region_iso, sex, year,
    e0_expected_avg = ex_expected_mean,
    e0_expected_q050 = ex_expected_q0.05,
    e0_expected_q950 = ex_expected_q0.95,
    e0_deficit_avg = ex_actual_minus_expected_mean,
    e0_deficit_q050 = ex_actual_minus_expected_q0.05,
    e0_deficit_q950 = ex_actual_minus_expected_q0.95
  )

e0deficits$data$combine <-
  left_join(e0deficits$data$e0observed, e0deficits$data$e0expected, by = c('year', 'sex', 'region')) |>
  left_join(dat$pval, by = c('year', 'sex', 'region' = 'region_iso')) |>
  left_join(cnst$region, by = c('region' = 'region_code_iso3166_2')) |>
  filter(year %in% c(2020:2024, '2020-2024')) |>
  mutate(
    group = case_when(
      region %in% groups$`A First wave peak` ~ 'A First wave peak',
      region %in% groups$`B Second wave peak` ~ 'B Second wave peak',
      region %in% groups$`C Late peak` ~ 'C Late peak',
      region %in% groups$`D Prolonged depression` ~ 'D Prolonged depression'
    ),
    label = paste(group, region_name_en)
  )

e0deficits$table <- list()
for (s in c('Female', 'Male', 'Total')) {
  e0deficits$table[[s]] <-
    e0deficits$data$combine |>
    filter(sex == s) |>
    mutate(
      pval = ifelse(
        e0_deficit_pval < 0.001,
        'p<0.001',
        paste0('p=', formatC(e0_deficit_pval, format = 'f', digits = 3))
      )
    ) |>
    mutate(
      cell = paste0(
        formatC(e0_deficit_avg, format = 'f', digits = 2),
        ' (',
        formatC(e0_deficit_q050, format = 'f', digits = 2), '; ',
        formatC(e0_deficit_q950, format = 'f', digits = 2), ')', '\n',
        pval
      ),
      cell = ifelse(grepl(pattern = 'NA|NaN', cell), '.', cell)
    ) |>
    select(year, sex, region_name_en, cell) |>
    pivot_wider(names_from = year, values_from = cell) |>
    arrange(region_name_en, sex) |>
    gt() |>
    cols_hide(sex) |>
    cols_label(
      sex = '',
      region_name_en = ''
    )
}
e0deficits$table$Total

# Export ----------------------------------------------------------

gtsave(
  e0deficits$table$Total,
  path = paths$output$fig,
  filename = paths$output$e0deficits_total.html
)
gtsave(
  e0deficits$table$Female,
  path = paths$output$fig,
  filename = paths$output$e0deficits_female.html
)
gtsave(
  e0deficits$table$Male,
  path = paths$output$fig,
  filename = paths$output$e0deficits_male.html
)

e0deficits$data$combine |>
  mutate(across(.cols = where(is.numeric), .fns = ~round(.x,6))) |>
  write_csv(paths$output$e0deficits.csv)
