# Compare cluster assignment of deficits under different counterfactuals

# Init ------------------------------------------------------------

library(yaml)
library(readr); library(dplyr); library(tidyr)
library(ggplot2)

# Constants -------------------------------------------------------

# input and output paths
setwd('.')
paths <- list()
paths$input <- list(
  config.yaml = './cfg/config.yaml',
  global_functions.R = './src/_global_functions.R',
  region_metadata.R = './cfg/region_metadata.csv',
  lifetables.rds = './out/50-lifetables.rds',
  deficit_clusters.csv = './out/51-deficit_clusters.csv'
)
paths$output <- list(
  lc_estimates_since_2000.csv = './out/95-lc_estimates_since_2000.csv',
  sensitivity_deficit_model_choice.csv = './out/95-sensitivity_deficit_model_choice.csv',
  sensitivity_deficit_model_choice.svg = './out/95-sensitivity_deficit_model_choice.svg'
)

# global configuration
config <- read_yaml(paths$input$config.yaml)

# global functions
source(paths$input$global_functions.R)

# constants specific to this analysis
cnst <- within(list(), {
  region = read_csv(paths$input$region_metadata.R)
})

# Load data -------------------------------------------------------

e0observed <- readRDS(paths$input$lifetables.rds)

# Select and subset -------------------------------------------------------

lc_estimates_since_2000 <-
  e0observed |>
  filter(region_iso %in% config$showinoutput) |>
  filter(age == 0,
         year_int >= 2000) |>
  select(region_iso, sex, year = year_int, ex_mean, scenario) |>
  pivot_wider(names_from = scenario, values_from = ex_mean) |>
  rename(e0_actual = actual, e0_expected_lc = projected)

# Random walk extrapolation -----------------------------------------------

estimates_since_2000_by_model <-
  lc_estimates_since_2000 |>
  group_by(region_iso, sex) |>
  group_modify(~{
    fitting_data <- .x |> filter(year < 2020)
    lm_fit <- lm(e0_actual~year, data = fitting_data)
    e0_jumpoff <- fitting_data$e0_actual[nrow(fitting_data)]
    e0_drift <- mean(diff(fitting_data$e0_actual))
    data.frame(
      .x,
      e0_expected_rw = c(fitting_data$e0_actual, e0_jumpoff+1:5*e0_drift)
    )
  }) |>
  ungroup()

# Deficit calculation ---------------------------------------------

deficits_by_model <-
  estimates_since_2000_by_model |>
  pivot_longer(
    cols = c(e0_expected_lc,
             e0_expected_rw),
    names_to = 'model', values_to = 'e0_expected'
  ) |>
  mutate(
    e0_deficit = e0_actual - e0_expected
  )

clusters <- read_csv(paths$input$deficit_clusters.csv)

sensitivity_deficit_model_choice <- list()

sensitivity_deficit_model_choice$dat <-
  left_join(deficits_by_model, clusters) |>
  filter(year >= 2020, sex == 'Total') |>
  mutate(
    model = factor(
      model,
      levels = c('e0_expected_lc', 'e0_expected_rw'),
      labels = c('Lee-Carter', 'Random Walk with drift')
    )
  ) |>
  select(
    region_iso, sex, year, model,
    e0_actual, e0_expected, e0_deficit, cluster_dtw
  )

sensitivity_deficit_model_choice$fig <-
  sensitivity_deficit_model_choice$dat |>
  ggplot() +
  aes(x = year, y = e0_deficit, color = model,
      group = interaction(region_iso, model)) +
  geom_hline(yintercept = 0, color = 'grey50') +
  geom_line() +
  facet_grid(cluster_dtw~model) +
  labs(x = 'Year', y = 'LE deficit', color = 'Forecasting model') +
  MyGGplotTheme(show_legend = FALSE)

# Export ----------------------------------------------------------

lc_estimates_since_2000 |>
  mutate(across(.cols = where(is.numeric), .fns = ~round(.x,6))) |>
  write_csv(paths$output$lc_estimates_since_2000.csv)

ExportSVG(
  sensitivity_deficit_model_choice$fig,
  paths$output$sensitivity_deficit_model_choice.svg,
  width = 170, height = 190
)
