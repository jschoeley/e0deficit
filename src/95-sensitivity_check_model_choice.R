# Compare deficits under different counterfactuals

# Init ------------------------------------------------------------

library(yaml)
library(readr); library(dplyr); library(tidyr)
library(ggplot2); library(ggflagsplus); library(patchwork)

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
  estimates_by_model.csv = './out/95-estimates_by_model.csv',
  deficit_cluster_by_model.csv = './out/95-deficit_cluster_by_model.csv',
  deficit_cluster_by_model.svg = './out/95-deficit_cluster_by_model.svg',
  deficit_rank_by_model.csv = './out/95-deficit_rank_by_model.csv',
  deficit_rank_by_model.svg = './out/95-deficit_rank_by_model.svg',
  expected_by_model.csv = './out/95-expected_by_model.csv',
  expected_by_model.svg = './out/95-expected_by_model.svg'
)

# global configuration
config <- read_yaml(paths$input$config.yaml)

# global functions
source(paths$input$global_functions.R)

# constants specific to this analysis
cnst <- within(list(), {
  region = read_csv(paths$input$region_metadata.R)
})

# Function --------------------------------------------------------

CalculateLifeTable <-
  function (df, x, nx, nmx) {

    require(dplyr)

    df |>
      transmute(
        x = {{x}},
        nx = {{nx}},
        px = exp(-{{nmx}}*{{nx}}),
        qx = 1-px,
        lx = head(cumprod(c(1, px)), -1),
        dx = c(-diff(lx), tail(lx, 1)),
        Lx = ifelse({{nmx}}==0, lx*nx, dx/{{nmx}}),
        Tx = rev(cumsum(rev(Lx))),
        ex = Tx/lx
      )

  }

# Load data -------------------------------------------------------

lifetables <-
  readRDS(paths$input$lifetables.rds) |>
  filter(region_iso %in% config$showinoutput) |>
  filter(year_int >= 2000) |>
  select(region_iso, sex, age, year = year_int, ex_mean, nmx_mean, scenario) |>
  pivot_wider(names_from = scenario, values_from = c(ex_mean, nmx_mean))

clusters <- read_csv(paths$input$deficit_clusters.csv)

# Observed e0 -------------------------------------------------------------

e0_actual <-
  lifetables |>
  filter(age == 0) |>
  select(region_iso, sex, year, e0_actual = ex_mean_actual)

# Original Lee Carter counterfactual --------------------------------------

e0_expected_lc <-
  lifetables |>
  filter(age == 0, year >= config$forecast$jumpoff) |>
  select(region_iso, sex, year, e0_expected_lc = ex_mean_projected)

# Random walk extrapolation ---------------------------------------

e0_expected_e0rw <-
  lifetables |>
  filter(age == 0) |>
  group_by(region_iso, sex) |>
  group_modify(~{
    fitting_data <- .x |> filter(year < config$forecast$jumpoff)
    e0_jumpoff <- fitting_data$ex_mean_actual[nrow(fitting_data)]
    e0_drift <- mean(diff(fitting_data$ex_mean_actual), na.rm = TRUE)
    data.frame(
      .x,
      e0_expected_e0rw = c(fitting_data$ex_mean_actual,
                           e0_jumpoff+1:config$forecast$h*e0_drift)
    )
  }) |>
  ungroup() |>
  filter(year >= config$forecast$jumpoff) |>
  select(region_iso, sex, year, e0_expected_e0rw)

# Age specific death rate extrapolation ---------------------------

e0_expected_nmxrw <-
  lifetables |>
  group_by(region_iso, sex, age) |>
  group_modify(~{
    fitting_data <- .x |> filter(year < config$forecast$jumpoff)
    nmx_series <- fitting_data$nmx_mean_actual
    log_nmx_series <- log(fitting_data$nmx_mean_actual)
    log_nmx_jumpoff <- log_nmx_series[nrow(fitting_data)]
    log_nmx_drift <- mean(diff(log_nmx_series), na.rm = TRUE)

    nmx_forecast <- exp(log_nmx_jumpoff+1:config$forecast$h*log_nmx_drift)
    if (any(nmx_series==0, na.rm = TRUE)) {
      nmx_forecast <- rep(mean(nmx_series, na.rm = TRUE), nrow(.x))
    }
    if (all(nmx_series==0, na.rm = TRUE)) {
      nmx_forecast <- rep(0, nrow(.x))
    }

    tibble(
      .x,
      nmx_expected = c(fitting_data$nmx_mean_actual, nmx_forecast)
    )
  }) |>
  group_by(region_iso, sex, year) |>
  group_modify(~{
    CalculateLifeTable(.x, age, 1, nmx_expected)
  }) |>
  ungroup() |>
  filter(x == 0, year >= config$forecast$jumpoff) |>
  select(region_iso, sex, year, e0_expected_nmxrw = ex)

# Counterfactual comparison table ---------------------------------

estimates_by_model <-
  e0_actual |>
  left_join(e0_expected_lc) |>
  left_join(e0_expected_e0rw) |>
  left_join(e0_expected_nmxrw)

# Deficit calculation ---------------------------------------------

deficits_by_model <-
  estimates_by_model |>
  pivot_longer(
    cols = c(e0_expected_lc,
             e0_expected_e0rw,
             e0_expected_nmxrw),
    names_to = 'model', values_to = 'e0_expected'
  ) |>
  mutate(
    e0_deficit = e0_actual - e0_expected
  )

# Deficit cluster by model ----------------------------------------

deficit_cluster_by_model <- list()

deficit_cluster_by_model$dat <-
  left_join(deficits_by_model, clusters) |>
  filter(year >= config$forecast$jumpoff, sex == 'Total') |>
  mutate(
    model = factor(
      model,
      levels = c('e0_expected_lc', 'e0_expected_e0rw', 'e0_expected_nmxrw'),
      labels = c('Lee-Carter', 'LE random walk with drift', 'ASDR random walk with drift')
    )
  ) |>
  select(
    region_iso, sex, year, model,
    e0_actual, e0_expected, e0_deficit, orig_cluster_assignment = cluster_dtw
  )

deficit_cluster_by_model$fig <-
  deficit_cluster_by_model$dat |>
  ggplot() +
  aes(x = year, y = e0_deficit, color = model,
      group = interaction(region_iso, model)) +
  geom_hline(yintercept = 0, color = 'grey50') +
  geom_line() +
  facet_grid(orig_cluster_assignment~model) +
  labs(x = 'Year', y = 'LE deficit', color = 'Forecasting model') +
  MyGGplotTheme(show_legend = FALSE)

# Rank order ------------------------------------------------------

deficit_rank_by_model <- list()

deficit_rank_by_model$dat <-
  deficit_cluster_by_model$dat |>
  group_by(sex, year, model) |>
  mutate(rank_e0_deficit = rank(e0_deficit)) |>
  ungroup() |>
  select(region_iso, sex, year, model, rank_e0_deficit) |>
  pivot_wider(names_from = model, values_from = rank_e0_deficit) |>
  mutate(
    ggflag_region = tolower(region_iso),
    # use uk flag for NIR as this is the most widely agreed upon flag
    ggflag_region = if_else(ggflag_region == 'gb-nir', 'gb', ggflag_region)
  )


deficit_rank_by_model$cnst <- within(list(), {
  breaks_rank <-
    seq(1,max(unique(deficit_rank_by_model$dat$`Lee-Carter`)))
  labels_rank <-
    c('Highest', rep('', max(breaks_rank)-2), 'Lowest')
})

deficit_rank_by_model$fig$a <-
  deficit_rank_by_model$dat |>
  filter(sex == 'Total') |>
  ggplot(aes(x = `LE random walk with drift`, y = `Lee-Carter`)) +
  geom_abline(linewidth = 2, color = 'grey75') +
  geom_abline(
    intercept = 6, slope = 1, linewidth = 1, color = 'grey75'
  ) +
  geom_abline(
    intercept = -6, slope = 1, linewidth = 1, color = 'grey75'
  ) +
  geom_point(size = 3.5) +
  geom_flag(
    aes(country = ggflag_region), size = 3
  ) +
  coord_equal() +
  MyGGplotTheme(grid = 'xy', axis = 'xy') +
  labs(x = 'LE deficit rank under LE RW with drift',
       y = 'LE deficit rank under Lee-Carter') +
  theme(
    panel.grid.major.x = element_line(linetype = 'solid'),
    panel.grid.major.y = element_line(linetype = 'solid')
  ) +
  scale_x_reverse(
    breaks = deficit_rank_by_model$cnst$breaks_rank,
    labels = deficit_rank_by_model$cnst$labels_rank
  ) +
  scale_y_reverse(
    breaks = deficit_rank_by_model$cnst$breaks_rank,
    labels = deficit_rank_by_model$cnst$labels_rank
  ) +
  MyGGplotTheme(grid = 'xy', axis = 'xy') +
  theme(
    panel.grid.major.x = element_line(linetype = 'solid'),
    panel.grid.major.y = element_line(linetype = 'solid')
  ) +
  facet_wrap(~year)

deficit_rank_by_model$fig$b <-
  deficit_rank_by_model$dat |>
  filter(sex == 'Total') |>
  ggplot(aes(x = `ASDR random walk with drift`, y = `Lee-Carter`)) +
  geom_abline(linewidth = 2, color = 'grey75') +
  geom_abline(
    intercept = 6, slope = 1, linewidth = 1, color = 'grey75'
  ) +
  geom_abline(
    intercept = -6, slope = 1, linewidth = 1, color = 'grey75'
  ) +
  geom_point(size = 3.5) +
  geom_flag(
    aes(country = ggflag_region), size = 3
  ) +
  coord_equal() +
  MyGGplotTheme(grid = 'xy', axis = 'xy') +
  labs(x = 'LE deficit rank under ASDR RW with drift',
       y = 'LE deficit rank under Lee-Carter') +
  theme(
    panel.grid.major.x = element_line(linetype = 'solid'),
    panel.grid.major.y = element_line(linetype = 'solid')
  ) +
  scale_x_reverse(
    breaks = deficit_rank_by_model$cnst$breaks_rank,
    labels = deficit_rank_by_model$cnst$labels_rank
  ) +
  scale_y_reverse(
    breaks = deficit_rank_by_model$cnst$breaks_rank,
    labels = deficit_rank_by_model$cnst$labels_rank
  ) +
  MyGGplotTheme(grid = 'xy', axis = 'xy') +
  theme(
    panel.grid.major.x = element_line(linetype = 'solid'),
    panel.grid.major.y = element_line(linetype = 'solid')
  ) +
  facet_wrap(~year)

deficit_rank_by_model$fig$total <-
  deficit_rank_by_model$fig$a /
  deficit_rank_by_model$fig$b +
  plot_annotation(tag_levels = 'A')

# Expected by model -----------------------------------------------

expected_by_model <- list()

expected_by_model$dat <-
  deficits_by_model |>
  filter(sex == 'Total') |>
  mutate(
    model = factor(
      model,
      levels = c('e0_expected_lc', 'e0_expected_e0rw', 'e0_expected_nmxrw'),
      labels = c('Lee-Carter', 'LE random walk with drift', 'ASDR random walk with drift')
    )
  )

expected_by_model$fig <-
  expected_by_model$dat |>
  left_join(cnst$region, by = c('region_iso' = 'region_code_iso3166_2')) |>
  ggplot() +
  geom_point(
    aes(x = year, y = e0_actual),
    data = . %>% filter(model == 'Lee-Carter'),
    size = 0.1
  ) +
  geom_line(
    aes(x = year, y = e0_expected, color = model),
    data = . %>% filter(year >= config$forecast$jumpoff)
  ) +
  scale_x_continuous(breaks = c(2000, 2024), labels = c("'00", "'24")) +
  facet_wrap(~region_name_en) +
  MyGGplotTheme() +
  labs(x = 'Year', y = 'Life expectancy', color = 'Forecasting model')

# Export ----------------------------------------------------------

estimates_by_model |>
  mutate(across(.cols = where(is.numeric), .fns = ~round(.x,6))) |>
  write_csv(paths$output$estimates_by_model.csv)

deficit_cluster_by_model$dat |>
  mutate(across(.cols = where(is.numeric), .fns = ~round(.x,6))) |>
  write_csv(paths$output$deficit_cluster_by_model.csv)
ExportSVG(
  deficit_cluster_by_model$fig,
  paths$output$deficit_cluster_by_model.svg,
  width = 170, height = 190
)

deficit_rank_by_model$dat |>
  mutate(across(.cols = where(is.numeric), .fns = ~round(.x,6))) |>
  write_csv(paths$output$deficit_rank_by_model.csv)
ExportSVG(
  deficit_rank_by_model$fig$total,
  paths$output$deficit_rank_by_model.svg,
  width = 170, height = 240
)

expected_by_model$dat |>
  mutate(across(.cols = where(is.numeric), .fns = ~round(.x,6))) |>
  write_csv(paths$output$expected_by_model.csv)
ExportSVG(
  expected_by_model$fig,
  paths$output$expected_by_model.svg,
  width = 170, height = 170
)
