# Cluster analysis of e0 deficit trajectories

# Init --------------------------------------------------------------------

library(yaml)
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)

# Constants ---------------------------------------------------------------

# input and output paths
setwd('.')
paths <- list()
paths$input <- list(
  config = 'cfg/config.yaml',
  global = './src/_global_functions.R',
  region = './cfg/region_metadata.csv',
  deficits_and_excesses_sim.qs = 'tmp/50-deficits_and_excesses_sim.qs'
)
paths$output <- list(
  deficit_clusters.csv = 'out/95-deficit_clusters.csv'
)

# global configuration
config <- read_yaml(paths$input$config)

# global objects and functions
global <- source(paths$input$global)

# constants specific to this analysis
cnst <- within(list(), {
  region = config$showinoutput
  years = as.character(config$forecast$jumpoff-1+1:config$forecast$h)
  sex = 'Total'
  age = '0'
})

# Functions ---------------------------------------------------------------

#' Calculate Time Series Features used For Clustering
#'
#' @param deficits_and_excesses_sim An array holding simulation draws of life
#'   expectancy deficits (50-deficits_and_excesses_sim.qs).
#' @param regions A vector of regions to subset the array to.
#' @param years A vector of years to subset the array to.
#' @param sex A vector of sexes to subset the array to.
#' @param age A single age to analyze life expectancy deficits at.
#'
#' @returns
#' A tibble with time series features by array strata.
GetClusterFeatures <- function(
    deficits_and_excesses_sim, regions, years, sex, age
) {

  peak_prominence <- function(ex_deficit) {
    sorted_deficits <- sort(ex_deficit)
    sorted_deficits[[2]] - sorted_deficits[[1]]
  }
  trajectory_range <- function(ex_deficit) {
    max(ex_deficit) - min(ex_deficit)
  }
  linear_slope <- function(ex_deficit) {
    unname(coef(lm(ex_deficit ~ seq_along(ex_deficit)))[[2]])
  }

  # array dimensions:
  # age, year, sex, region_iso, sim_id, var_id
  # sim_id 1 is the mean slot; stochastic draws start at sim_id 2.
  sim_ids <- as.character(2:(config$nsim+1))

  # for each region, calculate a range of time series features
  bind_rows(lapply(regions, function(region) {

    # simulation draws of life expectancy deficits
    deficit_draws <- deficits_and_excesses_sim[
      age, years, sex, region, sim_ids, "ex_actual_minus_expected",
      drop = TRUE
    ]
    # simulation draws of expected ex
    exexpected_draws <- deficits_and_excesses_sim[
      age, years, sex, region, sim_ids, "ex_expected",
      drop = TRUE
    ]
    # Mean actual - expected life expectancy by year
    mean_ex_deficit <- apply(deficit_draws, 1, mean)
    # Mean expected life expectancy by year
    mean_ex_expected <- apply(exexpected_draws, 1, mean)
    # the deficits one would expect under the null hypothesis of no true deficit
    ex_deficit_null_draws <- sweep(exexpected_draws, 1, mean_ex_expected, "-")
    # simulation draws of year with peak deficits
    peak_deficit_year_by_draw <-
      years[max.col(-t(deficit_draws), ties.method = "first")]
    # the probability of a given year being a peak year
    peak_prob <- prop.table(table(factor(peak_deficit_year_by_draw, levels = years)))
    # the difference between peak deficit and second largest deficit in years
    observed_peak_prominence <- peak_prominence(mean_ex_deficit)
    # the range of life expectancy deficits
    observed_trajectory_range <- trajectory_range(mean_ex_deficit)
    # the linear slope of life expectancy deficits
    observed_linear_slope <- linear_slope(mean_ex_deficit)

    tibble(
      region_iso = region,
      peak_year = as.integer(years[[which.min(mean_ex_deficit)]]),
      peak_prominence = observed_peak_prominence,
      trajectory_range = observed_trajectory_range,
      linear_slope = observed_linear_slope,
      # p-values for the probability of peak, range, and trend being >= observed under null hypothesis
      p_peak = mean(apply(ex_deficit_null_draws, 2, peak_prominence) >= observed_peak_prominence),
      p_range = mean(apply(ex_deficit_null_draws, 2, trajectory_range) >= observed_trajectory_range),
      p_trend = mean(abs(apply(ex_deficit_null_draws, 2, linear_slope)) >= abs(observed_linear_slope)),
      # deficits by year
      e0_2020 = mean_ex_deficit[[1]],
      e0_2021 = mean_ex_deficit[[2]],
      e0_2022 = mean_ex_deficit[[3]],
      e0_2023 = mean_ex_deficit[[4]],
      e0_2024 = mean_ex_deficit[[5]]
    )
  }))
}

AssignExDeficitClustersFromFeatures <- function(
    cluster_features,
    fixed_peak_prominence_threshold = 0.2,
    fixed_range_threshold = 0.75,
    alpha = 0.1
) {

  require(dplyr)

  # fixed thresholds
  cluster_fixed <- cluster_features |>
    mutate(cluster = case_when(
      # group D decision (no peak, flat trajectory)
      peak_prominence < fixed_peak_prominence_threshold &
        trajectory_range < fixed_range_threshold ~ 'D',
      # group A, B, C decision
      peak_year == 2020 ~ 'A',
      peak_year == 2021 ~ 'B',
      peak_year >= 2022 ~ 'C'
    )) |>
    select(region_iso, cluster_fixed = cluster)

  # fixed alpha
  cluster_alpha <- cluster_features |>
    mutate(cluster = case_when(
      # group D decision (no peak, flat trajectory)
      p_trend > alpha & p_peak > alpha ~ 'D',
      # group A, B, C decision
      peak_year == 2020 ~ 'A',
      peak_year == 2021 ~ 'B',
      peak_year >= 2022 ~ 'C'
    )) |>
    select(region_iso, cluster_alpha = cluster)

  # kmeans pvalues
  kmeans_input <-
    cluster_features |>
    mutate(p_structure = log(p_peak*p_range*p_trend+1e-6)) |>
    select(p_structure) |>
    as.matrix() |>
    scale()
  kmeans_fit <- kmeans(kmeans_input, centers = 2, nstart = 100)
  cluster_features$k_means_cluster <- kmeans_fit$cluster
  cluster_kmeans <- cluster_features |>
    mutate(cluster = case_when(
      # group D decision (no peak, flat trajectory)
      k_means_cluster == which.max(kmeans_fit$centers) ~ 'D',
      # group A, B, C decision
      peak_year == 2020 ~ 'A',
      peak_year == 2021 ~ 'B',
      peak_year >= 2022 ~ 'C'
    )) |>
    select(region_iso, cluster_kmeans = cluster)

  clusters <-
    cluster_features |>
    left_join(cluster_fixed) |>
    left_join(cluster_alpha) |>
    left_join(cluster_kmeans)

  return(clusters)

}

# Load data ---------------------------------------------------------------

# the manually choosen "visual" clustering
visual_clusters <-
  lapply(names(config$groups), function(group) {
    tibble(
      region_iso = unlist(config$groups[[group]]),
      cluster_visual = substr(group, 1, 1))
  }) |>
  bind_rows()

# ex deficit simulation draws by strata
deficits_and_excesses_sim <- qs::qread(paths$input$deficits_and_excesses_sim.qs)

# Assign clusters ---------------------------------------------------------

# calculate time series features for series of
# life expectancy deficits by region
cluster_features <-
  GetClusterFeatures(
    deficits_and_excesses_sim,
    region = cnst$region,
    years = cnst$years,
    sex = cnst$sex,
    age = cnst$age
  )

cluster_assignment <-
  AssignExDeficitClustersFromFeatures(cluster_features) |>
  left_join(visual_clusters, by = "region_iso") |>
  relocate(cluster_visual, .after = cluster_kmeans)

# Analyze concordance in cluster assignment -------------------------------

cluster_concordance <-
  cluster_assignment |>
  select(region_iso, starts_with('cluster')) |>
  pivot_longer(cols = c('cluster_fixed', 'cluster_alpha', 'cluster_kmeans')) |>
  mutate(match = cluster_visual == value) |>
  group_by(name) |>
  summarise(
    concordance = mean(match)
  )

# Tabulate cluster assignment ---------------------------------------------

clusters <-
  cluster_assignment |>
  select(region_iso, starts_with('cluster'))

# Export ------------------------------------------------------------------

write_csv(clusters, paths$output$deficit_clusters.csv)
