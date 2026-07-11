# Clustering of e0 deficit trajectories

# Init --------------------------------------------------------------------

library(yaml)
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(qs2)
library(dtwclust)

# Constants ---------------------------------------------------------------

# input and output paths
setwd('.')
paths <- list()
paths$input <- list(
  config = './cfg/config.yaml',
  global = './src/_global_functions.R',
  region = './cfg/region_metadata.csv',
  deficits_and_excesses_sim.qs = './tmp/50-deficits_and_excesses_sim.qs'
)
paths$output <- list(
  deficit_clusters.csv = './out/51-deficit_clusters.csv',
  cluster_concordance_with_visual.csv = './out/51-cluster_concordance_with_visual.csv'
)

# global configuration
config <- read_yaml(paths$input$config)

# global objects and functions
global <- source(paths$input$global)

# constants specific to this analysis
cnst <- within(list(), {
  # region, years, sex, and age to cluster ex deficits over
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
    n <- length(ex_deficit)
    # position of peak deficit
    pos_peak_deficit <- which.min(ex_deficit)
    # position of neighbours of peak deficit
    pos_neighbours_peak_deficit <- c(pos_peak_deficit-1, pos_peak_deficit+1)
    if (pos_peak_deficit == 1) pos_neighbours_peak_deficit <- 2
    if (pos_peak_deficit == n) pos_neighbours_peak_deficit <- n-1
    abs(ex_deficit[pos_peak_deficit] - mean(ex_deficit[pos_neighbours_peak_deficit]))
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
    # the probability
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
      e0_2024 = mean_ex_deficit[[5]],
      # peak probability by year
      p_peak_2020 = peak_prob[[1]],
      p_peak_2021 = peak_prob[[2]],
      p_peak_2022 = peak_prob[[3]],
      p_peak_2023 = peak_prob[[4]],
      p_peak_2024 = peak_prob[[5]]
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
    mutate(
      significant_peak_or_trend = p_peak < alpha | p_trend < alpha,
      cluster = case_when(
        significant_peak_or_trend & p_peak_2021 >= (1 - alpha) ~ 'B',
        significant_peak_or_trend & peak_year >= 2022 ~ 'C',
        significant_peak_or_trend & peak_year < 2022 ~ 'A',
        .default = 'D'
      )) |>
    select(region_iso, cluster_alpha = cluster)

  # dtw
  dtw_data <- tslist(
    cluster_features |>
    select(starts_with('e0_')) |>
    as.matrix()
  ); names(dtw_data) <- cluster_features$region_iso
  # cluster the time series
  dtw_fit <-
    tsclust(
      dtw_data, k = 4,
      type = 'hierarchical',
      preproc = function(series) {
        tslist(lapply(series, function(x) {
          trajectory_range <- diff(range(x))
          normalized <- x - mean(x)
          normalized <- normalized / trajectory_range
          c(normalized, diff(normalized), trajectory_range)
        }))
      },
      distance = 'L2',
      control = hierarchical_control(method = "ward.D2"),
      seed = 1987
    )
  # assign cluster labels based on features of cluster average series
  dtw_features <- do.call('rbind',
    lapply(split(cluster_features, dtw_fit@cluster), function (l) {
      data.frame(
        randomness = mean(l$p_peak*l$p_range*l$p_trend),
        p_peak_2021 = mean(l$p_peak_2021),
        p_peak_22plus = mean(l$p_peak_2022+l$p_peak_2023+l$p_peak_2024)
      )
    }))
  dtw_D <- which.max(dtw_features$randomness); dtw_features[dtw_D,] <- NA
  dtw_B <- which.max(dtw_features$p_peak_2021); dtw_features[dtw_B,] <- NA
  dtw_C <- which.max(dtw_features$p_peak_22plus)
  dtw_A <- setdiff(1:4, c(dtw_D, dtw_B, dtw_C))
  dtw_lookup <- c(A = dtw_A, B = dtw_B, C = dtw_C, D = dtw_D)

  cluster_dtw <-
    cluster_features |>
    mutate(cluster = as.character(factor(
      dtw_fit@cluster,
      levels = dtw_lookup, labels = names(dtw_lookup))
    )) |>
    select(region_iso, cluster_dtw = cluster)

  clusters <-
    cluster_features |>
    left_join(cluster_fixed) |>
    left_join(cluster_alpha) |>
    left_join(cluster_dtw)

  return(clusters)

}

# Load data ---------------------------------------------------------------

# the manually choosen "visual" clustering
visual_clusters <-
  lapply(names(config$visualclusters), function(cluster) {
    tibble(
      region_iso = unlist(config$visualclusters[[cluster]]),
      cluster_visual = substr(cluster, 1, 1))
  }) |>
  bind_rows()

# ex deficit simulation draws by strata
deficits_and_excesses_sim <- qs_read(paths$input$deficits_and_excesses_sim.qs)

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
  relocate(cluster_visual, .after = cluster_dtw)

# Analyse concordance in cluster assignment -------------------------------

cluster_concordance_with_visual <-
  cluster_assignment |>
  select(region_iso, starts_with('cluster')) |>
  pivot_longer(cols = c('cluster_fixed', 'cluster_alpha', 'cluster_dtw')) |>
  mutate(match = cluster_visual == value) |>
  group_by(name) |>
  summarise(
    concordance = mean(match)
  )

# Tabulate cluster assignment ---------------------------------------------

deficit_clusters <-
  cluster_assignment |>
  select(region_iso, starts_with('cluster'))

# Export ------------------------------------------------------------------

write_csv(deficit_clusters, paths$output$deficit_clusters.csv)
write_csv(cluster_concordance_with_visual,
          paths$output$cluster_concordance_with_visual.csv)
