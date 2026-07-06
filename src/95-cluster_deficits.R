# Formalize visual clustering of e0 deficit trajectories

library(yaml)
library(readr)
library(dplyr)
library(tidyr)

paths <- list(
  input = list(
    config = "cfg/config.yaml",
    deficit_sim_qs = "tmp/50-deficits_and_excesses_sim.qs"
  ),
  output = list(
    clusters = "out/95-cluster_deficits.csv"
  )
)

years_for_clustering <- as.character(2020:2024)

params <- list(
  nthreads = 1,
  n_clusters = 4,
  seed = 1,
  nstart = 100
)

read_visual_groups <- function(config) {
  bind_rows(lapply(names(config$groups), function(group) {
    tibble(region_iso = unlist(config$groups[[group]]), visual_group = group)
  }))
}

peak_prominence <- function(e0_deficit) {
  sorted_deficits <- sort(e0_deficit)
  sorted_deficits[[2]] - sorted_deficits[[1]]
}

trajectory_range <- function(e0_deficit) {
  max(e0_deficit) - min(e0_deficit)
}

linear_slope <- function(e0_deficit) {
  unname(coef(lm(e0_deficit ~ seq_along(e0_deficit)))[[2]])
}

peak_group <- function(
    peak_year,
    peak_probability_2020,
    peak_probability_2022,
    peak_probability_2023,
    peak_probability_2024
) {
  if (identical(peak_year, "2020")) {
    return("A First wave peak")
  }

  if (identical(peak_year, "2021")) {
    late_peak_probability <- max(
      peak_probability_2022,
      peak_probability_2023,
      peak_probability_2024
    )

    if (
      peak_probability_2020 > 0 &&
      peak_probability_2020 > late_peak_probability
    ) {
      return("A First wave peak")
    }

    return("B Second wave peak")
  }

  "C Late peak"
}

summarize_draws <- function(draw_array, regions, years, visual_groups) {
  # draw_array dimensions:
  # age, year, sex, region_iso, sim_id, var_id
  # sim_id 1 is the mean slot; stochastic draws start at sim_id 2.
  if (is.null(names(dimnames(draw_array)))) {
    names(dimnames(draw_array)) <- names(dim(draw_array))
  }

  sim_ids <- setdiff(dimnames(draw_array)$sim_id, "1")

  bind_rows(lapply(regions, function(region) {
    draws <- draw_array[
      "0", years, "Total", region, sim_ids, "ex_actual_minus_expected",
      drop = TRUE
    ]

    if (is.null(dim(draws))) {
      draws <- matrix(draws, nrow = length(years), dimnames = list(years, sim_ids))
    }

    e0_deficit <- apply(draws, 1, median, na.rm = TRUE)
    null_draws <- sweep(draws, 1, e0_deficit, "-") + mean(e0_deficit)
    peak_year_by_draw <- years[max.col(-t(draws), ties.method = "first")]
    peak_prob <- prop.table(table(factor(peak_year_by_draw, levels = years)))
    observed_peak_prominence <- peak_prominence(e0_deficit)
    observed_trajectory_range <- trajectory_range(e0_deficit)
    observed_linear_slope <- linear_slope(e0_deficit)

    tibble(
      region_iso = region,
      peak_year = years[[which.min(e0_deficit)]],
      peak_prominence = observed_peak_prominence,
      trajectory_range = observed_trajectory_range,
      linear_slope = observed_linear_slope,
      p_peak = mean(apply(null_draws, 2, peak_prominence) >= observed_peak_prominence),
      p_range = mean(apply(null_draws, 2, trajectory_range) >= observed_trajectory_range),
      p_trend = mean(abs(apply(null_draws, 2, linear_slope)) >= abs(observed_linear_slope)),
      peak_probability_2020 = as.numeric(peak_prob[["2020"]]),
      peak_probability_2021 = as.numeric(peak_prob[["2021"]]),
      peak_probability_2022 = as.numeric(peak_prob[["2022"]]),
      peak_probability_2023 = as.numeric(peak_prob[["2023"]]),
      peak_probability_2024 = as.numeric(peak_prob[["2024"]]),
      e0_2020 = e0_deficit[[1]],
      e0_2021 = e0_deficit[[2]],
      e0_2022 = e0_deficit[[3]],
      e0_2023 = e0_deficit[[4]],
      e0_2024 = e0_deficit[[5]]
    ) |>
      left_join(visual_groups, by = "region_iso")
  }))
}

read_deficit_series <- function(paths, regions, years, visual_groups, params) {
  if (!file.exists(paths$input$deficit_sim_qs)) {
    stop("Simulation draw file not found: ", paths$input$deficit_sim_qs)
  }

  if (!requireNamespace("qs", quietly = TRUE)) {
    stop("Package 'qs' is required to read ", paths$input$deficit_sim_qs)
  }

  message("Reading simulation draws from ", paths$input$deficit_sim_qs)
  draw_array <- qs::qread(paths$input$deficit_sim_qs, nthreads = params$nthreads)
  list(
    source = paths$input$deficit_sim_qs,
    data = summarize_draws(draw_array, regions, years, visual_groups)
  )
}

config <- read_yaml(paths$input$config)
visual_groups <- read_visual_groups(config)
regions <- visual_groups$region_iso

deficit_series <- read_deficit_series(
  paths, regions, years_for_clustering, visual_groups, params
)

cluster_features <-
  deficit_series$data

clustering_input <-
  cluster_features |>
  select(p_peak, p_range, p_trend) |>
  as.matrix() |>
  scale()

set.seed(params$seed)
unsupervised_fit <-
  kmeans(
    clustering_input,
    centers = params$n_clusters,
    nstart = params$nstart
  )

d_evidence_score <-
  rowMeans(select(cluster_features, p_peak, p_range, p_trend))

cluster_d_evidence <-
  tapply(d_evidence_score, unsupervised_fit$cluster, mean)

d_cluster <-
  as.integer(names(which.max(cluster_d_evidence)))

clusters <-
  cluster_features |>
  mutate(
    unsupervised_cluster = unsupervised_fit$cluster,
    d_evidence_score = d_evidence_score,
    is_prolonged_depression = unsupervised_cluster == d_cluster,
    peak_group = mapply(
      peak_group,
      peak_year,
      peak_probability_2020,
      peak_probability_2022,
      peak_probability_2023,
      peak_probability_2024
    ),
    formal_group = if_else(
      is_prolonged_depression,
      "D Prolonged depression",
      peak_group
    ),
    match_visual = formal_group == visual_group,
    data_source = deficit_series$source
  ) |>
  select(
    region_iso, formal_group, visual_group, match_visual,
    peak_year, peak_prominence, trajectory_range, is_prolonged_depression,
    unsupervised_cluster, d_evidence_score, p_peak, p_range, p_trend,
    peak_probability_2020, peak_probability_2021, peak_probability_2022,
    peak_probability_2023, peak_probability_2024,
    e0_2020, e0_2021, e0_2022, e0_2023, e0_2024,
    data_source
  )

write_csv(clusters, paths$output$clusters)

concordance <- mean(clusters$match_visual)
cat(sprintf(
  "Cluster concordance with visual groups: %.1f%% (%d/%d)\n",
  100 * concordance,
  sum(clusters$match_visual),
  nrow(clusters)
))

if (any(!clusters$match_visual)) {
  cat("\nDiscordant assignments:\n")
  print(
    clusters |>
      filter(!match_visual) |>
      select(region_iso, formal_group, visual_group, peak_year,
             peak_prominence, trajectory_range),
    n = Inf
  )
}
