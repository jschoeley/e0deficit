# Formalize clustering of e0 deficit trajectories

library(yaml)
library(readr)
library(dplyr)

paths <- list(
  input = list(
    config = "cfg/config.yaml",
    deficit_sim_qs = "tmp/50-deficits_and_excesses_sim.qs"
  ),
  output = list(
    clusters = "out/95-cluster_deficits.csv",
    kmeans_scatterplot_matrix = "out/95-kmeans_scatterplot_matrix.pdf",
    method_comparison = "out/95-cluster_method_comparison.pdf"
  )
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

assign_peak_group <- function(
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

summarize_deficit_draws <- function(draw_array, regions, years, sex, age) {
  # draw_array dimensions:
  # age, year, sex, region_iso, sim_id, var_id
  # sim_id 1 is the mean slot; stochastic draws start at sim_id 2.
  if (is.null(names(dimnames(draw_array)))) {
    names(dimnames(draw_array)) <- names(dim(draw_array))
  }

  sim_ids <- setdiff(dimnames(draw_array)$sim_id, "1")

  bind_rows(lapply(regions, function(region) {
    draws <- draw_array[
      age, years, sex, region, sim_ids, "ex_actual_minus_expected",
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
    )
  }))
}

#' Assign clusters to countries based on e0 deficit simulation draws.
#'
#' @param deficit_sim_qs Path to `50-deficits_and_excesses_sim.qs`.
#' @param method One of:
#'   - `fixed_thresholds`: D if peak prominence and trajectory range are below
#'     fixed cutoffs.
#'   - `kmeans_pvalues`: D from unsupervised k-means on p-values for peak,
#'     range, and trend features.
#'   - `alpha_rule`: D from a p-value decision rule.
#' @param alpha For `alpha_rule`, D is assigned when peak and trend features
#'   are not significant: `p_trend > alpha & p_peak > alpha`.
#'
#' @return A tibble with one row per country and assigned `formal_group`.
AssignE0DeficitClusters <- function(
    deficit_sim_qs,
    method = c("kmeans_pvalues", "fixed_thresholds", "alpha_rule"),
    years = as.character(2020:2024),
    sex = "Total",
    age = "0",
    regions = NULL,
    nthreads = 1,
    fixed_peak_prominence_threshold = 0.22,
    fixed_range_threshold = 0.75,
    alpha = 0.05,
    seed = 1,
    nstart = 100
) {
  method <- match.arg(method)

  if (!file.exists(deficit_sim_qs)) {
    stop("Simulation draw file not found: ", deficit_sim_qs)
  }

  if (!requireNamespace("qs", quietly = TRUE)) {
    stop("Package 'qs' is required to read ", deficit_sim_qs)
  }

  message("Reading simulation draws from ", deficit_sim_qs)
  draw_array <- qs::qread(deficit_sim_qs, nthreads = nthreads)

  if (is.null(names(dimnames(draw_array)))) {
    names(dimnames(draw_array)) <- names(dim(draw_array))
  }

  if (is.null(regions)) {
    regions <- dimnames(draw_array)$region_iso
  }

  cluster_features <-
    summarize_deficit_draws(draw_array, regions, years, sex, age)

  AssignE0DeficitClustersFromFeatures(
    cluster_features,
    method = method,
    fixed_peak_prominence_threshold = fixed_peak_prominence_threshold,
    fixed_range_threshold = fixed_range_threshold,
    alpha = alpha,
    seed = seed,
    nstart = nstart,
    data_source = deficit_sim_qs
  )
}

AssignE0DeficitClustersFromFeatures <- function(
    cluster_features,
    method = c("kmeans_pvalues", "fixed_thresholds", "alpha_rule"),
    fixed_peak_prominence_threshold = 0.22,
    fixed_range_threshold = 0.75,
    alpha = 0.05,
    seed = 1,
    nstart = 100,
    data_source = NA_character_
) {
  method <- match.arg(method)

  cluster_features$unsupervised_cluster <- NA_integer_
  cluster_features$d_evidence_score <- NA_real_

  d_decision <-
    switch(
      method,
      fixed_thresholds =
        cluster_features$peak_prominence < fixed_peak_prominence_threshold &
        cluster_features$trajectory_range < fixed_range_threshold,
      kmeans_pvalues = {
        clustering_input <-
          cluster_features |>
          select(p_peak, p_range, p_trend) |>
          as.matrix() |>
          scale()

        set.seed(seed)
        unsupervised_fit <-
          kmeans(clustering_input, centers = 4, nstart = nstart)

        d_evidence_score <-
          rowMeans(select(cluster_features, p_peak, p_range, p_trend))

        cluster_d_evidence <-
          tapply(d_evidence_score, unsupervised_fit$cluster, mean)

        d_cluster <-
          as.integer(names(which.max(cluster_d_evidence)))

        cluster_features$unsupervised_cluster <- unsupervised_fit$cluster
        cluster_features$d_evidence_score <- d_evidence_score
        unsupervised_fit$cluster == d_cluster
      },
      alpha_rule =
        cluster_features$p_trend > alpha & cluster_features$p_peak > alpha
    )

  peak_groups <-
    mapply(
      assign_peak_group,
      cluster_features$peak_year,
      cluster_features$peak_probability_2020,
      cluster_features$peak_probability_2022,
      cluster_features$peak_probability_2023,
      cluster_features$peak_probability_2024
    )

  cluster_features |>
    mutate(
      method = method,
      is_prolonged_depression = d_decision,
      formal_group = if_else(
        is_prolonged_depression,
        "D Prolonged depression",
        peak_groups
      ),
      data_source = data_source
    ) |>
    select(
      region_iso, formal_group, method, is_prolonged_depression,
      peak_year, peak_prominence, trajectory_range, linear_slope,
      any_of(c("unsupervised_cluster", "d_evidence_score")),
      p_peak, p_range, p_trend,
      peak_probability_2020, peak_probability_2021, peak_probability_2022,
      peak_probability_2023, peak_probability_2024,
      e0_2020, e0_2021, e0_2022, e0_2023, e0_2024,
      data_source
    )
}

ExportKmeansScatterplotMatrix <- function(
    clusters,
    filename,
    width = 7,
    height = 7
) {
  required_columns <- c(
    "p_peak", "p_range", "p_trend", "unsupervised_cluster", "region_iso"
  )
  missing_columns <- setdiff(required_columns, names(clusters))

  if (length(missing_columns) > 0) {
    stop(
      "Cannot export k-means scatterplot matrix; missing columns: ",
      paste(missing_columns, collapse = ", ")
    )
  }

  plot_data <-
    clusters |>
    select(p_peak, p_range, p_trend)

  names(plot_data) <- c("Peak p-value", "Range p-value", "Trend p-value")

  cluster_id <- factor(clusters$unsupervised_cluster)
  cluster_colors <-
    setNames(
      c("#0072B2", "#D55E00", "#009E73", "#CC79A7")[seq_along(levels(cluster_id))],
      levels(cluster_id)
    )

  point_colors <- cluster_colors[cluster_id]

  panel_points <- function(x, y, ...) {
    points(x, y, ...)
    text(
      x, y,
      labels = clusters$region_iso,
      pos = 3,
      cex = 0.45,
      col = grDevices::adjustcolor(point_colors, alpha.f = 0.75)
    )
  }

  panel_hist <- function(x, ...) {
    usr <- par("usr")
    on.exit(par(usr = usr))
    par(usr = c(usr[1:2], 0, 1.5))
    h <- hist(x, plot = FALSE)
    y <- h$counts / max(h$counts)
    rect(h$breaks[-length(h$breaks)], 0, h$breaks[-1], y, col = "grey85", border = "white")
  }

  grDevices::pdf(filename, width = width, height = height, useDingbats = FALSE)
  on.exit(grDevices::dev.off())

  pairs(
    plot_data,
    lower.panel = panel_points,
    upper.panel = panel_points,
    diag.panel = panel_hist,
    col = point_colors,
    pch = 19,
    cex = 0.8,
    main = "K-means clusters of e0 deficit trajectory features"
  )

  legend(
    "topright",
    legend = paste("Cluster", levels(cluster_id)),
    col = cluster_colors,
    pch = 19,
    bty = "n",
    cex = 0.8,
    inset = 0.01
  )

  invisible(filename)
}

ExportClusterMethodComparison <- function(
    method_clusters,
    filename,
    width = 8,
    height = 9
) {
  required_columns <- c("region_iso", "method", "formal_group")
  missing_columns <- setdiff(required_columns, names(method_clusters))

  if (length(missing_columns) > 0) {
    stop(
      "Cannot export method comparison; missing columns: ",
      paste(missing_columns, collapse = ", ")
    )
  }

  method_levels <-
    c("visual", "fixed_thresholds", "kmeans_pvalues", "alpha_rule")

  method_labels <-
    c("Visual", "Fixed thresholds", "K-means p-values", "Alpha rule")

  group_levels <-
    c(
      "A First wave peak",
      "B Second wave peak",
      "C Late peak",
      "D Prolonged depression"
    )

  group_colors <-
    c(
      "A First wave peak" = "#0072B2",
      "B Second wave peak" = "#D55E00",
      "C Late peak" = "#009E73",
      "D Prolonged depression" = "#CC79A7"
    )

  plot_data <-
    method_clusters |>
    mutate(
      method = factor(method, levels = method_levels),
      formal_group = factor(formal_group, levels = group_levels)
    ) |>
    filter(!is.na(method))

  region_order <-
    plot_data |>
    filter(method == "kmeans_pvalues") |>
    arrange(formal_group, region_iso) |>
    pull(region_iso)

  plot_data <-
    plot_data |>
    mutate(region_iso = factor(region_iso, levels = rev(region_order)))

  grDevices::pdf(filename, width = width, height = height, useDingbats = FALSE)
  on.exit(grDevices::dev.off())

  par(mar = c(6, 6, 3, 8), xpd = NA)
  plot(
    NA,
    xlim = c(0.5, length(method_levels) + 0.5),
    ylim = c(0.5, length(region_order) + 0.5),
    xaxt = "n",
    yaxt = "n",
    xlab = "",
    ylab = "",
    bty = "n",
    main = "Cluster assignments by method"
  )

  axis(
    1,
    at = seq_along(method_levels),
    labels = method_labels,
    las = 2
  )
  axis(
    2,
    at = seq_along(levels(plot_data$region_iso)),
    labels = levels(plot_data$region_iso),
    las = 2,
    cex.axis = 0.65
  )

  abline(
    h = seq_along(region_order) + 0.5,
    v = seq_along(method_levels) + 0.5,
    col = "grey92",
    lwd = 0.7
  )

  points(
    x = as.integer(plot_data$method),
    y = as.integer(plot_data$region_iso),
    pch = 22,
    cex = 2.2,
    bg = group_colors[as.character(plot_data$formal_group)],
    col = "grey20"
  )

  text(
    x = as.integer(plot_data$method),
    y = as.integer(plot_data$region_iso),
    labels = substr(as.character(plot_data$formal_group), 1, 1),
    cex = 0.75,
    font = 2
  )

  legend(
    "right",
    inset = c(-0.34, 0),
    legend = group_levels,
    pt.bg = group_colors,
    pch = 22,
    pt.cex = 1.5,
    bty = "n",
    cex = 0.75
  )

  invisible(filename)
}

if (sys.nframe() == 0) {
  config <- read_yaml(paths$input$config)
  visual_groups <- read_visual_groups(config)

  if (!requireNamespace("qs", quietly = TRUE)) {
    stop("Package 'qs' is required to read ", paths$input$deficit_sim_qs)
  }

  message("Reading simulation draws from ", paths$input$deficit_sim_qs)
  draw_array <- qs::qread(paths$input$deficit_sim_qs, nthreads = 1)
  if (is.null(names(dimnames(draw_array)))) {
    names(dimnames(draw_array)) <- names(dim(draw_array))
  }

  cluster_features <-
    summarize_deficit_draws(
      draw_array,
      visual_groups$region_iso,
      as.character(2020:2024),
      "Total",
      "0"
    )

  clusters <-
    AssignE0DeficitClustersFromFeatures(
      cluster_features,
      method = "kmeans_pvalues",
      data_source = paths$input$deficit_sim_qs
    ) |>
    left_join(visual_groups, by = "region_iso") |>
    mutate(match_visual = formal_group == visual_group) |>
    relocate(visual_group, match_visual, .after = formal_group)

  write_csv(clusters, paths$output$clusters)
  ExportKmeansScatterplotMatrix(clusters, paths$output$kmeans_scatterplot_matrix)

  method_clusters <-
    bind_rows(
      visual_groups |>
        transmute(
          region_iso,
          method = "visual",
          formal_group = visual_group
        ),
      clusters |>
        select(region_iso, method, formal_group),
      AssignE0DeficitClustersFromFeatures(
        cluster_features,
        method = "fixed_thresholds",
        data_source = paths$input$deficit_sim_qs
      ) |>
        select(region_iso, method, formal_group),
      AssignE0DeficitClustersFromFeatures(
        cluster_features,
        method = "alpha_rule",
        data_source = paths$input$deficit_sim_qs
      ) |>
        select(region_iso, method, formal_group)
    )

  ExportClusterMethodComparison(method_clusters, paths$output$method_comparison)

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
}
