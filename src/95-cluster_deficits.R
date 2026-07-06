# Formalize visual clustering of e0 deficit trajectories

library(yaml)
library(readr)
library(dplyr)
library(tidyr)

paths <- list(
  input = list(
    config = "cfg/config.yaml",
    deficit_sim_qs = "tmp/50-deficits_and_excesses_sim.qs",
    deficit_by_year = "out/61-e0deficitbyyear_total.csv"
  ),
  output = list(
    clusters = "out/95-cluster_deficits.csv"
  )
)

years_for_clustering <- as.character(2020:2024)

params <- list(
  # A clear peak must be separated from the second-worst year by this many
  # years of life expectancy. Smaller peak separation is treated as cluster D
  # when the whole trajectory range is also modest.
  peak_prominence_threshold = 0.22,
  range_threshold = 0.75,
  # If 2021 is the formal minimum but 2020 is essentially tied, preserve the
  # visual "first wave" interpretation.
  first_wave_tie = 0.07
)

read_visual_groups <- function(config) {
  bind_rows(lapply(names(config$groups), function(group) {
    tibble(region_iso = unlist(config$groups[[group]]), visual_group = group)
  }))
}

classify_trajectory <- function(e0_deficit, years, params) {
  stopifnot(length(e0_deficit) == length(years))

  peak_index <- which.min(e0_deficit)
  peak_year <- years[[peak_index]]
  sorted_deficits <- sort(e0_deficit)
  peak_prominence <- sorted_deficits[[2]] - sorted_deficits[[1]]
  trajectory_range <- max(e0_deficit) - min(e0_deficit)

  is_prolonged_depression <-
    peak_prominence < params$peak_prominence_threshold &&
    trajectory_range < params$range_threshold

  if (is_prolonged_depression) {
    formal_group <- "D Prolonged depression"
  } else if (
    identical(peak_year, "2021") &&
    e0_deficit[[1]] - e0_deficit[[2]] <= params$first_wave_tie
  ) {
    formal_group <- "A First wave peak"
  } else if (identical(peak_year, "2020")) {
    formal_group <- "A First wave peak"
  } else if (identical(peak_year, "2021")) {
    formal_group <- "B Second wave peak"
  } else {
    formal_group <- "C Late peak"
  }

  tibble(
    formal_group = formal_group,
    peak_year = peak_year,
    peak_prominence = peak_prominence,
    trajectory_range = trajectory_range,
    is_prolonged_depression = is_prolonged_depression
  )
}

max_or_na <- function(x) {
  if (all(is.na(x))) {
    NA_real_
  } else {
    max(x, na.rm = TRUE)
  }
}

summarize_draws <- function(draw_array, regions, years) {
  # draw_array dimensions:
  # age, year, sex, region_iso, sim_id, var_id
  # sim_id 1 is the mean slot; stochastic draws start at sim_id 2.
  sim_ids <- setdiff(dimnames(draw_array)$sim_id, "1")

  bind_rows(lapply(regions, function(region) {
    draws <- draw_array[
      "0", years, "Total", region, sim_ids, "ex_actual_minus_expected",
      drop = TRUE
    ]

    if (is.null(dim(draws))) {
      draws <- matrix(draws, nrow = length(years), dimnames = list(years, sim_ids))
    }

    peak_year_by_draw <- years[max.col(-t(draws), ties.method = "first")]
    peak_prob <- prop.table(table(factor(peak_year_by_draw, levels = years)))

    tibble(
      region_iso = region,
      year = years,
      e0_deficit = apply(draws, 1, median, na.rm = TRUE),
      peak_probability = as.numeric(peak_prob)
    )
  }))
}

read_deficit_series <- function(paths, regions, years) {
  if (
    file.exists(paths$input$deficit_sim_qs) &&
    requireNamespace("qs", quietly = TRUE)
  ) {
    message("Reading simulation draws from ", paths$input$deficit_sim_qs)
    draw_array <- qs::qread(paths$input$deficit_sim_qs)
    return(list(
      source = paths$input$deficit_sim_qs,
      data = summarize_draws(draw_array, regions, years)
    ))
  }

  message(
    "Package 'qs' is unavailable; using ",
    paths$input$deficit_by_year,
    " as a deterministic summary fallback."
  )

  summary_data <-
    read_csv(paths$input$deficit_by_year, show_col_types = FALSE) |>
    mutate(year = as.character(year)) |>
    filter(region %in% regions, sex == "Total", year %in% years) |>
    transmute(
      region_iso = region,
      year,
      e0_deficit = e0deficit_Q50,
      peak_probability = NA_real_
    )

  list(source = paths$input$deficit_by_year, data = summary_data)
}

config <- read_yaml(paths$input$config)
visual_groups <- read_visual_groups(config)
regions <- visual_groups$region_iso

deficit_series <- read_deficit_series(paths, regions, years_for_clustering)

clusters <-
  deficit_series$data |>
  arrange(region_iso, match(year, years_for_clustering)) |>
  group_by(region_iso) |>
  group_modify(\(.x, .y) {
    classification <- classify_trajectory(.x$e0_deficit, .x$year, params)
    bind_cols(
      classification,
      tibble(
        e0_2020 = .x$e0_deficit[.x$year == "2020"],
        e0_2021 = .x$e0_deficit[.x$year == "2021"],
        e0_2022 = .x$e0_deficit[.x$year == "2022"],
        e0_2023 = .x$e0_deficit[.x$year == "2023"],
        e0_2024 = .x$e0_deficit[.x$year == "2024"],
        max_peak_probability = max_or_na(.x$peak_probability)
      )
    )
  }) |>
  ungroup() |>
  left_join(visual_groups, by = "region_iso") |>
  mutate(
    match_visual = formal_group == visual_group,
    data_source = deficit_series$source
  ) |>
  select(
    region_iso, formal_group, visual_group, match_visual,
    peak_year, peak_prominence, trajectory_range, is_prolonged_depression,
    e0_2020, e0_2021, e0_2022, e0_2023, e0_2024,
    max_peak_probability, data_source
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
