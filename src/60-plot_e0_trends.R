# Plot life expectancy trends

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
  deficits_and_excesses.rds = './out/50-deficits_and_excesses.rds'
)
paths$output <- list(
  tmpdir = paths$input$tmpdir,
  e0trends.svg = './out/60-e0trends.svg',
  e0trends.csv = './out/60-e0trends.csv',
  e0deficittypology.svg = './out/60-e0deficittypology.svg',
  e0deficittypology.csv = './out/60-e0deficittypology.csv'
)

# global configuration
config <- read_yaml(paths$input$config.yaml)

# global objects and functions
source(paths$input$global_functions.R)

# constants specific to this analysis
cnst <- within(list(), {
  region = filter(
    read_csv(paths$input$region_metadata.R)
  )
})

dat <- list()

# Load data -------------------------------------------------------

dat$e0observed <- readRDS(paths$input$lifetables.rds)
dat$e0expected <- readRDS(paths$input$deficits_and_excesses.rds)

groups <- config$groups

# Basic formatting ------------------------------------------------

dat$e0observed <-
  dat$e0observed |>
  filter(region_iso %in% config$showinoutput) |>
  filter(age == 0, sex == 'Total', scenario == 'actual',
         year_int >= 2010 | year == '2020-2024') |>
  mutate(
    group = case_when(
      region_iso %in% groups$`A First wave peak` ~ 'A First wave peak',
      region_iso %in% groups$`B Second wave peak` ~ 'B Second wave peak',
      region_iso %in% groups$`C Late peak` ~ 'C Late peak',
      region_iso %in% groups$`D Prolonged depression` ~ 'D Prolonged depression'
    )
  ) |>
  select(group, region_iso, year, year_int, e0_actual = ex_mean)

dat$e0expected <-
  dat$e0expected |>
  filter(region_iso %in% config$showinoutput) |>
  filter(age == 0, sex == 'Total') |>
  mutate(
    year_int = ifelse(year == '2020-2024', NA_real_, as.numeric(year)),
    group = case_when(
      region_iso %in% groups$`A First wave peak` ~ 'A First wave peak',
      region_iso %in% groups$`B Second wave peak` ~ 'B Second wave peak',
      region_iso %in% groups$`C Late peak` ~ 'C Late peak',
      region_iso %in% groups$`D Prolonged depression` ~ 'D Prolonged depression'
    )
  ) |>
  select(
    group, region_iso, year, year_int,
    e0_expected_avg = ex_expected_mean,
    e0_expected_q025 = ex_expected_q0.025,
    e0_expected_q975 = ex_expected_q0.975,
    e0_deficit_avg = ex_actual_minus_expected_mean,
    e0_deficit_q025 = ex_actual_minus_expected_q0.025,
    e0_deficit_q975 = ex_actual_minus_expected_q0.975
  )

# Calculate and plot e0 -------------------------------------------

e0trends <- list()

e0trends$data$observedexpected <-
  left_join(dat$e0observed, dat$e0expected) |>
  left_join(cnst$region, by = c('region_iso' = 'region_code_iso3166_2'))

# this ensures equal y-scaling across facets but with a sliding y-window
e0trends$data$padding <-
  e0trends$data$observedexpected |>
  filter(year != '2020-2024') |>
  group_by(group, region_iso, region_name_en) |>
  summarise(
    ymin = min(c(e0_actual, e0_expected_q025), na.rm = TRUE),
    ymax = max(c(e0_actual, e0_expected_q975), na.rm = TRUE),
    yrange = ymax-ymin,
    ypadding = (5.65-yrange)/2,
    ymin_padded = ymin-ypadding,
    ymax_padded = ymax+ypadding
  ) |>
  ungroup()

e0trends$data$deficitsummary_by_country <-
  e0trends$data$observedexpected |>
  group_by(region_iso, region_name_en, group) |>
  summarise(
    peak_deficit = min(e0_deficit_avg[year %in% 2020:2024]),
    fiveyear_deficit = e0_deficit_avg[year == '2020-2024']
  ) |>
  select(group, region_iso, region_name_en, peak_deficit, fiveyear_deficit) |>
  ungroup()

e0trends$data$deficitsummary_global <-
  e0trends$data$deficitsummary_by_country |>
  group_by(group) |>
  summarise(
    n = n(),
    peak_deficit_avg = mean(peak_deficit, na.rm = TRUE),
    peak_deficit_sd = sd(peak_deficit, na.rm = TRUE),
    fiveyear_deficit_avg = mean(fiveyear_deficit, na.rm = TRUE),
    fiveyear_deficit_sd = sd(fiveyear_deficit, na.rm = TRUE)
  ) |>
  ungroup()

e0trends$subfig <- list()

for (i in 1:length(groups)) {
  group_regions <- groups[[i]]
  group_name <- names(groups[i])
  cat(group_name, '\n')

  observed_expected <-
    e0trends$data$observedexpected |>
    filter(region_iso %in% group_regions, year != '2020-2024')

  flag_positions <-
    e0trends$data$padding |>
    filter(region_iso %in% group_regions) |>
    group_by(region_iso, region_name_en) |>
    summarise(
      ymax_padded = ymax_padded[1]
    ) |>
    mutate(
      region_ggflag = tolower(region_iso),
      # use uk flag for NIR as this is the most widely agreed upon flag
      region_ggflag = if_else(region_ggflag == 'gb-nir', 'gb', region_ggflag)
    ) |>
    ungroup()

  country_summaries <-
    e0trends$data$deficitsummary_by_country |>
    filter(region_iso %in% group_regions)

  facet_labels <- paste0(
    country_summaries$region_name_en, '\n',
    'LE deficit: ',
    formatC(country_summaries$peak_deficit, format = 'f', digits = 1),
    ' (peak), ',
    formatC(country_summaries$fiveyear_deficit, format = 'f', digits = 1),
    ' (5-year)'
  )
  names(facet_labels) <- country_summaries$region_name_en
  facet_labeller <- as_labeller(
    facet_labels
  )

  e0trends$subfig[[group_name]] <-
    observed_expected |>
    ggplot(aes(x = year_int)) +
    geom_vline(xintercept = 2019.5, color = 'grey60') +
    geom_ribbon(
      aes(x = year_int, ymin = e0_expected_q025, ymax = e0_expected_q975),
      color = NA, fill = 'grey70'
    ) +
    geom_line(
      aes(x = year_int, y = e0_expected_avg),
    ) +
    geom_line(
      aes(y = e0_actual),
      data = . %>% filter(year_int < 2020)
    ) +
    geom_point(aes(y = e0_actual), data = . %>% filter(year < 2020),
               shape = 21, fill = 'white') +
    geom_point(
      aes(y = e0_actual), shape = 21, color = 'white', fill = 'black',
      data = . %>% filter(year >= 2020)
    ) +
    geom_point(aes(x = 2010.5, y = ymax_padded-0.2), size = 5.5,
               data = flag_positions) +
    geom_flag(
      aes(x = 2010.5, y = ymax_padded-0.2, country = region_ggflag),
      size = 5, data = flag_positions
    ) +
    # fake data to make each panel have equal range but shifted
    geom_linerange(
      aes(x = NA_real_, ymin = ymin_padded, ymax = ymax_padded),
      data = e0trends$data$padding |> filter(group %in% group_name)
    ) +
    scale_x_continuous(
      breaks = seq(2010, 2024, 1),
      labels = c('2010', rep('', 9), "'20", rep('', 3), "'24"),
      limits = c(2010, 2024),
    ) +
    scale_y_continuous(breaks = seq(70, 90, 1), expand = c(0, 0.2)) +
    coord_cartesian(clip = 'off') +
    MyGGplotTheme(grid = 'y', axis = 'x', panel_border = FALSE) +
    labs(
      y = 'Period life expectancy in years', x = NULL, fill = '',
      title = group_name
    ) +
    facet_wrap(
      ~region_name_en, ncol = 5, scales = 'free_y',
      labeller = facet_labeller
    )
}

e0trends$fig <-
  e0trends$subfig$`A First wave peak` /
  e0trends$subfig$`B Second wave peak` /
  e0trends$subfig$`C Late peak` /
  (e0trends$subfig$`D Prolonged depression`) +
  plot_layout(heights = c(2/9, 3/9, 2/9, 2/9))

e0trends$fig

# Plot Typology of e0 deficits ------------------------------------

e0deficittypology <- list()

e0deficittypology$data$bycountry <-
  e0trends$data$observedexpected |>
  filter(year_int %in% 2020:2024) |>
  select(group, region_iso, year = year_int, e0_deficit_avg)

e0deficittypology$data$groupmeans <-
  e0deficittypology$data$bycountry |>
  group_by(group, year) |>
  summarise(e0_deficit_avg = mean(e0_deficit_avg)) |>
  ungroup()

e0deficittypology$data$flag_positions <-
  e0deficittypology$data$bycountry |>
  group_by(group) |>
  reframe(region_iso = unique(region_iso)) |>
  group_by(group) |>
  mutate(
    n = 1:n(),
    region_ggflag = tolower(region_iso),
    # use uk flag for NIR as this is the most widely agreed upon flag
    region_ggflag = if_else(region_ggflag == 'gb-nir', 'gb', region_ggflag)
  )

e0deficittypology$facet_labels <- paste0(
  names(groups), '\n',
  '5-year deficit: ',
  formatC(e0trends$data$deficitsummary_global$fiveyear_deficit_avg, format = 'f', digits = 2),
  ' (avg) ',
  formatC(e0trends$data$deficitsummary_global$fiveyear_deficit_sd, format = 'f', digits = 2),
  ' (sd)', '\n',
  'Peak deficit: ',
  formatC(e0trends$data$deficitsummary_global$peak_deficit_avg, format = 'f', digits = 2),
  ' (avg) ',
  formatC(e0trends$data$deficitsummary_global$peak_deficit_sd, format = 'f', digits = 2),
  ' (sd)'
)

names(e0deficittypology$facet_labels) <- names(groups)
e0deficittypology$facet_labeller <- as_labeller(
  e0deficittypology$facet_labels
)

e0deficittypology$fig <-
  e0deficittypology$data$bycountry |>
  ggplot() +
  aes(x = year, y = e0_deficit_avg) +
  geom_hline(yintercept = 0, color = 'grey80', linewidth = 2) +
  geom_line(alpha = 0.3, aes(group = region_iso)) +
  geom_line(aes(x = year, y = e0_deficit_avg),
            data = e0deficittypology$data$groupmeans) +
  geom_point(aes(x = year, y = e0_deficit_avg),
             color = 'white', fill = 'black',
             shape = 21, size = 2,
             data = e0deficittypology$data$groupmeans) +
  geom_point(
    aes(x = 2020+(n-1)/3, y = 0.5), size = 5.5,
    data = e0deficittypology$data$flag_positions
  ) +
  geom_flag(
    aes(x = 2020+(n-1)/3, y = 0.5, country = region_ggflag),
    size = 5, data = e0deficittypology$data$flag_positions
  ) +
  geom_text(
    aes(x = 2020+(n-1)/3, y = 0.18, label = region_iso),
    size = 1.5, color = 'grey30', data = e0deficittypology$data$flag_positions
  ) +
  facet_wrap(~group, labeller = e0deficittypology$facet_labeller) +
  labs(x = 'Year', y = 'Life expectancy deficit in years') +
  MyGGplotTheme()

e0deficittypology$fig

# Export ----------------------------------------------------------

ExportSVG(
  e0trends$fig, paths$output$e0trends.svg,
  width = 170, height = 190, scale = 1.4
)
e0trends$data$observedexpected |>
  mutate(across(.cols = where(is.numeric), .fns = ~round(.x,6))) |>
  write_csv(paths$output$e0trends.csv)

ExportSVG(
  e0deficittypology$fig, paths$output$e0deficittypology.svg,
  width = 170, height = 140
)
e0deficittypology$data$bycountry |>
  mutate(across(.cols = where(is.numeric), .fns = ~round(.x,6))) |>
  write_csv(paths$output$e0deficittypology.csv)
