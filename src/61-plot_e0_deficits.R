# Plot pandemic e0 deficit

# Init ------------------------------------------------------------

library(yaml)
library(readr); library(dplyr)
library(tidyr)
library(ggplot2)
library(ggflagsplus)

# Constants -------------------------------------------------------

# input and output paths
setwd('.')
paths <- list()
paths$input <- list(
  config.yaml = './cfg/config.yaml',
  global_functions.R = './src/_global_functions.R',
  region_metadata.csv = './cfg/region_metadata.csv',
  deficits_and_excesses.rds = './out/50-deficits_and_excesses.rds',
  pval.rds = './out/50-pval.rds'
)
paths$output <- list(
  e0deficit_total.svg = './out/61-e0deficit_total.svg',
  e0deficit_male.svg = './out/61-e0deficit_male.svg',
  e0deficit_female.svg = './out/61-e0deficit_female.svg',
  e0deficit_total.csv = './out/61-e0deficit_total.csv',
  e0deficit_male.csv = './out/61-e0deficit_male.csv',
  e0deficit_female.csv = './out/61-e0deficit_female.csv',
  e0deficitbyyear_total.svg = './out/61-e0deficitbyyear_total.svg',
  e0deficitbyyear_total.csv = './out/61-e0deficitbyyear_total.csv',
  e0deficitbyyear_female.svg = './out/61-e0deficitbyyear_female.svg',
  e0deficitbyyear_female.csv = './out/61-e0deficitbyyear_female.csv',
  e0deficitbyyear_male.svg = './out/61-e0deficitbyyear_male.svg',
  e0deficitbyyear_male.csv = './out/61-e0deficitbyyear_male.csv',
  e0deficitbysex.svg = './out/61-e0deficitbysex.svg',
  e0deficitbysex.csv = './out/61-e0deficitbysex.csv',
  e0deficit24_total.svg = './out/61-e0deficit24_total.svg',
  e0deficit24_total.csv = './out/61-e0deficit24_total.csv',
  e0deficit24_female.svg = './out/61-e0deficit24_female.svg',
  e0deficit24_female.csv = './out/61-e0deficit24_female.csv',
  e0deficit24_male.svg = './out/61-e0deficit24_male.svg',
  e0deficit24_male.csv = './out/61-e0deficit24_male.csv'
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
  sex_strata = c('Female', 'Male', 'Total')
})

dat <- list()

# Load data -------------------------------------------------------

dat$e0deficits <-
  readRDS(paths$input$deficits_and_excesses.rds) |>
  filter(region_iso %in% config$showinoutput)
pval <- readRDS(paths$input$pval.rds)

# e0 deficit 2020-2024 --------------------------------------------

e0deficit <- list()

for (s in cnst$sex_strata) {
  e0deficit$data[[s]] <-
    dat$e0deficits |>
    filter(age == 0, sex == s, year == '2020-2024') |>
    select(
      region = region_iso, sex, year,
      e0deficit_Q025 = ex_actual_minus_expected_q0.025,
      e0deficit_Q50 = ex_actual_minus_expected_q0.5,
      e0deficit_Q975 = ex_actual_minus_expected_q0.975,
      e0expected_Q50 = ex_expected_q0.5,
      e0observed = ex_actual_q0.5
    )

  e0deficit$fig[[s]] <-
    e0deficit$data[[s]] |>
    mutate(
      region_ggflag = tolower(region),
      # use uk flag for NIR as this is the most widely agreed upon flag
      region_ggflag = if_else(region_ggflag == 'gb-nir', 'gb', region_ggflag),
      region_rank = rank(-e0deficit_Q50)
    ) |>
    left_join(cnst$region, by = c('region' = 'region_code_iso3166_2')) |>
    ggplot(aes(y = region_rank, yend = region_rank)) +
    geom_vline(aes(xintercept = 0), size = 0.5, color = 'grey80') +
    geom_segment(aes(x = e0deficit_Q025, xend = e0deficit_Q975),
                 linewidth = 1, color = 'grey70') +
    # country label
    geom_text(
      aes(
        x = e0deficit_Q50,
        label = region_name_en
      ),
      position = position_nudge(y = +0.25, x = -0.07), hjust = 1,
      size = 1.7, color = 'grey60'
    ) +
    # e0 deficit
    geom_text(
      aes(
        x = e0deficit_Q50,
        label = paste0(
          formatC(e0deficit_Q50, digits = 2, format = 'f'),
          ' (',
          formatC(e0deficit_Q025, digits = 2, format = 'f'),
          '; ',
          formatC(e0deficit_Q975, digits = 2, format = 'f'),
          ')'
        )
      ),
      position = position_nudge(y = +0.25, x = +0.07), hjust = 0,
      size = 1.7, color = 'grey60'
    ) +
    # e0 observed
    geom_text(
      aes(
        x = e0deficit_Q50,
        label = paste0(
          'O: ', formatC(e0observed, format = 'f', digits = 1), ', ',
          'E: ', formatC(e0expected_Q50, format = 'f', digits = 1)
        )
      ),
      position = position_nudge(y = -0.25, x = +0.07), hjust = 0,
      size = 1.7, color = 'grey60'
    ) +
    geom_point(aes(x = e0deficit_Q50), size = 5.5) +
    geom_flag(
      aes(x = e0deficit_Q50, country = region_ggflag), size = 5
    ) +
    scale_x_continuous(breaks = seq(-2.5, 0, 0.5)) +
    scale_y_continuous(breaks = NULL, expand = expansion(add = c(1, 1))) +
    coord_cartesian(xlim = c(NA, 0.2)) +
    MyGGplotTheme(grid = 'x', axis = 'x', background_color = 'white') +
    labs(
      y = NULL,
      x = 'Life expectancy deficit in years 2020-2024'
    )
}

e0deficit$fig$Female
e0deficit$fig$Male
e0deficit$fig$Total

# e0 deficit by year ----------------------------------------------

e0deficitbyyear <- list()

for (s in cnst$sex_strata) {
  e0deficitbyyear$data[[s]] <-
    dat$e0deficits |>
    filter(age == 0, sex == s) |>
    select(
      region = region_iso, sex, year,
      e0deficit_Q025 = ex_actual_minus_expected_q0.025,
      e0deficit_Q50 = ex_actual_minus_expected_q0.5,
      e0deficit_Q975 = ex_actual_minus_expected_q0.975,
      e0expected_Q50 = ex_expected_q0.5,
      e0observed = ex_actual_q0.5
    ) |>
    left_join(
      select(pval, region = region_iso, year, sex, e0_deficit_pval)
    ) |>
    mutate(
      sig = ifelse(e0_deficit_pval<=0.05, 'sig', 'ns'),
      year = factor(year, levels = c('2020', '2021', '2022',
                                     '2023', '2024', '2020-2024'))
    )

  e0deficitbyyear$fig[[s]] <-
    e0deficitbyyear$data[[s]] |>
    group_by(year) |>
    mutate(
      region_ggflag = tolower(region),
      # use uk flag for NIR as this is the most widely agreed upon flag
      region_ggflag = if_else(region_ggflag == 'gb-nir', 'gb', region_ggflag),
      region_rank = rank(-e0deficit_Q50)
    ) |>
    ungroup() |>
    left_join(cnst$region, by = c('region' = 'region_code_iso3166_2')) |>
    ggplot(aes(y = region_rank, yend = region_rank)) +
    geom_vline(aes(xintercept = 0), size = 0.5, color = 'grey80') +
    geom_segment(aes(x = 0, xend = e0deficit_Q50, color = sig),
                 size = 2) +
    # country label
    geom_text(
      aes(
        x = e0deficit_Q50,
        label = region_name_en
      ),
      position = position_nudge(y = 0, x = -0.09), hjust = 1,
      size = 1.7, color = 'grey50'
    ) +
    geom_flag(
      aes(x = e0deficit_Q50, country = region_ggflag), size = 2
    ) +
    scale_x_continuous(
      breaks = seq(-5.5, 0.5, 0.5),
      labels = c('', '', '', '-4', '', '-3', '', '-2', '', '-1', '', '0', '')
    ) +
    scale_y_continuous(breaks = NULL, expand = expansion(add = c(1, 1))) +
    scale_color_manual(values = c('sig' = 'grey50', 'ns' = 'grey80')) +
    coord_cartesian(xlim = c(-4.5, 0.5)) +
    facet_wrap(~year) +
    MyGGplotTheme(grid = 'x', axis = 'x', show_legend = FALSE) +
    labs(
      y = NULL,
      x = 'Life expectancy deficit in years'
    )
}

e0deficitbyyear$fig$Female
e0deficitbyyear$fig$Male
e0deficitbyyear$fig$Total

# e0 deficit by sex -----------------------------------------------

e0deficitbysex <- list()

e0deficitbysex$data <-
  dat$e0deficits |>
  filter(age == 0, sex != 'Total') |>
  select(
    region = region_iso, sex, year,
    e0deficit_Q025 = ex_actual_minus_expected_q0.025,
    e0deficit_Q50 = ex_actual_minus_expected_q0.5,
    e0deficit_Q975 = ex_actual_minus_expected_q0.975,
    e0expected_Q50 = ex_expected_q0.5,
    e0observed = ex_actual_q0.5
  ) |>
  pivot_wider(
    id_cols = c(region, year),
    names_from = sex,
    values_from = starts_with('e0')
  ) |>
  mutate(
    maleexcessdeficit = e0deficit_Q50_Female - e0deficit_Q50_Male,
    highermaledeficit = e0deficit_Q50_Male < e0deficit_Q50_Female
  )

e0deficitbysex$fig <-
  e0deficitbysex$data |>
  filter(year == '2020-2024') |>
  left_join(cnst$region, by = c('region' = 'region_code_iso3166_2')) |>
  mutate(
    region_ggflag = tolower(region),
    # use uk flag for NIR as this is the most widely agreed upon flag
    region_ggflag = if_else(region_ggflag == 'gb-nir', 'gb', region_ggflag),
    region_rank = rank(-e0deficit_Q50_Male),
    region_name_en = reorder(region_name_en, region_rank)
  ) |>
  ungroup() |>
  ggplot(aes(y = region_name_en)) +
  geom_vline(aes(xintercept = 0), size = 0.5, color = 'grey80') +
  geom_segment(
    aes(
      x = e0deficit_Q50_Male, xend = e0deficit_Q50_Female,
      color = highermaledeficit
    ),
    linewidth = 2
  ) +
  geom_point(
    aes(x = e0deficit_Q50_Male), size = 5,
    color = 'white'
  ) +
  geom_point(
    aes(x = e0deficit_Q50_Female), size = 5,
    color = 'white'
  ) +
  geom_point(
    aes(x = e0deficit_Q50_Male), size = 4,
    shape = 21,
    fill = config$figspec$colors$sex$Male
  ) +
  geom_point(
    aes(x = e0deficit_Q50_Female), size = 4,
    shape = 21,
    fill = config$figspec$colors$sex$Female
  ) +
  scale_x_continuous(breaks = seq(-2.5, 0, 0.5)) +
  scale_color_manual(
    values = c(
      `TRUE` = config$figspec$colors$sex$Male,
      `FALSE` = config$figspec$colors$sex$Female
    )
  ) +
  coord_cartesian(xlim = c(NA, 0.2)) +
  MyGGplotTheme(grid = 'x', axis = 'x', background_color = 'white') +
  labs(
    y = NULL,
    x = 'Life expectancy deficit 2020-2024 in years'
  ) +
  guides(fill = 'none', color = 'none')

e0deficitbysex$fig

# e0 deficit 2024 -------------------------------------------------

e0deficit24 <- list()

for (s in cnst$sex_strata) {
  e0deficit24$data[[s]] <-
    dat$e0deficits |>
    filter(age == 0, sex == s, year == '2024') |>
    select(
      region = region_iso, sex, year,
      e0deficit_Q025 = ex_actual_minus_expected_q0.025,
      e0deficit_Q50 = ex_actual_minus_expected_q0.5,
      e0deficit_Q975 = ex_actual_minus_expected_q0.975,
      e0expected_Q50 = ex_expected_q0.5,
      e0observed = ex_actual_q0.5
    ) |>
    left_join(select(pval, region_iso, sex, year, e0_deficit_pval),
              by = c('region' = 'region_iso', 'sex', 'year'))

  e0deficit24$fig[[s]] <-
    e0deficit24$data[[s]] |>
    mutate(
      region_ggflag = tolower(region),
      # use uk flag for NIR as this is the most widely agreed upon flag
      region_ggflag = if_else(region_ggflag == 'gb-nir', 'gb', region_ggflag),
      region_rank = rank(-e0deficit_Q50)
    ) |>
    left_join(cnst$region, by = c('region' = 'region_code_iso3166_2')) |>
    ggplot(aes(y = e0_deficit_pval)) +
    geom_vline(aes(xintercept = 0), size = 0.5, color = 'grey80') +
    geom_hline(aes(yintercept = 0.1), size = 0.5, color = 'grey80') +
    geom_point(aes(x = e0deficit_Q50), size = 5.5) +
    geom_flag(
      aes(x = e0deficit_Q50, country = region_ggflag), size = 5
    ) +
    scale_x_continuous(breaks = seq(-2.5, 0, 0.5)) +
    scale_y_continuous(breaks = c(0.001, 0.01, 0.05, 0.1, 0.2, 0.5, 1), trans = 'sqrt') +
    coord_cartesian(xlim = c(NA, 0.2), ylim = c(0,1)) +
    MyGGplotTheme(grid = 'xy', axis = 'xy', background_color = 'white',
                  ar = 1) +
    labs(
      y = 'p-value of life expectancy deficit',
      x = 'Life expectancy deficit 2024 in years'
    )
}

e0deficit24$fig$Female
e0deficit24$fig$Male
e0deficit24$fig$Total

# Export ----------------------------------------------------------

# e0deficit
## total
ExportSVG(
  e0deficit$fig$Total, paths$output$e0deficit_total.svg,
  width = 170, height = 170
)
e0deficit$data$Total |>
  mutate(across(.cols = where(is.numeric), .fns = ~round(.x,6))) |>
  write_csv(paths$output$e0deficit_total.csv)
## male
ExportSVG(
  e0deficit$fig$Male, paths$output$e0deficit_male.svg,
  width = 170, height = 170
)
e0deficit$data$Male |>
  mutate(across(.cols = where(is.numeric), .fns = ~round(.x,6))) |>
  write_csv(paths$output$e0deficit_male.csv)
## female
ExportSVG(
  e0deficit$fig$Female, paths$output$e0deficit_female.svg,
  width = 170, height = 170
)
e0deficit$data$Female |>
  mutate(across(.cols = where(is.numeric), .fns = ~round(.x,6))) |>
  write_csv(paths$output$e0deficit_female.csv)

# e0deficitbyyear
## total
ExportSVG(
  e0deficitbyyear$fig$Total, paths$output$e0deficitbyyear_total.svg,
  width = 170, height = 170
)
e0deficitbyyear$data$Total |>
  mutate(across(.cols = where(is.numeric), .fns = ~round(.x,6))) |>
  write_csv(paths$output$e0deficitbyyear_total.csv)
## male
ExportSVG(
  e0deficitbyyear$fig$Male, paths$output$e0deficitbyyear_male.svg,
  width = 170, height = 170
)
e0deficitbyyear$data$Male |>
  mutate(across(.cols = where(is.numeric), .fns = ~round(.x,6))) |>
  write_csv(paths$output$e0deficitbyyear_male.csv)
## female
ExportSVG(
  e0deficitbyyear$fig$Female, paths$output$e0deficitbyyear_female.svg,
  width = 170, height = 170
)
e0deficitbyyear$data$Female |>
  mutate(across(.cols = where(is.numeric), .fns = ~round(.x,6))) |>
  write_csv(paths$output$e0deficitbyyear_female.csv)

# e0deficitsex
ExportSVG(
  e0deficitbysex$fig, paths$output$e0deficitbysex.svg,
  width = 170, height = 170
)
e0deficitbysex$data |>
  mutate(across(.cols = where(is.numeric), .fns = ~round(.x,6))) |>
  write_csv(paths$output$e0deficitbysex.csv)

# e0deficit24
ExportSVG(
  e0deficit24$fig$Total, paths$output$e0deficit24_total.svg,
  width = 170, height = 170,
  scale = 0.8
)
e0deficit24$data$Total |>
  mutate(across(.cols = where(is.numeric), .fns = ~round(.x,6))) |>
  write_csv(paths$output$e0deficit24_total.csv)
ExportSVG(
  e0deficit24$fig$Female, paths$output$e0deficit24_female.svg,
  width = 170, height = 170,
  scale = 0.8
)
e0deficit24$data$Female |>
  mutate(across(.cols = where(is.numeric), .fns = ~round(.x,6))) |>
  write_csv(paths$output$e0deficit24_female.csv)
ExportSVG(
  e0deficit24$fig$Male, paths$output$e0deficit24_male.svg,
  width = 170, height = 170,
  scale = 0.8
)
e0deficit24$data$Male |>
  mutate(across(.cols = where(is.numeric), .fns = ~round(.x,6))) |>
  write_csv(paths$output$e0deficit24_male.csv)
