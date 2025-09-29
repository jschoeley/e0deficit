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
  e0deficit_total.pdf = './out/61-e0deficit_total.pdf',
  e0deficit_male.pdf = './out/61-e0deficit_male.pdf',
  e0deficit_female.pdf = './out/61-e0deficit_female.pdf',
  e0deficit_total.csv = './out/61-e0deficit_total.csv',
  e0deficit_male.csv = './out/61-e0deficit_male.csv',
  e0deficit_female.csv = './out/61-e0deficit_female.csv',
  e0deficitbyyear_total.pdf = './out/61-e0deficitbyyear_total.pdf',
  e0deficitbyyear_total.csv = './out/61-e0deficitbyyear_total.csv',
  e0deficitbyyear_female.pdf = './out/61-e0deficitbyyear_female.pdf',
  e0deficitbyyear_female.csv = './out/61-e0deficitbyyear_female.csv',
  e0deficitbyyear_male.pdf = './out/61-e0deficitbyyear_male.pdf',
  e0deficitbyyear_male.csv = './out/61-e0deficitbyyear_male.csv',
  e0deficitbysex.pdf = './out/61-e0deficitbysex.pdf',
  e0deficitbysex.csv = './out/61-e0deficitbysex.csv'
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
      e0deficit_Q05 = ex_actual_minus_expected_q0.05,
      e0deficit_Q50 = ex_actual_minus_expected_q0.5,
      e0deficit_Q95 = ex_actual_minus_expected_q0.95,
      e0expected_Q50 = ex_expected_q0.5,
      e0observed = ex_actual_q0.5
    )

  e0deficit$fig[[s]] <-
    e0deficit$data[[s]] |>
    mutate(
      region_ggflag = tolower(region),
      region_rank = rank(-e0deficit_Q50)
    ) |>
    left_join(cnst$region, by = c('region' = 'region_code_iso3166_2')) |>
    ggplot(aes(y = region_rank, yend = region_rank)) +
    geom_vline(aes(xintercept = 0), size = 0.5, color = 'grey80') +
    geom_segment(aes(x = e0deficit_Q05, xend = e0deficit_Q95),
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
          formatC(e0deficit_Q05, digits = 2, format = 'f'),
          '; ',
          formatC(e0deficit_Q95, digits = 2, format = 'f'),
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
      x = 'Life expectancy deficit 2020-2024'
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
      e0deficit_Q05 = ex_actual_minus_expected_q0.05,
      e0deficit_Q50 = ex_actual_minus_expected_q0.5,
      e0deficit_Q95 = ex_actual_minus_expected_q0.95,
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
      labels = c('', '', '', '-4 years', '', '-3', '', '-2', '', '-1', '', '0', '')
    ) +
    scale_y_continuous(breaks = NULL, expand = expansion(add = c(1, 1))) +
    scale_color_manual(values = c('sig' = 'grey50', 'ns' = 'grey80')) +
    coord_cartesian(xlim = c(-4.5, 0.5)) +
    facet_wrap(~year) +
    MyGGplotTheme(grid = 'x', axis = 'x', show_legend = FALSE) +
    labs(
      y = NULL,
      x = 'Life expectancy deficit'
    )
}

e0deficitbyyear$fig$Female
e0deficitbyyear$fig$Male
e0deficitbyyear$fig$Total

# e0 deficit by year rank -----------------------------------------

e0deficitrank <- list()

e0deficitrank$data <-
  dat$e0deficits |>
  filter(age == 0) |>
  select(
    region = region_iso, sex, year,
    e0deficit_Q05 = ex_actual_minus_expected_q0.05,
    e0deficit_Q50 = ex_actual_minus_expected_q0.5,
    e0deficit_Q95 = ex_actual_minus_expected_q0.95,
    e0expected_Q50 = ex_expected_q0.5,
    e0observed = ex_actual_q0.5
  ) |>
  mutate(
    year = factor(year, levels = c('2020', '2021', '2022', '2023', '2024', '2020-2024'))
  )

e0deficitrank$fig <-
  e0deficitbyyear$data |>
  filter(sex == 'Total') |>
  group_by(year) |>
  mutate(
    region_ggflag = tolower(region),
    region_rank = rank(-e0deficit_Q50)
  ) |>
  ungroup() |>
  left_join(cnst$region, by = c('region' = 'region_code_iso3166_2')) |>
  ggplot(aes(y = region_rank, group = region, x = year)) +
  geom_path(size = 1, color = 'grey70', alpha = 0.5) +
  # country label
  # geom_text(
  #   aes(
  #     x = e0deficit_Q50,
  #     label = region_name_en
  #   ),
  #   position = position_nudge(y = 0, x = -0.07), hjust = 1,
  #   size = 1.7, color = 'grey50'
  # ) +
  geom_point(aes(x = year), size = 5.5) +
  geom_flag(
    aes(x = year, country = region_ggflag), size = 5
  ) +
  MyGGplotTheme(grid = 'x', axis = 'x') +
  labs(
    y = NULL,
    x = 'LE deficit ranking'
  )

e0deficitrank$fig

# e0 deficit by sex -----------------------------------------------

e0deficitbysex <- list()

e0deficitbysex$data <-
  dat$e0deficits |>
  filter(age == 0, sex != 'Total') |>
  select(
    region = region_iso, sex, year,
    e0deficit_Q05 = ex_actual_minus_expected_q0.05,
    e0deficit_Q50 = ex_actual_minus_expected_q0.5,
    e0deficit_Q95 = ex_actual_minus_expected_q0.95,
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
    x = 'Life expectancy deficit 2020-2024'
  ) +
  guides(fill = 'none', color = 'none')

# Export ----------------------------------------------------------

# e0deficit
## total
ggsave(
  paths$output$e0deficit_total.pdf, e0deficit$fig$Total,
  units = 'mm', width = 170, height = 170, device = 'pdf'
)
e0deficit$data$Total |>
  mutate(across(.cols = where(is.numeric), .fns = ~round(.x,6))) |>
  write_csv(paths$output$e0deficit_total.csv)
## male
ggsave(
  paths$output$e0deficit_male.pdf, e0deficit$fig$Male,
  units = 'mm', width = 170, height = 170, device = 'pdf'
)
e0deficit$data$Male |>
  mutate(across(.cols = where(is.numeric), .fns = ~round(.x,6))) |>
  write_csv(paths$output$e0deficit_male.csv)
## female
ggsave(
  paths$output$e0deficit_female.pdf, e0deficit$fig$Female,
  units = 'mm', width = 170, height = 170, device = 'pdf'
)
e0deficit$data$Female |>
  mutate(across(.cols = where(is.numeric), .fns = ~round(.x,6))) |>
  write_csv(paths$output$e0deficit_female.csv)

# e0deficitbyyear
## total
ggsave(
  paths$output$e0deficitbyyear_total.pdf, e0deficitbyyear$fig$Total,
  units = 'mm', width = 170, height = 170, device = 'pdf'
)
e0deficitbyyear$data$Total |>
  mutate(across(.cols = where(is.numeric), .fns = ~round(.x,6))) |>
  write_csv(paths$output$e0deficitbyyear_total.csv)
## male
ggsave(
  paths$output$e0deficitbyyear_male.pdf, e0deficitbyyear$fig$Male,
  units = 'mm', width = 170, height = 170, device = 'pdf'
)
e0deficitbyyear$data$Male |>
  mutate(across(.cols = where(is.numeric), .fns = ~round(.x,6))) |>
  write_csv(paths$output$e0deficitbyyear_male.csv)
## female
ggsave(
  paths$output$e0deficitbyyear_female.pdf, e0deficitbyyear$fig$Female,
  units = 'mm', width = 170, height = 170, device = 'pdf'
)
e0deficitbyyear$data$Female |>
  mutate(across(.cols = where(is.numeric), .fns = ~round(.x,6))) |>
  write_csv(paths$output$e0deficitbyyear_female.csv)

# e0deficitsex
ggsave(
  paths$output$e0deficitbysex.pdf, e0deficitbysex$fig,
  units = 'mm', width = 170, height = 170, device = 'pdf'
)
e0deficitbysex$data |>
  mutate(across(.cols = where(is.numeric), .fns = ~round(.x,6))) |>
  write_csv(paths$output$e0deficitbysex.csv)
