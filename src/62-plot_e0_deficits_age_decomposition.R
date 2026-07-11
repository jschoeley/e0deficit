# Plot age decomposition of e0 deficit

# Init ------------------------------------------------------------

library(yaml)
library(readr); library(dplyr)
library(ggplot2); library(ggflagsplus); library(patchwork)

# Constants -------------------------------------------------------

# input and output paths
setwd('.')
paths <- list()
paths$input <- list(
  config.yaml = './cfg/config.yaml',
  global_functions.R = './src/_global_functions.R',
  region_metadata.csv = './cfg/region_metadata.csv',
  deficits_and_excesses.rds = './out/50-deficits_and_excesses.rds',
  deficit_clusters.csv = './out/51-deficit_clusters.csv'
)
paths$output <- list(
  e0deficitage_total.svg = './out/62-e0deficitage_total.svg',
  e0deficitage_total.csv = './out/62-e0deficitage_total.csv',
  e0deficitage_female.svg = './out/62-e0deficitage_female.svg',
  e0deficitage_female.csv = './out/62-e0deficitage_female.csv',
  e0deficitage_male.svg = './out/62-e0deficitage_male.svg',
  e0deficitage_male.csv = './out/62-e0deficitage_male.csv'
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

dat$lt <-
  readRDS(paths$input$deficits_and_excesses.rds) |>
  filter(region_iso %in% config$showinoutput, !region_iso %in% config$excludefromagedecomposition)

groups <- read_csv(paths$input$deficit_clusters.csv)
groups <- split(groups$region_iso, groups[[config$clustermethod]])
names(groups) <- config$clusternames[names(groups)]

# Plot e0 deficits by age -----------------------------------------

e0deficitage <- list()
e0deficitage$sex <- list()
e0deficitage$sex$subfig <- list()

e0deficitage$cnst <- list(
  colors = c('2020' = '#CCD800', '2021' = '#D89E00', '2022' = '#C05B00',
             '2023' = '#C02C00', '2024' = '#640099', '2020-2024' = 'black')
)

for (s in cnst$sex_strata) {
  for (g in names(groups)) {

    e0deficitage$sex[[s]]$subfig[[g]]$data <-
      dat$lt |>
      filter(region_iso %in% groups[[g]], sex == s) |>
      left_join(cnst$region, by = c('region_iso' = 'region_code_iso3166_2')) |>
      filter(year %in% c('2020', '2021', '2022', '2023', '2024', '2020-2024')) |>
      mutate(
        age = as.integer(age), age_group = (age %/% 20)*20
      ) |>
      ungroup() |>
      select(region_iso, region_name_en, year, age_group, e0_cntrb_t_mean) |>
      group_by(region_iso, region_name_en, year, age_group) |>
      summarise(
        e0_cntrb_t_mean = sum(e0_cntrb_t_mean)
      ) |>
      mutate(
        region_ggflag = tolower(region_iso),
        # use uk flag for NIR as this is the most widely agreed upon flag
        region_ggflag = if_else(region_ggflag == 'gb-nir', 'gb', region_ggflag)
      ) |>
      ungroup()

    e0deficitage$sex[[s]]$subfig[[g]]$fig <-
      e0deficitage$sex[[s]]$subfig[[g]]$data |>
      ggplot() +
      aes(x = e0_cntrb_t_mean, y = age_group) +
      geom_vline(xintercept = 0, color = 'grey') +
      geom_point(
        x = -2, y = 100, size = 5.5
      ) +
      geom_flag(
        aes(x = -2, y = 100, country = region_ggflag),
        size = 5
      ) +
      geom_path(aes(color = year)) +
      facet_wrap(~region_name_en, ncol = 5) +
      scale_color_manual(breaks = names(e0deficitage$cnst$colors),
                         values = e0deficitage$cnst$colors) +
      scale_y_continuous(
        breaks = c(0, 20, 40, 60, 80, 100),
        labels = c('0-19', '20-39', '40-59', '60-79', '80-99', '100+')
      ) +
      scale_x_continuous(breaks = seq(-2, 0.5, 0.5),
                         labels = c('-2', '-1.5', '-1', '-0.5', '0', '+0.5')) +
      labs(
        x = 'Years of contribution to annual life expectancy deficit',
        y = 'Age group', color = 'Year',
        title = g
      ) +
      MyGGplotTheme(grid = 'y', axis = 'x', panel_border = FALSE) +
      coord_cartesian(clip = 'off')

  }

  e0deficitage$sex[[s]]$data <-
    bind_rows(
      `A First wave peak` = e0deficitage$sex[[s]]$subfig$`A First wave peak`$data,
      `B Second wave peak` = e0deficitage$sex[[s]]$subfig$`B Second wave peak`$data,
      `C Late peak` = e0deficitage$sex[[s]]$subfig$`C Late peak`$data,
      `D Prolonged depression` = e0deficitage$sex[[s]]$subfig$`D Prolonged depression`$data,
      .id = 'group'
    )

  e0deficitage$sex[[s]]$fig <-
    (
      e0deficitage$sex[[s]]$subfig$`A First wave peak`$fig +
        labs(x = NULL)
    ) /
    (
      e0deficitage$sex[[s]]$subfig$`B Second wave peak`$fig +
        labs(x = NULL, y = NULL)
    ) /
    (
      e0deficitage$sex[[s]]$subfig$`C Late peak`$fig +
        labs(x = NULL, y = NULL)
    ) /
    (
      e0deficitage$sex[[s]]$subfig$`D Prolonged depression`$fig +
        labs(y = NULL)
    ) +
    plot_layout(
      heights = c(2/9, 3/9, 2/9, 2/9),
      guides = 'collect'
    )
}

e0deficitage$sex$Total$fig

# Export ----------------------------------------------------------

ExportSVG(
  e0deficitage$sex$Female$fig, paths$output$e0deficitage_female.svg,
  width = 170, height = 190, scale = 1.4
)
e0deficitage$sex$Female$data |>
  mutate(across(.cols = where(is.numeric), .fns = ~round(.x,6))) |>
  write_csv(paths$output$e0deficitage_female.csv)
ExportSVG(
  e0deficitage$sex$Male$fig, paths$output$e0deficitage_male.svg,
  width = 170, height = 190, scale = 1.4
)
e0deficitage$sex$Male$data |>
  mutate(across(.cols = where(is.numeric), .fns = ~round(.x,6))) |>
  write_csv(paths$output$e0deficitage_male.csv)
ExportSVG(
  e0deficitage$sex$Total$fig, paths$output$e0deficitage_total.svg,
  width = 170, height = 190, scale = 1.4
)
e0deficitage$sex$Total$data |>
  mutate(across(.cols = where(is.numeric), .fns = ~round(.x,6))) |>
  write_csv(paths$output$e0deficitage_total.csv)
