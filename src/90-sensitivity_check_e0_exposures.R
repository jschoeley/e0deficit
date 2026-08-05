# Sensitivity checks of e0 estimates against different exposure measures

# Init ------------------------------------------------------------

library(yaml)
library(readr); library(dplyr); library(openxlsx)
library(purrr)
library(ggplot2)

# Constants -------------------------------------------------------

# input and output paths
setwd('.')
paths <- list()
paths$input <- list(
  tmpdir = './tmp',
  config = './cfg/config.yaml',
  global = './src/_global_functions.R',
  region = './cfg/region_metadata.csv',
  projectioninput = './out/30-analysisinput.rds'
)
paths$output <- list(
  tmpdir = paths$input$tmpdir,
  fig = './out/',
  xlsx_e0sensitivity = './out/90-e0sensitivity.xlsx',
  xlsx_e0sensitivitysummary = './out/90-e0sensitivitysummary.xlsx',
  e0sensitivity.pdf = './out/90-e0sensitivity.pdf'
)

# global configuration
config <- read_yaml(paths$input$config)

# global objects and functions
global <- source(paths$input$global)

# constants specific to this analysis
cnst <- within(list(), {
  region = filter(
    read_csv(paths$input$region),
    region_code_iso3166_2 %in% config$skeleton$regions
  )
})

dat <- list()

# Function --------------------------------------------------------

CalculateLifeTable <-
  function (df, x, nx, Dx, Ex) {

    require(dplyr)

    df |>
      transmute(
        x = {{x}},
        nx = {{nx}},
        mx = {{Dx}}/{{Ex}},
        px = exp(-mx*{{nx}}),
        qx = 1-px,
        lx = head(cumprod(c(1, px)), -1),
        dx = c(-diff(lx), tail(lx, 1)),
        Lx = ifelse(mx==0, lx*nx, dx/mx),
        Tx = rev(cumsum(rev(Lx))),
        ex = Tx/lx
      )

  }

# Load data -------------------------------------------------------

dat$lt <- readRDS(paths$input$projectioninput)

# Calculate and plot e0 -------------------------------------------

e0sensitivity <- list()

e0sensitivity$data <-
  dat$lt |>
  group_by(year, sex, region) |>
  group_modify(~{
    e0_own_estimates_with_wpp22 <-
      CalculateLifeTable(.x, x = age_start, nx = age_width, Dx = death, Ex = population_py_wpp22) |>
      filter(x == 0) |>
      pull(ex)
    e0_own_estimates_with_wpp24 <-
      CalculateLifeTable(.x, x = age_start, nx = age_width, Dx = death, Ex = population_py_wpp24) |>
      filter(x == 0) |>
      pull(ex)
    e0_own_estimates_with_hmdpop <-
      CalculateLifeTable(.x, x = age_start, nx = age_width, Dx = death, Ex = population_py_hmd) |>
      filter(x == 0) |>
      pull(ex)
    e0_estimates_by_eurostat <- .x$lifeexpectancy_eurostat[.x$age_start == 0]
    e0_estimates_by_hmd <- .x$lifeexpectancy_hmd[.x$age_start == 0]
    tibble(
      e0_own_estimates_with_wpp22,
      e0_own_estimates_with_wpp24,
      e0_own_estimates_with_hmdpop,
      e0_estimates_by_eurostat,
      e0_estimates_by_hmd
    )
  }) |>
  ungroup() |>
  left_join(cnst$region, by = c('region' = 'region_code_iso3166_2')) |>
  filter(year %in% 2010:2024) |>
  select(
    region, year, sex, e0_own_estimates_with_wpp22,
    e0_own_estimates_with_wpp24,
    e0_own_estimates_with_hmdpop,
    e0_estimates_by_eurostat,
    e0_estimates_by_hmd
  )

e0sensitivity$padding <-
  e0sensitivity$data |>
  group_by(region) |>
  summarise(
    ymin = min(c(e0_own_estimates_with_wpp22,
                 e0_own_estimates_with_wpp24,
                 e0_own_estimates_with_hmdpop,
                 e0_estimates_by_eurostat,
                 e0_estimates_by_hmd), na.rm = TRUE),
    ymax = max(c(e0_own_estimates_with_wpp22,
                 e0_own_estimates_with_wpp24,
                 e0_own_estimates_with_hmdpop,
                 e0_estimates_by_eurostat,
                 e0_estimates_by_hmd), na.rm = TRUE),
    yrange = ymax-ymin,
    ypadding = (14.32-yrange)/2,
    ymin_padded = ymin-ypadding,
    ymax_padded = ymax+ypadding
  ) |>
  ungroup()

e0sensitivity$fig <-
  e0sensitivity$data |>
  ggplot(aes(x = year, group = sex, color = sex, fill = sex)) +
  geom_vline(xintercept = 2020, color = 'grey') +
  geom_point(aes(y = e0_own_estimates_with_hmdpop), shape = 21, fill = 'white', size = 2) +
  geom_point(aes(y = e0_own_estimates_with_wpp24), shape = 21, fill = 'white') +
  geom_point(aes(y = e0_own_estimates_with_wpp22), shape = 21, size = 0.5) +
  geom_point(aes(y = e0_estimates_by_eurostat), shape = 3) +
  geom_point(aes(y = e0_estimates_by_hmd), shape = 4) +
  # fake data to make each panel have equal range but shifted
  geom_linerange(
    aes(x = NA_real_, ymin = ymin_padded, ymax = ymax_padded),
    data = e0sensitivity$padding, inherit.aes = FALSE
  ) +
  scale_x_continuous(
    breaks = seq(2010, 2024, 1),
    labels = c('2010', rep('', 13), '2024'),
    limits = c(2010, 2024),
  ) +
  scale_y_continuous(breaks = seq(70, 90, 2)) +
  scale_color_manual(values = unlist(config$figspec$colors$sex)) +
  scale_fill_manual(values = unlist(config$figspec$colors$sex)) +
  MyGGplotTheme(grid = 'y', axis = 'x', panel_border = FALSE) +
  labs(
    y = 'Period life expectancy in years', x = NULL, color = NULL, fill = NULL,
    title = 'Life expectancy under different calculation methods',
    subtitle = 'Unadjusted estimates with WPP22 exposures (filled dots), with WPP24 exposures (small open circles), and with HMD exposures (large open circles).\nHMD estimates (angled crosses) and Eurostat estimates (straight crosses)'
  ) +
  facet_wrap(~region, scales = 'free_y', ncol = 5)

e0sensitivity$fig

# Maximum deviations from HMD estimates ---------------------------

# Country specific maximum and average absolute deviations between our life expectancy estimates, given different exposures, and HMD estimates.
e0sensitivity$summary <-
  e0sensitivity$data |>
  group_by(region) |>
  summarise(
    max_deviation_wpp22 = max(abs(e0_own_estimates_with_wpp22-e0_estimates_by_hmd), na.rm = TRUE),
    avg_deviation_wpp22 = mean(abs(e0_own_estimates_with_wpp22-e0_estimates_by_hmd), na.rm = TRUE),
    max_deviation_wpp24 = max(abs(e0_own_estimates_with_wpp24-e0_estimates_by_hmd), na.rm = TRUE),
    avg_deviation_wpp24 = mean(abs(e0_own_estimates_with_wpp24-e0_estimates_by_hmd), na.rm = TRUE),
    max_deviation_hmd = max(abs(e0_own_estimates_with_hmdpop-e0_estimates_by_hmd), na.rm = TRUE),
    avg_deviation_hmd = mean(abs(e0_own_estimates_with_hmdpop-e0_estimates_by_hmd), na.rm = TRUE)
  )

# Export ----------------------------------------------------------

ExportFigure(
  figure = e0sensitivity$fig, , filename = paths$output$e0sensitivity.pdf,
  width = 170, height = 190, device = 'pdf', scale = 1.4
)
write.xlsx(
  e0sensitivity$data, file = paths$output$xlsx_e0sensitivity,
  keepNA = TRUE, na.string = '.',
  firstRow = TRUE, firstCol = TRUE, overwrite = TRUE
)
write.xlsx(
  e0sensitivity$summary, file = paths$output$xlsx_e0sensitivitysummary,
  keepNA = TRUE, na.string = '.',
  firstRow = TRUE, firstCol = TRUE, overwrite = TRUE
)
