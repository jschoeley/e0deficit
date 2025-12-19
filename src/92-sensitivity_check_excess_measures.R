# Check sensitivity of results on choice of excess measure

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
  region_metadata.csv = './cfg/region_metadata.csv',
  deficits_and_excesses.rds = './out/50-deficits_and_excesses.rds'
)
paths$output <- list(
  excessmeasures20_24.pdf = './out/92-excessmeasures20_24.pdf',
  excessmeasures20_24.csv = './out/92-excessmeasures20_24.csv'
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
})

# Load data -------------------------------------------------------

excess <- read_rds(paths$input$deficits_and_excesses.rds)

# 2020-2024 -------------------------------------------------------

excessmeasures20_24 <- list()

excessmeasures20_24$data <-
  excess |>
  group_by(region_iso, sex, year) |>
  summarise(
    e0deficit = ex_actual_minus_expected_mean[age == 0],
    pscore = pscore_cum_mean[age == 0],
    mul =
      1/sum(death_total_actual_mean)*sum(
        ifelse(excess_deaths_mean>0, excess_deaths_mean, 0)*
          ex_expected_mean
      )
  ) |>
  ungroup() |>
  filter(sex == 'Total', year == '2020-2024', !region_iso %in% 'JP') |>
  mutate(
    rank_pscore = rank(-pscore),
    rank_e0deficit = rank(e0deficit),
    rank_mul = rank(-mul)
  )

excessmeasures20_24$cnst <- within(list(), {
  breaks_pscore_rank <-
    seq(1,max(unique(excessmeasures20_24$data$rank_pscore)))
  labels_pscore_rank <-
    c('Highest', rep('', max(breaks_pscore_rank)-2), 'Lowest')
  breaks_mul_rank <-
    seq(1,max(unique(excessmeasures20_24$data$rank_mul)))
  labels_mul_rank <-
    c('Highest', rep('', max(breaks_mul_rank)-2), 'Lowest')
  breaks_e0deficit_rank <-
    seq(1,max(unique(excessmeasures20_24$data$rank_e0deficit)))
  labels_e0deficit_rank <-
    c('Highest', rep('', max(breaks_e0deficit_rank)-2), 'Lowest')
})



excessmeasures20_24$fig$led_vs_mul <-
  excessmeasures20_24$data |>
  ggplot(aes(x = e0deficit, y = mul)) +
  geom_abline(linewidth = 2, color = 'grey75') +
  geom_abline(
    intercept = 1, slope = 1, linewidth = 1, color = 'grey75'
  ) +
  geom_abline(
    intercept = -1, slope = 1, linewidth = 1, color = 'grey75'
  ) +
  geom_point(size = 5.5) +
  geom_flag(
    aes(country = tolower(region_iso)), size = 5
  ) +
  coord_equal(xlim = c(0, NA), ylim = c(0, NA)) +
  scale_x_reverse() +
  MyGGplotTheme(grid = 'xy', axis = 'xy') +
  labs(x = 'LE deficit', y = 'MUL')

excessmeasures20_24$fig$led_vs_pscore <-
  excessmeasures20_24$data |>
  ggplot(aes(x = rank_e0deficit, y = rank_pscore)) +
  geom_abline(linewidth = 2, color = 'grey75') +
  geom_abline(
    intercept = 6, slope = 1, linewidth = 1, color = 'grey75'
  ) +
  geom_abline(
    intercept = -6, slope = 1, linewidth = 1, color = 'grey75'
  ) +
  geom_point(size = 4.5) +
  geom_flag(
    aes(country = tolower(region_iso)), size = 4
  ) +
  coord_equal() +
  scale_x_reverse(
    breaks = excessmeasures20_24$cnst$breaks_e0deficit_rank,
    labels = excessmeasures20_24$cnst$labels_e0deficit_rank
  ) +
  scale_y_reverse(
    breaks = excessmeasures20_24$cnst$breaks_pscore_rank,
    labels = excessmeasures20_24$cnst$labels_pscore_rank
  ) +
  MyGGplotTheme(grid = 'xy', axis = 'xy') +
  labs(x = 'LE deficit rank', y = 'P-score rank') +
  theme(
    panel.grid.major.x = element_line(linetype = 'solid'),
    panel.grid.major.y = element_line(linetype = 'solid')
  )

excessmeasures20_24$fig$mul_vs_pscore <-
  excessmeasures20_24$data |>
  ggplot(aes(x = rank_mul, y = rank_pscore)) +
  geom_abline(linewidth = 2, color = 'grey75') +
  geom_abline(
    intercept = 6, slope = 1, linewidth = 1, color = 'grey75'
  ) +
  geom_abline(
    intercept = -6, slope = 1, linewidth = 1, color = 'grey75'
  ) +
  geom_point(size = 4.5) +
  geom_flag(
    aes(country = tolower(region_iso)), size = 4
  ) +
  coord_equal() +
  scale_x_reverse(
    breaks = excessmeasures20_24$cnst$breaks_mul_rank,
    labels = excessmeasures20_24$cnst$labels_mul_rank
  ) +
  scale_y_reverse(
    breaks = excessmeasures20_24$cnst$breaks_pscore_rank,
    labels = excessmeasures20_24$cnst$labels_pscore_rank
  ) +
  MyGGplotTheme(grid = 'xy', axis = 'xy') +
  labs(x = 'MUL rank', y = 'P-score rank') +
  theme(
    panel.grid.major.x = element_line(linetype = 'solid'),
    panel.grid.major.y = element_line(linetype = 'solid')
  )

excessmeasures20_24$fig$comparison <-
  excessmeasures20_24$fig$led_vs_mul +
  excessmeasures20_24$fig$led_vs_pscore /
  excessmeasures20_24$fig$mul_vs_pscore +
  plot_annotation(tag_levels = 'A')

excessmeasures20_24$fig$comparison

# Export ----------------------------------------------------------

excessmeasures20_24$data |>
  mutate(across(.cols = where(is.numeric), .fns = ~round(.x,6))) |>
  write_csv(paths$output$excessmeasures20_24.csv)
ggsave(
  paths$output$excessmeasures20_24.pdf, excessmeasures20_24$fig$comparison,
  units = 'mm', width = 170, height = 130, device = 'pdf',
  scale = 1.2,
)
