library(dplyr)
library(readr)

trends <- read_csv('out/60-e0trends.csv')

trends |>
  filter(year != '2020-2024') |>
  group_by(region_name_en) |>
  summarise(
    year = year[which.max(e0_actual)],
    n = n()
  ) |>
  filter(
    year == 2024
  ) |>
  select(region_name_en) |>
  dput()
