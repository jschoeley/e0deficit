# Harmonize population data and rates. Project population where missing.
#
# (1) Jan 1st, midyear population estimates and fertility rates
#     for all nation states from World Population Prospects 22 and 24.
#     We harmonize this data into a common format for further analysis.
# (2) Jan 1st, midyear population estimates, fertility and death rates
#     from HMD/HFD for all HMD/HFD regions
#     - for recent years where HMD midyear estimates are not
#       available, we perform a stable population projection if midyear
#       population based on the most recent HMD population data and 5
#       year average (2015-19) age specific mortality and fertility
#       rates.
# (3) The default population estimates to use for further analysis and
#     life expectancy calculation are selected on a country by country
#     basis based on closeness of our estimates to benchmark estimates
#     (Eurostat, HMD).

# Init ------------------------------------------------------------

library(yaml)
library(readr); library(dplyr); library(tidyr)
library(openxlsx)
library(ggplot2)

# Constants -------------------------------------------------------

# input and output paths
setwd('.')
paths <- list()
paths$input <- list(
  config.yaml = './cfg/config.yaml',
  global_functions.R = './src/_global_functions.R',
  region_metadata.csv = './cfg/region_metadata.csv',
  skeleton.rds = './tmp/10-harmonized_skeleton.rds',
  wppjoint.rds = './dat/wppjoint/11-wppjoint.rds',
  hmdhfd.rds = './dat/hmdhfd/11-hmdhfd.rds'
)
paths$output <- list(
  harmonized_population.rds = './tmp/20-harmonized_population.rds',
  hmdprojjumpoffyears.xlsx = './out/20-hmdprojjumpoffyears.xlsx',
  wpp_population.pdf = './out/20-wpp_population.pdf',
  wpp24vs22.pdf = './out/20-wpp24vs22.pdf',
  hmd_midyear_population_projection_by_age.pdf = './out/20-hmd_midyear_population_projection_by_age.pdf',
  hmd_midyear_population_projection_total.pdf = './out/20-hmd_midyear_population_projection_total.pdf',
  out = './out'
)

# global configuration
config <- read_yaml(paths$input$config.yaml)

# meta data on regions
region_meta <- read_csv(paths$input$region_metadata.csv, na = '.')

# constants specific to this analysis
cnst <- list(); cnst <- within(cnst, {
  # translation of wpp sex code to harmonized sex code
  code_sex_wpp =
    c(`male` = config$skeleton$sex$Male,
      `female` = config$skeleton$sex$Female)
  # translation of hmdhfd sex code to harmonized sex code
  code_sex_hmdhfd =
    c(`Male` = config$skeleton$sex$Male,
      `Female` = config$skeleton$sex$Female)
  # translation of Japan data sex code to harmonized sex code
  code_sex_japan =
    c(`Male` = config$skeleton$sex$Male,
      `Female` = config$skeleton$sex$Female)
  # lookup table for region codes
  # only countries defined in skeleton
  region_lookup_wpp =
    region_meta |>
    filter(region_code_iso3166_2 %in% config$skeleton$region) |>
    select(region_code_iso3166_2, region_code_wpp) |>
    drop_na()
  region_lookup_hmd =
    region_meta |>
    filter(region_code_iso3166_2 %in% config$skeleton$region) |>
    select(region_code_iso3166_2, region_code_hmd)

  # first year in harmonized data set
  skeleton_first_year = config$skeleton$year$start
  # last year in harmonized data set
  skeleton_final_year = config$skeleton$year$end

})

# list containers for analysis artifacts
dat <- list()
fig <- list()

# Functions -------------------------------------------------------

# global functions
source(paths$input$global_functions.R)

#' Project a Population via Stable Assumption
StableProjection1x1 <- function (
    pop_m, pop_f, mx_m, mx_f, fx_f, n = 1, srb = 1.04
) {

  nage = length(pop_m)

  # first project the female population for n steps
  # Leslie projection matrix
  A_f <- matrix(0, nrow = nage, ncol = nage)
  diag(A_f[-1,-nage]) <- head(exp(-mx_f), -1)
  A_f[1,] <- fx_f * 1/(1+srb)
  # population matrix, first column is jump off population
  P_f <- matrix(0, nrow = nage, ncol = n+1)
  P_f[,1] <- pop_f
  # project population
  for (i in 1:n+1) {
    P_f[,i] <- A_f%*%P_f[,i-1]
  }

  # now project the male population with male births derived from
  # projected female births via sex ratio
  A_m <- matrix(0, nage, ncol = nage)
  diag(A_m[-1,-nage]) <- head(exp(-mx_m), -1)
  P_m <- matrix(0, nrow = nage, ncol = n+1)
  P_m[,1] <- pop_m
  for (i in 1:n+1) {
    P_m[,i] <- A_m%*%P_m[,i-1]
    P_m[1,i] <- P_f[1,i]*srb
  }

  projection <-
    expand_grid(n = 1:n, age_start = 1:nage-1) |>
    mutate(Female = c(P_f[,-1]), Male = c(P_m[,-1])) |>
    pivot_longer(
      cols = c(Female, Male),
      names_to = 'sex',
      values_to = 'population_midyear'
    )

  return(projection)

}

#' Make a Grid of Population Pyramids
PopPyramids <- function(
    dat, population, age, sex, year, highlight, facet, title
) {
  require(ggplot2); require(dplyr)
  dat |>
    transmute(
      pop = ifelse(sex == 'Male', -{{population}}, {{population}}),
      age = {{age}}, sex = {{sex}}, year = {{year}}, hl = {{highlight}},
      fct = {{facet}}
    ) |>
    ggplot(
      aes(x = age, y = pop, color = hl,
          group = interaction(sex, year, hl))
    ) +
    geom_line(show.legend = FALSE) +
    annotate('text', x = 90, y = -Inf, label = 'Male',
             hjust = 0, size = 2) +
    annotate('text', x = 90, y = Inf, label = 'Female',
             hjust = 1, size = 2) +
    geom_hline(yintercept = 0) +
    scale_x_continuous(
      breaks = seq(0, 100, 20),
      labels = function (x) ifelse(x == 100, '100+', x)
    ) +
    scale_y_continuous(
      labels = function(x) {formatC(abs(x*1e-3), format = 'd')}
    ) +
    coord_flip() +
    facet_wrap(~fct, scales = 'free_x') +
    labs(x = 'Age', y = 'Population in 1000s')
}

# Load data -------------------------------------------------------

dat$skeleton <- readRDS(paths$input$skeleton.rds)

dat$wpp <- list()
dat$wpp$input <- readRDS(paths$input$wppjoint.rds)
dat$hmdhfd <- list()
dat$hmdhfd$input <- readRDS(paths$input$hmdhfd.rds)

# Harmonize WPP population ----------------------------------------

dat$wpp$clean <-
  dat$wpp$input |>
  # select columns of interest
  select(
    population_source,
    region = ISO2_code,
    iso_year = Time, age = AgeGrpStart,
    male = Male, female = Female
  ) |>
  # sex to long format
  pivot_longer(
    cols = c(female, male),
    names_to = 'sex',
    values_to = 'value'
  ) |>
  # ensure proper names of factor variables
  mutate(
    sex =
      factor(sex, levels = names(cnst$code_sex_wpp),
             labels = cnst$code_sex_wpp) |>
      as.character()
  ) |>
  # add row id
  mutate(id = GenerateRowID(region, sex, iso_year, age)) |>
  # make popjan1st, popmidyear, fertility different columns
  separate(
    population_source, into = c('population_source', 'measure', 'timeframe'),
    sep = '_'
  ) |>
  mutate(
    measure = paste(measure, population_source, sep = '_')
  ) |>
  select(-timeframe, -population_source) |>
  pivot_wider(
    names_from = measure, values_from = value
  ) |>
  mutate(
    # WPP scales popnumbers in 1000's, so we scale back
    popjan1st_wpp24 = popjan1st_wpp24*1000,
    popmidyear_wpp24 = popmidyear_wpp24*1000,
    popjan1st_wpp22 = popjan1st_wpp22*1000,
    popmidyear_wpp22 = popmidyear_wpp22*1000,
    # set fertility to 0 where we know it to be essentially 0
    # also remove the x1000 scaling
    fertility = ifelse(sex == 'Male' | age < 15 | age > 49,
                       0, fertility_wpp24/1000),
    fertility_source = 'wpp24'
  )

# Check WPP data --------------------------------------------------

# check if data makes sense
fig$wpp_population <-
  PopPyramids(
    dat = dat$wpp$clean,
    population = popmidyear_wpp24,
    age = age, sex = sex, year = iso_year, highlight = iso_year,
    facet = region
  ) +
  scale_color_viridis_c() +
  labs(title = 'WPP midyear population estimates 1990-2024') +
  MyGGplotTheme(scaler = 0.8, panel_border = FALSE)
fig$wpp_population

fig$wpp24vs22 <-
  dat$wpp$clean |>
  filter(sex == 'Female') |>
  filter(iso_year %in% c(2020:2023)) |>
  mutate(delta = popmidyear_wpp24/popmidyear_wpp22) |>
  ggplot() +
  aes(x = age, y = delta, color = as.factor(iso_year)) +
  geom_hline(yintercept = 1, color = 'grey40') +
  geom_line() +
  scale_color_viridis_d() +
  facet_wrap(~region) +
  scale_y_log10() +
  coord_cartesian(ylim = c(0.9, 1.1)) +
  MyGGplotTheme(panel_border = FALSE) +
  theme(panel.background = element_rect(color = NA, fill = 'grey95')) +
  labs(
    title = 'Ratio of female midyear population WPP 24 vs. 22',
    y = 'Ratio', x = 'Age', color = 'Year'
  )
fig$wpp24vs22

# contrast wpp24 numbers with wpp22
dat$wpp$clean |>
  filter(iso_year == 2023, sex == 'Male', region == 'BG') |>
  select(region, iso_year, age, sex, popmidyear_wpp22, popmidyear_wpp24) |>
  mutate(ratio = popmidyear_wpp24/popmidyear_wpp22)
dat$wpp$clean |>
  filter(iso_year == 2023, sex == 'Male', region == 'BG') |>
  select(region, iso_year, age, sex, popmidyear_wpp22, popmidyear_wpp24) |>
  mutate(ratio = popmidyear_wpp24/popmidyear_wpp22)

# Harmonize HMD death rates ---------------------------------------

dat$hmdhfd$deathrate <-
  # skeleton
  expand_grid(
    year = config$skeleton$year$start:config$skeleton$year$end,
    region_code_hmd = cnst$region_lookup_hmd$region_code_hmd,
    sex = c('Female', 'Male'),
    age_start = 0:110
  ) |>
  # death rates
  left_join(
    dat$hmdhfd$input$hmd_mort |>
      pivot_longer(
        c(Female, Male),
        names_to = 'sex', values_to = 'death_rate'
      ) |>
      select(region_code_hmd, year = Year, age_start = Age, sex, death_rate)
  ) |>
  # ensure proper names of factor variables
  mutate(
    sex =
      factor(sex, levels = names(cnst$code_sex_hmdhfd),
             labels = cnst$code_sex_hmdhfd) |>
      as.character(),
    region_iso = factor(
      region_code_hmd,
      levels = cnst$region_lookup_hmd$region_code_hmd,
      labels = cnst$region_lookup_hmd$region_code_iso3166_2
    ) |> as.character()
  ) |>
  # add row id
  mutate(id = GenerateRowID(region_iso, sex, year, age_start)) |>
  select(id, deathrate_hmd = death_rate)

# Harmonize HMD population ----------------------------------------

# for years not yet in the data we get population estimates via a
# Leslie-Matrix projection of midyear population assuming a stable
# population, i.e. no migration and constant fertility and mortality
# rates. we use the 'female dominant' projection of the male population.
# We use 5 year average mortality and fertility rates based on years
# 2015-2019. If fertility or mortality rates are not available, we don't project.

# prepare a data set with required variables for
# Leslie matrix population projection for GB sub-regions
dat$hmdhfd$estimates <-
  # skeleton
  expand_grid(
    year = config$skeleton$year$start:config$skeleton$year$end,
    region_code_hmd = cnst$region_lookup_hmd$region_code_hmd,
    sex = c('Female', 'Male'),
    age_start = 0:110
  ) |>
  # midyear population
  left_join(
    dat$hmdhfd$input$hmd_midyear_pop |>
      pivot_longer(
        c(Female, Male),
        names_to = 'sex', values_to = 'population_midyear'
      ) |>
      select(region_code_hmd, year = Year, age_start = Age,
             sex, population_midyear)
  ) |>
  # death rates
  left_join(
    dat$hmdhfd$input$hmd_mort |>
      pivot_longer(
        c(Female, Male),
        names_to = 'sex', values_to = 'death_rate'
      ) |>
      select(region_code_hmd, year = Year, age_start = Age, sex, death_rate)
  ) |>
  # female fertility rates
  left_join(
    dat$hmdhfd$input$hfd_fert |>
      select(region_code_hmd, year = Year, age_start = Age,
             female_fertility_rate = ASFR)
  ) |>
  # add pre-pandemic averages of age specific death rates and fertility
  group_by(region_code_hmd, sex, age_start) |>
  mutate(
    death_rate_2015_2019 = mean(death_rate[year %in% 2015:2019], na.rm = TRUE),
    female_fertility_rate_2015_2019 = mean(female_fertility_rate[year %in% 2015:2019], na.rm = TRUE)
  ) |>
  ungroup() |>
  # NA handling
  mutate(
    # fertility rates are NA outside the age range [12, 55],
    # replace with 0
    female_fertility_rate_2015_2019 =
      ifelse(is.na(female_fertility_rate_2015_2019), 0, female_fertility_rate_2015_2019),
    # same with death rates in highest ages
    death_rate_2015_2019 =
      ifelse(is.na(death_rate_2015_2019), 0, death_rate_2015_2019)
  )

dat$hmdhfd$jumpoff_years <-
  dat$hmdhfd$estimates |>
  group_by(region_code_hmd) |>
  summarise(
    jumpoff = max(year[!is.na(population_midyear)]),
    h = config$skeleton$year$end-jumpoff,
    h = ifelse(h<0, 0, h),
    fertility_available = !all(female_fertility_rate_2015_2019==0),
    mortality_available = !all(death_rate_2015_2019==0)
  ) |>
  ungroup()

# subset estimates to jumpoff year
dat$hmdhfd$estimates <-
  left_join(dat$hmdhfd$estimates, dat$hmdhfd$jumpoff_years) |>
  filter(year <= jumpoff)

# perform the projections by region
# start at last available HMD midyear population and project through 2024
dat$hmdhfd$projection <-
  dat$hmdhfd$estimates |>
  filter(fertility_available, mortality_available, h > 0) |>
  group_by(region_code_hmd) |>
  group_modify(~{

    jumpoff <- .x$jumpoff[1]
    h <- .x$h[1]

    cat('Project', .y[[1]], 'from', jumpoff, 'for', h, 'years', '\n')

    male <- filter(.x, sex == 'Male', year == jumpoff)
    female <- filter(.x, sex == 'Female', year == jumpoff)

    projection <- StableProjection1x1(
      pop_m = male$population_midyear, pop_f = female$population_midyear,
      mx_m = male$death_rate_2015_2019, mx_f = female$death_rate_2015_2019,
      fx_f = female$female_fertility_rate_2015_2019,
      n = h, srb = 1.04
    )

    projection$year <- jumpoff + projection$n

    return(projection)

  }) |>
  ungroup()

# bind and harmonize estimates and projections
dat$hmdhfd$clean <-
  bind_rows(
    hmd_estimates = select(
      dat$hmdhfd$estimates,
      region_code_hmd, year, sex, age_start, population_midyear
    ),
    hmd_projections = select(
      dat$hmdhfd$projection,
      region_code_hmd, year, age_start, sex, population_midyear
    ),
    .id = 'population_source'
  ) |>
  # make age 100 an open age group
  mutate(
    age_start = ifelse(age_start > 100, 100, age_start)
  ) |>
  group_by(population_source, region_code_hmd, year, sex, age_start) |>
  summarise(population_midyear = sum(population_midyear)) |>
  ungroup() |>
  # derive jan1st population from midyear population
  group_by(region_code_hmd, sex, age_start) |>
  arrange(region_code_hmd, age_start, sex, year) |>
  mutate(
    population_jan1st = (population_midyear+lag(population_midyear))/2
  ) |>
  ungroup() |>
  # ensure proper names of factor variables
  mutate(
    sex =
      factor(sex, levels = names(cnst$code_sex_hmdhfd),
             labels = cnst$code_sex_hmdhfd) |>
      as.character(),
    region_iso = factor(
      region_code_hmd,
      levels = cnst$region_lookup_hmd$region_code_hmd,
      labels = cnst$region_lookup_hmd$region_code_iso3166_2
    ) |> as.character()
  ) |>
  # add row id
  mutate(id = GenerateRowID(region_iso, sex, year, age_start))

# Check HMD data --------------------------------------------------

# check if projection went well
fig$hmd_midyear_population_projection_by_age <-
  dat$hmdhfd$clean |>
  filter(year %in% c(2018:2024)) |>
  PopPyramids(
    population = population_midyear,
    age = age_start, sex = sex, year = year,
    highlight = population_source, facet = region_iso
  ) +
  scale_color_manual(values = c('grey50', 'red')) +
  labs(title = 'HMD population estimates (grey) and stable population projections (red)') +
  MyGGplotTheme(scaler = 0.8)
fig$hmd_midyear_population_projection_by_age

fig$hmd_midyear_population_projection_total <-
  dat$hmdhfd$clean |>
  group_by(year, population_source, region_iso) |>
  summarise(
    N = sum(population_midyear)/1000
  ) |>
  ggplot(aes(x = year, y = N, color = population_source)) +
  geom_point() +
  scale_x_continuous(breaks = seq(2000, 2024, 10)) +
  scale_y_continuous(labels = scales::comma_format()) +
  scale_color_manual(values = c('grey50', 'red')) +
  facet_wrap(~region_iso, scales = 'free_y') +
  MyGGplotTheme(scaler = 0.8) +
  theme(
    legend.position.inside = c(0.9, 0.2)
  ) +
  labs(
    title = 'HMD population estimates (grey) and stable population projections (red)',
    x = NULL, y = 'Population in 1000s',
    color = 'Population source'
  )
fig$hmd_midyear_population_projection_total

# Join population data with skeleton ------------------------------

# join the different sources of population count data
# with the skeleton
dat$pop_joined <-
  dat$skeleton |>
  left_join(
    select(
      dat$wpp$clean,
      id,
      fertility_rate_wpp24 = fertility,
      population_jan1st_wpp22 = popjan1st_wpp22,
      population_jan1st_wpp24 = popjan1st_wpp24,
      population_midyear_wpp22 = popmidyear_wpp22,
      population_midyear_wpp24 = popmidyear_wpp24

    ),
    by = 'id'
  ) |>
  left_join(
    select(
      dat$hmdhfd$clean,
      id,
      population_midyear_hmd = population_midyear,
      population_jan1st_hmd = population_jan1st,
      population_source_hmd = population_source
    ),
    by = 'id'
  ) |>
  left_join(
    dat$hmdhfd$deathrate, by = 'id'
  )

# Choose default sources ------------------------------------------

# default sources for population exposures are defined in the config.yaml
dat$pop_joined <-
  dat$pop_joined |>
  mutate(
    # midyear population
    population_midyear = case_when(
      region %in% config$exposureselection$wpp24 ~ round(population_midyear_wpp24, 1),
      region %in% config$exposureselection$hmd ~ round(population_midyear_hmd, 1),
      region %in% config$exposureselection$wpp22 ~ round(population_midyear_wpp22, 1),
      TRUE ~ as.numeric(NA)
    ),
    population_midyear_source = case_when(
      region %in% config$exposureselection$hmd ~ population_source_hmd,
      region %in% config$exposureselection$wpp24 ~ 'wpp24',
      region %in% config$exposureselection$wpp22 ~ 'wpp22',
      TRUE ~ as.character(NA)
    ),
    # jan 1st population
    population_jan1st = case_when(
      region %in% config$exposureselection$wpp24 ~ round(population_jan1st_wpp24, 1),
      region %in% config$exposureselection$hmd ~ round(population_jan1st_hmd, 1),
      region %in% config$exposureselection$wpp22 ~ round(population_jan1st_wpp22, 1),
      TRUE ~ as.numeric(NA)
    ),
    population_jan1st_source = case_when(
      region %in% config$exposureselection$wpp24 ~ 'wpp24',
      region %in% config$exposureselection$hmd ~ population_source_hmd,
      region %in% config$exposureselection$wpp22 ~ 'wpp22',
      TRUE ~ as.character(NA)
    )
  )

# Select variables of interest for export -------------------------

dat$export <-
  dat$pop_joined |>
  # select output variables of interest
  select(
    id,
    population_midyear, population_midyear_source,
    population_jan1st, population_jan1st_source,
    fertility_rate = fertility_rate_wpp24,
    deathrate_hmd,
    population_midyear_wpp22, population_midyear_wpp24, population_midyear_hmd,
    population_jan1st_wpp22, population_jan1st_wpp24, population_jan1st_hmd
  )

# Export ----------------------------------------------------------

saveRDS(dat$export, file = paths$output$harmonized_population.rds)

ExportFigure(
  fig$wpp_population, filename = paths$output$wpp_population.pdf,
  width = config$figspec$dimensions$width,
  height = 1.3*config$figspec$dimensions$width
)

ExportFigure(
  fig$wpp24vs22, filename = paths$output$wpp24vs22.pdf,
  width = config$figspec$dimensions$width,
  height = 1.3*config$figspec$dimensions$width
)

ExportFigure(
  fig$hmd_midyear_population_projection_by_age,
  filename = paths$output$hmd_midyear_population_projection_by_age.pdf,
  width = config$figspec$dimensions$width,
  height = 1*config$figspec$dimensions$width
)

ExportFigure(
  fig$hmd_midyear_population_projection_total,
  filename = paths$output$hmd_midyear_population_projection_total.pdf,
  width = config$figspec$dimensions$width,
  height = 1*config$figspec$dimensions$width
)

write.xlsx(dat$hmdhfd$jumpoff_years, file = paths$output$hmdprojjumpoffyears.xlsx,
           keepNA = TRUE, na.string = '.',
           firstRow = TRUE, firstCol = TRUE, overwrite = TRUE)
