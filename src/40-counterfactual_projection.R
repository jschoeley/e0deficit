# Project population under pandemic and non-pandemic scenarios
#
# Project Jan 1st 2020 population under application of pandemic
# and pre-pandemic trend mortality rates. Pre-pandemic mortality rates
# extrapolated via Lee-Carter forecast. Cohort-Component population
# forecast.

# Init ------------------------------------------------------------

library(yaml)
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(StMoMo)

# Constants -------------------------------------------------------

# input and output paths
setwd('.')
paths <- list()
paths$input <- list(
  config.yaml = './cfg/config.yaml',
  global_functions.R = './src/_global_functions.R',
  region_metadata.csv = './cfg/region_metadata.csv',
  analysisinput_bias_corrected.rds = './out/31-analysisinput_bias_corrected.rds',
  harmonized_skeleton.rds = './tmp/10-harmonized_skeleton.rds'
)
paths$output <- list(
  leecarter_forecasts_sim.rds = './tmp/40-leecarter_forecasts_sim.rds',
  counterfactual_projections_sim.rds = './tmp/40-counterfactual_projections_sim.rds',
  leecarter.pdf = './tmp/40-leecarter.pdf'
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
  lee_carter_fitting_period = 2010:2019
  # forecast population at Dec 31 of given year, based on Jan 1st jumpoff
  forecast_horizon =
    (config$forecast$jumpoff):(config$forecast$jumpoff + config$forecast$h-1)
  nsim = config$nsim
  seed = 1987
})

# Functions -------------------------------------------------------

# array structure for population data
MakePopArray <- function (
    measure,
    age = config$skeleton$age$start:config$skeleton$age$end,
    year = config$skeleton$year$start:config$skeleton$year$end,
    sex = config$skeleton$sex,
    region = config$skeleton$regions,
    nsim = 0
) {

  if (isTRUE(nsim > 0)) {

    X <- array(
      measure,
      dim = c(
        length(age), length(year), length(sex),
        length(region), length(1:nsim)
      ),
      dimnames = list(
        age = age, year = year, sex = sex,
        region = region, nsim = 1:nsim
      )
    )

  } else {

    X <- array(
      measure,
      dim = c(length(age), length(year), length(sex), length(region)),
      dimnames = list(age = age, year = year, sex = sex, region = region)
    )

    return(X)

  }

  return(X)

}

# create stmomo object
Pop2StMoMo <- function (
    X,
    years, sex, label
) {
  age = 0:100

  Dxt_vec <- X[['death']]
  Dxt <- matrix(Dxt_vec, nrow = 101, ncol = length(years))
  colnames(Dxt) <- years
  rownames(Dxt) <- age

  Ext_vec <- X[['population_py']]
  Ext <- matrix(Ext_vec, nrow = 101, ncol = length(years))
  colnames(Ext) <- years
  rownames(Ext) <- age

  stmomodata <- structure(
    list(Dxt = Dxt, Ext = Ext, ages = age, years = years,
         type = 'central', series = sex, label = label),
    class = "StMoMoData"
  )

  return(stmomodata)
}

# Lee-Carter Forecast
ForecastLeeCarter <- function (stmomo, h, nsim, seed) {

  constLC <- function(ax, bx, kt, b0x, gc, wxt, ages) {
    c1 <- mean(kt[1, ], na.rm = TRUE)
    c2 <- sum(bx[, 1], na.rm = TRUE)
    list(ax = ax + c1 * bx, bx = bx / c2, kt = c2 * (kt - c1))
  }
  LC_def <- StMoMo(link = 'log', staticAgeFun = TRUE,
                   periodAgeFun = 'NP', constFun = constLC)
  LC_fit <- fit(LC_def, data = stmomo)
  LC_sim <-
    simulate(LC_fit, nsim = nsim, h = h,
             jumpchoice = 'fit',
             seed = seed,
             kt.method = 'mrwd'#, kt.order = c(1,1,2)
             )[['rates']]

  return(list(fit = LC_fit, sim = LC_sim))
}

#' Leslie Population Projection
ProjectPopulation <- function (
    P0_xm, P0_xf, M_xtm, M_xtf, F_xtf, N_xtm, N_xtf, n = 1, srb = 1.04
) {

  nage = length(P0_xm)

  # first project the female population for n steps
  # population matrix, first column is jump off population
  Proj_xtf <- matrix(0, nrow = nage, ncol = n+1)
  Proj_xtf[,1] <- P0_xf
  for (i in 1:n+1) {
    # Leslie projection matrix
    A_tf <- matrix(0, nrow = nage, ncol = nage)
    diag(A_tf[-1,-nage]) <- head(exp(-M_xtf[,i-1]), -1)
    A_tf[1,] <- F_xtf[,i-1] * 1/(1+srb)

    # project population
    Proj_xtf[,i] <- A_tf%*%Proj_xtf[,i-1] + N_xtf[,i-1]
  }

  # now project the male population with male births derived from
  # projected female births via sex ratio
  Proj_xtm <- matrix(0, nrow = nage, ncol = n+1)
  Proj_xtm[,1] <- P0_xm
  for (i in 1:n+1) {
    A_tm <- matrix(0, nage, ncol = nage)
    diag(A_tm[-1,-nage]) <- head(exp(-M_xtm[,i-1]), -1)
    Proj_xtm[,i] <- A_tm%*%Proj_xtm[,i-1] + N_xtm[,i-1]
    Proj_xtm[1,i] <- Proj_xtf[1,i]*srb
  }

  dimnames(Proj_xtf) <-
    list(age = 0:(nrow(Proj_xtf)-1), year = 0:n)
  dimnames(Proj_xtm) <-
    list(age = 0:(nrow(Proj_xtm)-1), year = 0:n)

  projection <- list(
    female = Proj_xtf, male = Proj_xtm
  )

  return(projection)

}

# Load population data --------------------------------------------

skeleton <- readRDS(paths$input$harmonized_skeleton.rds)
analysisinput <- readRDS(paths$input$analysisinput_bias_corrected.rds)

# Patch data ------------------------------------------------------

# updated data with official data for Sweden in the "death2" column
# analysisinput <-
#   readxl::read_xlsx('out/30-analysisinput_sensitivity.xlsx', na = '.',
#                     col_types = c(
#                       rep('text', 3),
#                       rep('numeric', 11),
#                       'text', 'numeric',
#                       'text', rep('numeric', 4),
#                       'text', 'numeric', 'text', 'numeric', 'text'
#                     )) |>
#   mutate(death = ifelse(region == 'SE', death2, death),
#          age_width = ifelse(is.na(age_width), Inf, age_width))

# patch 0 fertility and migration for regions with no data
# this is no problem as long as this assumption is made both in the
# actual and in the counterfactual scenario
analysisinput <-
  analysisinput |>
  mutate(
    fertility_rate =
      ifelse(
        region %in% c('US', 'GB-EAW', 'GB-SCT', 'GB-NIR', 'TW', 'CL', 'IL'),
        0, fertility_rate
      ),
    netmigration_eurostat =
      ifelse(
        region %in% c('US', 'GB-EAW', 'GB-SCT', 'GB-NIR', 'TW', 'CL', 'IL', 'GR'),
        0, netmigration_eurostat)
  )

# Forecast mortality ----------------------------------------------

mortality_forecast <- list()

mortality_forecast$matrix <-
  MakePopArray(NA, year = cnst$forecast_horizon, nsim = cnst$nsim)

# stochastic forecast of mortality rates 2020-24
for (country in config$skeleton$regions) {
  for (sex in unlist(config$skeleton$sex)) {

    cat('Forecast:', country, sex, '\n')

    fitting_period <- cnst$lee_carter_fitting_period
    # adjust fitting periods based on data availability
    if (country == 'CZ') {fitting_period <- 2010:2019}
    if (country == 'DK') {fitting_period <- 2010:2019}
    if (country == 'IT') {fitting_period <- 2011:2019}
    #if (country == 'GR') {fitting_period <- 2015:2019}
    if (country == 'GB-EAW') {fitting_period <- 2010:2019}
    #if (country == 'GB-NIR') {fitting_period <- 2015:2019}
    if (country == 'GB-SCT') {fitting_period <- 2010:2019}
    if (country == 'HR') {fitting_period <- 2010:2019}

    # subset to single population by period and age
    analysisinput_sub <- analysisinput[
      analysisinput$region == country &
      analysisinput$year %in% fitting_period &
      analysisinput$sex == sex,]
    # skip stratum if data is NA
    if (
      anyNA(c(analysisinput_sub$death,
              analysisinput_sub$population_py))|
      nrow(analysisinput_sub)==0
    ) { next }

    # fit and forecast
    model_input <- Pop2StMoMo(
      analysisinput_sub,
      years = fitting_period,
      sex = sex, label = country
    )
    fit <- ForecastLeeCarter(
      model_input, h = config$forecast$h,
      nsim = cnst$nsim,
      # this is to ensure a different seed for every stratum
      seed = cnst$seed+sum(utf8ToInt(paste0(country,sex)))
    )

    # forecasted and simulated mortality rates
    mortality_forecast$matrix[,,sex,country,] <- fit[['sim']]

  }
}

# Prepare population matrices -------------------------------------

fertility <- MakePopArray(analysisinput$fertility_rate)
migration <- MakePopArray(analysisinput$netmigration_eurostat)
jumpoff <- MakePopArray(analysisinput$population_jan1st)
mortality_observed <-
  MakePopArray(analysisinput$death/analysisinput$population_py)

apply(fertility, c('region', 'year'), anyNA)
apply(migration, c('region', 'year'), anyNA)
apply(jumpoff, c('region', 'year'), anyNA)[,'2020']
apply(mortality_observed, c('region', 'year'), anyNA)
apply(mortality_forecast$matrix, c('region', 'year'), anyNA)

# Projection result matrix ----------------------------------------

projections <- list()

# Dec31st 2020, 21, 22, 23, 24
projections$horizont <- as.character(cnst$forecast_horizon)
# Jan 1st 2020
projections$jumpoff <- as.character(config$forecast$jumpoff)

projections$expected <-
  array(
    dim = c(
      config$skeleton$age$end-config$skeleton$age$start+1,
      config$forecast$h, 2, length(config$skeleton$regions), cnst$nsim
    ),
    dimnames = list(
      age = config$skeleton$age$start:config$skeleton$age$end,
      year = projections$horizont,
      sex = config$skeleton$sex,
      region = config$skeleton$regions,
      nsim = 1:cnst$nsim
    )
  )
projections$observed <- projections$expected

# Project populations under pandemic and nonpandemic scena --------

for (region in config$skeleton$regions) {

  cat('Project Jan 1st population', region, '\n')

  # project Jan 1st 2020 population
  # under application of pandemic mortality rates
  observed <- ProjectPopulation(
    P0_xm = jumpoff[,projections$jumpoff,'Male',region],
    P0_xf = jumpoff[,projections$jumpoff,'Female',region],
    M_xtm = mortality_observed[,projections$horizont,'Male',region],
    M_xtf = mortality_observed[,projections$horizont,'Female',region],
    F_xtf = fertility[,projections$horizont,'Female',region],
    N_xtm = migration[,projections$horizont,'Male',region],
    N_xtf = migration[,projections$horizont,'Female',region],
    n = config$forecast$h, srb = 1.04
  )
  projections$observed[,,'Female',region,] <- observed$female[,-1]
  projections$observed[,,'Male',region,] <- observed$male[,-1]

  # project Jan 1st 2020 population
  # under application of pre-pandemic trend mortality rates
  for (sim in 1:cnst$nsim) {

    expected <- ProjectPopulation(
      P0_xm = jumpoff[,projections$jumpoff,'Male',region],
      P0_xf = jumpoff[,projections$jumpoff,'Female',region],
      M_xtm = mortality_forecast$matrix[,projections$horizont,'Male',region,sim],
      M_xtf = mortality_forecast$matrix[,projections$horizont,'Female',region,sim],
      F_xtf = fertility[,projections$horizont,'Female',region],
      N_xtm = migration[,projections$horizont,'Male',region],
      N_xtf = migration[,projections$horizont,'Female',region],
      n = config$forecast$h, srb = 1.04
    )
    projections$expected[,,'Female',region,sim] <-
      expected$female[,-1]
    projections$expected[,,'Male',region,sim] <-
      expected$male[,-1]

  }
}

# Convert projection array to df ----------------------------------

projections$all <- abind::abind(
  projections$observed,
  projections$expected,
  along = 6, use.dnns = TRUE
)
dimnames(projections$all)[[6]] <-
  c('population_dec31st_covidmortality',
    'population_dec31st_noncovidmortality')
names(dimnames(projections$all))[[6]] <- 'scenario'

projections$df <-
  as.data.frame.table(projections$all, stringsAsFactors = FALSE) |>
  pivot_wider(names_from = scenario, values_from = Freq) |>
  mutate(across(c(age, year, nsim), ~as.integer(.x)))

# Validate Lee-Carter projection ----------------------------------

leecarter <- list()

# calculate life-expectancy for observed data
leecarter$data$observed <-
  as.data.frame.table(mortality_observed, responseName = 'mx') |>
  mutate(year = as.integer(as.character(year))) |>
  group_by(year, sex, region) |>
  mutate(
    nx = ifelse(age == 100, Inf, 1),
    px = exp(-mx*nx),
    qx = 1-px,
    lx = head(cumprod(c(1, px)), -1),
    dx = c(-diff(lx), tail(lx, 1)),
    Lx = ifelse(mx==0, lx*nx, dx/mx),
    Tx = rev(cumsum(rev(Lx))),
    ex = Tx/lx
  ) |>
  filter(year < 2020, year >= 2000, age == 0)

# calculate life-expectancy from forecasted data
leecarter$data$predicted <-
  as.data.frame.table(mortality_forecast$matrix, responseName = 'mx') |>
  mutate(year = as.integer(as.character(year))) |>
  group_by(year, sex, region, nsim) |>
  mutate(
    nx = ifelse(age == 100, Inf, 1),
    px = exp(-mx*nx),
    qx = 1-px,
    lx = head(cumprod(c(1, px)), -1),
    dx = c(-diff(lx), tail(lx, 1)),
    Lx = ifelse(mx==0, lx*nx, dx/mx),
    Tx = rev(cumsum(rev(Lx))),
    ex = Tx/lx
  ) |>
  filter(age == 0)

# plot life expectancy projections
leecarter$plot <-
  data.frame() |>
  ggplot() +
  geom_line(
    aes(x = year, y = ex, color = sex),
    data = leecarter$data$observed
  ) +
  geom_line(
    aes(x = year, y = ex, group = interaction(nsim,sex), color = sex),
    alpha = 0.01, data = leecarter$data$predicted
  ) +
  scale_color_manual(values = unlist(config$figspec$colors$sex)) +
  scale_x_continuous(
    breaks = c(2000, 2005, 2010, 2015, 2020, 2025),
    labels = c('2000', '05', '2010', '15', '2020', '25')
  ) +
  scale_y_continuous(breaks = 0:100, labels = c(t(cbind(seq(0, 98, 2), ' ')), '100')) +
  facet_wrap(~region, scales = 'free_y') +
  MyGGplotTheme(panel_border = FALSE) +
  labs(y = 'Life expectancy', x = NULL,
       title = 'Poisson-Lee-Carter projections of life expectancy 2020-24')

leecarter$plot

# Prepare forecasted mortality for export -------------------------

leecarter$harmonized_forecast_sim <-
  mortality_forecast$matrix |>
  as.data.frame.table(
    responseName = 'mx',
    stringsAsFactors = FALSE
  ) |>
  mutate(year = as.integer(year), age = as.double(age)) |>
  left_join(
    skeleton |> select(-id),
    by = c('region', 'sex', 'year', 'age' = 'age_start')
  )

# Export ----------------------------------------------------------

ExportFigure(
  leecarter$plot, filename = paths$output$leecarter.pdf,
  scale = 1.5
)
saveRDS(
  leecarter$harmonized_forecast_sim,
  file = paths$output$leecarter_forecasts_sim.rds,
  compress = 'xz'
)

saveRDS(
  projections$df,
  file = paths$output$counterfactual_projections_sim.rds,
  compress = 'xz'
)
