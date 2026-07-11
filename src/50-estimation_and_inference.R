# Estimate life tables and associated statistics with CIs
#
# Simulation based life table inference based on Poisson samples of
# observed death counts. Implemented in a huge array.

# Init ------------------------------------------------------------

library(yaml)
library(dplyr); library(tidyr); library(readr)
library(qs2)

# Constants -------------------------------------------------------

# randomness in Poisson sampling
set.seed(1987)

# input and output paths
setwd('.')
paths <- list()
paths$input <- list(
  config.yaml = './cfg/config.yaml',
  analysisinput_bias_corrected.rds = './out/31-analysisinput_bias_corrected.rds',
  leecarter_forecast_sim.rds = './tmp/40-leecarter_forecasts_sim.rds',
  global_functions = './src/_global_functions.R'
)
paths$output <- list(
  lifetables.rds = './out/50-lifetables.rds',
  lifetables.csv = './out/50-lifetables.csv',
  lifetables_sim.qs = './tmp/50-lifetables_sim.qs',
  deficits_and_excesses.rds = './out/50-deficits_and_excesses.rds',
  deficits_and_excesses.csv = './out/50-deficits_and_excesses.csv',
  deficits_and_excesses_sim.qs = './tmp/50-deficits_and_excesses_sim.qs',
  pval.rds = './out/50-pval.rds',
  pval.csv = './out/50-pval.csv',
  pval.pdf = './out/50-pval.pdf'
)

# global configuration
config <- read_yaml(paths$input$config.yaml)

# constants specific to this analysis
cnst <- list(); cnst <- within(cnst, {
  regions_for_analysis = config$regions_for_all_cause_analysis
  # number of Poisson life-table replicates
  n_sim = config$nsim
  # quantiles for CI's
  quantiles = c(0.025, 0.05, 0.1, 0.5, 0.9, 0.95, 0.975)
  forecast_period = seq(
    config$forecast$jumpoff,
    config$forecast$jumpoff+config$forecast$h-1
  )
})

tmp <- list()

# Function --------------------------------------------------------

source(paths$input$global_functions)

# this function returns TRUE wherever elements are the same,
# including NA's, and FALSE everywhere else
compareNA <- function(v1, v2) {
  same <- (v1 == v2) | (is.na(v1) & is.na(v2))
  same[is.na(same)] <- FALSE
  return(same)
}

# return a vector of mean and quantiles
QuantileWithMean <- function (x, prob = cnst$quantiles) {
  x <- x[!(is.na(x)|is.nan(x)|is.infinite(x))]
  q <- quantile(x, prob = prob, names = FALSE, na.rm = TRUE)
  m <- mean(x, na.rm = TRUE)
  result <- c(m, q)
  names(result) <- c('mean', paste0('q', prob))
  return(result)
}

# interpolate a range of values with its mean
InterpolateWithMean <- function (x) {
  rep(mean(x), length(x))
}

# Data ------------------------------------------------------------

lt_input <- list()

# input data for life-table calculation
# harmonized death counts and population exposures with open age group 100+
lt_input$openage_100 <- readRDS(paths$input$analysisinput_bias_corrected.rds)

# harmonized counterfactual Lee-Carter forecasts
lt_input$leecarter <- readRDS(paths$input$leecarter_forecast_sim.rds)

# Create Poisson replicates of counts -----------------------------

# create life table replicates by region, sex, and year
# based on repeatedly sampling death counts from a Poisson
# distribution with mean equal to estimated mean from PCLM

lifetables <- list()
lifetables$cnst <- list(
  nage = length(unique(lt_input$openage_100$age_start)),
  nyears =
    # number of years
    config$skeleton$year$end-config$skeleton$year$start+1 +
    # number of aggregate periods of interest
    1,
  nregions = length(config$skeleton$region)
)

lifetables$input <-
  lt_input$openage_100 |>
  select(
    id, region, sex, year, age_start, age_width,
    population_py, death_observed = death,
    nweeks_year
  )
# vector ordered by sex, region, year, age distributed into
# 7D array [age, year, sex, region_id, sim_id, var_id, scenario]
# DESCRIPTION OF VAR ID'S
# LIFE TABLE
# (1)  <population_py>    person years exposure
# (2)  <death_total>      total death count
# (3)  ---empty---
# (4)  <nmx>              death rate
# (5)  <npx>              conditional probability of surviving age x
# (6)  <nqx>              conditional probability of dying within age x
# (7)  <lx>               probability of surviving to age x
# (8)  <ndx>              probability of dying within age x
# (9)  <nLx>              life table person years of exposure in age x
# (10) <Tx>               life table person years of exposure above age x
# (11) <ex>               life expectancy at age x
# ARRIAGA DECOMPOSITION LE CHANGE
# (12) <ex_lag>           1 year lag in ex
# (13) <ex_diff>          1 year difference in ex
# (14) <ex_diff_lag>      1 year lag in ex difference
# (15) <lx_lag>           1 year lag in lx
# (16) <Lx_lag>           1 year lag in Lx
# (17) <Tx_lag>           1 year lag in Tx
# (18) <e0_cntrb_d>       direct contribution of nmx changes to e0 changes
# (19) <e0_cntrb_i>       indirect contribution of nmx changes to e0 changes
# (20) <e0_cntrb_t>       total contrib. of nmx changes to e0 changes
lifetables$simulation <-
  array(
    dim = c(
      age = lifetables$cnst$nage,
      year = lifetables$cnst$nyears,
      sex = 3,
      region_iso = lifetables$cnst$nregions,
      sim_id = cnst$n_sim+1, # first index is mean
      var_id = 20,
      scenario = 2
    ),
    dimnames = list(
      0:(lifetables$cnst$nage-1),
      c(
        config$skeleton$year$start:config$skeleton$year$end,
        '2020-2024'
      ),
      c(unlist(config$skeleton$sex), 'Total'),
      config$skeleton$region,
      1:(cnst$n_sim+1),
      c('population_py', 'death_total', 'empty',
        'nmx', 'npx', 'nqx', 'lx', 'ndx', 'nLx', 'Tx', 'ex',
        'ex_lag', 'ex_diff', 'ex_diff_lag', 'lx_lag',
        'nLx_lag', 'Tx_lag',
        'e0_cntrb_d', 'e0_cntrb_i', 'e0_cntrb_t'),
      c('actual', 'projected')
    )
  )

# actual observables
lifetables$simulation[,-lifetables$cnst$nyears,-3,,,'population_py','actual'] <-
  lifetables$input$population_py
lifetables$simulation[,-lifetables$cnst$nyears,-3,,1,'death_total','actual'] <-
  lifetables$input$death_observed

# projected observables based on pre-pandemic trends
# here we store all-cause death counts how we would
# expect them under continuing pre-pandemic trends
lifetables$simulation[,-lifetables$cnst$nyears,-3,,,'population_py','projected'] <-
  lifetables$simulation[,-lifetables$cnst$nyears,-3,,,'population_py','actual']
lifetables$simulation[,as.character(cnst$forecast_period),
                      -3,,-1,'death_total','projected'] <-
  (
    lt_input$leecarter |>
      filter(year %in% cnst$forecast_period) |> pull(mx)
  ) *
  (
    lifetables$input |> filter(year %in% cnst$forecast_period) |>
      pull(population_py)
  )

# simulate observed total death counts
lifetables$simulation[,-lifetables$cnst$nyears,-3,,-1,'death_total','actual'] <-
  apply(lifetables$simulation[,-lifetables$cnst$nyears,-3,,1,'death_total','actual'],
        MARGIN = 1:4, function (lambda) rpois(n = cnst$n_sim, lambda),
        simplify = TRUE) |>
  aperm(c(2,3,4,5,1))

# check if actuals and projected somewhat line-up
lifetables$simulation[,'2020','Male','AT',2,'death_total',c('actual', 'projected')]

# Add sex-specific counts to total --------------------------------

# [age, year, sex, region_id, sim_id, var_id, scenario]
lifetables$simulation[,,'Total',,,c('death_total', 'population_py'),] <-
  lifetables$simulation[,,'Female',,,c('death_total', 'population_py'),] +
  lifetables$simulation[,,'Male',,,c('death_total', 'population_py'),]

# Add annual counts to 2020-2024 total ----------------------------

# [age, year, sex, region_id, sim_id, var_id, scenario]
lifetables$simulation[,'2020-2024',,,,c('death_total', 'population_py'),] <-
  lifetables$simulation[,'2020',,,,c('death_total', 'population_py'),] +
  lifetables$simulation[,'2021',,,,c('death_total', 'population_py'),] +
  lifetables$simulation[,'2022',,,,c('death_total', 'population_py'),] +
  lifetables$simulation[,'2023',,,,c('death_total', 'population_py'),] +
  lifetables$simulation[,'2024',,,,c('death_total', 'population_py'),]

# Calculate lifetables over simulated counts ----------------------

# nmx
lifetables$simulation[,,,,,'nmx',] <-
  lifetables$simulation[,,,,,'death_total',] /
  lifetables$simulation[,,,,,'population_py',]

# npx, using constant hazard assumption,
# i.e. npx = exp(-nmx)) for single year age groups
lifetables$simulation[,,,,,'npx',] <-
  exp(-lifetables$simulation[,,,,,'nmx',])
lifetables$simulation[lifetables$cnst$nage,,,,,'npx',] <- 0

# no need for fancy nax adjustment, I checked, virtually
# same results as with PWE
# nax <- lifetables$simulation[,,,,,'nmx',]
# nax[1:101,,,,,] <- 0.5
# I <- lifetables$simulation[1,,,'F',,'nmx',]<0.107
# I[is.na(I)] <- FALSE
# nax[1,,,'F',,][I] <-
#   0.053+2.8*lifetables$simulation[1,,,'F',,'nmx',][I]
# nax[1,,,'F',,][!I] <- 0.350
# I <- lifetables$simulation[1,,,'M',,'nmx',]<0.107
# I[is.na(I)] <- FALSE
# nax[1,,,'M',,][I] <-
#   0.045+2.684*lifetables$simulation[1,,,'M',,'nmx',][I]
# nax[1,,,'M',,][!I] <- 0.330
# lifetables$simulation[,,,,,'nqx',] <-
#   lifetables$simulation[,,,,,'nmx',] /
#   (1+(1-nax)*lifetables$simulation[,,,,,'nmx',])
# lifetables$simulation[lifetables$cnst$nage,,,,,'nqx',] <- 1
# lifetables$simulation[,,,,,'npx',] <-
#   1-lifetables$simulation[,,,,,'nqx',]

# nqx
lifetables$simulation[,,,,,'nqx',] <-
  1-lifetables$simulation[,,,,,'npx',]
# lx
lifetables$simulation[,,,,,'lx',] <-
  apply(
    lifetables$simulation[,,,,,'npx',],
    # apply function to vector of data by age
    MARGIN = 2:6, function (npx) head(cumprod(c(1, npx)), -1)
  )
# ndx
lifetables$simulation[,,,,,'ndx',] <-
  apply(
    lifetables$simulation[,,,,,'lx',],
    # apply function to vector of data by age
    MARGIN = 2:6, function (lx) c(-diff(lx), tail(lx, 1))
  )
# nLx = ifelse(mx==0, lx*nx, ndx/nmx)
lifetables$simulation[,,,,,'nLx',] <-
  lifetables$simulation[,,,,,'ndx',]/lifetables$simulation[,,,,,'nmx',]
tmp$I <- compareNA(lifetables$simulation[,,,,,'nmx',],0)
lifetables$simulation[,,,,,'nLx',][tmp$I] <-
  lifetables$simulation[,,,,,'lx',][tmp$I]
# Tx = rev(cumsum(rev(nLx)))
lifetables$simulation[,,,,,'Tx',] <-
  apply(
    lifetables$simulation[,,,,,'nLx',],
    # apply function to vector of data by age
    MARGIN = 2:6, function (nLx) rev(cumsum(rev(nLx)))
  )
# ex = Tx/lx
lifetables$simulation[,,,,,'ex',] <-
  lifetables$simulation[,,,,,'Tx',] /
  lifetables$simulation[,,,,,'lx',]

# Calculate annual ex change --------------------------------------

lifetables$simulation[,,,,,'ex_lag',] <-
  lifetables$simulation[,c(NA,1:(lifetables$cnst$nyears-2),NA),,,,'ex',]
lifetables$simulation[,,,,,'ex_diff',] <-
  lifetables$simulation[,,,,,'ex',]-lifetables$simulation[,,,,,'ex_lag',]
lifetables$simulation[,,,,,'ex_diff_lag',] <-
  lifetables$simulation[,c(NA,1:(lifetables$cnst$nyears-2),NA),,,,'ex_diff',]

# Calculate Arriaga decomposition of annual e0 changes ------------

# decompose annual changes in e0 into age specific mortality changes
# see Arriaga (1984)
# Measuring and explaining the change in life expectancies
# DOI 10.2307/2061029

lifetables$simulation[,,,,,'lx_lag',] <-
  lifetables$simulation[,c(NA,1:(lifetables$cnst$nyears-2),NA),,,,'lx',]
lifetables$simulation[,,,,,'nLx_lag',] <-
  lifetables$simulation[,c(NA,1:(lifetables$cnst$nyears-2),NA),,,,'nLx',]
lifetables$simulation[,,,,,'Tx_lag',] <-
  lifetables$simulation[,c(NA,1:(lifetables$cnst$nyears-2),NA),,,,'Tx',]

lifetables$simulation[,,,,,'e0_cntrb_d',] <-
  (
    lifetables$simulation[,,,,,'nLx',]/lifetables$simulation[,,,,,'lx',]-
      lifetables$simulation[,,,,,'nLx_lag',]/lifetables$simulation[,,,,,'lx_lag',]
  ) * lifetables$simulation[,,,,,'lx',]

lifetables$simulation[,,,,,'e0_cntrb_i',] <-
  (
    lifetables$simulation[,,,,,'lx_lag',]/
      lifetables$simulation[,,,,,'lx',]-
      apply(lifetables$simulation[,,,,,'lx_lag',],
            # apply function to vector of data by age
            2:6, function (x) c(x[-1], 0))/
      apply(lifetables$simulation[,,,,,'lx',],
            # apply function to vector of data by age
            2:6, function (x) c(x[-1], 0))
  ) * apply(lifetables$simulation[,,,,,'Tx',],
            # apply function to vector of data by age
            2:6, function (x) c(x[-1], 0))
lifetables$simulation[lifetables$cnst$nage,,,,,'e0_cntrb_i',] <- 0

lifetables$simulation[,,,,,'e0_cntrb_t',] <-
  lifetables$simulation[,,,,,'e0_cntrb_d',] +
  lifetables$simulation[,,,,,'e0_cntrb_i',]

# test if age decomposition sums to observed e0 difference
arriaga_e0diff_test <- abs(
  apply(
    lifetables$simulation[,,,,1,'e0_cntrb_t',], 2:5, sum
  ) -
    lifetables$simulation[1,,,,1,'ex_diff',]
)

# maximum decomposition error less than 0.1 years?
max(arriaga_e0diff_test, na.rm = TRUE) < 0.1
# distribution of decomposition errors
hist(log(arriaga_e0diff_test), breaks = 50)

# Counterfactual Arriaga decomposition ----------------------------

# decompose life expectancy deficit (observed minus expected e0)
# into age specific mortality changes
# see Arriaga (1984)
# Measuring and explaining the change in life expectancies
# DOI 10.2307/2061029

deficits_and_excesses <- list()
deficits_and_excesses$simulation <-
  array(
    dim = c(
      age = lifetables$cnst$nage,
      year = lifetables$cnst$nyears,
      sex = 3,
      region_iso = lifetables$cnst$nregions,
      sim_id = cnst$n_sim+1,
      var_id = 22
    ),
    dimnames = list(
      0:(lifetables$cnst$nage-1),
      c(
        config$skeleton$year$start:config$skeleton$year$end,
        '2020-2024'
      ),
      c(unlist(config$skeleton$sex), 'Total'),
      config$skeleton$region,
      1:(cnst$n_sim+1),
      c('death_total_actual', 'death_total_expected',
        'excess_deaths', 'pscore',
        # cumulated over age, i.e. excess deaths starting from age x,
        # age 0 is totals over age
        'death_total_actual_cum', 'death_total_expected_cum',
        'excess_deaths_cum', 'pscore_cum',
        # life table and life expectancy differences observed-expected
        'nmx_actual', 'nmx_expected',
        'ex_actual_minus_expected',
        'ex_actual', 'ex_expected',
        'lx_actual', 'lx_expected',
        'nLx_actual', 'nLx_expected',
        'Tx_actual', 'Tx_expected',
        'e0_cntrb_d', 'e0_cntrb_i', 'e0_cntrb_t')
    )
  )

deficits_and_excesses$simulation[,,,,,'death_total_actual'] <-
  lifetables$simulation[,,,,,'death_total','actual']
deficits_and_excesses$simulation[,,,,,'death_total_expected'] <-
  lifetables$simulation[,,,,,'death_total','projected']

deficits_and_excesses$simulation[,,,,,'excess_deaths'] <-
  deficits_and_excesses$simulation[,,,,,'death_total_actual'] -
  deficits_and_excesses$simulation[,,,,,'death_total_expected']
deficits_and_excesses$simulation[,,,,,'pscore'] <-
  deficits_and_excesses$simulation[,,,,,'excess_deaths']/
  deficits_and_excesses$simulation[,,,,,'death_total_expected']

deficits_and_excesses$simulation[,,,,,'death_total_actual_cum'] <-
  apply(
    deficits_and_excesses$simulation[,,,,,'death_total_actual'],
    # apply function to vector of data by age
    MARGIN = 2:5, function (Dx) rev(cumsum(Dx))
  )
deficits_and_excesses$simulation[,,,,,'death_total_expected_cum'] <-
  apply(
    deficits_and_excesses$simulation[,,,,,'death_total_expected'],
    # apply function to vector of data by age
    MARGIN = 2:5, function (Dx) rev(cumsum(Dx))
  )
deficits_and_excesses$simulation[,,,,,'excess_deaths_cum'] <-
  deficits_and_excesses$simulation[,,,,,'death_total_actual_cum'] -
  deficits_and_excesses$simulation[,,,,,'death_total_expected_cum']
deficits_and_excesses$simulation[,,,,,'pscore_cum'] <-
  deficits_and_excesses$simulation[,,,,,'excess_deaths_cum']/
  deficits_and_excesses$simulation[,,,,,'death_total_expected_cum']

deficits_and_excesses$simulation[,,,,,'nmx_actual'] <-
  lifetables$simulation[,,,,,'nmx','actual']
deficits_and_excesses$simulation[,,,,,'nmx_expected'] <-
  lifetables$simulation[,,,,,'nmx','projected']

deficits_and_excesses$simulation[,,,,,'lx_actual'] <-
  lifetables$simulation[,,,,,'lx','actual']
deficits_and_excesses$simulation[,,,,,'lx_expected'] <-
  lifetables$simulation[,,,,,'lx','projected']

deficits_and_excesses$simulation[,,,,,'nLx_actual'] <-
  lifetables$simulation[,,,,,'nLx','actual']
deficits_and_excesses$simulation[,,,,,'nLx_expected'] <-
  lifetables$simulation[,,,,,'nLx','projected']

deficits_and_excesses$simulation[,,,,,'Tx_actual'] <-
  lifetables$simulation[,,,,,'Tx','actual']
deficits_and_excesses$simulation[,,,,,'Tx_expected'] <-
  lifetables$simulation[,,,,,'Tx','projected']

deficits_and_excesses$simulation[,,,,,'ex_actual'] <-
  lifetables$simulation[,,,,,'ex','actual']
deficits_and_excesses$simulation[,,,,,'ex_expected'] <-
  lifetables$simulation[,,,,,'ex','projected']

deficits_and_excesses$simulation[,,,,,'ex_actual_minus_expected'] <-
  deficits_and_excesses$simulation[,,,,,'ex_actual'] -
  deficits_and_excesses$simulation[,,,,,'ex_expected']

deficits_and_excesses$simulation[,,,,,'e0_cntrb_d'] <-
  (
    deficits_and_excesses$simulation[,,,,,'nLx_actual'] /
      deficits_and_excesses$simulation[,,,,,'lx_actual'] -
      deficits_and_excesses$simulation[,,,,,'nLx_expected'] /
      deficits_and_excesses$simulation[,,,,,'lx_expected']
  ) * deficits_and_excesses$simulation[,,,,,'lx_actual']

deficits_and_excesses$simulation[,,,,,'e0_cntrb_i'] <-
  (
    deficits_and_excesses$simulation[,,,,,'lx_expected'] /
      deficits_and_excesses$simulation[,,,,,'lx_actual'] -
      apply(deficits_and_excesses$simulation[,,,,,'lx_expected'],
            # apply function to vector of data by age
            2:5, function (x) c(x[-1], 0)) /
      apply(deficits_and_excesses$simulation[,,,,,'lx_actual'],
            # apply function to vector of data by age
            2:5, function (x) c(x[-1], 0))
  ) * apply(deficits_and_excesses$simulation[,,,,,'Tx_actual'],
            # apply function to vector of data by age
            2:5, function (x) c(x[-1], 0))
deficits_and_excesses$simulation[lifetables$cnst$nage,,,,,'e0_cntrb_i'] <- 0

deficits_and_excesses$simulation[,,,,,'e0_cntrb_t'] <-
  deficits_and_excesses$simulation[,,,,,'e0_cntrb_d'] +
  deficits_and_excesses$simulation[,,,,,'e0_cntrb_i']

# test if age decomposition sums to observed e0 deficit
deficits_and_excesses$test <- log(
  apply(
    deficits_and_excesses$simulation[,,,,2,'e0_cntrb_t'], 2:4, sum
  ) /
    deficits_and_excesses$simulation[1,,,,2,'ex_actual_minus_expected']
)

# maximum decomposition error less than 10%?
max(abs(deficits_and_excesses$test), na.rm = TRUE) < 0.1
# distribution of decomposition errors
ecdf(deficits_and_excesses$test) |> plot()
hist(deficits_and_excesses$test, breaks = 500, xlim = c(-0.2, 0.2))

# Calculate p-value of e0 deficit ---------------------------------

pval <- list()

# simulations of actual and expected e0 by region, sex, and year
# sim position 1 reserved for means over simulations
pval$sim <- deficits_and_excesses$simulation[
  '0',c(as.character(2020:2024),'2020-2024'),,,,c('ex_actual', 'ex_expected')]
# actual and expected mean
pval$mean <- apply(pval$sim[,,,-1,], c(1:3, 5), mean)

# calculate distribution of test statistic (life expectancy deficit)
# under H0: continuation of pre-pandemic mortality trends.
pval$test <- pval$sim[,,,,'ex_expected']
for (sim in 2:(cnst$n_sim+1)) {
  pval$test[,,,sim] <-
    abs(pmin(
      0,
      pval$sim[,,,sim,'ex_expected'] - pval$mean[,,,'ex_expected']
    ))
}
# add observed test statistic
pval$test[,,,1] <-
  abs(pmin(0,
           pval$mean[,,,'ex_actual'] - pval$mean[,,,'ex_expected']
  ))

# calculate probability of having at least the observed effect under H0
pval$p <- apply(pval$test, 1:3, function (x) {
  if (any(is.na(x)) | any(is.nan(x))) {
    p <- NA
  } else {
    p <- 1-ecdf(x[-1])(x[1])
    p[x[1]<1e-12] <- 1
  }
  return(p)
})

pval$df <- as.data.frame.table(pval$p, stringsAsFactors = FALSE)
names(pval$df) <- c('year', 'sex', 'region_iso', 'e0_deficit_pval')
pval$df <-
  pval$df |>
  mutate(year_int = as.integer(year)) |>
  select(region_iso, sex, year, year_int, everything())
#pval$df$year <- as.integer(pval$df$year)
ecdf(pval$test['2024','Total','DE',]) |> plot(verticals = TRUE)

# library(ggplot2)
#
# pval$demo <- list()
# pval$demo$x <- pval$test['2024','Total','DE',]
# pval$demo$test <- pval$demo$x[1]
# pval$demo$x <- pval$demo$x[-1]
# pval$demo$m <- mean(pval$demo$x)
# pval$demo$sd <- sd(pval$demo$x)
# pval$demo$p <- pval$p['2024','Total','DE']
#
# pval$plot <-
#   pval$demo$x |>
#   as_tibble() |>
#   #filter(value >0) |>
#   ggplot() +
#   aes(x = -value) +
#   # geom_histogram(
#   #   aes(y = after_stat(density)),
#   #   bins = 15,
#   #   fill = 'grey70'
#   # ) +
#   stat_function(
#     fun = dnorm,
#     geom = 'area',
#     args = list(mean = pval$demo$m, sd = pval$demo$sd),
#     xlim = -c(min(pval$demo$x)*1.2, pval$demo$test),
#     fill = 'red', linewidth = 1, alpha = 0.3
#   ) +
#   geom_function(
#     fun = dnorm,
#     args = list(mean = pval$demo$m, sd = pval$demo$sd),
#     xlim = range(-1.5, 1.5),
#     color = 'red', linewidth = 1
#   ) +
#   annotate(
#     'point', fill = 'red', color = 'black', size = 3, shape = 21,
#     x = -pval$demo$test,
#     y = dnorm(pval$demo$test, mean = pval$demo$m, pval$demo$sd)
#   ) +
#   annotate(
#     'text',
#     x = -pval$demo$test+0.05,
#     y = dnorm(pval$demo$test, mean = pval$demo$m, pval$demo$sd),
#     label = paste0('Estimated LE deficit: ',
#                    formatC(-pval$demo$test, digits = 2, format = 'f'),
#                    ' years'),
#     hjust = 0, color = 'red', size = 3
#   ) +
#   annotate(
#     'text',
#     x = -pval$demo$test+0.05,
#     y = 0.23*dnorm(pval$demo$test, mean = pval$demo$m, pval$demo$sd),
#     label = paste0('p=',
#                    formatC(pval$demo$p, digits = 3, format = 'f')),
#     hjust = 0, color = 'red', size = 3
#   ) +
#   scale_x_continuous(breaks = seq(-1.5, 1.5, 0.5)) +
#   scale_y_continuous(expand = c(0,0), limits = c(0,1)) +
#   MyGGplotTheme() +
#   labs(
#     x = 'Distribution of life expectancy deficit under expected mortality',
#     y = NULL
#   )

# Calculates CI over simulations ----------------------------------

# ci's for life tables
lifetables$ci <-
  apply(
    lifetables$simulation[
      ,,,,-1,
      c('nmx', 'npx', 'nqx', 'lx', 'ex', 'ex_diff',
        'e0_cntrb_t'),
    ],
    -5,
    QuantileWithMean,
    simplify = TRUE
  ) |>
  aperm(c(2:7,1))
V <- dimnames(lifetables$ci)
names(attr(lifetables$ci, 'dim'))[7] <- 'quantile'
dimnames(lifetables$ci) <- V

# ci's for age specific contributions to e0 deviations from expectation
deficits_and_excesses$ci <-
  apply(
    deficits_and_excesses$simulation[,,,,-1,],
    -5,
    QuantileWithMean,
    simplify = TRUE
  ) |>
  aperm(c(2:6,1))
V <- dimnames(deficits_and_excesses$ci)
names(attr(deficits_and_excesses$ci, 'dim'))[6] <- 'quantile'
dimnames(deficits_and_excesses$ci) <- V

# Transform to data frame -----------------------------------------

lifetables$ci_df <-
  as.data.frame.table(lifetables$ci, stringsAsFactors = FALSE)
names(lifetables$ci_df) <-
  c(names(attr(lifetables$ci, 'dim')), 'value')
lifetables$ci_df <-
  lifetables$ci_df |>
  as_tibble() |>
  pivot_wider(id_cols = c(age, year, sex, region_iso, scenario),
              names_from = c(var_id, quantile),
              values_from = value) |>
  mutate(
    age = as.integer(age),
    year_int = as.integer(year)
  ) |>
  select(scenario, region_iso, sex, year, year_int, age, everything())

deficits_and_excesses$ci_df <-
  as.data.frame.table(deficits_and_excesses$ci, stringsAsFactors = FALSE)
names(deficits_and_excesses$ci_df) <-
  c(names(attr(deficits_and_excesses$ci, 'dim')), 'value')
deficits_and_excesses$ci_df <-
  deficits_and_excesses$ci_df |>
  filter(year %in% c(cnst$forecast_period, '2020-2024')) |>
  as_tibble() |>
  pivot_wider(id_cols = c(region_iso, sex, year, age),
              names_from = c(var_id, quantile),
              values_from = value) |>
  mutate(age = as.integer(age)) |>
  select(region_iso, sex, year, age, everything())

# Test ------------------------------------------------------------

# this is to test if the array labels match with the array data
# should be equal
(
  lifetables$simulation['0','2021','Female',,1,'population_py','actual'] ==
    lt_input$openage_100 |>
    filter(age_start == 0, year == 2021, sex == 'Female') |>
    pull(population_py)
) |> all()

cbind(
  lifetables$simulation['0','2021','Female',,2,'nmx','projected'],
  lt_input$leecarter |>
    filter(age == 0, year == 2021, sex == 'Female', nsim == 1) |>
    pull(mx)
)

# Export ----------------------------------------------------------

qs_save(lifetables$simulation, paths$output$lifetables_sim.qs)

saveRDS(lifetables$ci_df, paths$output$lifetables.rds)
lifetables$ci_df |>
  mutate(across(.cols = where(is.numeric), .fns = ~round(.x,6))) |>
  write_csv(paths$output$lifetables.csv)

qs_save(deficits_and_excesses$simulation, paths$output$deficits_and_excesses_sim.qs)

saveRDS(deficits_and_excesses$ci_df, paths$output$deficits_and_excesses.rds)
deficits_and_excesses$ci_df |>
  mutate(across(.cols = where(is.numeric), .fns = ~round(.x,6))) |>
  write_csv(paths$output$deficits_and_excesses.csv)

saveRDS(pval$df, paths$output$pval.rds)
pval$df |>
  mutate(across(.cols = where(is.numeric), .fns = ~round(.x,6))) |>
  write_csv(paths$output$pval.csv)

# ExportFigure(
#   pval$plot, filename = paths$output$pval.pdf,
#   scale = 1
# )
