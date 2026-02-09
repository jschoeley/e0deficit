# Correct bias in mortality rates

# Init ------------------------------------------------------------

library(yaml)
library(dplyr)
library(mgcv)
library(ggplot2)
library(patchwork)

# Constants -------------------------------------------------------

# input and output paths
setwd('.')
paths <- list()
paths$input <- list(
  config.yaml = './cfg/config.yaml',
  analysisinput.rds = './out/30-analysisinput.rds',
  global_functions.R = './src/_global_functions.R'
)
paths$output <- list(
  analysisinput_bias_corrected.rds = './out/31-analysisinput_bias_corrected.rds'
)

# global configuration
config <- read_yaml(paths$input$config.yaml)

# global objects and functions
source(paths$input$global_functions.R)

# Load data -------------------------------------------------------

analysisinput <- readRDS(paths$input$analysisinput.rds)

# Prepare fields for bias corrected data --------------------------

analysisinput_noncorrected <-
  analysisinput |>
  transmute(
    id, region, sex, year, age_start, age_width,
    deathrate_uncorrected = death/population_py,
    deathrate_uncorrected = # avoid infinities
      if_else(population_py == 0, NA, deathrate_uncorrected),
    deathrate_benchmark = deathrate_hmd,
    death_uncorrected = round(death, 0),
    death_benchmark = round(deathrate_benchmark*population_py, 0),
    population_py
  )

# Estimate death rate correction factor ---------------------------

# Estimate annual death rate correction factor by comparing STMF annuals
# with HMD benchmark
correction <- list()

# .x <- filter(analysisinput_noncorrected, region == 'FR', sex == 'Female')
correction$correctionfactors <-
  analysisinput_noncorrected |>
  group_by(region, sex) |>
  group_modify(~{

    cat('Fit correction model for', .y$region, .y$sex, '\n')

    # interval with benchmark data
    # this is used for a least value carry forward correction factor
    # where the correction factor for the last available year
    # with benchmark data gets used subsequently
    locf_year_factor <-
      .x |> group_by(year) |>
      summarise(available = any(is.na(death_benchmark))) |>
      mutate(
        year_fac = ifelse(available, NA, year),
        year_fac = zoo::na.locf(year_fac, na.rm = FALSE),
        year_fac = as.factor(year_fac)
      ) |> select(year, year_fac)

    # prepare data for regression
    regression_input <-
      .x |>
      mutate(
        # avoid infinite likelihoods
        weights = if_else(death_uncorrected == 0, 0, 1),
        # only train on timeframe 2010 through 2024
        # weights = weights*ifelse(
        #   year %in% correction$config$precovid_correction_window |
        #     year %in% correction$config$covid_correction_window,
        #   1, 0),
        # age 100 is not benchmarked because it is 100+ in our data and
        # 100 in HMD benchmark
        weights = weights*ifelse(age_start == 100, 0, 1)
      ) |>
      left_join(locf_year_factor, by = 'year') |>
      mutate(empirical_correction = death_benchmark/death_uncorrected)

    # desired output
    result <- tryCatch({

      if (.y$region %in% config$ratebiascorrection$smoothlyvaryingbias) {
        model_spec <- formula(
          death_benchmark ~
            s(log1p(age_start), m = 2) +
            s(year, m = 2) +
            te(log1p(age_start), year, m = c(2, 2)) +
            offset(log(death_uncorrected))
        )
      }
      if (.y$region %in% config$ratebiascorrection$simpleaveragebias) {
        model_spec <- formula(
          death_benchmark ~
            s(log1p(age_start), m = 2) +
            offset(log(death_uncorrected))
        )
      }
      if (.y$region %in% config$ratebiascorrection$factorsmoothlocf) {
        model_spec <- formula(
          death_benchmark ~
            year_fac + s(log1p(age_start), year_fac, bs = "fs") +
            #s(log1p(age_start), m = 2, by = year_fac) +
            offset(log(death_uncorrected))
        )
      }

      # estimate correction factor
      fit <- gam(
        formula = model_spec,
        method = 'REML',
        select = TRUE,
        gamma   = 1.4,
        data = regression_input,
        family = poisson(link = 'log'),
        weights = regression_input$weights
      )
      # extract correction factor from fitted model
      ageeffect <-
        expand.grid(
          age_start = config$skeleton$age$start:config$skeleton$age$end,
          year = config$skeleton$year$start:config$skeleton$year$end,
          death_uncorrected = 1
        ) |>
        left_join(locf_year_factor, by = 'year')
      ageeffect$deathrate_correctionfactor <-
        exp(c(predict.gam(fit, newdata = ageeffect, type = 'link')))

      output <-
        left_join(
          regression_input,
          select(ageeffect, year, age_start, deathrate_correctionfactor),
          by = c('age_start', 'year')
        ) |>
        mutate(
          # age 100 is actually 100+ whereas the HMD benchmark relates to
          # age 100. thus keep original value
          deathrate_correctionfactor =
            if_else(age_start == 100, 1, deathrate_correctionfactor),
          deathrate_corrected =
            deathrate_correctionfactor*deathrate_uncorrected
        )

    },

    # error handling
    error = function(e) {

      regression_input |>
        mutate(
          deathrate_correctionfactor = NA,
          deathrate_corrected = NA
        )

    })# End of tryCatch()

    return(result)
  }) |>
  ungroup()

# Plot diagnostics on correction factors --------------------------

plot_strata <- expand.grid(
  region = unique(correction$correctionfactors$region),
  sex = unique(correction$correctionfactors$sex)
) |>
  mutate(id = 1:n())

pdf('./out/31-bias_correction_diagnostics.pdf')
for (i in 1:nrow(plot_strata)){
  output <-
    correction$correctionfactors |>
    filter(region == plot_strata$region[i], sex == plot_strata$sex[i])

  label <- paste(plot_strata$region[i], plot_strata$sex[i])

  print(
    output |>
      ggplot() +
      aes(x = age_start) +
      geom_point(aes(y = deathrate_uncorrected), size = 0.2, shape = 4) +
      geom_point(aes(y = deathrate_benchmark),
                 color = 'forestgreen', size = 0.2, shape = 1) +
      geom_line(aes(y = deathrate_corrected, group = year),
                color = 'red') +
      scale_y_log10() +
      facet_wrap(~year) +
      labs(y = 'Deathrate', x = 'Age', title = label,
           caption = 'corrected (red), benchmark (green), raw (grey)') +
      MyGGplotTheme(panel_border = TRUE)
  )

  print(
    output |>
      ggplot() +
      aes(x = age_start) +
      geom_hline(yintercept = 1, color = 'darkgrey', size = 1) +
      geom_point(aes(y = empirical_correction), size = 0.2, shape = 4) +
      geom_line(aes(y = deathrate_correctionfactor, group = year),
                color = 'red') +
      scale_y_log10() +
      facet_wrap(~year) +
      labs(y = 'Deathrate correction factor', x = 'Age', title = label,
           caption = 'smooth (red), raw (grey)') +
      MyGGplotTheme(panel_border = TRUE)
  )
}
dev.off()

# Merge with analysisdata -----------------------------------------

analysisinput_bias_corrected <-
  analysisinput |>
  left_join(
    select(
      correction$correctionfactors,
      id, deathrate_benchmark, deathrate_uncorrected,
      deathrate_corrected, deathrate_correctionfactor
    )
  ) |>
  mutate(
    death = case_when(
      region %in% config$ratebiascorrection$nocorrection ~ death,
      is.na(deathrate_correctionfactor) ~ death,
      TRUE ~ deathrate_corrected*population_py
    )
  )

# Export ----------------------------------------------------------

saveRDS(
  analysisinput_bias_corrected,
  paths$output$analysisinput_bias_corrected.rds
)
