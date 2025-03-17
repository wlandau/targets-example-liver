library(targets)
library(tarchetypes)
library(tibble)

tar_option_set(
  packages = c("dplyr", "posterior", "rstanarm", "tibble"),
  format = "qs"
)

tar_source()

list(
  tar_target(
    name = fit_historical_data,
    command = model_historical_data(
      chains = 4,
      iterations = 10,
      cores = 1
    )
  ),
  tar_target(
    name = prior_hazard_ratio,
    command = prior_hazard_ratio_draws(fit_historical_data, n = 1)
  ),
  tar_map2_count(
    name = simulation,
    command1 = prior_hazard_ratio,
    command2 = simulate_trial(
      fit_historical_data,
      n_patient = 100,
      n_measurement = 25,
      hazard_ratio = hazard_ratio_draw,
      iterations = 2e3
    ),
    values = tibble(n_patient = c(50, 100, 200, 500)),
    names = any_of("n_patient"),
    suffix1 = "prior",
    suffix2 = "reps"
  )
)
