library(targets)
library(tarchetypes)
library(tibble)

tar_option_set(
  packages = c("dplyr", "posterior", "rstanarm", "tibble"),
  format = "qs"
)

tar_source()

targets_prior <- list(
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
  )
)

targets_simulation <- tar_map(
  values = tibble(n_patient = c(100, 500)),
  tar_target(
    name = sample_size,
    command = simulate_trial(
      n_patient = 100,
      n_measurement = 25,
      hazard_ratio = prior_hazard_ratio,
      iterations = 2e3
    ),
    pattern = map(prior_hazard_ratio)
  )
)

targets_combine <- tar_combine(
  name = simulations,
  targets_simulation
)

list(
  targets_prior,
  targets_simulation,
  targets_combine
)
