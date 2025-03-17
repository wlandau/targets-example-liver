library(crew.cluster)
library(targets)
library(tarchetypes)
library(tibble)

tar_option_set(
  packages = c("dplyr", "ggplot2", "posterior", "rstanarm", "tibble"),
  format = "qs",
  controller = crew_controller_sge(
    workers = 100,
    options_cluster = crew_options_sge(
      script_lines = file.path("module load R", getRversion())
    )
  )
)

tar_source()

targets_prior <- list(
  tar_target(
    name = fit_historical_data,
    command = model_historical_data(
      chains = 1,
      iterations = 10,
      cores = 1
    )
  ),
  tar_target(
    name = prior_hazard_ratio,
    command = prior_hazard_ratio_draws(fit_historical_data, n_draws = 1)
  )
)

targets_simulation <- tar_map(
  values = tibble(n_patient = c(100, 500)),
  tar_target(
    name = sample_size,
    command = simulate_trial(
      sample_size = n_patient,
      hazard_ratio = prior_hazard_ratio,
      chains = 1,
      iterations = 10,
      cores = 1
    ),
    pattern = map(prior_hazard_ratio)
  )
)

targets_summaries <- list(
  tar_combine(
    name = simulations,
    targets_simulation
  ),
  tar_target(
    name = plot,
    command = plot_probabilities(simulations)
  ),
  tar_quarto(
    name = quarto,
    path = "results.qmd"
  )
)

list(
  targets_prior,
  targets_simulation,
  targets_summaries
)
