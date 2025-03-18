library(crew.cluster)
library(targets)
library(tarchetypes)
library(tibble)

tar_option_set(
  packages = c("dplyr", "ggplot2", "posterior", "rstanarm", "tibble"),
  format = "qs",
  controller = crew_controller_sge(
    workers = 500,
    options_cluster = crew_options_sge(
      cores = 4,
      script_lines = file.path("module load R", getRversion())
    )
  )
)

tar_source()

targets_prior <- list(
  tar_target(
    name = fit,
    command = model_historical_data()
  ),
  tar_target(
    name = coefficients,
    command = colMeans(as_draws_df(fit))
  ),
  tar_target(
    name = prior,
    command = prior_hazard_ratio(fit, n_draws = 1000)
  )
)

targets_simulation <- tar_map(
  values = tibble(n_events = c(50, 200)),
  tar_target(
    name = efficacy,
    command = simulate_trial(
      hazard_ratio = prior,
      coefficients,
      n_events,
      scenario = "Efficacy"
    ),
    pattern = map(prior)
  ),
  tar_target(
    name = type_1_error,
    command = simulate_trial(
      hazard_ratio = 1,
      coefficients,
      n_events,
      scenario = "Type 1 Error"
    ),
    pattern = map(prior)
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
