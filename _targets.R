library(crew.cluster)
library(targets)
library(tarchetypes)
library(tibble)

tar_source()

controller <- crew_controller_sge(
  workers = 500,
  options_cluster = crew_options_sge(
    cores = 4,
    script_lines = file.path("module load R", getRversion())
  )
)

tar_option_set(
  packages = c(
    "dplyr", "ggplot2", "gt", "posterior",
    "rstanarm", "survival", "tibble", "tidyr"
  ),
  format = "qs",
  controller = controller
)

list(
  tar_target(
    name = hazard_ratio,
    command = historical_hazard_ratio(n_draws = 50)
  ),
  tar_target(
    name = events,
    command = c(50, 100, 150)
  ),
  tar_target(
    name = trials,
    command = trial(hazard_ratio, events),
    pattern = cross(hazard_ratio, events)
  ),
  tar_target(
    name = plot,
    command = plot_results(trials)
  ),
  tar_quarto(
    name = quarto,
    path = "results.qmd"
  )
)
