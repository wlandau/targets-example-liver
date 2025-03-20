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
    name = hazard_ratios,
    command = hazard_ratio_draws(n_draws = 1000)
  ),
  tar_target(
    name = events,
    command = c(40, 50, 60, 70)
  ),
  tar_target(
    name = trials,
    command = trial(hazard_ratios, events),
    pattern = cross(hazard_ratios, events)
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
