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
  tar_target(prior, historical_prior(n_draws = 1000)),
  tar_target(events, c(50, 100, 150, 200)),
  tar_target(trials, trial(prior, events), pattern = cross(prior, events)),
  tar_target(plot, plot_results(trials)),
  tar_quarto(quarto, "results.qmd")
)
