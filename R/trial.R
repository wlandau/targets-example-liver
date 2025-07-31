#' @title Simulate a trial
#' @description Simulate one replication of the clinical trial.
#' @return A one-row `tibble` with numerical summaries of the simulated trial.
#' @param prior A data frame with one row with a single draw from the prior.
#'   The columns correspond to individual parameters.
#' @param events Number of events (death or liver transplant)
#'   at which to conduct the analysis.
#' @param scenario Character string with the name of the simulation scenario.
#' @examples
#'   library(dplyr)
#'   library(rstanarm)
#'   library(survival)
#'   trial(1, events = 50)
trial <- function(hazard_ratio, events) {
  simulated_data <- simulate_data(hazard_ratio, events)
  samples_hazard_ratio <- joint_model(simulated_data) |>
    as_draws_df() |>
    pull(`Event|study_armtreatment`) |>
    exp()
  tibble::tibble(
    events = events,
    efficacy = mean(samples_hazard_ratio < 0.75),
    enrolled = unique(simulated_data$data_survival$enrolled),
    years = unique(simulated_data$data_survival$years_analysis)
  )
}
