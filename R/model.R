#' @title Historical data model.
#' @description Fit a Bayesian joint model to
#'   the primary biliary cirrhosis datasets
#'   in the `rstanarm` package.
#' @return A fitted model object from [rstanarm::stan_jm()].
#' @param chains Number of MCMC chains.
#' @param iterations Number of MCMC iterations per chain.
#' @param cores Number of CPU cores to parallelize the MCMC chains.
#' @examples
#'   library(rstanarm)
#'   model_historical_data(cores = 1)
model_historical_data <- function(
  chains = 4,
  iterations = 4e3,
  cores = 4
) {
  stan_jm(
    formulaLong = logBili ~ year + albumin + (1 | id),
    formulaEvent = Surv(futimeYears, event) ~ trt,
    time_var = "year",
    dataLong = pbcLong,
    dataEvent = mutate(pbcSurv, event = status > 0),
    chains = chains,
    iter = iterations,
    cores = cores
  )
}

#' @title Simulated data model.
#' @description Fit a Bayesian joint model to
#'   the primary biliary cirrhosis datasets
#'   in the `rstanarm` package.
#' @return A fitted model object from [rstanarm::stan_jm()].
#' @param chains Number of MCMC chains.
#' @param iterations Number of MCMC iterations per chain.
#' @param cores Number of CPU cores to parallelize the MCMC chains.
#' @examples
#'   library(dplyr)
#'   library(rstanarm)
#'   historical <- model_historical_data(cores = 1)
#'   coefficients <- colMeans(as_draws_df(historical))
#'   data <- simulate_data(hazard_ratio = 0.75, coefficients, n_events = 50)
#'   model_simulated_data(data, cores = 1)
model_simulated_data <- function(
  simulated_data,
  chains = 4,
  iterations = 4e3,
  cores = 4
) {
  # Try to avoid the stan_jm() error running variational Bayes (VB)
  # to create initial values:
  for (try in seq_len(6)) {
    out <- try(
      stan_jm(
        formulaLong = log_bilirubin ~ years_measured + albumin +
          (1 | patient_id),
        formulaEvent = Surv(years_survived, event) ~ study_arm,
        time_var = "years_measured",
        dataLong = simulated_data$data_longitudinal,
        dataEvent = simulated_data$data_survival,
        chains = chains,
        iter = iterations,
        cores = cores
        # init = 0 # unfortunatley doesn't avoid the error. stan_jm() runs VB anyway.
      ),
      silent = TRUE
    )
    if (inherits(out, "try-error")) {
      message(conditionMessage(attr(out, "condition")))
      message(paste("retry", try))
      next
    } else {
      return(out)
    }
  }
  stop(conditionMessage(attr(out, "condition")))
}

#' @title Prior hazard ratio
#' @description Extract `n_draws` posterior draws of the hazard ratio
#'   from a fitted joint model.
#' @return A numeric vector of hazard ratios of treatment vs control.
#' @param fit A fitted joint model object from [rstanarm::stan_jm()].
#' @param n_draws Number of posterior draws to extract.
#' @examples
#'   library(dplyr)
#'   library(rstanarm)
#'   historical <- model_historical_data(cores = 1)
#'   hist(prior_hazard_ratio(historical, n_draws = 1000))
prior_hazard_ratio <- function(fit, n_draws) {
  fit |>
    as_draws_df() |>
    pull(`Event|trt`) |>
    tail(n = n_draws) |>
    exp()
}
