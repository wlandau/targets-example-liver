#' @title Prior on the hazard ratio.
#' @description Construct a prior on the hazard ratio of
#'   treatment vs control.
#'   The prior comes from a Bayesian joint model
#'   fit to historical data.
#' @return A fitted model object from [rstanarm::stan_jm()].
#' @param chains Number of MCMC chains.
#' @param iterations Number of MCMC iterations per chain.
#' @param cores Number of CPU cores to parallelize the MCMC chains.
#' @param n_draws Number of draws to keep for the simulation study.
#' @examples
#'   library(rstanarm)
#'   library(survival)
#'   hazard_ratio_draws(cores = 1)
hazard_ratio_draws <- function(
  chains = 4,
  iterations = 4e3,
  cores = 4,
  n_draws = 1000
) {
  data_longitudinal <- pbcseq |>
    mutate(
      log_bilirubin = log(bili),
      years_measured = day / 365.25,
      study_arm = case_when(
        trt == 1 ~ "treatment",
        trt != 1 ~ "control"
      )
    ) |>
    rename(patient_id = id)
  drop <- setdiff(colnames(data_longitudinal), c("patient_id", "study_arm"))
  data_survival <- pbc |>
    filter(!is.na(trt)) |>
    mutate(
      years_survived = time / 365.25,
      event = status > 0,
      study_arm = case_when(
        trt == 1 ~ "treatment",
        trt != 1 ~ "control"
      )
    ) |>
    rename(patient_id = id) |>
    select(-contains(drop))
  datasets <- filter_datasets(data_longitudinal, data_survival)
  fit <- joint_model(datasets, chains, iterations, cores)
  log <- as_draws_df(fit)[["Event|study_armtreatment"]]
  hazard_ratio <- exp(log)
  tail(hazard_ratio, n = n_draws)
}

#' @title Bayesian joint model.
#' @description Fit a Bayesian joint model using the `rstanarm` package.
#' @return A fitted model object from [rstanarm::stan_jm()].
#' @param datasets A named list of simulated longitudinal and survival
#'   datasets.
#' @param chains Number of MCMC chains.
#' @param iterations Number of MCMC iterations per chain.
#' @param cores Number of CPU cores to parallelize the MCMC chains.
#' @param baseline_hazard Character string naming the baseline hazard
#'   function of the model.
#' @param priorEvent_aux `rstanarm` prior distribution of the survival
#'   auxiliary parameters. See `?rstanarm::stan_jm` for details.
#' @examples
#'   library(dplyr)
#'   library(rstanarm)
#'   historical <- model_historical_data(cores = 1)
#'   coefficients <- colMeans(as_draws_df(historical))
#'   data <- simulate_data(hazard_ratio = 0.75, coefficients, n_events = 50)
#'   joint_model(data, cores = 1)
joint_model <- function(
  datasets,
  chains = 4,
  iterations = 4e3,
  cores = 4
) {
  # Try to avoid the stan_jm() error running variational Bayes (VB)
  # to create initial values:
  for (try in seq_len(6)) {
    out <- try(
      stan_jm(
        formulaLong = log_bilirubin ~ study_arm + albumin + years_measured +
          (1 | patient_id),
        formulaEvent = Surv(years_survived, event) ~ study_arm,
        time_var = "years_measured",
        dataLong = datasets$data_longitudinal,
        dataEvent = datasets$data_survival,
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
