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
#'   draw_hazard_ratios(cores = 1)
draw_hazard_ratios <- function(
  chains = 4,
  iterations = 8e3,
  cores = 4,
  n_draws = 10000
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
