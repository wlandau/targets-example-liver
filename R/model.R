#' @title Historical prior for a Bayesian joint model.
#' @description Construct a prior for a Bayesian joint model
#'   of primary biliary cholangitis (PBC).
#'   This prior comes from the posterior distribution of
#'   an equivalent model fit to the `pbc` and `pbcseq`
#'   datasets in the `survival` package.
#'   These datasets come from the Mayo Clinic PBC trial conducted between
#'   1974 and 1984 (Therneau and Grambsch 2000).
#' @references T Therneau and P Grambsch (2000),
#'   Modeling Survival Data: Extending the Cox Model,
#'   Springer-Verlag, New York. ISBN: 0-387-98784-3.
#' @return A fitted model object from [rstanarm::stan_jm()].
#' @param chains Number of MCMC chains.
#' @param iterations Number of MCMC iterations per chain.
#' @param cores Number of CPU cores to parallelize the MCMC chains.
#' @param n_draws Number of draws to keep for the simulation study.
#' @examples
#'   library(rstanarm)
#'   library(survival)
#'   historical_prior(cores = 1)
historical_prior <- function(
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
  draws <- as_draws_df(fit) |>
    select(-starts_with("b[long1"), -starts_with("Event|b-splines")) |>
    tail(n = n_draws)
}

#' @title Constant baseline hazard from historical data.
#' @description Fits an intercept-only parameteric exponential survival model
#'   to the `pbc` data from the survival package, than takes an estimate
#    of the constant baseline hazard rate to be `exp(- coef(fit))`.
#'   Used in simulations because [rstanarm::stan_jm()] does not return
#'   direct estimates of baseline hazard rates.
#' @return Numeric scalar, estimated baseline hazard rate of the `pbc` data.
#' @examples
#'   historical_baseline_hazard()
historical_baseline_hazard <- function() {
  data_survival <- pbc |>
    filter(!is.na(trt)) |>
    mutate(years_survived = time / 365.25, event = status > 0)
  fit <- survreg(
    Surv(years_survived, event) ~ 1,
    data = data_survival,
    dist = "exponential"
  )
  exp(- coef(fit)) |>
    unname()
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
