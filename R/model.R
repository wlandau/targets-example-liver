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

model_simulated_data <- function(
  simulated_data,
  chains = 4,
  iterations = 2e3,
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

prior_hazard_ratio <- function(fit, n_draws) {
  fit |>
    as_draws_df() |>
    pull(`Event|trt`) |>
    tail(n = n_draws) |>
    exp()
}
