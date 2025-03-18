historical_coefficients <- function(
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
  ) |>
    as_draws_df() |>
    colMeans()
}

model_simulated_data <- function(
  simulated_data,
  chains = 4,
  iterations = 2e3,
  cores = 4
) {
  message <- ""
  # Try to avoid the stan_jm() error running variational Bayes (VB)
  # to create initial values:
  for (try in seq_len(1000)) {
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
      message <- conditionMessage(attr(out, "condition"))
      if (any(grepl("mvmer|variational", message))) {
        next
      } else {
        stop(message)
      }
    } else {
      return(out)
    }
  }
  stop("stan_jm() initialization failed.")
}
