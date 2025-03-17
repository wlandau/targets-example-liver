model_historical_data <- function(
  chains = 4,
  iterations = 2e3,
  cores = 4
) {
  stan_jm(
    formulaLong = logBili ~ year + albumin + (1 | id),
    formulaEvent = Surv(futimeYears, death) ~ trt,
    time_var = "year",
    dataLong = pbcLong,
    dataEvent = pbcSurv,
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
  stan_jm(
    formulaLong = log_bilirubin ~ years_measured + albumin + (1 | patient_id),
    formulaEvent = Surv(years_survived, death) ~ study_arm,
    time_var = "years_measured",
    dataLong = simulated_data$data_longitudinal,
    dataEvent = simulated_data$data_survival,
    chains = chains,
    iter = iterations,
    cores = cores
  )
}

prior_hazard_ratio_draws <- function(fit_historical_data, n_draws = 1e3) {
  fit_historical_data |>
    as_draws_df() |>
    pull(`Event|trt`) |>
    tail(n = n_draws) |>
    exp()
}
