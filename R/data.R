simulate_data_longitudinal <- function(
  fit_historical_data,
  n_patient,
  n_measurement
) {
  n_control <- n_patient / 2
  n_treatment <- n_control
  n_long <- n_patient * n_measurement
  parameters <- posterior::summarize_draws(fit_historical_data)
  beta <- parameters$mean
  names(beta) <- parameters$variable
  patient_id <- rep(seq_len(n_patient), each = n_measurement)
  study_arm <- c(
    rep("control", n_measurement * n_control),
    rep("treatment", n_measurement * n_treatment)
  )
  years_measured <- rexp(n = n_long, rate = 1 / mean(pbcLong$year))
  years_measured[seq(from = 1, to = n_long, by = 8)] <- 0
  albumin <- rnorm(
    n = n_long,
    mean = mean(pbcLong$albumin),
    sd = sd(pbcLong$albumin)
  )
  log_bilirubin <- beta["Long1|(Intercept)"] +
    beta["Long1|year"] * years_measured +
    beta["Long1|albumin"] * albumin +
    rnorm(n = n_long, sd = beta["Long1|sigma"])
  tibble(
    patient_id = patient_id,
    study_arm = study_arm,
    years_measured = years_measured,
    albumin = albumin,
    log_bilirubin = log_bilirubin
  ) |>
    arrange(patient_id, years_measured)
}

simulate_data_survival <- function(
  fit_historical_data,
  data_longitudinal,
  hazard_ratio,
  baseline_hazard = 0.25
) {
  data_patients <- data_longitudinal |>
    group_by(patient_id) |>
    summarize(
      patient_id = patient_id[1],
      study_arm = study_arm[1],
      log_bilirubin = mean(log_bilirubin),
      last_measurement = max(years_measured),
      .groups = "drop"
    )
  parameters <- posterior::summarize_draws(fit_historical_data)
  beta <- parameters$mean
  names(beta) <- parameters$variable
  log_hazard <- log(baseline_hazard) +
    beta["Event|(Intercept)"] +
    log(hazard_ratio) * (data_patients$study_arm == "treatment") + 
    beta["Assoc|Long1|etavalue"] + data_patients$log_bilirubin
  years_survived <- rexp(n = length(log_hazard), rate = exp(log_hazard))
  death <- years_survived < data_patients$last_measurement
  years_survived[!death] <- data_patients$last_measurement[!death]
  tibble(
    patient_id = data_patients$patient_id,
    study_arm = data_patients$study_arm,
    years_survived = years_survived,
    death = death
  )
}

filter_data_longitudinal <- function(
  data_longitudinal,
  data_survival
) {
  left_join(
    x = data_longitudinal,
    y = data_survival,
    by = c("patient_id", "study_arm")
  ) |>
    filter(years_measured <= years_survived) |>
    select(contains(colnames(data_longitudinal)))
}

simulate_data <- function(
  fit_historical_data,
  n_patient = 1000,
  n_measurement = 25,
  hazard_ratio = 0.5
) {
  data_longitudinal <- simulate_data_longitudinal(
    fit_historical_data = fit_historical_data,
    n_patient = n_patient,
    n_measurement = n_measurement
  )
  data_survival <- simulate_data_survival(
    fit_historical_data = fit_historical_data,
    data_longitudinal = data_longitudinal,
    hazard_ratio = hazard_ratio
  )
  data_longitudinal <- filter_data_longitudinal(
    data_longitudinal = data_longitudinal,
    data_survival = data_survival
  )
  list(
    data_longitudinal = data_longitudinal,
    data_survival = data_survival
  )
}
