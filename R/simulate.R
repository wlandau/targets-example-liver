simulate_trial <- function(hazard_ratio, coefficients, n_events, scenario) {
  simulated_data <- simulate_data(hazard_ratio, coefficients, n_events)
  fit <- model_simulated_data(simulated_data)
  samples_hazard_ratio <- fit |>
    as_draws_df() |>
    pull(`Event|study_armtreatment`) |>
    exp()
  tibble::tibble(
    probability_effect = mean(samples_hazard_ratio < 0.75),
    probability_significant = mean(samples_hazard_ratio < 1),
    mean_hazard_ratio = mean(samples_hazard_ratio),
    n_events = n_events,
    years_rescued = unique(simulated_data$data_survival$years_rescued),
    scenario = scenario
  )
}

simulate_data <- function(hazard_ratio, coefficients, n_events) {
  data_longitudinal <- simulate_data_longitudinal(coefficients)
  data_survival <- simulate_data_survival(
    data_longitudinal,
    hazard_ratio,
    coefficients,
    n_events
  )
  data_longitudinal <- filter_longitudinal(data_longitudinal, data_survival)
  list(
    data_longitudinal = data_longitudinal,
    data_survival = data_survival
  )
}

simulate_data_longitudinal <- function(
  coefficients,
  n_patients = 400,
  n_time = 25
) {
  n_control <- n_patients / 2
  n_treatment <- n_control
  n_long <- n_patients * n_time
  patient_id <- rep(seq_len(n_patients), each = n_time)
  study_arm <- c(
    rep("control", n_time * n_control),
    rep("treatment", n_time * n_treatment)
  )
  years_measured <- rexp(n = n_long, rate = 1 / mean(pbcLong$year))
  years_measured[seq(from = 1, to = n_long, by = n_time)] <- 0
  albumin <- rnorm(
    n = n_long,
    mean = mean(pbcLong$albumin),
    sd = sd(pbcLong$albumin)
  )
  log_bilirubin <- coefficients["Long1|(Intercept)"] +
    years_measured * coefficients["Long1|year"] +
    albumin * coefficients["Long1|albumin"] +
    rnorm(n = n_long, sd = coefficients["Long1|sigma"])
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
  data_longitudinal,
  hazard_ratio,
  coefficients,
  n_events,
  accrual = 2
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
  log_hazard <- coefficients[["Event|(Intercept)"]] +
    log(hazard_ratio) * (data_patients$study_arm == "treatment") + 
    data_patients$log_bilirubin * coefficients[["Assoc|Long1|etavalue"]]
  years_survived <- rexp(n = nrow(data_patients), rate = exp(log_hazard))
  years_enrolled <- runif(n = nrow(data_patients), min = 0, max = accrual)
  years_total <- years_enrolled + years_survived
  years_rescued <- sort(years_total)[n_events]
  event <- years_total <= years_rescued 
  years_total <- pmin(years_total, years_rescued)
  years_survived <- years_total - years_enrolled
  tibble(
    patient_id = data_patients$patient_id,
    study_arm = data_patients$study_arm,
    event = event,
    years_survived = years_survived,
    years_rescued = years_rescued
  )
}

filter_longitudinal <- function(
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
