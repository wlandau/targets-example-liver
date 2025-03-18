#' @title Simulate a trial
#' @description Simulate one replication of the clinical trial.
#' @return A one-row `tibble` with numerical summaries of the simulated trial.
#' @param hazard_ratio Numeric scalar, hazard ratio of treatment vs control.
#' @param coefficients Named numeric vector of fitted model coefficients
#'   from a historical study in a similar indication.
#' @param n_events Number of events (death or liver transplant)
#'   at which to conduct an interim analysis.
#' @param scenario Character string with the name of the simulation scenario.
#' @examples
#'   library(dplyr)
#'   library(rstanarm)
#'   historical <- model_historical_data(cores = 1)
#'   coefficients <- colMeans(as_draws_df(historical))
#'   simulate_trial(
#'     hazard_ratio = 1,
#'     coefficients,
#'     n_events = 50,
#'     scenario = "No efficacy"
#'   )
simulate_trial <- function(hazard_ratio, coefficients, n_events, scenario) {
  simulated_data <- simulate_data(hazard_ratio, coefficients, n_events)
  fit <- model_simulated_data(simulated_data)
  samples_hazard_ratio <- fit |>
    as_draws_df() |>
    pull(`Event|study_armtreatment`) |>
    exp()
  tibble::tibble(
    probability_effect = mean(samples_hazard_ratio < 0.75),
    mean_hazard_ratio = mean(samples_hazard_ratio),
    n_events = n_events,
    years_n_events = unique(simulated_data$data_survival$years_n_events),
    scenario = scenario
  )
}

#' @title Simulate trial data
#' @description Simulate one set of clinical trial data.
#' @return A list of `tibbles`: a longidinal
#'   bioamarker dataset (with bilirubin and albumin levels)
#'   and a non-longitudinal survival dataset
#'   (time until death or liver transplant).
#' @param hazard_ratio Numeric scalar, hazard ratio of treatment vs control.
#' @param coefficients Named numeric vector of fitted model coefficients
#'   from a historical study in a similar indication.
#' @param n_events Number of events (death or liver transplant)
#'   at which to conduct an interim analysis.
#' @examples
#'   library(dplyr)
#'   library(rstanarm)
#'   historical <- model_historical_data(cores = 1)
#'   coefficients <- colMeans(as_draws_df(historical))
#'   simulate_data(
#'     hazard_ratio = 1,
#'     coefficients,
#'     n_events = 50
#'   )
simulate_data <- function(hazard_ratio, coefficients, n_events) {
  data_longitudinal <- simulate_data_longitudinal(coefficients)
  data_survival <- simulate_data_survival(
    data_longitudinal,
    hazard_ratio,
    coefficients,
    n_events
  )
  filter_datasets(data_longitudinal, data_survival)
}

#' @title Simulate longitudinal data
#' @description Simulate one set of longitudinal biomarker trial data.
#' @return A `tibble` with longidinal
#'   bioamarker data (with bilirubin and albumin levels).
#'   It has one row for each observation of each patient.
#' @param coefficients Named numeric vector of fitted model coefficients
#'   from a historical study in a similar indication.
#' @param n_patients Number of patients in the trial.
#'   Should be an even number.
#' @param n_time Maximum number of biomarker measurements to perform
#'   on each patient.
#'   Simulated readings occur at baseline and then
#'   at random about every 2 or 3 years.
#' @examples
#'   library(dplyr)
#'   library(rstanarm)
#'   historical <- model_historical_data(cores = 1)
#'   coefficients <- colMeans(as_draws_df(historical))
#'   simulate_data_longitudinal(
#'     coefficients,
#'     n_patients = 250,
#'     n_time = 25
#'   )
simulate_data_longitudinal <- function(
  coefficients,
  n_patients = 250,
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

#' @title Simulate interim survival data
#' @description Simulate one set of survival data at an interim analysis.
#'   An interim analysis occurs when `n_events` events happen,
#'   at which point the survival times are censored
#'   for all patients who are still alive and have not had liver transplants.
#' @return A `tibble` with interim survival data.
#'   It has one row for each patient.
#' @param data_longitudinal A `tibble` from [simulate_data_longitudinal()].
#' @param hazard_ratio Numeric scalar, hazard ratio to assume for simulating
#'   survival outcomes.
#' @param coefficients Named numeric vector of fitted model coefficients
#'   from a historical study in a similar indication.
#' @param n_events Number of events (death or liver transplant)
#'   at which to conduct an interim analysis.
#' @param accrual Length of the accrual period in years.
#'   During this period, patients enroll uniformly at random.
#' @examples
#'   library(dplyr)
#'   library(rstanarm)
#'   historical <- model_historical_data(cores = 1)
#'   coefficients <- colMeans(as_draws_df(historical))
#'   data_longitudinal <- simulate_data_longitudinal(
#'     coefficients,
#'     n_patients = 250,
#'     n_time = 25
#'   )
#'   simulate_data_survival(
#'     data_longitudinal,
#'     hazard_ratio = 1,
#'     coefficients,
#'     n_events = 50,
#'     accrual = 2
#'   )
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
  years_n_events <- sort(years_total)[n_events]
  event <- years_total <= years_n_events 
  years_total <- pmin(years_total, years_n_events)
  years_survived <- years_total - years_enrolled
  tibble(
    patient_id = data_patients$patient_id,
    study_arm = data_patients$study_arm,
    event = event,
    years_survived = years_survived,
    years_n_events = years_n_events
  )
}

#' @title Filter simulated datasets.
#' @description Filter the longitudinal dataset to exclude biomarker
#'   measurements that would have occurred after the patient had
#'   an event (liver transplant or death).
#'   Then, ensure both datasets have exactly the same set of patients.
#' @return A list of `tibbles`: a longidinal
#'   bioamarker dataset (with bilirubin and albumin levels)
#'   and a non-longitudinal survival dataset
#'   (time until death or liver transplant).
#' @param data_longitudinal A `tibble` from [simulate_data_longitudinal()].
#' @param data_survival A `tibble` from [simulate_data_survival()].
#' @examples
#'   library(dplyr)
#'   library(rstanarm)
#'   historical <- model_historical_data(cores = 1)
#'   coefficients <- colMeans(as_draws_df(historical))
#'   data_longitudinal <- simulate_data_longitudinal(
#'     coefficients,
#'     n_patients = 250,
#'     n_time = 25
#'   )
#'   data_survival <- simulate_data_survival(
#'     data_longitudinal,
#'     hazard_ratio = 1,
#'     coefficients,
#'     n_events = 50,
#'     accrual = 2
#'   )
#'   filter_datasets(data_longitudinal, data_survival)
filter_datasets <- function(
  data_longitudinal,
  data_survival
) {
  data_longitudinal <- left_join(
    x = data_longitudinal,
    y = data_survival,
    by = c("patient_id", "study_arm")
  ) |>
    filter(years_measured <= years_survived) |>
    select(contains(colnames(data_longitudinal)))
  patients <- intersect(
    data_longitudinal$patient_id,
    data_survival$patient_id
  )
  list(
    data_longitudinal = filter(data_longitudinal, patient_id %in% patients),
    data_survival = filter(data_survival, patient_id %in% patients)
  )
}
