#' @title Simulate a trial
#' @description Simulate one replication of the clinical trial.
#' @return A one-row `tibble` with numerical summaries of the simulated trial.
#' @param prior A data frame with one row with a single draw from the prior.
#'   The columns correspond to individual parameters.
#' @param events Number of events (death or liver transplant)
#'   at which to conduct the interim analysis.
#' @param scenario Character string with the name of the simulation scenario.
#' @examples
#'   library(dplyr)
#'   library(rstanarm)
#'   prior <- historical_prior()[1, ]
#'   trial(prior, events = 50)
trial <- function(prior, events) {
  simulated_data <- simulate_data(prior, events)
  fit <- joint_model(simulated_data)
  samples_hazard_ratio <- fit |>
    as_draws_df() |>
    pull(`Event|study_armtreatment`) |>
    exp()
  tibble::tibble(
    probability_efficacy = mean(samples_hazard_ratio < 0.75),
    mean_hazard_ratio = mean(samples_hazard_ratio),
    events = events,
    years_interim = unique(simulated_data$data_survival$years_interim)
  )
}

#' @title Simulate trial data
#' @description Simulate one set of clinical trial data.
#' @return A list of `tibbles`: a longidinal
#'   bioamarker dataset (with bilirubin and albumin levels)
#'   and a non-longitudinal survival dataset
#'   (time until death or liver transplant).
#' @param prior A data frame with one row with a single draw from the prior.
#'   The columns correspond to individual parameters.
#' @param events Number of events (death or liver transplant)
#'   at which to conduct the interim analysis.
#' @examples
#'   library(dplyr)
#'   library(rstanarm)
#'   prior <- historical_prior()[1, ]
#'   simulate_data(prior, events = 50)
simulate_data <- function(prior, events) {
  data_longitudinal <- simulate_data_longitudinal(prior)
  data_survival <- simulate_data_survival(data_longitudinal, prior, events)
  filter_datasets(data_longitudinal, data_survival)
}

#' @title Simulate longitudinal data
#' @description Simulate one set of longitudinal biomarker trial data.
#' @return A `tibble` with longidinal
#'   bioamarker data (with bilirubin and albumin levels).
#'   It has one row for each observation of each patient.
#' @param prior A data frame with one row with a single draw from the prior.
#'   The columns correspond to individual parameters.
#' @param patients Number of patients in the trial.
#'   Should be an even number.
#' @param readings Maximum number of biomarker measurements to perform
#'   on each patient.
#'   Simulated readings occur at baseline and then
#'   at random about every 2 or 3 years.
#' @examples
#'   library(dplyr)
#'   library(rstanarm)
#'   library(survival)
#'   prior <- historical_prior()[1, ]
#'   simulate_data_longitudinal(prior, patients = 200, readings = 25)
simulate_data_longitudinal <- function(prior, patients = 200, readings = 25) {
  rows <- patients * readings
  patient_id <- rep(seq_len(patients), each = readings)
  study_arm <- rep(c("control", "treatment"), each = rows / 2)
  years_measured <- rexp(n = rows, rate = 1)
  years_measured[seq(from = 1, to = rows, by = readings)] <- 0
  albumin <- rnorm(
    n = rows,
    mean = mean(pbcseq$albumin),
    sd = sd(pbcseq$albumin)
  )
  log_bilirubin <- prior[["Long1|(Intercept)"]] +
    prior[["Long1|study_armtreatment"]] * (study_arm == "treatment") +
    prior[["Long1|albumin"]] * albumin +
    prior[["Long1|years_measured"]] * years_measured +
    rnorm(n = rows, sd = prior[["Long1|sigma"]])
  tibble(
    patient_id = patient_id,
    study_arm = study_arm,
    years_measured = years_measured,
    albumin = albumin,
    log_bilirubin = log_bilirubin
  ) |>
    arrange(patient_id, years_measured)
}

#' @title Simulate survival data
#' @description Simulate one set of survival data.
#'   The interim analysis occurs when `events` events happen,
#'   at which point the survival times are censored
#'   for all patients who are still alive and have not had liver transplants.
#' @return A `tibble` with survival data.
#'   It has one row for each patient.
#' @param prior A data frame with one row with a single draw from the prior.
#'   The columns correspond to individual parameters.
#' @param events Number of events (death or liver transplant)
#'   at which to conduct the interim analysis.
#' @param accrual Length of the accrual period in years.
#'   During this period, patients enroll uniformly at random.
#' @examples
#'   library(dplyr)
#'   library(rstanarm)
#'   library(survival)
#'   prior <- historical_prior()[1, ]
#'   long <- simulate_data_longitudinal(prior, patients = 200, readings = 25)
#'   survival <- simulate_data_survival(long, prior, events = 50)
simulate_data_survival <- function(
  data_longitudinal,
  prior,
  events,
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
  is_treatment <- (data_patients$study_arm == "treatment")
  log_hazard <- log(historical_baseline_hazard()) +
    # prior[["Event|(Intercept)"]] + # Overlaps too much with the baseline hazard rate.
    prior[["Event|study_armtreatment"]] * is_treatment + 
    prior[["Assoc|Long1|etavalue"]] * data_patients$log_bilirubin
  years_survived <- rexp(n = nrow(data_patients), rate = exp(log_hazard))
  years_enrolled <- runif(n = nrow(data_patients), min = 0, max = accrual)
  years_total <- years_enrolled + years_survived
  years_interim <- sort(years_total)[events]
  event <- years_total <= years_interim 
  years_total <- pmin(years_total, years_interim)
  years_survived <- years_total - years_enrolled
  tibble(
    patient_id = data_patients$patient_id,
    study_arm = data_patients$study_arm,
    event = event,
    years_survived = years_survived,
    years_interim = years_interim
  )
}

baseline_hazard <- function() {
  data_survival <- pbc |>
    filter(!is.na(trt)) |>
    mutate(years_survived = time / 365.25, event = status > 0)
  fit <- survreg(
    Surv(years_survived, event) ~ 1,
    data = data_survival,
    dist = "exponential"
  )
  exp(1 / coef(fit))
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
#'   library(survival)
#'   prior <- historical_prior()[1, ]
#'   long <- simulate_data_longitudinal(prior, patients = 200, readings = 25)
#'   survival <- simulate_data_survival(long, prior, events = 50)
#'   filter_datasets(long, survival)
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
