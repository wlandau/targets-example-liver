#' @title Simulate a trial
#' @description Simulate one replication of the clinical trial.
#' @return A one-row `tibble` with numerical summaries of the simulated trial.
#' @param prior A data frame with one row with a single draw from the prior.
#'   The columns correspond to individual parameters.
#' @param events Number of events (death or liver transplant)
#'   at which to conduct the analysis.
#' @param scenario Character string with the name of the simulation scenario.
#' @examples
#'   library(dplyr)
#'   library(rstanarm)
#'   trial(1, events = 50)
trial <- function(hazard_ratio, events) {
  simulated_data <- simulate_data(hazard_ratio, events)
  samples_hazard_ratio <- joint_model(simulated_data) |>
    as_draws_df() |>
    pull(`Event|study_armtreatment`) |>
    exp()
  tibble::tibble(
    probability_efficacy = mean(samples_hazard_ratio < 0.75),
    mean_hazard_ratio = mean(samples_hazard_ratio),
    events = events,
    years_analysis = unique(simulated_data$data_survival$years_analysis),
    enrolled = mean(simulated_data$data_survival$enrolled)
  )
}

#' @title Simulate trial data
#' @description Simulate one set of clinical trial data.
#' @return A list of `tibbles`: a longidinal
#'   bioamarker dataset (with bilirubin and albumin levels)
#'   and a non-longitudinal survival dataset
#'   (time until death or liver transplant).
#' @param hazard_ratio Numeric scalar, hazard ratio of treatment vs control.
#' @param events Number of events (death or liver transplant)
#'   at which to conduct the analysis.
#' @examples
#'   library(dplyr)
#'   library(rstanarm)
#'   simulate_data(1, events = 50)
simulate_data <- function(hazard_ratio, events) {
  data_longitudinal <- simulate_data_longitudinal()
  data_survival <- simulate_data_survival(
    data_longitudinal,
    hazard_ratio,
    events
  )
  filter_datasets(data_longitudinal, data_survival)
}

#' @title Simulate longitudinal data
#' @description Simulate one set of longitudinal biomarker trial data.
#' @return A `tibble` with longidinal
#'   bioamarker data (with bilirubin and albumin levels).
#'   It has one row for each observation of each patient.
#' @param patients Number of patients in the trial.
#'   Should be an even number.
#' @param readings Maximum number of annual biomarker measurements to perform
#'   on each patient.
#' @examples
#'   library(dplyr)
#'   library(rstanarm)
#'   library(survival)
#'   simulate_data_longitudinal(patients = 200, readings = 25)
simulate_data_longitudinal <- function(patients = 100, readings = 50) {
  rows <- patients * readings
  patient_id <- rep(seq_len(patients), each = readings)
  study_arm <- rep(c("control", "treatment"), each = rows / 2)
  years_measured <- rep(seq_len(readings) - 1, times = patients)
  albumin <- rnorm(rows, mean = mean(pbcseq$albumin), sd = sd(pbcseq$albumin))
  # Model coefficients below come from posterior means of a Bayesian
  # joint model fit to the pbc and pbcseq datasets from the survival package.
  log_bilirubin <- 2.169617 +
    -0.09582189 * (study_arm == "treatment") +
    -0.4425171 * albumin +
    0.06929517 * years_measured +
    rnorm(n = rows, sd = 0.4779195)
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
#'   The analysis occurs when `events` events happen,
#'   at which point the survival times are censored
#'   for all patients who are still alive and have not had liver transplants.
#' @return A `tibble` with survival data.
#'   It has one row for each patient.
#' @param hazard_ratio Numeric scalar, hazard ratio of treatment vs control.
#' @param events Number of events (death or liver transplant)
#'   at which to conduct the analysis.
#' @param accrual Length of the accrual period in years.
#'   During this period, patients enroll uniformly at random.
#' @examples
#'   library(dplyr)
#'   library(rstanarm)
#'   library(survival)
#'   long <- simulate_data_longitudinal()
#'   survival <- simulate_data_survival(long, 1, events = 50)
simulate_data_survival <- function(
  data_longitudinal,
  hazard_ratio,
  events,
  accrual = 10
) {
  data_patients <- distinct(data_longitudinal, patient_id, study_arm)
  is_treatment <- (data_patients$study_arm == "treatment")
  log_hazard <- - 1 + log(hazard_ratio) * is_treatment
  years_survived <- rexp(n = nrow(data_patients), rate = exp(log_hazard))
  years_enrolled <- runif(n = nrow(data_patients), min = 0, max = accrual)
  years_total <- years_enrolled + years_survived
  years_analysis <- sort(years_total)[events]
  event <- years_total <= years_analysis 
  years_total <- pmin(years_total, years_analysis)
  years_survived <- years_total - years_enrolled
  enrolled <- sum(years_enrolled < years_analysis)
  tibble(
    patient_id = data_patients$patient_id,
    study_arm = data_patients$study_arm,
    event = event,
    years_survived = years_survived,
    years_analysis = years_analysis,
    enrolled = enrolled
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
#'   library(survival)
#'   long <- simulate_data_longitudinal(patients = 200, readings = 50)
#'   survival <- simulate_data_survival(long, 1, events = 50)
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
