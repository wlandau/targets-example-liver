simulate_trial <- function(
  sample_size = 100,
  n_measurement = 25,
  hazard_ratio = 0.5,
  chains = 4,
  iterations = 2e3,
  cores = 4
) {
  simulated_data <- simulate_data(
    sample_size = sample_size,
    n_measurement = n_measurement,
    hazard_ratio = hazard_ratio
  )
  fit_simulated_data <- model_simulated_data(
    simulated_data = simulated_data,
    iterations = iterations,
    chains = chains,
    cores = cores
  )
  hazard_ratio <- fit_simulated_data |>
    as_draws_df() |>
    pull(`Event|study_armtreatment`) |>
    exp()
  tibble::tibble(
    probability_efficacy = mean(hazard_ratio < 0.75),
    mean_hazard_ratio = mean(hazard_ratio),
    sample_size = sample_size
  )
}
