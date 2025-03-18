plot_probabilities <- function(simulations) {
  probabilities <- simulations |>
    group_by(scenario, n_events) |>
    summarize(
      scenario = as.factor(scenario[1]),
      n_events = as.factor(n_events[1]),
      probability_study_success = mean(probability_effect > 0.75),
      .groups = "drop"
    )
  ggplot(probabilities) +
    geom_bar(
      aes(
        x = n_events,
        y = probability_study_success,
        fill = n_events
      ),
      stat = "identity"
    ) +
    geom_label(
      aes(
        x = n_events,
        y = probability_study_success,
        label = probability_study_success
      ),
      size = 5
    ) +
    xlab("\nNumber of events") +
    ylab("Probability of declaring efficacy\n") +
    guides(fill = "none") +
    ylim(c(0, 1)) +
    facet_wrap(~ scenario) +
    theme_gray(20)
}

average_years_rescued <- function(simulations) {
  simulations |>
    group_by(n_events) |>
    summarize(years_rescued = mean(years_rescued)) |>
    rename(
      `Number of events` = n_events,
      `Years until rescue` = years_rescued
    ) |>
    gt()
}
