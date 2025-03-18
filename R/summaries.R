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
    ylab("Probability of study success\n") +
    labs(fill = "Number of events") +
    ylim(c(0, 1)) +
    facet_wrap(~ scenario) +
    theme_gray(20)
}
