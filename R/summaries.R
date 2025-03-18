plot_probabilities <- function(simulations) {
  probabilities <- simulations |>
    group_by(sample_size) |>
    summarize(
      sample_size = as.factor(sample_size[1]),
      probability_study_success = mean(probability_efficacy > 0.6),
      .groups = "drop"
    )
  ggplot(probabilities) +
    geom_bar(
      aes(
        x = sample_size,
        y = probability_study_success,
        fill = sample_size
      ),
      stat = "identity"
    ) +
    geom_label(
      aes(
        x = sample_size,
        y = probability_study_success,
        label = probability_study_success
      ),
      size = 5
    ) +
    xlab("Sample size") +
    ylab("Probability of study success") +
    theme_gray(20)
}
