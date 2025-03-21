#' @title Plot probabilities.
#' @description Plot the probability of declaring efficacy
#'   for each scenario and analysis timing criterion (number of events).
#' @return A `ggplot` object.
#' @param simulations A `tibble` with one row per simulation and columns
#'   with simulation results.
#' @examples
#'   library(dplyr)
#'   library(ggplot2)
#'   library(tibble)
#'   trials <- tibble(
#'     events = c(40, 50, 60, 70),
#'     probability_efficacy = c(0.05, 0.04, 0.81, 0.82),
#'     enrolled = c(64, 75, 85, 95)
#'   )
#'   plot_results(trials)
plot_results <- function(trials) {
  label_enrolled <- "Patients enrolled"
  label_futility <- "Probability of futility (%)"
  results <- trials |>
    group_by(events) |>
    summarize(
      `Probability of futility (%)` = 100 *
        round(mean(probability_efficacy < 0.4), 2),
      `Patients enrolled` = round(mean(enrolled)),
      .groups = "drop"
    ) |>
    pivot_longer(
      cols = any_of(c(label_futility, label_enrolled)),
      names_to = "facet",
      values_to = "value"
    ) |>
    mutate(events = as.factor(events))
  ggplot(results) +
    geom_bar(
      aes(
        x = events,
        y = value,
        fill = events
      ),
      stat = "identity"
    ) +
    geom_label(
      aes(
        x = events,
        y = value,
        label = ifelse(
          facet == label_futility,
          paste0(value, "%"),
          value
        )
      ),
      size = 5
    ) +
    xlab("\nEvents at analysis") +
    ylab("") +
    ylim(c(0, 100)) +
    guides(fill = "none") +
    facet_wrap(~ facet) +
    theme_gray(20)
}
