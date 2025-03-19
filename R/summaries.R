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
#'   simulations <- tibble(
#'     scenario = c(rep("No efficacy", 2), rep("Strong efficacy", 2)),
#'     n_events = c(50, 100, 50, 100),
#'     probability_effect = c(0.05, 0.04, 0.81, 0.82)
#'   )
#'   plot_probabilities(simulations)
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

#' @title Create a table of average years until the analysis.
#' @description Plot the probability of declaring efficacy
#'   for each scenario and analysis timing criterion (number of events).
#' @return A `ggplot` object.
#' @param simulations A `tibble` with one row per simulation and columns
#'   with simulation results.
#' @examples
#'   library(dplyr)
#'   library(ggplot2)
#'   library(tibble)
#'   simulations <- tibble(
#'     scenario = c(rep("No efficacy", 2), rep("Strong efficacy", 2)),
#'     n_events = c(50, 100, 50, 100),
#'     probability_effect = c(0.05, 0.04, 0.81, 0.82),
#'     years_n_events = c(3, 5, 3, 5)
#'   )
#'   average_years_n_events(simulations)
average_years_n_events <- function(simulations) {
  simulations |>
    group_by(n_events) |>
    summarize(time = years_months(mean(years_n_events))) |>
    rename(
      `Number of events` = n_events,
      `Time until n events` = time
    ) |>
    gt() |>
    cols_align(align = "left", columns = everything())
}

#' @title Print a time span.
#' @description Print a time span in a friendly format
#'   rounded to the nearest month.
#' @return A character string with a human-readable description of
#'   the time span.
#' @param years Numeric, span of years.
#' @examples
#'   years_months(2.25)
years_months <- function(years) {
  sprintf(
    "%s years and %s months",
    floor(years),
    round((years - floor(years)) * 12)
  )
}
