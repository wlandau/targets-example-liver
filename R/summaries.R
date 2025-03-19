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
plot_results <- function(trials) {
  results <- trials |>
    group_by(events) |>
    summarize(
      `Probability of futility` = mean(probability_efficacy < 0.3),
      `Years until interim` = mean(years_interim),
      .groups = "drop"
    ) |>
    pivot_longer(
      cols = c("Probability of futility", "Years until interim"),
      names_to = "facet",
      values_to = "value"
    ) |>
    mutate(value = round(value, 2))
  ggplot(results) +
    geom_blank(
      aes(x = events, y = value),
      tibble(events = 50, facet = "Probability of futility", value = 1)
    ) +
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
        label = round(value, 3)
      ),
      size = 5
    ) +
    xlab("\nEvents at interim") +
    ylab("") +
    guides(fill = "none") +
    facet_wrap(~ facet, scales = "free_y") +
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
    summarize(time = years_months(mean(years_interim))) |>
    rename(
      `Events at interim` = n_events,
      `Time until interim` = time
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
