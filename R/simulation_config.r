#' Simulation config constructor
#'
#' @param tasks Number of tasks to produce
#' @param until Time until simulation should last
#' @param units Until time units
#'
#' @return New object of 'simulation_config'
#' @export
new_simulation_config <- function(tasks = numeric(), until = double(), units = "seconds") {
  stopifnot(is.numeric(tasks))

  if (!rlang::is_empty(tasks) & rlang::is_empty(until))
    until <- Inf

  until_duration <- as.numeric(lubridate::duration(until, units = units))

  structure(list(tasks = tasks, until = until, until_duration = until_duration),
            class = "simulation_config",
            units = units)
}

#' Simulation config
#'
#' @inheritParams new_simulation_config
#'
#' @return Validated object of class 'simulation_config'
#' @export
simulation_config <- function(tasks = numeric(), until = double(), units = "seconds") {
  validate_simulation_config(new_simulation_config(tasks, until, units))
}



#' Validate simulation config object
#'
#' @param x Object to validate
#'
#' @export
validate_simulation_config <- function(x) {
  match.arg(attr(x, "units"), c("seconds", "minutes", "hours", "days", "weeks", "months", "years"))

  if (all(sapply(x[c("tasks", "until")], rlang::is_empty)))
    stop("Any of simulation limit `until` or `tasks` must be not empty", call. = FALSE)

  invisible(x)
}

#' Simulation config type check
#'
#' @param x Object to verify
#'
#' @return TRUE or FALSE
#' @export
is_simulation_config <- function(x) {
  methods::is(x, "simulation_config")
}
