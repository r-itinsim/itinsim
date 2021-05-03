#' Create IT infrastructure simulation environment
#'
#' @inheritDotParams simmer::simmer
#'
#' @return simmer environment with the name "IT-infrastructure"
#' @export
it_infrastructure <- function(...) {
  simmer::simmer(iti_entities$`IT infrastructure`, ...)
}

#' Validation of IT infrastructure
#'
#' @param x object to test
#'
#' @return TRUE or FALSE
#' @export
is_it_infrastructure <- function(x) {
  (is_simmer(x) && hasName(x, "name") && x$name == iti_entities$`IT infrastructure`)
}

#' Validation of IT infrastructure
#'
#' @inheritParams is_it_infrastructure
#'
#' @return same object as provided
#' @export
validate_it_infrastructure <- function(x) {
  validate_simmer_with_name(x, iti_entities$`IT infrastructure`)
}
