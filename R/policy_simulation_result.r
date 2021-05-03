class_names.policy_simulation_result <- "policy_simulation_result"

#' Convert object to policy_simulation_result
#'
#' @param x Object to convert
#'
#' @return New 'policy_simulation_result' object
#' @export
as.policy_simulation_result <- function(x)
{
  validate_policy_simulation_result(x) %>%
    gendatypes::with_class(class_names.policy_simulation_result)
}

#' Validate policy_simulation_result object
#'
#' @param x Object to validate
#'
#' @export
validate_policy_simulation_result <- function(x) {
  if (!utils::hasName(x, "sim"))
    stop("`policy_simulation_result` must have `sim` property!")

  if (!utils::hasName(x, "policy"))
    stop("`policy_simulation_result` must have `policy` property!")

  if (!is.character(x$policy) || !nzchar(x$policy) || rlang::is_empty(x$policy))
    stop("`policy` must be of non-zero and non-empty character type!")

  if (!is.environment(x$sim))
    stop("`sim` must be of type: environment")

  if (!is(x$sim, "wrap") && !is(x$sim, "simmer"))
    stop("`sim` must be a simmer object")

  invisible(x)
}

#' Check if object of class 'policy_simulation_result'
#'
#' @param x Object to test
#'
#' @return TRUE or FALSE
#' @export
is_policy_simulation_result <- function(x)
{
  is(x, class_names.policy_simulation_result)
}


# Simmer get_mon S3 extensions --------------------------------------------


#' S3 Methods for get_mon functions
#'
#' @param .envs vector of environments
#'
#' @return Enriched data
#' @importFrom rlang .data
#' @export
get_mon_resources.policy_simulation_result <- function(.envs) {
  enrich_get_mon(.envs, simmer::get_mon_resources) %>%
    dplyr::arrange(rlang::.data$resource, rlang::.data$time) %>%
    gendatypes::with_class(iti_metadata$iti_resources)
}

#' S3 Methods for get_mon functions
#'
#' @param .envs vector of environments
#' @param per_resource Display information per resource
#' @param ongoing Display not finished tasks
#'
#' @inherit get_mon_resources.policy_simulation_result return
#' @importFrom rlang .data
#' @export
get_mon_arrivals.policy_simulation_result <- function(.envs, per_resource = FALSE, ongoing = FALSE) {
  enrich_get_mon(.envs, function(env) env %>% simmer::get_mon_arrivals(per_resource, ongoing)) %>%
    dplyr::arrange(rlang::.data$name) %>%
    gendatypes::with_class(iti_metadata$iti_arrivals)
}

#' S3 Methods for get_mon functions
#'
#' @param .envs vector of environments
#'
#' @inherit get_mon_resources.policy_simulation_result return
#' @export
get_mon_attributes.policy_simulation_result <- function(.envs) {
  enrich_get_mon(.envs, simmer::get_mon_attributes) %>%
    gendatypes::with_class(iti_metadata$iti_attributes)
}
