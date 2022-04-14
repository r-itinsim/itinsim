#' Simmer policy wrapper
#'
#' @param name policy name (either default or custom)
#' @param resources vector of resource names to enumerate over
#' @param ... additional arguments
#' @param class custom class name, used for extensions
#' @param initial_container container of items from which policy object is going to be created
#' @param is_simmer_policy defines whether wrapper should hold simmer policy in order to match policy name or no
#'
#' @return new policy object wrapper
#' @export
new_policy <- function(name = character(), resources = character(), ..., class = character(), initial_container = list(),
                       is_simmer_policy = TRUE)
{
  validate_policy_name(name)

  if (is_simmer_policy)
    simmer_policy_name_match(name)

  dots = list(...)

  initial_container[names(dots)] <- dots
  initial_container[c("name", "resources")] <- list(name, resources)

  structure(initial_container, class = c(class, "policy"))
}

#' Check object is policy
#'
#' @param x object to test
#'
#' @return TRUE or FALSE
#' @export
is_policy <- function(x) {
  is(x, "policy")
}


# S3 Generics -------------------------------------------------------------

#' S3 genric for policy initialization
#'
#' @param policy policy wrapper object
#' @param ... additional parameters to enrich with
#'
#' @export
policy_initialize <- function(policy, ...) {
  UseMethod("policy_initialize")
}

#' S3 generic for getting resources from policy
#'
#' @param policy policy wrapper object
#'
#' @export
get_resources_function <- function(policy) {
  UseMethod("get_resources_function")
}

#' S3 generic for getting policy name
#'
#' @param policy policy wrapper object
#'
#' @export
get_policy_name <- function(policy) {
  UseMethod("get_policy_name")
}


# S3 Methods --------------------------------------------------------------

#' Policy initialize function
#'
#' @param policy policy wrapper
#' @param ... arguments to enrich with
#'
#' @return policy wrapper
#' @export
policy_initialize.policy <- function(policy, ...) {
  dots <- list(...)

  if (!rlang::is_named(dots))
    stop("Only named arguments must be provided!")

  policy[names(dots)] <- dots
  policy
}

#' Get policy resources function
#'
#' @param policy policy wrapper object
#'
#' @return policy function to be used
#' @export
get_resources_function.policy <- function(policy) {
  get_simmer_policy_resources(policy)
}

#' Get policy name
#'
#' @param policy policy wrapper object
#'
#' @return policy name consumed by simmer
#' @export
get_policy_name.policy <- function(policy) {
  simmer_policy_name_match(policy$name)
}


# Default policy object methods for simmer policies -----------------------

#' Simmer default policy
#' @keywords internal
#' @export
get_simmer_policy_resources <- function(policy) {
  simmer_policy_name_match(policy$name)
  function() policy$resources
}

#' Simmer default policy name match
#' @keywords internal
#' @export
simmer_policy_name_match <- function(policyName = c("shortest-queue", "shortest-queue-available",
                                                "round-robin", "round-robin-available", "first-available",
                                                "random", "random-available")) {
  match.arg(policyName)
}


#' Validate text which is going to be used for naming policy
#'
#' @param x input text
#'
#' @return input text if it is validated
#' @keywords internal
#' @export
validate_policy_name <- function(x)
{
  if (!is.character(x) || !nzchar(x) || rlang::is_empty(x))
    stop(paste("policy name must be of non-zero and non-empty character type! Provided value:", x, sep = " "))

  x
}
