#' Custom policy
#'
#' @param name policy name
#' @param resources vector of resource names
#' @param env environment as a part of policy container, which can be used as a parameter in custom function
#' @param select_function resource selection function
#' @param initial_container container of values, from which policy object will be constructed
#'
#' @return new custom policy object
#' @export
new_custom_policy <- function(name = character(), resources = character(), env = NA,
                              select_function = get_simmer_policy_resources, initial_container = list()) {
  policy <- new_policy(name, resources, env = env, class = "custom_policy", initial_container = initial_container, is_simmer_policy = FALSE)
  policy$select_function <- adapt_policy_selection_function(select_function, policy)
  policy
}


# S3 Methods --------------------------------------------------------------

#' Get policy name for custom policy
#'
#' In other words, there are 2 iterations of resources selection:
#' 1. Produce list of resource names. Production logic can incapsulate decision logic.
#' 2. Simmer default algorithm selection.
#'
#' @param policy policy object
#'
#' @return policy name with respect to "simmer"
#' @export
get_policy_name.custom_policy <- function(policy) {
  simmer_policy_name_match()
}

#' Get resource function for custom policy S3 method
#'
#' In any "simmer" scenario there is no explicit way to define custom policy with custom any.
#' However, custom policy can be provided by function which produces a list of resources for selection.
#' If such function will provide only one value then any "simmer" policy function will only choose the one.
#' Technically, it means that custom function will provide "optimal" value for it's algorithm.
#'
#' @param policy policy object
#'
#' @return resource function
#' @export
get_resources_function.custom_policy <- function(policy) {
  policy$select_function(policy)
}


#' Wrapper for custom policy selection function
#' @keywords internal
#' @export
adapt_policy_selection_function <- function(select_function, policy) {
  arguments <- methods::formalArgs(select_function)

  if ("policy" %in% arguments)
    select_function
  else
    function(policy) {
      force(policy)
      function() purrr::lift_dl(select_function)(policy[arguments])
    }
}
