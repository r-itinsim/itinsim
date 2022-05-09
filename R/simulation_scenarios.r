
#' Run IT infrastructure simulation scenario
#'
#' @param scenario Scenario to run
#' @param policies Policies to use in simulation
#' @param resource_configs Resource configs which will be used for resource creation
#' @param sim_config Simulation config
#'
#' @return list of simulation results
#' @export
run_scenario <- function(scenario, policies, resource_configs, sim_config)
{
  validate_iti_simulation_scenario(scenario, "iti", "policy", "sim_config", "sim_cache")
  wrapped_policies <- policies %>% purrr::modify_if(Negate(is_policy), wrap_policy)

  f_opts <- furrr::furrr_options(seed = 73)
  p <- progressr::progressor(steps = length(policies) * 2)

  create_iti <- function()
  {
    it_infrastructure() %>% add_iti_entities(.configs = resource_configs)
  }

  sim_results <- furrr::future_map(wrapped_policies, function(policy)
  {
    p(message = paste("Start simulation for:", policy$name))
    iti <- create_iti()
    sim_cache <- new_simulation_cache(iti)
    initialized_policy <- policy_initialize(policy, env = iti, cache = sim_cache)
    sim_result <- scenario(iti = iti, policy = initialized_policy, sim_config = sim_config, sim_cache = sim_cache)
    p(message = paste("End simulation for:", policy$name))

    sim_result$policy <- policy
    sim_result$sim <- sim_result

    as.policy_simulation_result(sim_result)
  },
  .options = f_opts)

  sim_results %>%
    gendatypes::as.typed_list(class_names.policy_simulation_result)
}

#' Repeat same simulation with different resource-selection policies
#'
#' @param policies Policies to use in simulation
#' @param scenario Scenario to evaluate
#' @param sim_config Simulation config
#'
#' @return A list of `policy_simulation_results` type
#' @export
repeat_policy_scenario <- function(policies, scenario, sim_config)
{
  validate_policy_scenario(scenario)

  f_opts <- furrr::furrr_options(seed = 73)
  p <- progressr::progressor(steps = length(policies) * 2)

  sim_results <- furrr::future_map(policies, function(policy)
  {
    p(message = paste("Start simulation for:", policy$name))
    sim_result <- scenario(policy, sim_config)
    p(message = paste("End simulation for:", policy$name))

    list(sim = sim_result, policy = policy) %>%
      pack_policy_simulation_result() %>%
      as.policy_simulation_result()
  },
  .options = f_opts)

  sim_results %>%
    gendatypes::as.typed_list(class_names.policy_simulation_result)
}

#' Validate IT infrastructure simulation scenario function
#'
#' @importFrom gendatypes %??%
#' @keywords internal
#' @export
validate_iti_simulation_scenario <- function(x, ...) {
  if (!is.function(x))
    stop("Simulation scenario must be a function!")

  args <- methods::formalArgs(x)

  if (rlang::is_empty(args))
    stop("Simulation scenario function must have at least one argument from the list: ", names(list(...)) %??% c(...))

  are_dots_in_args <- length(args) == 1 && args == "..."

  if (!are_dots_in_args && length(diffArgs <- args %>% dplyr::setdiff(c(...))) > 0)
    warning("Some arguments have different names: ", paste(diffArgs, collapse = ", "),
            "\nMake sure you are using provided arguments correctly.")
  x
}

#' Validate policy scenario function
#'
#' @keywords internal
#' @export
validate_policy_scenario <- function(x) {
  if (!is.function(x))
    stop("Policy scenario must be a function!")

  args <- methods::formalArgs(x)

  if (length(args) != 2)
    stop("Policy scenario function must have 2 arguments: policy and sim_config!")

  if (length(diffArgs <- args %>% dplyr::setdiff(c("policy", "sim_config"))) > 0)
    warning("Some arguments have different names: ", paste(diffArgs, collapse = ", "),
            "\nMake sure you are using provided arguments correctly.")
  x
}
