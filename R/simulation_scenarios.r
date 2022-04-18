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

  gendatypes::as.typed_list(sim_results, class_names.policy_simulation_result)
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
