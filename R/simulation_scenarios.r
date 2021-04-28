#' Repeat same simulation with different resource-selection policies
#'
#' @param policies Policies to use in simulation
#' @param scenario Scenario to evaluate
#' @param sim_config Simulation config
#'
#' @return A list of policty_simulation_results
#' @export
repeat_policy_scenario <- function(policies, scenario, sim_config)
{
  f_opts <- furrr::furrr_options(seed = 73, globals = names(globalenv()))
  p <- progressr::progressor(steps = length(policies) * 2)

  sim_results <- furrr::future_map(policies, function(policy)
  {
    p(message = paste("Start simulation for:", policy$name))
    result <- scenario(policy, sim_config)
    p(message = paste("End simulation for:", policy$name))

    if (utils::hasName(result, "sim") & utils::hasName(result, "policy")) {
      result$sim$policy <- result$policy
      result$sim$sim <- result$sim
      return (as.policy_simulation_result(result$sim))
    }

    as.policy_simulation_result(result)
  },
  .options = f_opts)

  gendatypes::as.typed_list(sim_results, class_names.policy_simulation_result)
}
