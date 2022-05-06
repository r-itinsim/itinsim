#' Add scheduler/load balancer
#'
#' @param .env Simulation environment with name "IT infrastructure"
#' @param ... Add resource parameters from simmer
#'
#' @return Simulation environment with resource named "Scheduler"
#' @export
add_scheduler <- function(.env, ..., .config)
{
  if (missing(.config) || rlang::is_empty(.config))
    .config <- new_scheduler_config(prepared_args = list(...))

  .env %>% add_iti_resource_entity(.config = .config)
}
