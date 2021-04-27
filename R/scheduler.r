#' Add scheduler/load balancer
#'
#' @param .env Simulation environment with name "IT infrastructure"
#' @param ... Add resource parameters from simmer
#'
#' @return Simulation environment with resource named "Scheduler"
#' @export
add_scheduler <- function(.env, ...)
{
  .env %>%
    validate_it_infrastructure() %>%
    simmer::add_resource(name = iti_entities$Scheduler, ...)
}
