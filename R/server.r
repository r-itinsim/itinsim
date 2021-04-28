#' Add server
#'
#' @param .env simulation environment
#' @param ... other resource parameters
#'
#' @export
add_server <- function(.env, ...)
{
  .env %>%
    validate_it_infrastructure() %>%
    simmer::add_resource(name = iti_entities$Server, ...)
}

#' Add a list of servers to IT infrastructure
#' @description Define resources with predefined name "Server"
#' @param .env the simulation environment as IT infrastructure
#' @param .servers server configurations
#'
#' @return environment with added server resources
#' @export
add_servers <- function(.env, .servers)
{
  validate_it_infrastructure(.env)

  for (server in .servers)
  {
    .env %>% simmer::add_resource(name = server$name,
                                  capacity = server$capacity,
                                  queue_size = server$queue_size,
                                  preemptive = server$preemptive)
  }
  .env
}
