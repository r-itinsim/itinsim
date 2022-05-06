#' Add server
#'
#' @param .env simulation environment
#' @param ... other resource parameters
#' @param .config server config object which will be used as primary source of parameters for the resource
#'
#' @export
add_server <- function(.env, ..., .config)
{
  if (missing(.config) || rlang::is_empty(.config))
    .config <- new_server_config(prepared_args = list(...))

  .env %>% add_iti_resource_entity(.config = .config)
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

  add_resource_dynamic <- function(env, args) {
    args$.env <- env
    do.call(simmer::add_resource, args)
  }

  .servers %>% Reduce(f = add_resource_dynamic, init = .env)
}
