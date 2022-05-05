#' Create multiple server configs
#'
#' @param n Number of server configs to create
#' @inheritParams new_resource_config
#' @param name_template Template for naming servers, must contain %d to map number of server
#'
#' @return list of server configs
#' @export
create_servers_configs <- function(n = numeric(), ..., name_template = "Server-%d") {
  sprintf(name_template, 1:n) %>%
    lapply(function(serverName) new_server_config(name = serverName, ...)) %>%
    gendatypes::as.typed_list(type_class = "server_config")
}

#' Create server config
#'
#' @inheritParams new_resource_config
#' @param prepared_args Primary args to use
#'
#' @return 'server config' object
#' @export
new_server_config <- function(..., prepared_args = list()) {
  generate_resource_config(..., prepared_args = prepared_args, class_name = iti_configs$server_config)
}
