#' Create multiple server configs
#'
#' @param n Number of server configs to create
#' @inheritParams new_resource_config
#' @param name_template Template for naming servers, must contain %d to map number of server
#'
#' @return list of server configs
#' @importFrom gendatypes as.typed_list
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
#' @importFrom purrr lift_dl
#' @importFrom rlang is_empty
#' @export
new_server_config <- function(..., prepared_args = list()) {
  args <- if (rlang::is_empty(prepared_args)) list(...) else prepared_args
  purrr::lift_dl(new_resource_config, class = "server_config")(args)
}
