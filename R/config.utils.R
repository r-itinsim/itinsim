#' Create list of config objects with the same parameters and different names
#'
#' @param n Number of items
#' @param config_type Type of config to generate
#' @param ... Parameters of the config
#'
#' @return list of config objects
#' @export
config.repeat <- function(n = 1, config_type = c("server", "scheduler"), ...)
{
  config_type <- match.arg(config_type)
  args <- c(list(n = n), list(...))
  do.call(paste0("config.repeat.", config_type), args)
}

#' Repeat function handler for server_config
#'
#' @param n Number of items
#' @param ... Additional arguments
#'
#' @return list of server_config objects with the same parameters
#' @export
config.repeat.server <- function(n = 1, ...)
{
  generate_list_of_named_items(n, name_template = "Server-%s", create_function = new_server_config)
}

#' Repeat function handler for scheduler_config
#'
#' @param n Number of items
#' @param ... Additional arguments
#'
#' @return list of scheduler_config object with the same parameters
#' @export
config.repeat.scheduler <- function(n = 1, ...)
{
  generate_list_of_named_items(n, name_template = "Scheduler-%s", create_function = new_scheduler_config)
}

#' Generate list of named items according to passed creation function
#'
#' @param n Number of items to create
#' @param name_template Name template for sprintf
#' @param create_function Function which will be used for create
#' @param ... Additional parameters required for specific function
#'
#' @keywords internal
generate_list_of_named_items <- function(n = 1, name_template, create_function, ...)
{
  sprintf(name_template, 1:n) %>%
    lapply(function(name)
    {
      args <- list(...)
      args$name <- name
      do.call(create_function, list(prepared_args = args))
    })
}

#' Generate many configs with the same properties
#'
#' @param n Number of copies
#' @param ... Properties
#'
#' @return list of generated configs
#' @export
generate.server_config <- function(n = 1, ...)
{
  sprintf("Server-%s", 1:n) %>%
    lapply(function(server_name)
    {
      args <- list(...)
      args$name <- server_name
      new_server_config(prepared_args = args)
    })
}

#' Generate many scheduler configs with the same properties
#'
#' @param n Number of copies
#' @param ... Properties
#'
#' @return list of generated configs
#' @export
generate.scheduler_config <- function(n = 1, ...)
{
  sprintf("Scheduler-%s", 1:n) %>%
    lapply(function(scheduler_name)
    {
      args <- list(...)
      args$name <- scheduler_name
      new_scheduler_config(prepared_args = args)
    })
}
