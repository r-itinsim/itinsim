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
  generate_list_of_named_items(n, name_template = "Server%s", create_function = new_server_config)
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
  generate_list_of_named_items(n, name_template = "Scheduler%s", create_function = new_scheduler_config)
}

#' Generate list of named items according to passed creation function
#'
#' @param n Number of items to create
#' @param name_template Name template for sprintf
#' @param create_function Function which will be used for create
#' @param ... Additional parameters required for specific function
#'
#' @keywords internal
generate_list_of_named_items <- function(x, name_template, create_function, ...)
{
  UseMethod("generate_list_of_named_items", x)
}

#' Generate list of objects with name parameter based on creation function
#' @description All arguments with 'name' argument will be passed to creation function as 'prepared_args' list argument
#'
#' @param x input vector for names generation
#' @param name_template template for sprintf with %s replace character
#' @param create_function creation function
#' @param ... additional arguments
#'
#' @return list of object
#' @export
generate_list_of_named_items.numeric <- function(x, name_template, create_function, ...)
{
  if (isTRUE(x == 1)) x <- ""
  else if (length(x) == 1) x <- paste0("-", 1:x)
  else x <- paste0("-", x)
  NextMethod()
}

#' S3 method for character overload of generate_list_of_named_items function
#'
#' @keywords internal
generate_list_of_named_items.character <- function(x, name_template, create_function, ...)
{
  x <- paste0("-", x)
  NextMethod()
}

#' S3 method for default overload of generate_list_of_named_items function
#'
#' @keywords.internal
generate_list_of_named_items.default <- function(x, name_template, create_function, ...)
{
  result <- sprintf(name_template, x)
  result %>%
    lapply(function(name)
    {
      args <- list(...)
      args$name <- name
      do.call(create_function, list(prepared_args = args))
    })
}
