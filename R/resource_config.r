#' Resource config object
#'
#' List of parameters for add_resource function from simmer package
#'
#' @param ... Holds list of items
#' @param class Custom class name to apply to resulting object
#'
#' @return 'resource_config' object with prepared named items
#' @export
new_resource_config <- function(..., class = character()) {
  config_arguments <- prepare_args_from_caller(simmer::add_resource, list(...))
  structure(config_arguments, class = c(class, "resource_config"))
}
