class_names.resource_config <- "resource_config"

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
  structure(config_arguments, class = c(class, class_names.resource_config))
}

#' Generic method for creating typed configs with specific class name
#'
#' @param ... Holds list of items which simmer::add_resource requires. If any argument is unknown the error will be displayed.
#' @param class_name Class of resource config
#' @param prepared_args Primary args to use
#'
#' @return Typed resource config
#' @keywords internal
generate_resource_config <- function(..., class_name, prepared_args = list())
{
  if (missing(class_name)) gendatypes::throw_exception(class_name, message = "Class must be provided!")
  args <- if (rlang::is_empty(prepared_args)) unlist(list(...)) else prepared_args
  purrr::lift_dl(new_resource_config, class = class_name)(args)
}

#' Check whether object is resource_config
#'
#' @param obj Object to verify
#'
#' @return TRUE or FALSE
#' @export
is_resource_config <- function(obj)
{
  is(obj, class_names.resource_config)
}

#' Validate resource config object
#'
#' @details Checks if object is of type resource_config and if not will stop execution
#' @param obj object to validate
#'
#' @return same object
#' @export
validate_resource_config <- function(obj)
{
  gendatypes::validate_classes(obj, class_names.resource_config)
  gendatypes::as.typed_list
}

validate_typed_list_of_resource_configs <- function(obj)
{
  gendatypes::validate_typed_list_class(obj)
  stopifnot(gendatypes::typed_list.is(obj, class_names.resource_config))
}
