#' Wrapper around simmer::add_resource based on provided list of arguments
#'
#' @param .env IT infrastructure environment
#' @param .config config object or basically a list of arguments
#'
#' @return Enriched IT infrastructure environment
#' @export
add_iti_resource_entity <- function(.env, .config)
{
  stopifnot(is_resource_config(.config))

  .env %>%
    validate_it_infrastructure() %>%
    (function(env) { do.call(simmer::add_resource, c(.env = env, .config)) })
}

#' Add single IT infrastructure entity based on provided config
#'
#' @param .env IT infrastructure environment
#' @param .config config object
#'
#' @return
add_single_iti_entity <- function(.env, .config)
{
  validate_resource_config(.config)
  validate_it_infrastructure(.env)
  UseMethod("add_single_iti_entity", .config)
}

#' Add IT infrastructure entities based on provided list of config objects
#'
#' @param .env IT infrastructure environment
#' @param ... configs to be wrapped into typed list
#' @param .configs typed list of configs. This is primary list of configs to be used in case if dots and configs are both not empty
#'
#' @return enriched IT infrastructure environment
#' @export
add_iti_entities <- function(.env, ..., .configs)
{
  configs_are_missing <- missing(.configs)
  dots <- list(...)
  if (!configs_are_missing && !rlang::is_empty(dots))
    warning("Both dots (...) and .configs arguments are not empty. In such case dots will be ignored.")

  if (configs_are_missing)
    .configs = dots %>% gendatypes::as.typed_list(type_class = class_names.resource_config)

  validate_it_infrastructure(.env)
  gendatypes::validate_typed_list_class(.configs)
  Reduce(add_single_iti_entity, .configs, init = .env)
}

#' @describeIn add_single_iti_entity S3 Method for server_config
#' @export
add_single_iti_entity.server_config <- function(.env, .config) {
  add_server(.env, .config = .config)
}

#' @describeIn add_single_iti_entity S3 Method for scheduler_config
#' @export
add_single_iti_entity.scheduler_config <- function(.env, .config) {
  add_scheduler(.env, .config = .config)
}
