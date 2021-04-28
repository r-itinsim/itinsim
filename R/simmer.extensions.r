#' Simmer extensions
#'
#' Check if object is of class "simmer" or "wrap"
#'
#' @param x object to test
#'
#' @return TRUE or FALSE
#' @export
is_simmer <- function(x) {
  (is.environment(x) & any(class(x) %in% c("simmer", "wrap")))
}

#' Simmer extensions
#'
#' Validate object as a simmer object, otherwise fail
#'
#' @inheritParams is_simmer
#'
#' @return same object as was provided
#' @export
validate_simmer <- function(x)
{
  if (!is_simmer(x))
    gendatypes::throw_exception(x, "Provided object must be of type `environment` and of either class `simmer` or `wrap`")

  invisible(x)
}

#' Simmer extensions
#'
#' Validate if object is of class "simmer" and it's name is equal to asertee name
#'
#' @param x Object to test
#' @param assertedName Expected name of simmer object
#'
#' @return TRUE or FALSE
#' @export
validate_simmer_with_name <- function(x, assertedName) {
  validate_simmer(x)

  if (!(hasName(x, "name") && x[["name"]] == assertedName))
    gendatypes::throw_exception(x, paste("Provided object has a different name!\n", "Expected:",assertedName,"Actual:",x[["name"]]))

  invisible(x)
}

#' Enrich method for get_mon_* results from simmer
#'
#' @param envs simmer environments
#' @param get_mon_function get_mon_* function from the family of monitoring functions
#'
#' @return Monitored data with enriched columns
#' @export
enrich_get_mon <- function(envs, get_mon_function) {
  UseMethod("enrich_get_mon", unlist(list(envs), recursive = F)[[1]])
}

#' S3 Method of 'enrich_get_mon' for 'policy_simulation_result' objects
#'
#' @inheritParams enrich_get_mon
#'
#' @inherit enrich_get_mon return
#' @importFrom rlang .data
#' @export
enrich_get_mon.policy_simulation_result <- function(envs, get_mon_function) {
  policies <- if (gendatypes::is_typed_list(envs)) envs$policy else sapply(envs, function(x) x$policy)
  get_policy_index <- Vectorize(function(x) which(x == policies))

  envs %>%
    lapply(function(item) {
      item %>%
        validate_policy_simulation_result %>%
        rlang::env_clone() %>%
        structure(class = dplyr::intersect(c("simmer", "wrap"), class(item))) %>%
        get_mon_function() %>%
        dplyr::mutate(policy = item$policy)
    }) %>%
    dplyr::bind_rows() %>%
    dplyr::mutate(replication = get_policy_index(.data$policy))
}


# Masking simmer resources methods ----------------------------------------


#' Mask resources in simmer environment
#'
#' @param .envs simulation environments
#' @param values Vector of resource names to mask
#' @param mask Expected mask name
#'
#' @return Cloned environment with masked resources
#' @export
mask_resources <- function(.envs, values, mask) {
  UseMethod("mask_resources", unlist(list(.envs))[[1]])
}

#' Mask resources in simmer environment
#'
#' @inheritParams mask_resources
#'
#' @inherit mask_resources return
#' @export
mask_resources.simmer <- function(.envs, values, mask) {
  .envs %>%
    lapply(function(env) env %>% simmer::wrap()) %>%
    mask_resources(values, mask)
}

#' Mask resources in simmer environment
#'
#' @inheritParams mask_resources
#'
#' @inherit mask_resources return
#' @export
mask_resources.wrap <- function(.envs, values, mask) {
  .envs %>% lapply(function(env) mask_wrap_resources(env, values, mask))
}

#' @keywords internal
mask_wrap_resources <- function(.env, values, mask) {
  if (!methods::is(.env, "wrap"))
    gendatypes::throw_exception(.env, "Provided object is not of class 'wrap'")

  env <- .env %>%
    validate_simmer() %>%
    rlang::env_clone() %>%
    structure(class = class(.env), cloned = TRUE)

  env %>%
    mask_resource_in_mon_arrivals_res(values, mask) %>%
    mask_resources_in_mon_resources(values, mask)
}

#' @keywords internal
mask_resource_in_mon_arrivals_res <- function(.env, values, mask)
{
  if (!(attr(.env, "cloned") %??% FALSE))
    gendatypes::throw_exception(.env, "Environment should be cloned!")

  .env$mon_arrivals_res %<>%
    dplyr::mutate(resource = ifelse(.data$resource %in% values, mask, .data$resource))

  .env
}

#' @keywords internal
mask_resources_in_mon_resources <- function(.env, values, mask)
{
  if (!attr(.env, "cloned")) gendatypes::throw_exception(.env, "Environment should be cloned!")

  mutate_if_in_list <- function(data, column, true, .false = NULL)
  {
    data %>%
      dplyr::mutate("{{column}}" := ifelse(
        .data$resource %in% values, !!rlang::enexpr(true), .false %??% {{column}}))
  }

  env <- structure(rlang::env_clone(.env), class = class(.env))

  total <- simmer::get_mon_resources(.env) %>%
    dplyr::filter(.data$resource %in% values) %>%
    dplyr::distinct(.data$resource, .data$capacity, .data$queue_size) %>%
    dplyr::summarise(capacity = sum(.data$capacity), queue_size = sum(.data$queue_size))

  env$mon_resources %<>%
    dplyr::group_by(.data$resource) %>%
    mutate_if_in_list(.data$server_changed_by, diff(c(0,.data$server)), 0) %>%
    mutate_if_in_list(.data$queue_changed_by, diff(c(0,.data$queue)), 0) %>%
    dplyr::ungroup() %>%
    mutate_if_in_list(.data$server, cumsum(.data$server_changed_by)) %>%
    mutate_if_in_list(.data$queue, cumsum(.data$queue_changed_by)) %>%
    dplyr::select(-.data$server_changed_by, -.data$queue_changed_by) %>%
    mutate_if_in_list(.data$capacity, total$capacity) %>%
    mutate_if_in_list(.data$queue_size, total$queue_size) %>%
    mutate_if_in_list(.data$system, .data$server + .data$queue) %>%
    mutate_if_in_list(.data$resource, mask)

  env
}
