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
