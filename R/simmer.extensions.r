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
