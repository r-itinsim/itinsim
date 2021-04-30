#' Simulation cache object
#'
#' @param env simmer environment
#'
#' @return new simulation cache object
#' @export
new_simulation_cache <- function(env)
{
  env <- validate_simmer(env)
  cache <- list()
  object <- list(
    clean_selected = function() {
      selected <- simmer::get_selected(env)
      cache[[selected]] <<- NULL
    },
    save_selected = function(key) {
      selected <- as.character(simmer::get_selected(env))
      cache[[selected]][[key]] <<- simmer::get_attribute(env, key)
    },
    get_cached_or_default = function(resources, default, key = character(), index = numeric()) {
      if (missing(key) & missing(index)) stop("key and index both can't be missing. Provide any of them")
      if (!missing(key) && !is.character(key)) stop("key must be of type `character`")
      if (!missing(index) && !is.numeric(index)) stop("index must be of type `numeric`")
      sapply(cache[resources], function(item) item[[key %??% index]] %??% default)
    }
  )
  structure(object, class = "simulation_cache")
}

#' Validate simulation cache object
#'
#' @param x object to test
#'
#' @return invisible object or validation message
#' @export
validate_simulation_cache <- function(x) {
  if (!is(x, "simulation_cache"))
    gendatypes::throw_exception(x, "Object is not of type 'policy_cache'")

  if (!utils::hasName(x, "clean_selected"))
    gendatypes::throw_exception(x, "Input object doesn't contain `clean_selected` function")

  if (!utils::hasName(x, "save_selected"))
    gendatypes::throw_exception(x, "Input object doesn't contain `save_selected` function")

  if (!utils::hasName(x, "get_cached_or_default"))
    gendatypes::throw_exception(x, "Input object doesn't contain `get_cached_or_default` function")

  invisible(x)
}
