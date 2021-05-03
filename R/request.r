#' Generic request constructor
#'
#' @param name request name, like http, ftp, ...
#'
#' @return new simmer trajectory abstracted to IT request
#' @export
new_request <- function(name = character())
{
  match.arg(name, iti_requests)

  request_trajectory <- simmer::trajectory(iti_requests$http)
  request_trajectory$activities <- list()
  request_trajectory %>% gendatypes::with_class(c(sprintf("%s-request", name), "request"))
}

#' Request methods
#'
#' Check whether object has class 'request' and is simmer trajectory
#'
#' @param x object to test
#'
#' @return TRUE or FALSE
#' @export
is_request <- function(x) {
  is_trajectory(x) && is(x, "request")
}

#' Request methods
#'
#' Validate that object is a request and fail if it is not
#'
#' @param x object to validate
#'
#' @export
validate_request <- function(x) {
  if (!is_request(x))
    gendatypes::throw_exception(x, paste("Provided object is of wrong classs! Actual class:", class(x)))
  x
}



# Trajectory modification functions ---------------------------------------

#' Handle method for request trajectory
#'
#' @param .trj request trajectory
#' @param resource resource name to seize and release
#' @param handlers number of discrete resources to be acquired
#' @param handle_function timeout function or handle time function
#'
#' @return request trajectory with handled resource
#' @export
handle_request_with <- function(.trj, resource, handle_function, handlers = 1)
{
  lastIndex <- .trj %>% validate_request() %>% length()

  .trj %>%
    simmer::seize(resource, amount = handlers) %>%
    simmer::timeout(function() handle_function(1)) %>%
    simmer::release(resource)

  resource_activities_indexes <- .trj$activities[[resource]]

  if (!rlang::is_empty(resource_activities_indexes))
    warning("Resource index with name", resource, "was already handled!")

  .trj$activities[[resource]] <- list(timeout = lastIndex + 2)
  .trj
}

#' Change timeout handle time for resource
#'
#' @param .trj request trajectory
#' @param resource resource name to lookup
#' @param handle_function new timeout handle function
#'
#' @return updated trajectory
#' @export
change_timeout_for <- function(.trj, resource, handle_function)
{
  validate_request(.trj)

  resource_activities <- .trj$activities[[resource]]

  distribution <- adapt_distribution(handle_function)

  .trj[resource_activities[["timeout"]]] <- simmer::timeout(simmer::trajectory(), distribution)

  .trj
}



# Resource-specific trajectory modification functions ---------------------


#' Handle method for request trajectory
#'
#' Handle wrapper for scheduler
#'
#' @inheritParams handle_request_with
#' @inheritDotParams handle_request_with
#'
#' @export
handle_request_with_scheduler <- function(.trj, ...) {
  .trj %>% handle_request_with(resource = iti_entities$Scheduler, ...)
}

#' Handle method for request trajectory
#'
#' Handle wrapper for server
#'
#' @inheritParams handle_request_with
#' @inheritDotParams handle_request_with
#'
#' @export
handle_request_with_server <- function(.trj, ...) {
  .trj %>% handle_request_with(resource = iti_entities$Server, ...)
}

#' Change timeout handle time for resource
#'
#' Change timeout wrapper for server
#'
#' @inheritParams change_timeout_for
#' @inheritDotParams change_timeout_for
#'
#' @export
change_server_processing_time <- function(.trj, ...)
{
  .trj %>% change_timeout_for(resource = iti_entities$Server, ...)
}


#' Change timeout handle time for resource
#'
#' Change timeout wrapper for scheduler
#'
#' @inheritParams change_timeout_for
#' @inheritDotParams change_timeout_for
#'
#' @export
change_scheduler_handle_time <- function(.trj, ...)
{
  .trj %>% change_timeout_for(resource = iti_entities$Scheduler, ...)
}


# Internal functions ------------------------------------------------------

#' Simmer generator distribution adaptor function
#'
#' @param distribution_function distribution functions
#'
#' @return adapted generator functions, which has empty formal arguments and produces an element
#' @keywords internal
#' @export
adapt_distribution <- function(distribution_function) {
  switch(class(distribution_function),
         "numeric" = distribution_function,
         "function" = ifelse(is.null(formals(distribution_function)),
                             distribution_function,
                             function() distribution_function(1)),
         gendatypes::throw_exception(distribution_function, "Input object must be either function or numeric!"))
}
