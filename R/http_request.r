#' Http request constructor
#'
#' Create simmer trajectory wrapper for http request
#'
#' @return http request trajectory
#' @export
create_http_request <- function() {
  new_request(iti_requests$http)
}

#' Http request methods
#'
#' Abstraction analogy for add_generator in simmer, which is applicable only to
#' IT infrastructure object and to request trajectory
#'
#' @param .env IT infrastructure environment
#' @param .trj request trajectory
#' @param interarrival_time_generator interarrival time generator
#'
#' @export
send_http_request <- function(.env, .trj, interarrival_time_generator)
{
  validate_it_infrastructure(.env)
  validate_request(.trj)

  .env %>%
    simmer::add_generator(iti_requests$http,
                          .trj,
                          adapt_distribution(interarrival_time_generator),
                          mon = 2)
}
