#' Extend mon arrivals or mon attributes with request ID
#'
#' @param .data Monitor data obtained from simmer environment
#' @param request_class Request class whose ID's have to be parsed
#'
#' @export
parse_request_id <- function(.data, request_class)
{
  match.arg(request_class, iti_requests)

  .data %>%
    gendatypes::validate_classes(unlist(iti_metadata)) %>%
    dplyr::mutate(request_id = strsplit(rlang::.data$name, request_class) %>%
                    unlist() %>%
                    as.numeric() %>%
                    Filter(x = rlang::.data, f = Negate(is.na))) %>%
    dplyr::relocate(rlang::.data$request_id)
}

#' Extension for simmer which allows to get arrivals by time window of certain duration
#'
#' @param .envs environments to get monitored data
#' @param period window time period
#' @param units window time unit
#' @param ongoing get ongoing tasks or no
#' @param per_resource display data per resource
#'
#' @export
get_windowed_mon_arrivals <- function(.envs, period = 1, units = "hours", ongoing = FALSE, per_resource = FALSE)
{
  window_duration <- lubridate::duration(period, units) %>% lubridate::time_length()

  .envs %>%
    simmer::get_mon_arrivals(per_resource, ongoing) %>%
    # Filter tasks, which were generated, but not submitted yet
    dplyr::filter(rlang::.data$start_time > 0) %>%
    # Group by policy
    dplyr::group_by(rlang::.data$policy) %>%
    # Create few more columns which will represent a ceiling of specific decimal time, for example if task was started at 1.56 hours
    # Then it will be reduced to the time period of 2nd hour in terms it was started during second hour during a simulation
    # For this specific case 2 columns will be created, start_time_period and end_time_period in order to know start and end periods
    dplyr::mutate(dplyr::across(dplyr::ends_with("_time") & !dplyr::starts_with("activity"),
                                list(period = ~ ceiling(./window_duration) ))) %>%
    # Calculate wait time as a difference between end time, activitiy time and start time
    dplyr::mutate(wait_time = abs(rlang::.data$end_time - rlang::.data$activity_time - rlang::.data$start_time)) %>%
    # Group data into rows-based groups in order to apply mutation per row
    dplyr::rowwise() %>%
    # Add `awaiting period` column, which represents a list of periods, during which current task is awaiting
    dplyr::mutate(time_period = lapply(rlang::.data$start_time_period, function(x) { (x %??% rlang::.data$end_time_period) : (rlang::.data$end_time_period %??% x) })) %>%
    # Ungroup row-based grouping
    dplyr::ungroup() %>%
    # Convert a list of 'awaiting period' values into duplicated rows with all awaiting periods in order to count them during calcualitons
    tidyr::unnest(window_duration) %>%
    # Group data into rows-based groups
    dplyr::rowwise() %>%
    # First let us define next values:
    # system start period time: either start time of task or start time of current period
    # system end period time: either end time of task or end time of current period
    # wait end period time: either end of current period or time when task stopped waiting
    dplyr::mutate(system_start_period_time = max((rlang::.data$time_period - 1) * window_duration, rlang::.data$start_time),
                  system_end_period_time = min(rlang::.data$end_time, rlang::.data$time_period * window_duration),
                  wait_end_period_time = min(rlang::.data$start_time + rlang::.data$wait_time, rlang::.data$time_period * window_duration)) %>%
    # Calculate system time and wait time of each task per period based on previously defined values
    dplyr::mutate(system_time_in_period = rlang::.data$system_end_period_time - rlang::.data$system_start_period_time,
                  wait_time_in_period = max(rlang::.data$wait_end_period_time - rlang::.data$system_start_period_time,0)) %>%
    dplyr::select(-dplyr::ends_with("period_time")) %>%
    # Ungroup row-based grouping
    dplyr::group_by(rlang::.data$name) %>%
    # Mark record as finished if its current time period is equal to end time period
    dplyr::mutate(is_finished = rlang::.data$time_period == rlang::.data$end_time_period,
                  is_arrived = rlang::.data$time_period == rlang::.data$start_time_period,
                  is_processing = !(rlang::.data$is_finished && rlang::.data$is_arrived)) %>%
    # Remove helpers for start time and end time periods
    dplyr::select(-dplyr::ends_with("_time_period"))

}


#' Get summary from mon arrivals
#'
#' @param .data arrivals data
#' @param period interarrival period
#' @param units interarrival period time unitss
#'
#' @export
get_mon_arrival_summary <- function(.data, period = 1, units = "hours")
{
  arrive_period <- lubridate::duration(period, units) %>%
    lubridate::time_length()

  .data %>%
    gendatypes::validate_classes(c(unlist(iti_metadata), "arrivals")) %>%
    dplyr::group_by(rlang::.data$time_period, rlang::.data$policy) %>%
    dplyr::summarise(finished_wait_time = sum(rlang::.data$wait_time * rlang::.data$is_finished, na.rm = T),
                     mean_wait_time = mean(rlang::.data$wait_time_in_period, na.rm = T),
                     mean_finished_wait_time = mean(rlang::.data$wait_time * rlang::.data$is_finished, na.rm = T),
                     finished = sum(rlang::.data$is_finished, na.rm = T),
                     arrived = sum(rlang::.data$is_arrived, na.rm = T),
                     processing = sum(rlang::.data$is_processing, na.rm = T),
                     total_in_work = rlang::.data$arrived + rlang::.data$processing) %>%

    dplyr::mutate(performance = rlang::.data$finished / rlang::.data$total,
                  interarrival_rate = arrive_period / rlang::.data$arrived,
                  cum_finished_wait_time = cumsum(rlang::.data$finished_wait_time) / cumsum(rlang::.data$finished)) %>%

    dplyr::group_by(rlang::.data$policy)
}
