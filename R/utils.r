#' Prepare arguments from the list for function evaluation
#'
#' @param func Functions whose arguments have to be mapped
#' @param args Arguments list used for mapping
#'
#' @return Subset of arguments specific for function call
#' @export
prepare_args_from_caller <- function(func, args) {
  func_args <- formals(func)

  if (!rlang::is_empty(not_mapped_args_names <- dplyr::setdiff(names(args), names(func_args)))) {
    warning(paste("Some arguments are unknown for", paste(trimws(deparse(substitute(func))), collapse = ""),
                  "and will be skipped:", not_mapped_args_names,
                  "\n  Use arguments: ", paste(names(func_args), collapse = ", ")))
  }

  args[dplyr::setdiff(names(args), not_mapped_args_names)]
}


#' S3 method for as.data.frame when 'density' type is provided
#'
#' @inheritParams base::as.data.frame
#'
#' @return data frame which contains only 'x' and 'y' values from 'density' object
#' @export
as.data.frame.density <- function(x, row.names, optional, ...)
{
  data.frame(y = x$y, x = x$x)
}


#' Convert vector of values from some column in data frame to distribution function
#' Original approach was taken from answer here:
#' https://stackoverflow.com/questions/32871602/r-generate-data-from-a-probability-density-distribution
#'
#' @param data Provided data.frame
#' @param column Column name
#' @param only.value If is equal to TRUE then generator function will always return single value, otherwise it will return pairs of x and y
#' @importFrom stats runif
#' @importFrom rlang .data
#'
#' @return function which accepts number and returns random value from generated distribution
#' @export
get_distribution_function <- function(data, column, only.value = T)
{
  approx_data <- data %>%
    dplyr::pull({{column}}) %>%
    stats::density(n = nrow(data), bw = 1) %>%
    as.data.frame() %>%
    dplyr::mutate(relcumsum = cumsum(.data$y)/sum(.data$y)) %>%
    dplyr::select(x = .data$relcumsum, y = .data$x) %>%
    as.list()

  function(n)
  {
    result <- approx_data %>%
      stats::approx(xout = stats::runif(n), ties = min) %>%
      as.data.frame() %>%
      dplyr::mutate(y = ifelse(.data$y < 0 | is.na(.data$y), 0, floor(.data$y)))

    if (only.value) result$y else result
  }
}
