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
    warning(paste("Some arguments are unknown for", deparse(substitute(func)),
                  "and will be skipped:", not_mapped_args_names,
                  "\n  Use arguments: ", paste(names(func_args), collapse = ", ")))
  }

  args[dplyr::setdiff(names(args), not_mapped_args_names)]
}
