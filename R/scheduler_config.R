#' Create scheduler config
#'
#' @inheritParams new_resource_config
#' @param prepared_args Primary args to use
#'
#' @return 'scheduler config' object
#' @export
new_scheduler_config <- function(..., prepared_args = list()) {
  config <- generate_resource_config(..., prepared_args = prepared_args, class_name = iti_configs$scheduler_config)
  if (!utils::hasName(config, "name")) config$name <- iti_entities$Scheduler
  config
}
