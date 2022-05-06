add_iti_resource_entity <- function(.env, .config)
{
  stopifnot(is_resource_config(.config))

  .env %>%
    validate_it_infrastructure() %>%
    (function(env) { do.call(simmer::add_resource, c(.env = env, .config)) })
}

add_single_iti_entity <- function(.env, .config)
{
  validate_resource_config(.config)
  validate_it_infrastructure(.env)
  UseMethod("add_single_iti_entity", .config)
}

add_iti_entities <- function(.env, .configs)
{
  gendatypes::validate_typed_list_class(.configs)
}

add_single_iti_entity.server_config <- add_server

add_single_iti_entity.scheduler_config <- add_scheduler
