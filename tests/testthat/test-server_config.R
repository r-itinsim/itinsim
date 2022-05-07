test_that("new_server_config returns resource_config with class server_config and default name parameter", {
  config <- new_server_config()
  expect_s3_class(config, c(iti_configs$server_config, class_names.resource_config))
  expect_named(config, "name")
  expect_equal(config$name, iti_entities$Server)
})
