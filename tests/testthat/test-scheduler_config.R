test_that("new_scheduler_config returns resource_config with class scheduler_config and default name parameter", {
  config <- new_scheduler_config()
  expect_s3_class(config, c(iti_configs$scheduler_config, class_names.resource_config))
  expect_named(config, "name")
  expect_equal(config$name, iti_entities$Scheduler)
})
