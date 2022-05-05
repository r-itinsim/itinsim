test_that("list of different resource config types can be coerced to typed.list", {
  server_config <- new_server_config(name = "Server-1")
  scheduler_config <- new_scheduler_config(name = "Scheduler-1")
  result <- list(server_config, scheduler_config) %>% gendatypes::as.typed_list(type_class = "resource_config")
  expect_s3_class(result, "typed_list")
  expect_equal(gendatypes::typed_list.typeof(result), "resource_config")
})
