test_that("generate_resource_config fails with empty class", {
  expect_message(expect_error(generate_resource_config()), "Class must be provided")
})

test_that("generate_resource_config creates new config with provided class name", {
  config <- generate_resource_config(class_name = iti_classes.server)
  expect_s3_class(config, iti_classes.server)
})

test_that("generate_resource_config displays warning if some arguments are unknown for simmer function", {
  expect_warning(generate_resource_config(some_arg = "some_arg", class_name = iti_classes.server), "Some arguments are unknown for simmer::add_resource")
})
