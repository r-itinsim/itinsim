test_that("new_simulation_config throws exception if no arguments provided", {
  expect_error(new_simulation_config(), "At least one argument either 'tasks' or 'until' must be provided!")
})

test_that("when new_simulation_config created until parameter will be set to Inf if not provided and tasks are provided", {
  sim_config <- new_simulation_config(tasks = 1)
  expect_equal(sim_config$until, Inf)
  expect_equal(sim_config$tasks, 1)
})
