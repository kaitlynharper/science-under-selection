# Tests

test_that("the test framework is working", {
  expect_true(TRUE) #should always pass
})

test_that("there are no duplicate paper IDs", {
  expect_equal(length(unique(results$papers[, 1])), nrow(results$papers))
})

test_that("the number of papers is correct", {
  expect_equal(nrow(results$papers), n_agents * n_timesteps * papers_per_agent_per_timestep)
})
