context('apply_heuristic')

library(readr)


test_unsorted <- c(0.1, 0.2, 0.2, 0.4, 0.3, 0.5, 1.0)
test_sorted   <- sort(test_unsorted, decreasing = TRUE)
test_ones     <- append(x = rep(1.0, 3), test_sorted)



test_that("apply_heuristic only accepts sorted values", {
  expect_error(object = sccar:::apply_heuristic(test_unsorted), 'Eigenvalues are not decreasingly sorted')
})

test_that("apply_heuristic computes gap", {
  expect_error(object = sccar:::apply_heuristic(test_sorted), NA)
})

test_that("apply_heuristic computes gap", {
  expect_equal(object = sccar:::apply_heuristic(test_ones), expected = 4, info = 'Number of ones not correct')
})

