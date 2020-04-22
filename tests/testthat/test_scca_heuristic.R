context('scca_heuristic')

library(readr)


test_unsorted <- c(0.1, 0.2, 0.2, 0.4, 0.3, 0.9, 1.0)
test_vectors  <- matrix(1:70, ncol = 7)
test_sorted   <- sort(test_unsorted, decreasing = TRUE)
test_ones     <- append(x = rep(1.0, 3), test_sorted)



test_that("scca_heuristic only accepts sorted values", {
  expect_error(object = sccar:::eigengap_heuristic(test_unsorted, test_vectors), 'Eigenvalues are not sorted decreasingly')
})

test_that("scca_heuristic computes gap", {
  expect_error(object = sccar:::eigengap_heuristic(test_sorted, test_vectors), NA)
})

test_that("scca_heuristic computes gap", {
  expect_equal(object = sccar:::eigengap_heuristic(test_ones, test_vectors)$k, expected = 4, info = 'Number of ones not correct')
})

