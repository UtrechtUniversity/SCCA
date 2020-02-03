context('compute_symmetric')

library(readr)

test_unsorted <- c(0.1, 0.2, 0.2, 0.4, 0.3, 0.5, 1.0)
test_sorted   <- sort(test_sorted, decreasing = TRUE)
test_ones     <- append(x = rep(1.0, 3), test_sorted)

test_ones <- c(1, 1, 1, )


test_that("apply_heuristic only accepts sorted values", {
  expect_error(object = apply_heuristics(test_unsorted), 'Eigenvalues are not decreasingly sorted')
})

