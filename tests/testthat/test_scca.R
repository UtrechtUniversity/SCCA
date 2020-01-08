context('scca function')

M <- matrix(1:12, ncol = 4)

test_that("scca returns correct output", {
  expect_equal(scca(M), M)
})

N <- "invalid input"

test_that("scca rejects invalid input", {
  expect_error(scca(N), 'input not a matrix')
})
