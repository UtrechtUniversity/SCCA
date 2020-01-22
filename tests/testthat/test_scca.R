context('scca function')

M <- matrix(1:12, ncol = 4)
print(M)

# test_that("scca returns correct output", {
#   expect_equal(scca_compute(M), M)
# })

N <- "invalid input"

test_that("scca rejects invalid input", {
  expect_error(scca_compute(N), 'input not a matrix')
})

