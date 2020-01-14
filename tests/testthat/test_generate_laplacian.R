context('generate_laplacian function')

M <- matrix(1:12, ncol = 4)


test_that("generate_laplacian rejects invalid input", {
  expect_error(generate_laplacian(M, cluster_dim = 'XXXX'), 'Unknown cluster dimension')
})

