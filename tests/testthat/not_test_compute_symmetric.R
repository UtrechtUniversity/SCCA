context('compute_symmetric')

library(readr)

M1 <- readr::read_rds(path = system.file("M1.rds", package = "sccar"))


test_that("compute_symmetric doesn't compute same result", {
  expect_known_value(object = sccar:::compute_symmetric(M1, decomp_axis = 'cols'),
                     #file   = 'inst/csM1'
                     file   = system.file("csM1", package = "sccar")
  )
})

