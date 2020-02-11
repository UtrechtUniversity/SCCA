

M1 <- matrix(sample(x = c(0,1), size = 120, prob = c(0.8, 0.2), replace = TRUE), ncol = 8)
M1 <- M1[ ,colSums(M1) != 0]
M1 <- M1[rowSums(M1) != 0, ]
readr::write_rds(x = M1, path = 'inst/M1.rds')
