context("Colley ratings")

# Data for Langville/Meyer examples
teams <- c("Duke", "Miami", "UNC", "UVA", "VT")
rec <- c(-4,4,0,-2,2)
colley_mtx <- matrix(-1, nrow = 5, ncol = 5) + diag(7, nrow = 5, ncol = 5)
names(rec) <- teams
colnames(colley_mtx) <- teams; rownames(colley_mtx) <- teams


test_that("Colley ratings match running example", {
  expect_equal(colley(colley_mtx, rec),
               c(Duke = 0.21, Miami = 0.79, UNC = 0.50, UVA = 0.36, VT = 0.65),
               tolerance = 1e-2)
})
