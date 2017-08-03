context("Colley ratings")

# Data for Langville/Meyer running example
teams <- c("Duke", "Miami", "UNC", "UVA", "VT")
rec <- c(-4,4,0,-2,2)
colley_mtx <- matrix(-1, nrow = 5, ncol = 5) + diag(7, nrow = 5, ncol = 5)
names(rec) <- teams
colnames(colley_mtx) <- teams; rownames(colley_mtx) <- teams

# Data for Langville/Meyer movie example
clly_mov_mtx1 <- matrix(
  c(9, -2, -2, -3,
    -2, 7, -2, -1,
    -2, -2, 7, -1,
    -3, -1, -1, 7),
  ncol = 4,
  byrow = TRUE
)
clly_mov_rec <- c(4, 2, -3, -3)
clly_mov_rtng1 <- c(0.67, 0.63, 0.34, 0.35)


test_that("Colley ratings match Langville/Meyer examples", {
  expect_equal(colley(colley_mtx, rec),
               c(Duke = 0.21, Miami = 0.79, UNC = 0.50, UVA = 0.36, VT = 0.65),
               tolerance = 1e-2)
  expect_equal(colley(clly_mov_mtx1, clly_mov_rec),
               clly_mov_rtng1,
               tolerance = 1e-2)
})
