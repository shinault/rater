context("Markov ratings")

# Data for tests
mkv_mtx1 <- matrix(
  c(0, 1/4, 1/4, 1/4, 1/4,
    1/5, 1/5, 1/5, 1/5, 1/5,
    0, 1/2, 0, 0, 1/2,
    0, 1/3, 1/3, 0, 1/3,
    0, 1, 0, 0, 0),
  nrow = 5,
  byrow = TRUE
)
mkv_rtg1 <- matrix(c(0.087, 0.438, 0.146, 0.110, 0.219), ncol = 1)

mkv_mtx2 <- matrix(
  c(0, 45/124, 3/124, 31/124, 45/124,
    1/5, 1/5, 1/5, 1/5, 1/5,
    0, 18/45, 0, 0, 27/45,
    0, 8/48, 2/48, 0, 38/48,
    0, 1, 0, 0, 0),
  nrow = 5,
  byrow = TRUE
)
mkv_rtg2 <- matrix(c(0.088, 0.442, 0.095, 0.110, 0.265), ncol = 1)

mkv_mtx3 <- matrix(
  c(0, 52/159, 24/159, 38/159, 45/159,
    7/47, 0, 16/47, 17/47, 7/47,
    21/90, 34/90, 0, 5/90, 30/90,
    7/91, 25/91, 7/91, 0, 52/91,
    0, 27/44, 3/44, 14/44, 0),
  nrow = 5,
  byrow = TRUE
)
mkv_rtg3 <- matrix(c(0.095, 0.296, 0.149, 0.216, 0.244), ncol = 1)

# Tests
test_that("markov ratings consistent with Langville/Meyer examples", {
  expect_equal(markov(mkv_mtx1), mkv_rtg1, tolerance=1e-3)
  expect_equal(markov(mkv_mtx2), mkv_rtg2, tolerance=1e-3)
  expect_equal(markov(mkv_mtx3), mkv_rtg3, tolerance=1e-3)
})
