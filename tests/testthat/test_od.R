context("Offense-defense ratings")

score_mtx <- matrix(
  c(0 , 52, 24, 38, 45,
     7, 0 , 16, 17,  7,
    21, 34,  0,  5, 30,
     7, 25,  7,  0, 52,
     0, 27,  3, 14,  0),
  nrow = 5,
  byrow = TRUE
)

yardage_mtx <- matrix(
  c(  0, 557, 357, 315, 362,
    100,   0, 188, 452, 167,
    209, 321,   0, 199, 338,
    207, 500, 270,   0, 552,
     35, 304, 196, 334,   0),
  nrow = 5,
  byrow = TRUE
)

score_rtg <- data.frame(
  Off = c(33.99, 151.6, 48.66, 82.05, 114.8),
  Def = c(1.691, 0.8029, 1.164, 0.9675, 0.4104),
  Rtg = c(20.1, 188.8, 41.8, 84.8, 279.8)
)

yardage_rtg <- data.frame(
  Off = c(537.3, 1608, 1024, 1537, 1249),
  Def = c(1.1900, 0.7975, 0.9886, 1.4020, 0.6629),
  Rtg = c(451.6, 2016, 1036, 1096, 1885)
)

test_that("OD ratings consistent with Langville/Meyer examples", {
  expect_equal(od(score_mtx)[, 1], score_rtg[, 1], tolerance = 1e-1)
  expect_equal(od(score_mtx)[, 2], score_rtg[, 2], tolerance = 1e-3)
  expect_equal(od(score_mtx)[, 3], score_rtg[, 3], tolerance = 1e-1)
  expect_equal(od(yardage_mtx)[, 1], yardage_rtg[, 1], tolerance = 1e0)
  expect_equal(od(yardage_mtx)[, 2], yardage_rtg[, 2], tolerance = 1e-3)
  expect_equal(od(yardage_mtx)[, 3], yardage_rtg[, 3], tolerance = 1e0)
})
