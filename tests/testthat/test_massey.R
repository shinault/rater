context("Massey ratings")

# Data for Langville/Meyer example
test_data <- dget(file = "massey_data.R")
ex_massey <- test_data[[1]]
pts_f <- test_data[[2]]
pts_a <- test_data[[3]]

test_that("Massey rating matches running example", {
  expect_equal(massey(ex_massey, pts_f - pts_a),
               c(Duke = -24.8, Miami = 18.2, UNC = -8.0, UVA = -3.4, VT = 18.0))
})

test_that("Advanced Massey rating matches running example", {
  expect_equal(massey_adv(ex_massey, pts_f, pts_a),
               cbind(ratings = c(Duke = -24.8, Miami = 18.2, UNC = -8.0, UVA = -3.4, VT = 18.0) ,
                     defensive = c(Duke = -26.8, Miami = -3.8, UNC = -9.4, UVA = -11.2, VT = -2.7),
                     offensive = c(Duke = 2.0, Miami = 22.0, UNC = 1.4, UVA = 7.8, VT = 20.7)),
               tolerance = 1e-1)
})
