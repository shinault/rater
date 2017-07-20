context("Kendall correlation")

test_that("kendall catches errors", {
  expect_error(kendall(c(1,1,1), 1:10), "duplicate entry")
  expect_error(kendall(1:10, c(1,1,1)), "duplicate entry")
  expect_error(kendall(c(3,1,3,54,356), c(1,2,1)), "duplicate entry")
})

# Sample rankings for standard definition
rand1 <- sample(letters, size = 8, replace = FALSE)
rand2 <- sample(letters, size = 12, replace = FALSE)

test_that("kendall is consistent with standard definition", {
  expect_equal(kendall(1:10, 1:10), 1)
  expect_equal(kendall(1:10, 10:1), -1)
  expect_equal(kendall(rand1, rev(rand1)), -1)
  expect_equal(kendall(rand2, rev(rand2)), -1)
})

# Sample rankings in Langville/Meyer
l1 <- c("A", "D", "C", "B")
l2 <- c("A", "C", "D", "B")
l3 <- c("B", "D", "C", "A")
l4 <- c("A", "E", "B")
l5 <- c("B", "E", "A")
l6 <- c("C", "A", "F")
l7 <- c("C", "D", "F")

test_that("kendall is consistent with Langville/Meyer full examples", {
  expect_equal(kendall(l1, l2), 4/6)
  expect_equal(kendall(l1, l3), -4/6)
  expect_equal(kendall(l2, l3), -6/6)
})

test_that("kendall is consistent with Langville/Meyer partial examples", {
  expect_equal(kendall(l4, l5, c(l6, l7)), 1/4)
  expect_equal(kendall(l4, l6, c(l5, l7)), -7/9)
  expect_equal(kendall(l4, l7, c(l5, l6)), -5/3)
})

