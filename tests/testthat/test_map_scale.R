library(feedr)
library(magrittr)
context("Scaling by area for maps")

test_that("scale.area() returns correct min/max/length", {
  s <- list(seq(0.1, 1, length.out = 5),
            1:5,
            11:15,
            seq(1000, 5000, 1000),
            2:6,
            c(4,19,500,6000,30000))

  for(i in 1:5){
    for(n in s) {
      mx <- sample(100:10000, size = 1)
      mn <- sample(1:99, size = 1)
      r <- scale.area(n, max = mx, min = mn)
      expect_equal(min(r), mn)
      expect_equal(max(r), mx)
      expect_length(r, 5)
    }
  }
})

test_that("scale.area(radius = TRUE) returns correct min/max/length", {
  s <- list(seq(0.1, 1, length.out = 5),
            1:5,
            11:15,
            seq(1000, 5000, 1000),
            2:6,
            c(4,19,500,6000,30000))

  for(i in 1:5){
    for(n in s) {
      mx <- sample(100:10000, size = 1)
      mn <- sample(1:99, size = 1)
      r <- scale.area(n, radius = TRUE, max = mx, min = mn)
      expect_equal(min(r), mn/2)
      expect_equal(max(r), mx/2)
      expect_length(r, 5)
    }
  }
})

test_that("scale.area() returns expected values", {
  s <- list(seq(0.1, 1, length.out = 5),
            1:5,
            11:15,
            seq(1000, 5000, 1000),
            2:6)

  for(r in s) {
    cat(r, "\n")
    r <- scale.area(r)
    expect_equal(r, c(5, 30, 55, 80, 105))
  }

  expect_equal(round(scale.area(c(4, 8, 19, 500, 6000, 30000)), 6),
               c(5, 5.013335, 5.050007, 6.653554, 24.989332, 105.000000))
})

test_that("scale.area(radius) returns expected values", {
  s <- list(seq(0.1, 1, length.out = 5),
            1:5,
            11:15,
            seq(1000, 5000, 1000),
            2:6)

  for(r in s) {
    cat(r, "\n")
    r <- scale.area(r, radius = TRUE)
    expect_equal(round(r, 5), c(2.50000, 26.33913, 37.16517, 45.48351, 52.50000))
  }

  expect_equal(round(scale.area(c(4,8,19,500,6000,30000)), 6),
              c(5.000000, 5.013335, 5.050007, 6.653554, 24.989332, 105.000000))
})
