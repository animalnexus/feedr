library(magrittr)
context("Scaling by area for maps")

test_that("scale_area() returns correct min/max/length", {
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
      r <- scale_area(n, max = mx, min = mn)
      expect_equal(min(r), mn)
      expect_equal(max(r), mx)
      expect_length(r, 5)
    }
  }
})

test_that("scale_area(radius = TRUE) returns correct min/max/length", {
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
      r <- scale_area(n, radius = TRUE, max = mx, min = mn)
      expect_equal(min(r), mn)
      expect_equal(max(r), mx)
      expect_length(r, 5)
    }
  }
})

test_that("scale_area() returns expected values", {
  s <- list(seq(0.1, 1, length.out = 5),
            1:5,
            11:15,
            seq(1000, 5000, 1000),
            2:6)

  for(r in s) {
    #cat(r, "\n")
    r <- scale_area(r, min = 5)
    expect_equal(r, c(5, 30, 55, 80, 105))
  }

  expect_equal(round(scale_area(c(4, 8, 19, 500, 6000, 30000), min = 5), 6),
               c(5, 5.013335, 5.050007, 6.653554, 24.989332, 105.000000))
})

test_that("scale_area(radius) returns expected values", {
  s <- list(seq(0.1, 1, length.out = 5),
            1:5,
            11:15,
            seq(1000, 5000, 1000),
            2:6)

  for(r in s) {
    #cat(r, "\n")
    r <- scale_area(r, radius = TRUE, min = 5)
    expect_equal(round(r, 5), c(5.00000, 52.67827, 74.33034, 90.96703, 105.00000))
  }

  expect_equal(round(scale_area(c(4,8,19,500,6000,30000), radius = TRUE, min = 5), 6),
              c(5.000000, 5.144595, 5.522747, 14.383703, 47.157465, 105.000000))
})
