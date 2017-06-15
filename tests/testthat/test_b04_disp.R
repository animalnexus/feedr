library(feedr)
library(magrittr)
context("Transformations to displacements")

# disp()
test_that("disp() returns appropriate, non-empty dataframe", {

  ## Errors
  expect_message(d <- visits(finches) %>% disp(.))
  expect_silent(d <- visits(finches) %>% disp(., bw = 300))

  ## Format
  expect_is(d1 <- d[["displacements"]], "data.frame")
  expect_is(d2 <- d[["summaries"]], "data.frame")
  expect_is(d3 <- d[["interactions"]], "data.frame")

  expect_length(d1, 12)
  expect_length(d2, 4)
  expect_length(d3, 3)

  expect_equal(nrow(d1), 24)
  expect_equal(nrow(d2), 5)
  expect_equal(nrow(d3), 20)

  expect_equal(sum(is.na(d1)), 0)
  expect_equal(sum(is.na(d2)), 0)
  expect_equal(sum(is.na(d3)), 0)

  expect_match(names(d1)[1:3], "^animal_id$|^date$|^left$|^arrived$|^logger_id$|^role$")
  expect_is(d1$animal_id, "factor")
  expect_is(d1$date, "Date")
  expect_is(d1$logger_id, "factor")
  expect_is(d1$left, "POSIXct")
})

test_that("disp() has correct spacing", {
  for(b in c(15, 30, 100, 300, 1000)) {
    d <- visits(finches) %>% disp(., bw = b) %>% .$displacements
    expect_true(all(as.numeric(difftime(d$arrived, d$left, units = "secs")) <= b))
  }
})

test_that("disp() returns correct data", {
  d <- visits(finches) %>% disp(., bw = 300)
  d1 <- d[["displacements"]]
  d2 <- d[["summaries"]]
  d3 <- d[["interactions"]]

  ## Data
  expect_equal(unique(d1$animal_id[1]), factor("0620000514", levels = c("041868D396", "041868D861", "062000043E", "06200004F8", "0620000514")))
  expect_equal(d1$logger_id[1], factor(c("2200"), levels = c("2100", "2200", "2400", "2700")))
  expect_equal(d1$left[1], as.POSIXct(c("2016-01-28 12:34:28"), tz = "America/Vancouver"))
  expect_equal(d1$animal_n[1], 5)
  expect_equal(d1$logger_n[1], 4)
  expect_equal(round(d1$lon[2], 4), -120.3612)
  expect_equal(round(d1$lat[2], 5), 50.66778)
  expect_equal(d1$role[1], "displacee")
  expect_equal(d1$species[1], "House Finch")

  expect_equal(d2$p_win[1], 1.00)
  expect_equal(sum(d3$n == 0), 11)
})

test_that("disp() pass", {
  expect_length(visits(finches) %>% disp(., bw = 30, pass = FALSE) %>% .$displacements, 6)
  expect_length(visits(finches) %>% disp(., bw = 30, pass = TRUE) %>% .$displacements, 12)
})
