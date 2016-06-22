library(feedr)
library(magrittr)
context("Transformations to visits")

# visits()
test_that("visits() returns appropriate, non-empty dataframe", {
  expect_silent(v <- visits(finches))
  expect_is(v, "data.frame")
  expect_equal(sum(is.na(v)), 0)
  expect_match(names(v)[1:3], "^bird_id$|^start$|^end$|^feeder_id$")
  expect_is(v$bird_id, "factor")
  expect_is(v$feeder_id, "factor")
  expect_is(v$start, "POSIXct")
  expect_is(v$end, "POSIXct")
  expect_true(all(as.numeric(v$end - v$start) >= 0))
})

test_that("visits() has correct spacing", {
  for(b in c(3, 5, 30)) {
    v <- visits(finches[finches$bird_id == "041868D396",], bw = b)
    expect_true(all(as.numeric(difftime(v$start[2:nrow(v)], v$end[1:(nrow(v)-1)], units = "secs")) > b))
  }
})

test_that("visits() returns correct data", {
  v <- visits(finches)
  expect_equal(v$bird_id[2], factor("041868D396", levels = c("041868D396", "041868D861", "041868FF93", "062000043E", "06200004F8", "0620000514")))
  expect_equal(v$feeder_id[2], factor("2100", levels = c("2100", "2200", "2400", "2700")))
  expect_equal(v$start[2], as.POSIXct("2016-01-29 11:21:23", tz = "America/Vancouver"))
  expect_equal(v$end[2], as.POSIXct("2016-01-29 11:21:26", tz = "America/Vancouver"))
  expect_equal(v$bird_n[2], 6)
  expect_equal(v$feeder_n[2], 4)
  expect_equal(round(v$lon[2], 4), -120.3624)
  expect_equal(round(v$lat[2], 5), 50.66896)
  expect_equal(v$species[2], "House Finch")
  expect_equal(nrow(v[v$end != v$start,]), 49) ## 49 visits > 0 seconds
})

test_that("visits() allow.imp", {
  f <- rbind(finches, finches[1:5,] %>% dplyr::mutate(feeder_id = "2400"))
  expect_error(visits(f), "Impossible")
  expect_silent(visits(f, allow.imp = TRUE))
})

test_that("visits() na.rm", {
  f <- finches
  f[4, 'time'] <- NA
  expect_error(visits(f), "NAs")
  expect_silent(v2 <- visits(f, na.rm = TRUE))
  expect_equal(nrow(v2) + 1, nrow(visits(finches)))
})

test_that("visits() pass", {
  expect_length(visits(finches, pass = FALSE), 6)
  expect_length(visits(finches), 9)
})
