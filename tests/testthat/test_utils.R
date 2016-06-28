library(feedr)
context("Timezone offset")

# tz_offset()
test_that("tz_offset returns correct offset number", {
  expect_is(tz_offset("America/Vancouver"), "numeric")
  expect_equal(tz_offset("America/Vancouver"), -8)
  expect_equal(tz_offset("America/Vancouver", dst = TRUE), -7)

  expect_equal(tz_offset("America/Toronto"), -5)
  expect_equal(tz_offset("America/Toronto", dst = TRUE), -4)

  expect_equal(tz_offset("Europe/London"), 0)
  expect_equal(tz_offset("Europe/London", dst = TRUE), 1)

  expect_equal(tz_offset("Europe/Paris"), 1)
  expect_equal(tz_offset("Europe/Paris", dst = TRUE), 2)
})

test_that("tz_offset returns correct offset tzone", {
  expect_is(tz_offset("America/Vancouver", tz_name = TRUE), "character")
  expect_equal(tz_offset("America/Vancouver", tz_name = TRUE), "Etc/GMT+8")
  expect_equal(tz_offset("America/Vancouver", dst = TRUE, tz_name = TRUE), "Etc/GMT+7")

  expect_equal(tz_offset("America/Toronto", tz_name = TRUE), "Etc/GMT+5")
  expect_equal(tz_offset("America/Toronto", dst = TRUE, tz_name = TRUE), "Etc/GMT+4")

  expect_equal(tz_offset("Europe/London", tz_name = TRUE), "Etc/GMT+0")
  expect_equal(tz_offset("Europe/London", dst = TRUE, tz_name = TRUE), "Etc/GMT-1")

  expect_equal(tz_offset("Europe/Paris", tz_name = TRUE), "Etc/GMT-1")
  expect_equal(tz_offset("Europe/Paris", dst = TRUE, tz_name = TRUE), "Etc/GMT-2")


})
