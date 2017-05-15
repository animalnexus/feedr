context("Convert functions")

test_that("convert_asnipe converts gmmevents as expected", {

  expect_silent(a <- convert_asnipe(finches))
  expect_is(a, "data.frame")
  expect_equal(nrow(a), 412)
  expect_named(a, c("time", "identity", "location"))
  expect_is(a$time, "numeric")
  expect_equivalent(a[1,], data.frame(time = 0, identity = factor("0620000514"), location = factor("2200_2016-01-28")))
  expect_equivalent(a[nrow(a),], data.frame(time = 97247, identity = factor("041868D396"), location = factor("2100_2016-01-29")))

  expect_silent(a <- convert_asnipe(finches, by_day = FALSE))
  expect_is(a, "data.frame")
  expect_equal(nrow(a), 412)
  expect_named(a, c("time", "identity", "location"))
  expect_is(a$time, "numeric")
  expect_equivalent(a[1,], data.frame(time = 0, identity = factor("0620000514"), location = factor("2200")))
  expect_equivalent(a[nrow(a),], data.frame(time = 97247, identity = factor("041868D396"), location = factor("2100")))

  expect_silent(a <- convert_asnipe(finches, time_scale = "hours"))
  expect_is(a, "data.frame")
  expect_equal(nrow(a), 412)
  expect_named(a, c("time", "identity", "location"))
  expect_is(a$time, "numeric")
  expect_equivalent(a[1,], data.frame(time = 0, identity = factor("0620000514"), location = factor("2200_2016-01-28")))
  expect_equivalent(a[nrow(a),], data.frame(time = 97247/60/60, identity = factor("041868D396"), location = factor("2100_2016-01-29")))
})

test_that("convert_asnipe converts get_associations_points_tw as expected", {

  expect_silent(a <- convert_asnipe(finches, fun = "get_associations_points_tw"))
  expect_is(a, "data.frame")
  expect_named(a, c("Date", "Time", "ID", "Location"))
  expect_is(a$Time, "numeric")
  expect_is(a$Date, "numeric")
  expect_equivalent(a[1,], data.frame(Date = 1, Time = 0, ID = "0620000514", Location = "2200"))
  expect_equivalent(a[nrow(a),], data.frame(Date = 2, Time = 97247, ID = "041868D396", Location = "2100"))

})

test_that("convert_asnipe data runs in asnipe gmmevents function", {
  ## gmmevents
  a <- convert_asnipe(finches)[1:100,]
  expect_output(expect_error(b <- asnipe::gmmevents(time = a$time, identity = a$identity, location = a$location), NA))

  expect_is(b, "list")
  expect_length(b, 3)
  expect_named(b, c("gbi", "metadata", "B"))
  expect_is(b$gbi, "matrix")
  expect_is(b$metadata, "data.frame")
  expect_is(b$B, "matrix")

  a <- convert_asnipe(chickadees)[1:100,]
  expect_output(expect_error(b <- asnipe::gmmevents(time = a$time, identity = a$identity, location = a$location), NA))

  expect_is(b, "list")
  expect_length(b, 3)
  expect_named(b, c("gbi", "metadata", "B"))
  expect_is(b$gbi, "matrix")
  expect_is(b$metadata, "data.frame")
  expect_is(b$B, "matrix")
})

test_that("convert_asnipe data runs in asnipe get_associations_points_tw function", {
  a <- convert_asnipe(finches, fun = "get_associations_points_tw")[1:100,]
  expect_silent(b <- asnipe::get_associations_points_tw(a))

  expect_is(b, "list")
  expect_named(b, NULL)
  expect_length(b, 3)
  expect_is(b[[1]], "matrix")
  expect_is(b[[2]], "numeric")
  expect_is(b[[3]], "numeric")

  a <- convert_asnipe(chickadees, fun = "get_associations_points_tw")[1:100,]
  expect_silent(b <- asnipe::get_associations_points_tw(a))

  expect_is(b, "list")
  expect_named(b, NULL)
  expect_length(b, 3)
  expect_is(b[[1]], "matrix")
  expect_is(b[[2]], "numeric")
  expect_is(b[[3]], "numeric")
})
