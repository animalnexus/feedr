test_that("In/Out runs silently", {
  expect_silent(inout(finches_lg, dir_in = "2100_2700", type = "out"))
  expect_silent(inout(finches_lg, dir_in = "2100_2700", type = "in"))
})

out_dir <- inout(finches_lg, dir_in = "2100_2700", type = "out")
in_dir <- inout(finches_lg, dir_in = "2100_2700", type = "in")


test_that("Data looks as expected", {
  expect_s3_class(out_dir, "data.frame")
  expect_s3_class(out_dir$animal_id, "factor")
  expect_type(out_dir$trip_id, "double")
  expect_type(out_dir$inout_dir, "character")
  expect_s3_class(out_dir$exit, "POSIXct")
  expect_s3_class(out_dir$enter, "POSIXct")
  expect_type(out_dir$trip_length, "double")
  expect_type(out_dir$max_time_away, "double")

  expect_s3_class(in_dir, "data.frame")
  expect_s3_class(in_dir$animal_id, "factor")
  expect_type(in_dir$trip_id, "double")
  expect_type(in_dir$inout_dir, "character")
  expect_s3_class(in_dir$exit, "POSIXct")
  expect_s3_class(in_dir$enter, "POSIXct")
  expect_type(in_dir$trip_length, "double")
  expect_type(in_dir$max_time_away, "double")
})

test_that("Trips have valid values", {

  # No enters == exits
  expect_true(all(out_dir$exit < out_dir$enter))
  expect_true(all(in_dir$enter < in_dir$exit))

  # Positive trip time and max time away
  expect_true(all(out_dir$trip_length > 0))
  expect_true(all(in_dir$trip_length > 0))

  # Max time away < trip time
  expect_true(all(out_dir$trip_length >= out_dir$max_time_away))
  expect_true(all(in_dir$trip_length >= in_dir$max_time_away))
})
