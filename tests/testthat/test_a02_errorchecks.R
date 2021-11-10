# check_name --------------------------------------------------------------
test_that("check_name returns error if name missing", {
  d <- data.frame(animal_id = NA, logger_id = NA, start = 2, end = 2)
  expect_error(check_name(d, c("animal_ID", "logGer_id")))
  expect_silent(check_name(d, c("animal_id", "logger_id")))
})

# check_time --------------------------------------------------------------
test_that("check_time returns error if col not time", {
  d <- data.frame(animal_id = NA, logger_id = NA, start = as.POSIXct("2015-01-01 12:34:00"), end = 54)
  expect_error(check_time(d, c("start", "end")))
  expect_silent(check_name(d, c("start")))

  expect_error(check_time(d, c("start", "end")), "Columns 'start', 'end' must be in R's date/time formating \\(POSIXct\\).")
  expect_error(check_time(d, c("start", "end"), internal = FALSE), "Consider using as.POSIXct()")
})

# check_tz --------------------------------------------------------------
test_that("check_tz returns UTC if problems", {
  tz1 <- "America/Vancouver"
  tz2 <- "UTC"
  tz3 <- c("America/Halifax", "UTC")
  tz4 <- NULL
  tz5 <- NA
  tz6 <- ""

  expect_silent(check_tz("America/Vancouver"))
  expect_equal(check_tz("America/Vancouver"), "America/Vancouver")

  expect_silent(check_tz("UTC"))
  expect_equal(check_tz("UTC"), "UTC")

  expect_message(expect_equal(check_tz("AMerica/Vancouver"), "America/Vancouver"), "not in OlsonNames\\(\\), assuming")
  expect_message(expect_equal(check_tz("Amer"), "UTC"), "not in OlsonNames\\(\\), defaulting to UTC")

  expect_message(expect_equal(check_tz(NULL), "UTC"), "Cannot set timezone, defaulting to UTC")
  expect_message(expect_equal(check_tz(NA), "UTC"), "Cannot set timezone, defaulting to UTC")
  expect_message(expect_equal(check_tz(""), "UTC"), "Cannot set timezone, defaulting to UTC")
})

# check_indiv --------------------------------------------------------------
test_that("check_indiv returns error if more than one animal_id", {
  d <- data.frame(animal_id = c("041868D396", "041868D861", "041868FF93", "062000043E", "06200004F8", "0620000514"),
                  logger_id = NA)
  expect_error(check_indiv(d))
  expect_silent(check_indiv(data.frame(animal_id = rep("041868D396", 3), logger_id = 45)))
})

# check_format --------------------------------------------------------------
test_that("check_format returns error if '_' in column values", {
  d <- data.frame(
    animal_id = c("041868D396", "041868D861", "041868FF93",
                  "062000__043E", "0620__0004F8", "0620000514"),
    logger_id = c("456", "5678", "TR_567", "GH_563", "YU848", "56_67"))
  check_format(d) %>%
    expect_message("Using '_' in logger_id") %>%
    expect_message("Using '_' in animal_id")
  expect_silent(check_format(d[1:2,]))

  check_format(d, map = TRUE) %>%
    expect_message("Using '_' in logger_id") %>%
    expect_message("Using '_' in animal_id")

  check_format(d, disp = TRUE) %>%
    expect_message("Using '_' in logger_id") %>%
    expect_message("Using '_' in animal_id")
})


# check_input -------------------------------------------------------------

test_that("check_input renames columns (lat/lon)", {

  # Rename columns
  for(i in c("longitude", "LonGiTude", "long", "Long")){
    r <- dplyr::rename(finches, !!i := "lon")

    expect_message(r2 <- check_input(r), paste0("Renaming column '", i, "' to 'lon'"))
    expect_s3_class(r2, "data.frame")
    expect_equal(finches, r2)
  }

  for(i in c("latitude", "LatItuDE")){
    r <- dplyr::rename(finches, !!i := "lat")

    expect_message(r2 <- check_input(r, input = "lat", options = c("lat", "latitude")),
                   paste0("Renaming column '", i, "' to 'lat'"))
    expect_s3_class(r2, "data.frame")
    expect_equal(finches, r2)
  }
})

test_that("check_input renames columns (regular)", {

  # Rename columns
  r1 <- finches %>%
    dplyr::rename(Animal_ID = animal_id, TIME = time, feeder_id = logger_id)

  expect_message(r2 <- check_input(r1, input = "animal_id",
                                   options = c("animal_id", "bird_id")),
                 "Renaming column 'Animal_ID' to 'animal_id'")
  expect_s3_class(r2, "data.frame")
  expect_named(r2, c("animal_id", "date", "TIME", "feeder_id",
                     "species", "age", "sex", "site_name", "lon", "lat"))

  expect_message(r3 <- check_input(r2, input = "time", options = "time"),
                 "Renaming column 'TIME' to 'time'")
  expect_s3_class(r3, "data.frame")
  expect_named(r3, c("animal_id", "date", "time", "feeder_id", "species",
                     "age", "sex", "site_name", "lon", "lat"))

  expect_message(r4 <- check_input(r3, input = "logger_id",
                                   options = c("feeder_id", "logger_id")),
                 "Renaming column 'feeder_id' to 'logger_id'")
  expect_s3_class(r4, "data.frame")
  expect_named(r4, c("animal_id", "date", "time", "logger_id", "species",
                     "age", "sex", "site_name", "lon", "lat"))

  expect_equal(r4, finches)
})

test_that("check_input omits duplicate columns", {

  for(i in list(c("longitude", "lon"),
                c("LonGiTUDE", "long"),
                c("lon", "long", "longitude"))){

     r <- finches %>%
      dplyr::rename(!!i[1] := "lon") %>%
      dplyr::mutate(!!i[2] := .data[[i[1]]])

    # tibble
    expect_message(r2 <- check_input(r),
                   "Omitting duplicate columns for lon") %>%
      suppressMessages()
    # data frame
    expect_message(as.data.frame(check_input(r)),
                   "Omitting duplicate columns for lon")  %>%
      suppressMessages()
    expect_s3_class(r2, "data.frame")
    expect_equal(finches, r2, ignore_attr = TRUE)
  }

  for(i in list(c("latitude", "lat"),
                c("LaTITUDE", "Lat"),
                c("lat", "LAT", "Lat"))){
    r <- finches %>%
      dplyr::rename(!!i[1] := "lat") %>%
      dplyr::mutate(!!i[2] := .data[[i[1]]])
    expect_message(r2 <- check_input(r, input = "lat",
                                     options = c("lat", "latitude")),
                   "Omitting duplicate columns for lat") %>%
      suppressMessages()
    expect_s3_class(r2, "data.frame")
    expect_equal(finches, r2, ignore_attr = TRUE)
  }
})

test_that("check_input renames AND omits duplicates", {
  r <- finches %>%
    dplyr::rename(Lon = lon) %>%
    dplyr::mutate(Longitude = Lon)

  # tibble
  expect_message(r2 <- check_input(r), "Omitting duplicate columns for lon") %>%
    expect_message("Renaming column 'Lon' to 'lon'")

  # data.frame
  expect_message(as.data.frame(check_input(r)),
                 "Omitting duplicate columns for lon") %>%
    expect_message("Renaming column 'Lon' to 'lon'")


  expect_s3_class(r2, "data.frame")
  expect_equal(finches, r2, ignore_attr = TRUE)

  r <- finches %>%
    dplyr::rename(Lat = lat) %>%
    dplyr::mutate(Latitude = Lat)

  expect_message(r2 <- check_input(r, input = "lat",
                                   options = c("lat", "latitude")),
                 "Omitting duplicate columns for lat") %>%
    expect_message("Renaming column 'Lat' to 'lat'")
  expect_s3_class(r2, "data.frame")
  expect_equal(finches, r2, ignore_attr = TRUE)

})

test_that("check_input returns error if ambiguous", {
  r <- finches %>%
    dplyr::mutate(Lon = lon + 1)
  expect_error(check_input(r),
               "There are multiple lon columns which are not equivalent")

  r <- finches %>%
    dplyr::mutate(Lat = lat + 1)
  expect_error(check_input(r, input = "lat", options = c("lat", "latitude")),
               "There are multiple lat columns which are not equivalent")

  r <- finches %>%
    dplyr::mutate(Lon = lon + 1,
                  long = Lon + 0.2,
                  LonGiTude = long - 0.46,
                  LON = lon -0.49)
  expect_error(check_input(r), "There are too many duplicate lon columns")

  r <- finches %>%
    dplyr::mutate(Lat = lat + 1,
                  laT = Lat + 0.2,
                  LATITUDE = lat - 0.46,
                  LAT = lat -0.49)
  expect_error(check_input(r, input = "lat", options = c("lat", "latitude")),
               "There are too many duplicate lat columns")
})

test_that("check_input returns unchanged data frame if col doesn't exist", {
  expect_silent(r1 <- check_input(finches, input = "site_name",
                                  options = c("site_name", "site_id")))
  expect_equal(r1, finches)
})

