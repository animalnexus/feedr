# load_format -------------------------------------------------------------
test_that("load_format() loads and formats data correctly", {
  f <- system.file("extdata", "chickadees.csv", package = "feedr") %>%
    read.csv()

  current_tz <- feedr:::check_tz(Sys.timezone())
  current_tz_nodst <- tz_offset(current_tz, tz_name = TRUE)

  expect_s3_class(r <- load_format(f), "data.frame")
  expect_match(names(r)[1:4], "^animal_id$|^time$|^logger_id$|^date$")
  expect_s3_class(r$animal_id, "factor")
  expect_s3_class(r$logger_id, "factor")
  expect_s3_class(r$time, "POSIXct")
  expect_s3_class(r$date, "Date")

  expect_equal(r$animal_id[1], chickadees$animal_id[1])
  expect_equal(r$logger_id[1], chickadees$logger_id[1])
  expect_equal(r$time[1], as.POSIXct("2016-01-11 10:48:49", tz = current_tz_nodst))
  expect_equal(r$date[1], lubridate::as_date("2016-01-11"))
  expect_equal(substr(r$time, 1, 10), as.character(r$date))

  # Current tz
  expect_equal(load_format(f)$time[1], as.POSIXct("2016-01-11 10:48:49", tz = current_tz_nodst))
  expect_equal(load_format(f, dst = TRUE)$time[1], as.POSIXct("2016-01-11 10:48:49", tz = current_tz))

  # No dst
  expect_equal(load_format(f, tz = "America/Vancouver")$time[1],
               as.POSIXct("2016-01-11 10:48:49", tz = "Etc/GMT+8"))
  expect_equal(load_format(f, tz = "America/Vancouver", tz_disp = "America/Toronto")$time[1],
               as.POSIXct("2016-01-11 13:48:49", tz = "Etc/GMT+5"))

  # With dst
  expect_equal(load_format(f, tz = "America/Vancouver", dst = TRUE)$time[1],
               as.POSIXct("2016-01-11 10:48:49", tz = "America/Vancouver"))
  expect_equal(load_format(f, tz = "America/Vancouver", tz_disp = "America/Toronto", dst = TRUE)$time[1],
               as.POSIXct("2016-01-11 13:48:49", tz = "America/Toronto"))
})

test_that("load_format fixes names", {

  r1 <- finches %>%
    dplyr::rename(Animal_ID = animal_id, TIME = time, feeder_id = logger_id,
                  specIes = species, SEX = sex, longitude = lon, latitude = lat)

  expect_message(load_format(r1), "Renaming column 'Animal_ID' to 'animal_id'") %>%
    expect_message("Renaming column 'feeder_id' to 'logger_id'") %>%
    expect_message("Renaming column 'longitude' to 'lon'") %>%
    expect_message("Renaming column 'latitude' to 'lat'") %>%
    expect_message("Renaming column 'TIME' to 'time'")

  expect_named(suppressMessages(load_format(r1)),
               c("animal_id", "date", "time", "logger_id", "specIes",
                 "age", "SEX", "site_name","lon", "lat"))
})

test_that("load_format doesn't change columns already time", {

  r1 <- finches[1:5,] %>%
    dplyr::mutate(time = lubridate::ymd_hms(c("2001-01-01 00:00:00.111",
                                              "2001-01-01 00:00:00.211",
                                              "2001-01-01 00:00:00.331",
                                              "2001-01-01 00:00:00.783",
                                              "2001-01-01 00:00:00.923")))

  expect_true(r1$time[1] < r1$time[2])
  expect_false(r1$time[1] == r1$time[2])

  r2 <- load_format(r1)

  expect_true(r2$time[1] < r2$time[2])
  expect_false(r2$time[1] == r2$time[2])

})


# load_raw ----------------------------------------------------------------
test_that("load_raw loads and formats data correctly - logger id from file name", {
  f <- system.file("extdata", "raw", "exp2", "GR10DATA_2016_01_16.TXT", package = "feedr")
  expect_message(load_raw(r_file = f), "Loading file")
  expect_silent(r <- load_raw(r_file = f, verbose = FALSE))
  expect_s3_class(r, "data.frame")

  expect_match(names(r)[1:4], "^animal_id$|^date$|^time$|^logger_id$")
  expect_type(r$animal_id, "character")
  expect_type(r$logger_id, "character")
  expect_s3_class(r$time, "POSIXct")
  expect_s3_class(r$date, "Date")

  expect_equal(r$animal_id[2], "062000038F")
  expect_equal(r$logger_id[2], "GR10DATA")

  expect_equal(substr(r$time, 1, 10), as.character(r$date))

  # No dst
  expect_equal(load_raw(f)$time[2],
               as.POSIXct("2016-01-11 10:48:50",
                          tz = tz_offset(check_tz(Sys.timezone()), tz_name = TRUE))) %>%
    expect_message()
  expect_equal(load_raw(f, tz = "America/Vancouver")$time[2],
               as.POSIXct("2016-01-11 10:48:50", tz = "Etc/GMT+8")) %>%
    expect_message()
  expect_equal(load_raw(f, tz = "America/Vancouver",
                        tz_disp = "America/Toronto")$time[2],
               as.POSIXct("2016-01-11 13:48:50", tz = "Etc/GMT+5")) %>%
    expect_message()

  # With dst
  expect_equal(load_raw(f, dst = TRUE)$time[2], as.POSIXct("2016-01-11 10:48:50", tz = check_tz(Sys.timezone()))) %>%
    expect_message()
  expect_equal(load_raw(f, dst = TRUE, tz = "America/Vancouver")$time[2], as.POSIXct("2016-01-11 10:48:50", tz = "America/Vancouver")) %>%
    expect_message()
  expect_equal(load_raw(f, dst = TRUE, tz = "America/Vancouver", tz_disp = "America/Toronto")$time[2], as.POSIXct("2016-01-11 13:48:50", tz = "America/Toronto")) %>%
    expect_message()
})

test_that("load_raw loads and formats data correctly - logger id from first line", {
  f <- system.file("extdata", "raw", "exp2", "GR10DATA_2016_01_16.TXT", package = "feedr")
  expect_s3_class(r <- load_raw(r_file = f, details = 1, tz = "America/Vancouver"), "data.frame") %>%
    expect_message()
  expect_s3_class(r2 <- load_raw(r_file = f, details = 1, tz = "America/Vancouver", logger_pattern = "[GPR]{2,3}[0-9]{1,2}"), "data.frame") %>%
    expect_message()
  expect_match(names(r)[1:4], "^animal_id$|^date$|^time$|^logger_id$")
  expect_type(r$animal_id, "character")
  expect_type(r$logger_id, "character")
  expect_s3_class(r$time, "POSIXct")
  expect_s3_class(r$date, "Date")

  expect_equal(substr(r$time, 1, 10), as.character(r$date))

  expect_equal(r$animal_id[2], "062000038F")
  expect_true(all(r$logger_id == "GR10DATA"))
  expect_true(all(r2$logger_id == "GR10"))
  expect_equal(r$time[2], as.POSIXct("2016-01-11 10:48:50", tz = "Etc/GMT+8"))
  expect_equal(load_raw(f, details = 1, tz = "America/Vancouver", tz_disp = "America/Toronto")$time[2], as.POSIXct("2016-01-11 13:48:50", tz = "Etc/GMT+5"))  %>%
    expect_message()
})

test_that("load_raw handles empty files gracefully", {
  f <- system.file("extdata", "raw", "exp2", "GR10DATA_2016_01_18.TXT", package = "feedr")
  expect_message(r <- load_raw(r_file = f)) %>%
    expect_message()
  expect_null(r, NULL)
})

test_that("load_raw stops/warns if can't get logger_id", {

  # Not in title as should be
  f <- system.file("extdata", "import_tests", "logger_inline.TXT", package = "feedr")
  expect_message(expect_error(r <- load_raw(r_file = f, skip = 2, details = 0,
                                            logger_pattern = "[GPR]{2,3}[0-9]{1,2}"),
                              "logger_id not detected in file name"), "Loading file")
  expect_error(r <- load_raw(r_file = f, skip = 2, details = 0, verbose = FALSE,
                             logger_pattern = "[GPR]{2,3}[0-9]{1,2}"),
               "logger_id not detected in file name") %>%
    expect_silent()

  # Not in body as should be
  f <- system.file("extdata", "import_tests", "logger_inname_GR10DATA.TXT", package = "feedr")
  expect_warning(r <- load_raw(r_file = f, details = 1),
                 "logger_id extracted from first line of the file as ") %>%
    expect_warning("All formats failed to parse") %>%
    expect_message("Loading file") %>%
    expect_silent()

  expect_equal(r$logger_id[1], "06200004BF 15/01/2016 10:48:49",
               ignore_attr = TRUE)
  expect_warning(r <- load_raw(r_file = f, details = 1, verbose = FALSE),
                 "logger_id extracted from first line of the file as ") %>%
    expect_warning("All formats failed to parse") %>%
    expect_silent()

  # No Lat/Lon in body as should be
  f <- system.file("extdata", "raw", "exp2", "GR10DATA_2016_01_16.TXT", package = "feedr")
  expect_error(r <- load_raw(r_file = f, details = 2),
               "Expecting one pair of lat/lon on second line of the file") %>%
  expect_message("Loading file")

  expect_error(r <- load_raw(r_file = f, details = 2, verbose = FALSE),
               "Expecting one pair of lat/lon on second line of the file") %>%
    expect_silent()
})


# load_raw_all ------------------------------------------------------------
test_that("load_raw_all loads and formats data correctly", {
  d <- system.file("extdata", "raw", package = "feedr")
  expect_message(load_raw_all(r_dir = d, extra_pattern = "exp[0-9]{1,2}",
                              extra_name = "Experiment",
                              tz = "America/Vancouver"),
                 "Empty file skipped") %>%
    suppressMessages()

  expect_silent(load <- load_raw_all(r_dir = d, extra_pattern = "exp[0-9]{1,2}",
                                     extra_name = "Experiment",
                                     tz = "America/Vancouver", verbose = FALSE))

  expect_message(load_raw_all(r_dir = d, extra_pattern = "exp[0-9]{1,2}",
                              extra_name = "Experiment",
                              tz = "America/Vancouver",
                              tz_disp = "America/Toronto"),
                 "Empty file skipped") %>%
    suppressMessages()

  expect_silent(load2 <- load_raw_all(r_dir = d,
                                      extra_pattern = "exp[0-9]{1,2}",
                                      extra_name = "Experiment",
                                      tz = "America/Vancouver",
                                      tz_disp = "America/Toronto",
                                      verbose = FALSE))

  expect_s3_class(load, "data.frame")
  expect_match(names(load)[1:3], "^animal_id$|^date$|^time$|^logger_id$")
  expect_s3_class(load$animal_id, "factor")
  expect_s3_class(load$logger_id, "factor")
  expect_s3_class(load$time, "POSIXct")
  expect_s3_class(load$date, "Date")

  expect_equal(substr(load$time, 1, 10), as.character(load$date))

  expect_equal(load$animal_id[1], factor("06200004BF", levels =  c("0000000000", "011017A536", "011017A605", "03000314F9", "0620000062", "062000014F", "06200001F0", "06200002E7", "0620000380", "062000038D", "062000038F", "0620000392", "06200003A7", "06200003B4", "06200003C3", "06200003F3", "0620000400", "0620000418", "06200004A9", "06200004BB", "06200004BE", "06200004BF", "06200004E4", "0620000525", "07008D9E08", "0700ED9E0E", "0700EDAB15", "0700EDF012", "0700EDF015", "0700EE022B", "0700EE0E42", "0700EE1461", "0700EE1467", "0700EE147F", "0700EE1504", "0700EE19CE", "0700EE2B10", "0700EE2B11")))
  expect_equal(load$logger_id[1], factor("GR10DATA", levels = c("GR10DATA", "GR11DATA", "GR12DATA", "GR13DATA")))
  expect_equal(load$time[1], as.POSIXct("2016-01-11 10:48:49", tz = "Etc/GMT+8"))
  expect_equal(load2$time[1], as.POSIXct("2016-01-11 13:48:49", tz = "Etc/GMT+5"))
})

