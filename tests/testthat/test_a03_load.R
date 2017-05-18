library(magrittr)
context("Loading files")


# load_format -------------------------------------------------------------
test_that("load_format() loads and formats data correctly", {
  f <- system.file("extdata", "chickadees.csv", package = "feedr") %>%
    read.csv()

  current_tz <- feedr:::check_tz(Sys.timezone())
  current_tz_nodst <- tz_offset(current_tz, tz_name = TRUE)

  expect_is(r <- load_format(f), "data.frame")
  expect_match(names(r)[1:3], "^animal_id$|^time$|^logger_id$|^date$")
  expect_is(r$animal_id, "factor")
  expect_is(r$logger_id, "factor")
  expect_is(r$time, "POSIXct")
  expect_is(r$date, "Date")

  expect_equal(r$animal_id[1], chickadees$animal_id[1])
  expect_equal(r$logger_id[1], chickadees$logger_id[1])
  expect_equal(r$time[1], as.POSIXct("2016-01-11 10:48:49", tz = current_tz_nodst))
  expect_equal(r$date[1], as.Date("2016-01-11"))

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
    dplyr::rename(Animal_ID = animal_id, TIME = time, feeder_id = logger_id, specIes = species, SEX = sex, longitude = lon, latitude = lat)

  expect_message(load_format(r1), "Renaming column 'Animal_ID' to 'animal_id'")
  expect_message(load_format(r1), "Renaming column 'feeder_id' to 'logger_id'")
  expect_message(load_format(r1), "Renaming column 'longitude' to 'lon'")
  expect_message(load_format(r1), "Renaming column 'latitude' to 'lat'")
  expect_message(load_format(r1), "Renaming column 'TIME' to 'time'")

  expect_named(suppressMessages(load_format(r1)), c("animal_id", "time", "logger_id", "specIes", "SEX", "lon", "lat", "date"))
})



# load_raw ----------------------------------------------------------------
test_that("load_raw loads and formats data correctly - logger id from file name", {
  f <- system.file("extdata", "raw", "exp2", "GR10DATA_2016_01_16.TXT", package = "feedr")
  expect_message(load_raw(r_file = f), "Loading file")
  expect_silent(r <- load_raw(r_file = f, verbose = FALSE))
  expect_is(r, "data.frame")

  expect_match(names(r)[1:3], "^animal_id$|^time$|^logger_id$")
  expect_is(r$animal_id, "character")
  expect_is(r$logger_id, "character")
  expect_is(r$time, "POSIXct")

  expect_equal(r$animal_id[2], "062000038F")
  expect_equal(r$logger_id[2], "GR10")

  # No dst
  expect_equal(load_raw(f)$time[2], as.POSIXct("2016-01-11 10:48:50", tz = tz_offset(check_tz(Sys.timezone()), tz_name = TRUE)))
  expect_equal(load_raw(f, tz = "America/Vancouver")$time[2], as.POSIXct("2016-01-11 10:48:50", tz = "Etc/GMT+8"))
  expect_equal(load_raw(f, tz = "America/Vancouver", tz_disp = "America/Toronto")$time[2], as.POSIXct("2016-01-11 13:48:50", tz = "Etc/GMT+5"))

  # With dst
  expect_equal(load_raw(f, dst = TRUE)$time[2], as.POSIXct("2016-01-11 10:48:50", tz = check_tz(Sys.timezone())))
  expect_equal(load_raw(f, dst = TRUE, tz = "America/Vancouver")$time[2], as.POSIXct("2016-01-11 10:48:50", tz = "America/Vancouver"))
  expect_equal(load_raw(f, dst = TRUE, tz = "America/Vancouver", tz_disp = "America/Toronto")$time[2], as.POSIXct("2016-01-11 13:48:50", tz = "America/Toronto"))
})

test_that("load_raw loads and formats data correctly - logger id from first line", {
  f <- system.file("extdata", "raw", "exp2", "GR10DATA_2016_01_16.TXT", package = "feedr")
  expect_is(r <- load_raw(r_file = f, details = 1, tz = "America/Vancouver"), "data.frame")
  expect_match(names(r)[1:3], "^animal_id$|^time$|^logger_id$")
  expect_is(r$animal_id, "character")
  expect_is(r$logger_id, "character")
  expect_is(r$time, "POSIXct")

  expect_equal(r$animal_id[2], "062000038F")
  expect_equal(r$logger_id[2], "GR10")
  expect_equal(r$time[2], as.POSIXct("2016-01-11 10:48:50", tz = "Etc/GMT+8"))
  expect_equal(load_raw(f, details = 1, tz = "America/Vancouver", tz_disp = "America/Toronto")$time[2], as.POSIXct("2016-01-11 13:48:50", tz = "Etc/GMT+5"))
})

test_that("load_raw handles empty files gracefully", {
  f <- system.file("extdata", "raw", "exp2", "GR10DATA_2016_01_18.TXT", package = "feedr")
  expect_message(r <- load_raw(r_file = f))
  expect_null(r, NULL)
})


# load_raw_all ------------------------------------------------------------
test_that("load_raw_all loads and formats data correctly", {
  d <- system.file("extdata", "raw", package = "feedr")
  expect_message(expect_error(load_raw_all(r_dir = d, extra_pattern = "exp[0-9]{1,2}", extra_name = "Experiment", tz = "America/Vancouver"), NA), "Empty file skipped")
  expect_silent(load <- load_raw_all(r_dir = d, extra_pattern = "exp[0-9]{1,2}", extra_name = "Experiment", tz = "America/Vancouver", verbose = FALSE))

  expect_message(expect_error(load_raw_all(r_dir = d, extra_pattern = "exp[0-9]{1,2}", extra_name = "Experiment", tz = "America/Vancouver", tz_disp = "America/Toronto"), NA), "Empty file skipped")
  expect_silent(load2 <- load_raw_all(r_dir = d, extra_pattern = "exp[0-9]{1,2}", extra_name = "Experiment", tz = "America/Vancouver", tz_disp = "America/Toronto", verbose = FALSE))

  expect_is(load, "data.frame")
  expect_match(names(load)[1:3], "^animal_id$|^time$|^logger_id$")
  expect_is(load$animal_id, "factor")
  expect_is(load$logger_id, "factor")
  expect_is(load$time, "POSIXct")

  expect_equal(load$animal_id[1], factor("06200004BF", levels =  c("0000000000", "011017A536", "011017A605", "03000314F9", "0620000062", "062000014F", "06200001F0", "06200002E7", "0620000380", "062000038D", "062000038F", "0620000392", "06200003A7", "06200003B4", "06200003C3", "06200003F3", "0620000400", "0620000418", "06200004A9", "06200004BB", "06200004BE", "06200004BF", "06200004E4", "0620000525", "07008D9E08", "0700ED9E0E", "0700EDAB15", "0700EDF012", "0700EDF015", "0700EE022B", "0700EE0E42", "0700EE1461", "0700EE1467", "0700EE147F", "0700EE1504", "0700EE19CE", "0700EE2B10", "0700EE2B11")))
  expect_equal(load$logger_id[1], factor("GR10", levels = c("GR10", "GR11", "GR12", "GR13")))
  expect_equal(load$time[1], as.POSIXct("2016-01-11 10:48:49", tz = "Etc/GMT+8"))
  expect_equal(load2$time[1], as.POSIXct("2016-01-11 13:48:49", tz = "Etc/GMT+5"))
})


# dl_data -----------------------------------------------------------------
test_that("dl_data loads and formats data correctly", {
  expect_silent(load <- dl_data(start = "2016-01-01", end = "2016-02-01"))
  expect_silent(load2 <- dl_data(start = "2016-01-01", end = "2016-02-01", tz_disp = "America/Toronto"))
  expect_is(load, "data.frame")
  expect_match(names(load)[1:3], "^animal_id$|^time$|^logger_id$")
  expect_is(load$animal_id, "factor")
  expect_is(load$logger_id, "factor")
  expect_is(load$time, "POSIXct")

  expect_equal(load$animal_id[1], factor("0620000514", levels = c("041868D396", "041868D861", "041868FF93", "062000043E", "06200004F8", "0620000514")))
  expect_equal(load$logger_id[1], factor("2200", levels = c("2100", "2200", "2400", "2700")))
  expect_equal(load$time[1], as.POSIXct("2016-01-28 12:34:25", tz = "Etc/GMT+8"))
  expect_equal(load2$time[1], as.POSIXct("2016-01-28 15:34:25", tz = "Etc/GMT+5"))
})

test_that("dl_data gracefully ends if no data to download", {
  expect_error(dl_data(start = "2010-01-01", end = "2010-02-02"), "There are no online data matching these parameters.")
})

test_that("dl_data filters by species_code and site_id", {
  all <- dl_data(start = "2016-01-28", end = "2016-02-01")
  hofi <- dl_data(start = "2016-01-28", end = "2016-02-01", species = "House Finch")
  moch <- dl_data(start = "2016-01-28", end = "2016-02-01", species = "Mountain Chickadee")

  expect_equal(unique(all$species), c("House Finch", "Mountain Chickadee"))
  expect_equal(unique(hofi$species), c("House Finch"))
  expect_equal(unique(moch$species), c("Mountain Chickadee"))
  expect_equal(nrow(all), 413)
  expect_equal(nrow(hofi), 412)
  expect_equal(nrow(moch), 1)
  expect_true(min(all$time) >= as.POSIXct("2016-01-28"))
  expect_true(max(all$time) <= as.POSIXct("2016-02-01"))

  hofi_sp <- dl_data(start = "2016-01-28", end = "2016-02-01", species = "house finch")
  moch_sp <- dl_data(start = "2016-01-28", end = "2016-02-01", species = "mountain CHICKadee")

  expect_equal(hofi, hofi_sp)
  expect_equal(moch, moch_sp)
})

test_that("dl_data sites and that uses credentials appropriatly", {
  if(!is.null(check_db())) {
    both <- dl_data(start = "2013-05-24", end = "2015-09-04")
    expect_equal(unique(both$site_name), c("Costa Rica", "Kamloops, BC"))
    kl <- dl_data(start = "2013-05-24", end = "2015-09-04", site_id = "kl")
    expect_equal(unique(kl$site_name), "Kamloops, BC")
    cr <- dl_data(start = "2013-05-24", end = "2015-09-04", site_id = "cr")
    expect_equal(unique(cr$site_name), "Costa Rica")
  } else {
    both <- dl_data(start = "2013-05-24", end = "2015-09-04")
    expect_equal(unique(both$site_name), "Kamloops, BC")
    kl <- dl_data(start = "2013-05-24", end = "2015-09-04", site_id = "kl")
    expect_equal(unique(kl$site_name), "Kamloops, BC")
    expect_error(dl_data(start = "2013-05-24", end = "2015-09-04", site_id = "cr"), "There are no online data matching these parameters.")
  }
})


