library(RSelenium)
library(magrittr)
library(testthat)
library(feedr)


# Setup -------------------------------------------------------------------
test_dir <- "/home/steffi/Projects/feedr Project/tests/"
download_dir <- "/home/steffi/Downloads/"
appURL <- "http://127.0.0.1:4100"
siteURL <- "http://gaia.tru.ca:8080/animalnexus_exp"

f_import <- "feedr::ui_import"
f_db <- "feedr::ui_db"
f_current <- "feedr:::ui_current"
f_trans <- "feedr::ui_trans"
f_animate <- "feedr::ui_animate"

# Man file for ui_trans()
man2 <- dplyr::filter(feedr:::man, !is.na(lab))

# Reference data set for ui_db()
suppressWarnings({
  write.csv(dl_data(start = "2017-01-01", end = "2017-03-02", site_id = "kl", species = "Mountain Chickadee"),
            "~/Projects/feedr Project/tests/ref_db_kamloops.csv", row.names = FALSE)
  write.csv(dl_data(start = "2013-04-01", end = "2013-05-02",
                    tz_disp = "America/Costa_Rica", site_id = "cr", species = "Green Hermit"),
            "~/Projects/feedr Project/tests/ref_db_costa_rica.csv", row.names = FALSE)
})

# Files for ui_import()
d_logger <- c(system.file("extdata", "raw", "exp2", "GR10DATA_2016_01_16.TXT", package = "feedr"),
              system.file("extdata", "raw", "exp2", "GR11DATA_2016_01_16.TXT", package = "feedr"))

d_preformat <- c(system.file("extdata", "chickadees.csv", package = "feedr"),
                 system.file("extdata", "finches.csv", package = "feedr"))

d_preformat_errors <- c(system.file("extdata", "import_tests", "finches_colnames.csv", package = "feedr"),
                        system.file("extdata", "import_tests", "finches_cols.csv", package = "feedr"),
                        system.file("extdata", "import_tests", "finches_colnames2.csv", package = "feedr"))

d_preformat_dmy <- c(system.file("extdata", "import_tests", "finches_dmy1.csv", package = "feedr"),
                     system.file("extdata", "import_tests", "finches_dmy2.csv", package = "feedr"),
                     system.file("extdata", "import_tests", "finches_dmy3.csv", package = "feedr"))

d_preformat_mdy <- c(system.file("extdata", "import_tests", "finches_mdy1.csv", package = "feedr"),
                     system.file("extdata", "import_tests", "finches_mdy2.csv", package = "feedr"),
                     system.file("extdata", "import_tests", "finches_mdy3.csv", package = "feedr"))

d_preformat_sep <- c(system.file("extdata", "import_tests", "finches_tab.csv", package = "feedr"),
                     system.file("extdata", "import_tests", "finches_semicolon.csv", package = "feedr"))

d_preformat_skip <- c(system.file("extdata", "import_tests", "finches_skip.csv", package = "feedr"))


d_logger_ymd <- c(system.file("extdata", "import_tests", "logger_ymd1.TXT", package = "feedr"))

d_logger_dmy <- c(system.file("extdata", "import_tests", "logger_dmy1.TXT", package = "feedr"),
                  system.file("extdata", "import_tests", "logger_dmy2.TXT", package = "feedr"))

d_logger_mdy <- c(system.file("extdata", "import_tests", "logger_mdy1.TXT", package = "feedr"),
                  system.file("extdata", "import_tests", "logger_mdy2.TXT", package = "feedr"))

d_logger_inline <- c(system.file("extdata", "import_tests", "logger_inline.TXT", package = "feedr"))

d_logger_index <- c(system.file("shiny-examples", "app_files", "logger_example1.txt", package = "feedr"),
                    system.file("shiny-examples", "app_files", "logger_index_example.csv", package = "feedr"))

# finches %>%
#   dplyr::rename(Animal_ID = animal_id, TIME = time, feeder_id = logger_id, LON = lon, LATITUDE = lat) %>%
#   write.csv("./inst/extdata/import_tests/finches_colnames2.csv", row.names = FALSE)

