library(RSelenium)
library(testthat)
library(feedr)
library(magrittr)

context("ui_current() locally")

###############
# Start Selenium Server
###############

system("(java -jar ~/R/x86_64-pc-linux-gnu-library/3.3/RSelenium/bin/selenium-server-standalone.jar &)", ignore.stdout = TRUE, ignore.stderr = TRUE)

#########
# Setup
#########

appURL <- "http://127.0.0.1:4100"
f <- "feedr:::ui_current"

test_that("Current map shows correct info", {
  remDr <- shiny_test_startup(f, appURL)

  t <- remDr$findElement(using = 'css selector', value = "[id $= 'current_time']")
  t <- unlist(stringr::str_split(unlist(t$getElementText()), "\n"))

  # Times
  t_dates <- as.POSIXct(stringr::str_extract(t, "[0-9]{4}-[0-9]{2}-[0-9]{2} [0-9]{2}:[0-9]{2}:[0-9]{2}"), tz = "America/Vancouver")
  now <- lubridate::with_tz(Sys.time(), "America/Vancouver")
  expect_lte(as.numeric(difftime(now, t_dates[1], units = "sec")), 60)

  # Text
  expect_true(all(stringr::str_detect(t, "(Most recent update:)|(Most recent activity:)|(Time window:)")))

  # Markers (there should be some)
  m <- remDr$findElements(using = 'css selector', value = "[class ^= 'awesome-marker']")
  expect_gt(length(m), 0)

  shiny_test_cleanup(remDr, f)
})

###########
# Clean Up
###########

# Get server PIDs and terminate
pid_sel <- system("pgrep -f [s]elenium-server-standalone.jar", intern = TRUE)
system(paste0("kill -TERM ", pid_sel))
