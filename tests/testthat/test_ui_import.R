run <- FALSE

if(run) {
context("basic")

library(RSelenium)
library(testthat)

user <- "rselenium0"
pass <- "***************************"
port <- 80
ip <- paste0(user, ':', pass, "@ondemand.saucelabs.com")
browser <- "firefox"
version <- "26"
platform <- "Windows 8.1"
extraCapabilities <- list(name = "shinytestapp screenshot", username = user, accessKey = pass)

remDr <- remoteDriver$new(remoteServerAddr = ip, port = port, browserName = browser
                          , version = version, platform = platform
                          , extraCapabilities = extraCapabilities)


###########################
remDr <- remoteDriver(browser = "firefox")
remDr$open(silent = TRUE)

appURL <- "http://127.0.0.1:4142"
remDr$navigate(appURL)

webElems <- remDr$findElements("css selector", "#ctrlSelect input")
lapply(webElems, function(x){x$clickElement()})
scr <- remDr$screenshot(display = TRUE)

sysDetails <- remDr$getStatus()
browser <- remDr$sessionInfo$browserName

test_that("can connect to app", {  
  remDr$navigate(appURL)
  appTitle <- remDr$getTitle()[[1]]
  expect_equal(appTitle, "")  
})

test_that("controls are present", {  
  ids <- remDr$findElements("css selector", "[id^=standalone]")
  e <- sapply(ids, function(x){x$getElementAttribute("id")})
  
  expect_equal(appCtrlLabels[[1]], "Select controls required:")  
  expect_equal(appCtrlLabels[[2]], "selectInput")  
  expect_equal(appCtrlLabels[[3]], "numericInput")  
  expect_equal(appCtrlLabels[[4]], "dateRangeInput")  
  expect_equal(appCtrlLabels[[5]], "sliderInput")  
})

remDr$navigate(appURL)
e <- remDr$findElement("id", "standalone-format")
initState <- e$isElementSelected()[[1]]

# check if we can select/deselect
if(browser == "internet explorer"){
  e$sendKeysToElement(list(key = "space"))
}else{
  e$clickElement()
}
changeState <- e$isElementSelected()[[1]]
expect_is(initState, "logical")  
expect_is(changeState, "logical")  
expect_false(initState == changeState)  


test <- remDr$findElements("css selector", "[data-value]")

e <- remDr$findElement("css selector", "[id=standalone-tz]")
val <- e$getElementAttribute("value")

e$isElementSelected()

ceState <- sapply(ce, function(x){x$isElementSelected()})
newState <- sample(seq_along(ceState)[!unlist(ceState)], 1)

outElem <- remDr$findElement("css selector", "#summary")
initOutput <- outElem$getElementText()[[1]]

# change dataset 
childElems[[newState]]$clickElement()
outElem <- remDr$findElement("css selector", "#summary")  
changeOutput <- outElem$getElementText()[[1]]

expect_false(initOutput == changeOutput)


remDr$close()

}