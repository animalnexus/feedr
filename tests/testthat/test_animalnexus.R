# DB TEST THAT DATA LOADED PROPERLY
# expect_false(test_error(remDr))
# ad <- remDr$findElement(using = "css selector", value = "[id = 'data_info']")
# expect_true(stringr::str_detect(unlist(ad$getElementText()), "Active dataset: Kamloops, BC. Loaded at"))


# appURL <- "http://gaia.tru.ca:8080/animalnexus_exp"
#
# test_that("Connect to animalnexus", {
#   # Check current map loaded: look for 'class leaflet-clickable'
#   remDr$navigate(appURL)
#
#   start <- Sys.time()
#   while(!app_loaded()){
#     Sys.sleep(1)
#     if(Sys.time() > start + 30) break # bail if takes too long
#   }
#
#   expect_lt(as.numeric(difftime(Sys.time(), start, units = "secs")), 30) # Shouldn't take too long to load
#   expect_true(app_loaded())   #Loading message gone
#
#   # Tabs
#   tabs <- remDr$findElements(using = "css selector", value = "[data-toggle = 'tab']")
#   tabs <- data.frame(names = unlist(sapply(tabs, function(x) x$getElementAttribute("data-value"))),
#                      state = unlist(sapply(tabs, function(x) x$isElementDisplayed())))
#
#   expect_true(all(c("Home", "Database", "Import", "Visualizations", "Individuals",
#                     "Transformations", "Settings", "Help") %in% tabs$names))
#   expect_true(all(tabs$state[tabs$names %in% c("Home", "Database", "Import", "Help")]))
#
#   # Active data set message
#   ad <- remDr$findElement(using = "css selector", value = "[id = 'data_info']")
#   expect_equivalent(unlist(ad$getElementText()), "Active dataset: None")
#
# })


# test_that("Database selection", {
#   # Navigate to DB
#
#   #db$clickElement()
# })

# test_that("Transformations", {
#
#
# })
