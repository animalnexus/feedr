context("ui_trans() locally")


# Initial loading: Finches dataset ----------------------------------------
test_that("Initial loading: Finches dataset", {

  remDr <- shiny_test_startup(f_trans, appURL, args = "feedr::finches")

  # All download buttons as expected
  b <- remDr$findElements("css selector", "a[id *= 'data_dl']")
  expect_equivalent(sapply(b, function(x) unlist(x$getElementText())),
                    c("All", "Raw", "Visits", "Movements", "Presence", "Displacements",
                      "Activity", "Daily activity", "Log"))

  bd <- remDr$findElements("css selector", "a[id *= 'data_dl'][disabled]")
  expect_equivalent(sapply(bd, function(x) unlist(x$getElementText())),
                    c("Displacements"))

  # Test table values
  test_tables(remDr, trans = "Raw", data = finches)
  test_tables(remDr, trans = "Visits", data = (v <- visits(finches)))
  test_tables(remDr, trans = "Movements", data = (m <- move(v)))
  test_tables(remDr, trans = "Presence", data = (p <- presence(v)))
  test_tables(remDr, trans = "Displacements", data = try(d <- disp(v), silent = TRUE))
  test_tables(remDr, trans = "Activity", data = (act <- activity(p)))
  test_tables(remDr, trans = "Daily activity", data = daily(act))

  expect_false(test_error(remDr))

  # Check Data access message

  # Take and compare screenshots
  #take_screenshot(remDr, file = paste0(test_dir, "/screenshots/trans_kam_"), ref = TRUE)
  take_screenshot(remDr, file = paste0(test_dir, "/screenshots/trans_kam_"))
  expect_lt(99, compare_screenshot(file = paste0(test_dir, "/screenshots/trans_kam_")))

  shiny_test_cleanup(remDr, f_trans)
})


# Log returns correct values: Finches dataset -----------------------------
test_that("Log returns correct values: Finches dataset", {
  remDr <- shiny_test_startup(f_trans, appURL, args = "feedr::finches")

  nav_tab(remDr, "Settings")
  s <- lapply(1:nrow(man2), function(x) get_settings(remDr, setting = man2$id[x], type = man2$class[x]))
  names(s) <- man2$id

  nav_tab(remDr, "Transformations")
  nav_tab(remDr, "log")

  l <- unlist(remDr$findElement("css", value = "div[id $= 'log']")$getElementText())

  expect_match(l, as.character(Sys.Date())) # Match date
  expect_match(l, paste0("feedr version: ", packageVersion('feedr'))) # feedr version

  sapply(1:nrow(man2), function(x) {
    value <- s[[man2$id[x]]]
    if(is.logical(value)){
      if(value == TRUE) value <- "Yes"
      if(value == FALSE) value <- "No"
    }
    expect_match(l, reg_escape(paste0(man2$lab[x], " = ", value)), info = man2$id[x])
  })

  msgs <- c("No messages",
            "No messages",
            "No messages",
            "There are no displacement events with a bw = 2",
            "041868D396: Skipping. Individual has less than 24hrs of data",
            "041868D861: Skipping. Individual has less than 24hrs of data",
            "062000043E: Skipping. Individual has less than 24hrs of data",
            "06200004F8: Active less than 5% of the total time period...",
            "06200004F8: 100% of obs are shorter than 'res' (15 min). Median obs is 1.75 min.",
            "0620000514: Active less than 5% of the total time period...",
            "0620000514: 88.89% of obs are shorter than 'res' (15 min). Median obs is 4.42 min.")
  lapply(msgs, function(x) expect_match(l, reg_escape(x), info = x))

  shiny_test_cleanup(remDr, f_trans)
})


# Test Activity Settings --------------------------------------------------
test_that("Test Activity Settings", {

  remDr <- shiny_test_startup(f_trans, appURL, args = "feedr::finches")

  # Set settings
  nav_tab(remDr, "Settings")

  s <- default_settings(man2)
  s[['set_activity_sun']] <- c('set_activity_sun' = FALSE)

  change_settings(remDr, "set_activity_sun", s[['set_activity_sun']])
  click_button(remDr, id = "settings_get")

  ui_loaded(remDr) #wait for transformations to happen

  # Run transformations
  t <- get_trans(s)

  # Compare to table values
  expect_false(test_error(remDr))
  nav_tab(remDr, "Transformations")
  expect_false(test_error(remDr))
  test_tables(remDr, trans = "Activity",
              data = t[['activity']])
  test_tables(remDr, trans = "Daily activity",
              data = t[['daily']])

  shiny_test_cleanup(remDr, f_trans)
})


# Test Movement Settings --------------------------------------------------
test_that("Test Movement Settings", {

  remDr <- shiny_test_startup(f_trans, appURL, args = "feedr::finches")

  # Set settings
  nav_tab(remDr, "Settings")

  s <- default_settings(man2)
  s[['set_move_all']] <- c('set_move_all' = TRUE)

  change_settings(remDr, "set_move_all", s[['set_move_all']])
  click_button(remDr, id = "settings_get")

  ui_loaded(remDr) #wait for transformations to happen

  # Run transformations
  t <- get_trans(s)

  # Compare to table values
  expect_false(test_error(remDr))
  nav_tab(remDr, "Transformations")
  expect_false(test_error(remDr))
  test_tables(remDr, trans = "Movements",
              data = t[['movements']])

  shiny_test_cleanup(remDr, f_trans)
})


# Test Random Settings ----------------------------------------------------
test_that("Test Random Settings", {

  remDr <- shiny_test_startup(f_trans, appURL, args = "feedr::finches")

  # Set settings randomly and test
  for(i in 1:3){
    nav_tab(remDr, "Settings")

    s <- random_settings(man2)

    # Set and submit settings (loop until all correct)
    #s <- file_settings("../tests/settings/settings_2017-03-30_2.csv")
    for(x in names(s)) change_settings(remDr, x, s[[x]])
    change_settings(remDr, "set_move_all", s[['set_move_all']])

    click_button(remDr, id = "settings_get")
    ui_loaded(remDr) #wait for transformations to happen

    # Save, just in case
    write.csv(as.data.frame(s), paste0(test_dir, "settings/settings_", Sys.Date(), "_", i, ".csv"), row.names = FALSE)

    # Calculate values
    t <- get_trans(s)

    # Compare to table values
    expect_false(test_error(remDr))
    nav_tab(remDr, "Transformations")
    expect_false(test_error(remDr))

    test_tables(remDr, trans = "Visits",
                data = t[['visits']])
    test_tables(remDr, trans = "Movements",
                data = t[['movements']])
    test_tables(remDr, trans = "Presence",
                data = t[['presence']])
    test_tables(remDr, trans = "Displacements",
                data = t[['displacements']])
    test_tables(remDr, trans = "Activity",
                data = t[['activity']])
    test_tables(remDr, trans = "Daily activity",
                data = t[['daily']])
  }

  file.remove(list.files(paste0(test_dir, "/settings/"), pattern = "settings", full.names = TRUE))
  shiny_test_cleanup(remDr, f_trans)
})


# All data download to csv ------------------------------------------------
test_that("All data download to csv", {
  dl_profile <- makeFirefoxProfile(list(
    "browser.helperApps.neverAsk.saveToDisk"='application/csv,application/zip'
  ))

  remDr <- shiny_test_startup(f_trans, appURL, args = "feedr::finches", extra = dl_profile)

  dl <- remDr$findElement("css", "[id $= 'data_dl']")
  dl$clickElement()

  start <- Sys.time()
  while(!file.exists(file.path("/home/steffi/Downloads/", paste0("feedr_all_", Sys.Date(), ".zip")))) {
    if(difftime(Sys.time(), start, units = "sec") > 15) break
    Sys.sleep(1)
  }
  expect_true(file.exists(file.path("/home/steffi/Downloads/", paste0("feedr_all_", Sys.Date(), ".zip"))))

  utils::unzip(file.path("/home/steffi/Downloads/", paste0("feedr_all_", Sys.Date(), ".zip")),
               exdir = paste0(test_dir, "/downloads/"))

  t <- get_trans(default_settings(man2))

  lapply(1:length(t), function(x) {
    d <- t[[x]]
    trans <- names(t)[x]
    if(trans == "displacements") d <- d$displacements

    if(any(class(d) == "try-error") ||
       length(d) == 0 ||
       nrow(d) == 0 ||
       is.null(d)) d <- data.frame()

    t_file <- paste0(test_dir, "/downloads/", trans, ".csv")
    write.csv(d, t_file, row.names = FALSE)
    expect_equivalent(readLines(t_file),
                      readLines(paste0(test_dir, "/downloads/", trans, "_", Sys.Date(), ".csv")),
                      info = trans)
  })

  file.remove(list.files(paste0(test_dir, "/downloads/"), full.names = TRUE))
  file.remove(list.files("/home/steffi/Downloads/", "feedr_all", full.names = TRUE))

  shiny_test_cleanup(remDr, f_trans)
})


# Settings download properly ----------------------------------------------
test_that("Settings download properly", {
  dl_profile <- makeFirefoxProfile(list(
    "browser.helperApps.neverAsk.saveToDisk"='application/csv,text/csv'
  ))
  remDr <- shiny_test_startup(f_trans, appURL, args = "feedr::finches", extra = dl_profile)

  # Settings
  nav_tab(remDr, "Settings")
  s <- random_settings(man2)
  for(x in names(s)) change_settings(remDr, x, s[[x]])
  change_settings(remDr, "set_move_all", s[['set_move_all']]) #force because never works (!!?!??!)

  # Download
  click_button(remDr, id = "settings_save", type = "a")

  # Wait until complete
  s_file <- paste0(download_dir, "animalnexus_settings_", Sys.Date(), ".csv")
  start <- Sys.time()
  while(!file.exists(s_file)) {
    if(difftime(Sys.time(), start, units = "sec") > 15) break
    Sys.sleep(1)
  }
  expect_true(file.exists(s_file))

  # Read Downloaded settings
  s <- utils::read.csv(s_file)
  s2 <- as.character(s$value)
  names(s2) <- s$id

  # Read current settings
  s_set <- lapply(1:nrow(man2), function(x) get_settings(remDr, setting = man2$id[x], type = man2$class[x]))
  s_set2 <- sapply(s_set, as.character)
  names(s_set2) <- sapply(s_set, names)

  # Expect downloaded == current
  expect_true(all(sort(s2) == sort(s_set2)))

  file.remove(list.files(download_dir, pattern = "animalnexus_settings", full.names = TRUE))
  shiny_test_cleanup(remDr, f_trans)
})


# Settings load properly --------------------------------------------------
test_that("Settings load properly", {
  dl_profile <- makeFirefoxProfile(list(
    "browser.helperApps.neverAsk.saveToDisk"='application/csv,text/csv'
  ))
  remDr <- shiny_test_startup(f_trans, appURL, args = "feedr::finches", extra = dl_profile)

  # Get random Settings
  nav_tab(remDr, "Settings")
  s <- random_settings(man2)
  for(x in names(s)) change_settings(remDr, x, s[[x]])
  change_settings(remDr, "set_move_all", s[['set_move_all']])
  Sys.sleep(0.5)
  s1 <- lapply(1:nrow(man2), function(x) get_settings(remDr, setting = man2$id[x], type = man2$class[x]))

  # Download
  click_button(remDr, id = "settings_save", type = "a")
  # Wait until complete
  s_file <- file.path(download_dir, paste0("animalnexus_settings_", Sys.Date(), ".csv"))
  start <- Sys.time()
  while(!file.exists(s_file)) {
    if(difftime(Sys.time(), start, units = "sec") > 15) break
    Sys.sleep(1)
  }
  expect_true(file.exists(s_file))

  # Change settings
  s <- random_settings(man2)
  for(x in names(s)) change_settings(remDr, x, s[[x]])
  change_settings(remDr, "set_move_all", s[['set_move_all']])

  # Load first settings
  click_button(remDr, id = "import_reveal")
  e <- remDr$findElement("css", "[id $= 'import_settings']")
  e$sendKeysToElement(list(paste0(download_dir, "animalnexus_settings_", Sys.Date(), ".csv")))

  Sys.sleep(0.5)
  s2 <- lapply(1:nrow(man2), function(x) get_settings(remDr, setting = man2$id[x], type = man2$class[x]))
  Sys.sleep(0.5)

  expect_equivalent(s1, s2)

  file.remove(list.files(download_dir, pattern = "animalnexus_settings", full.names = TRUE))
  shiny_test_cleanup(remDr, f_trans)
})
