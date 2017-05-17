start_shiny <- function(f, args = NULL){
  if(is.null(args)) args <- ""
  stop_shiny(f)
  suppressWarnings(system(paste0("(Rscript -e \"options(shiny.port = 4100); d <- ", f, "(", args, "); write.csv(d, '/home/steffi/Projects/feedr Project/tests/downloads/output.csv', row.names = FALSE)\" &)"), ignore.stderr = TRUE))#, ignore.stdout = TRUE, ignore.stderr = TRUE)
}

stop_shiny <- function(f){
  suppressWarnings(pid_shiny <- system(paste0("pgrep -f ", stringr::str_replace(f, "^([a-z])", "[\\1]")), intern = TRUE, ignore.stderr = TRUE))
  if(length(pid_shiny) > 0) system(paste0("kill -TERM ", pid_shiny), ignore.stderr = TRUE)
}

shiny_test_startup <- function(f = NULL, appURL, args = NULL,
                               browserName = "firefox", extra = NULL, type = "local") {
  #skip_on_cran()
  skip_on_travis()
  skip_on_appveyor()

  # Start Selenium Server
  system("(java -jar ~/R/x86_64-pc-linux-gnu-library/3.3/RSelenium/bin/selenium-server-standalone.jar &)",
         ignore.stdout = TRUE, ignore.stderr = TRUE)

  Sys.sleep(2)

  if(type == "local"){
    start_shiny(f, args)
  }

  if(!is.null(extra)) {
    remDr <- remoteDriver(browserName = browserName, extraCapabilities = extra)
  } else remDr <- remoteDriver(browserName = browserName)

  remDr$open(silent = TRUE)
  remDr$setImplicitWaitTimeout(milliseconds = 1000) #Wait 1s for elements to load
  remDr$navigate(appURL)
  page_loaded(remDr) #wait until page started to load
  ui_loaded(remDr) #wait until loaded
  expect_false(test_error(remDr)) #make sure no errors off the bat
  return(remDr)
}

shiny_test_cleanup <- function(remDr, f = NULL, type = "local"){
  if(type == "local") stop_shiny(f)
  remDr$closeWindow()
  remDr$close()

  # Get server PIDs and terminate Selenium server
  suppressWarnings({
    pid_sel <- system("pgrep -f [s]elenium-server-standalone.jar", intern = TRUE)
    system(paste0("kill -TERM ", pid_sel))
  })
}

app_loaded <- function(remDr) {
  msg <- remDr$findElement(using = "css selector", value = "[id = 'loading_app']")
  !unlist(msg$isElementDisplayed())
}

page_loaded <- function(remDr) {
 ready <- FALSE
 start <- Sys.time()

 while(!ready){
   message("Wait for page...")
   if(as.numeric(difftime(Sys.time(), start, units = "sec")) > 30) {
     message("breaking")
     break
   }
   s <- remDr$findElements("css", value = "[class ^= 'shiny']")
   ready <- length(s) > 0
   if(!ready) Sys.sleep(1)
 }
 return(ready)
}

ui_loaded <- function(remDr) {
  ready <- FALSE
  start <- Sys.time()
  while(!ready) {
    message("Wait..")
    msg <- remDr$findElements(using = "css selector",
                              value = "[id = 'shiny-notification-panel']")
    ready <- length(msg) == 0
    if(as.numeric(difftime(Sys.time(), start, units = "sec")) > 30) {
      message("breaking")
      break
    }
    if(!ready) Sys.sleep(1)
  }
  return(ready)
}

data_loaded <- function(remDr) {
  ready <- FALSE
  start <- Sys.time()
  while(!ready) {
    message("Wait..")
    msg <- remDr$findElement(using = "css selector",
                              value = "[class = 'progress-bar']")
    msg <- unlist(msg$getElementText())
    ready <- msg == "Upload complete"
    if(as.numeric(difftime(Sys.time(), start, units = "sec")) > 30) {
      message("Too long, breaking")
      break
    }
    if(!ready) Sys.sleep(0.25)
  }
  return(ready)
}

slider_move <- function(remDr, id, dist = 1) {
  e <- remDr$findElement("css", paste0("label[for $= '", id, "']+[class ^= 'irs'] [class ^= 'irs-slider']"))

  w_current <- remDr$findElement("css",
                                 paste0("label[for $= '", id, "']+[class ^= 'irs'] [class = 'irs-bar']"))$getElementSize()$width

  w <- remDr$findElement("css",
                         paste0("label[for $= '", id, "']+[class ^= 'irs']"))$getElementSize()$width


  # Reset
  remDr$mouseMoveToLocation(webElement = e)
  remDr$buttondown()
  remDr$mouseMoveToLocation(x = -w_current, y = -1L)
  remDr$buttonup()
  Sys.sleep(0.25)

  # Set
  remDr$mouseMoveToLocation(webElement = e)
  remDr$buttondown()
  remDr$mouseMoveToLocation(x = w * dist, y = -1L)
  remDr$buttonup()
  Sys.sleep(0.25)
}

click_setting <- function(remDr, css, id = "time-instant") {
  remDr$findElement("css", css)$clickElement()
  ui_loaded(remDr)
  expect_false(test_error(remDr))
  slider_move(remDr, id)
}

nav_tab <- function(remDr, tab){
  remDr$findElement(using = "css",
                    value = paste0("[data-value = '", tab, "']"))$clickElement()
  Sys.sleep(0.5)
}

change_settings <- function(remDr, setting, value){
  e <- remDr$findElement(using = "css selector", value = paste0("[id $= '", setting, "']"))

  if(is.logical(value)){
    for(i in 1:2){
      c <- e$findChildElement(using = "css selector",
                              value = paste0("[name $= '", setting, "'][type = 'radio'][value = '", value,"']"))
      # Click twice, as sometimes doesn't work the first time
      c$clickElement()
    }
  } else if(is.numeric(value)) {
    c <- e$findChildElement(using = "css selector", value = "input")
    c$clearElement()
    c$sendKeysToElement(list(as.character(value)))
  }
  Sys.sleep(0.25)
}

get_settings <- function(remDr, setting, type) {
  e <- remDr$findElement(using = "css", value = paste0("[id $= '", setting, "']"))

  if(type == "Logical"){
    c <- e$findChildElement(using = "css",
                            value = paste0("[type = 'radio']:checked"))
    value <- as.logical(c$getElementAttribute('value'))
  } else if(type == "Numeric") {
    c <- e$findChildElement(using = "css", value = "input")
    value <- as.numeric(c$getElementAttribute('value'))
  }
  Sys.sleep(0.25)
  names(value) <- setting
  return(value)
}

random_settings <- function(m){
  # Get random settings
  s <- lapply(1:nrow(m), function(x) {
    if(m$class[x] == "Logical") y <- sample(c(TRUE, FALSE), 1)
    if(m$class[x] == "Numeric") y <- sample(1:20, 1)
    names(y) <- m$id[x]
    return(y)
  })
  names(s) <- m$id
  return(s)
}

reg_escape <- function(string) {
  stringr::str_replace_all(string, "(\\W)", "\\\\\\1")
}

click_button <- function(remDr, id, type = "button") {
  remDr$findElement(using = "css",
                    value = paste0(type, "[id $= '", id, "']"))$clickElement()
  Sys.sleep(0.5)
}

default_settings <- function(m) {
  s <- lapply(1:nrow(man2), function(x) {
    y <- man2$value[x]
    if(y %in% c("TRUE", "FALSE")) y <- as.logical(y) else y <- as.numeric(y)
    names(y) <- man2$id[x]
    return(y)
    })
  names(s) <- man2$id
  return(s)
}

file_settings <- function(file) {
  s_file <- utils::read.csv(file)
  s <- lapply(1:length(s_file), function(x) {
    y <- as.vector(s_file[, x])
    names(y) <- names(s_file)[x]
    return(y)
  })
  names(s) <- names(s_file)
  return(s)
}

get_trans <- function(s) {
  # Calculate values
  v <- try(visits(finches,
                  allow_imp = s[['set_visits_allow_imp']],
                  na_rm = s[['set_visits_na_rm']],
                  bw = s[['set_visits_bw']],
                  bw_imp = s[['set_visits_bw_imp']]), silent = TRUE)

  p <- try(presence(v, bw = s[['set_presence_bw']]), silent = TRUE)
  m <- try(move(v, all = s[['set_move_all']]), silent = TRUE)
  disp <- try(disp(v, bw = s[['set_disp_bw']]), silent = TRUE)
  dom <- try(dom(disp,
                 tries = s[['set_dom_tries']],
                 omit_cutoff = s[['set_dom_omit_cutoff']]), silent = TRUE)
  a <- try(activity(p,
                    res = s[['set_activity_res']],
                    by_logger = s[['set_activity_by_logger']],
                    sun = s[['set_activity_sun']],
                    keep_all = s[['set_activity_keep_all']]), silent = TRUE)
  da <- try(daily(a), silent = TRUE)
  return(list("visits" = v, "presence" = p, "movements" = m, "displacements" = disp, "dominance" = dom, "activity" = a, "daily" = da))
}


test_tables <- function(remDr, trans = "Raw", data, n = 20){
  # Navigate to Tab
  nav_tab(remDr, trans)

  Sys.sleep(0.5)

  expect_false(test_error(remDr))

  if(trans == "Displacements") data <- data$displacements
  if(trans == "Dominance") {
    if(class(data) != "try-error" && length(data$matrices) > 0) {
      data <- data[['matrices']][[1]]
      data <- as.data.frame(data, optional = TRUE)
      data <- cbind(row.names(data), data)
    } else data <- NULL
  }

  # First make sure data is present:
  e <- remDr$findElements("css", "[class ~= 'shiny-output-error-validation']")
  e <- unlist(sapply(e, function(x) x$getElementText()))
  if(length(e) > 0 & any(e == "No data (see log for more details)")) {
    expect_true(any(class(data) == "try-error") ||
                  length(data) == 0 ||
                  nrow(data) == 0 ||
                  is.null(data))
  } else {
    # Format data to match DT
    if(any(class(data) == "data.frame")){
      data <- dplyr::mutate_all(data, as.character)
    }

    # Get max values
    m <- remDr$findElement("css selector", value = paste0("div[data-value = '", trans, "'] * [class = 'dataTables_info']"))
    m <- as.numeric(stringr::str_replace(unlist(m$getElementText()),
                                         "Showing [0-9,]* to ([0-9,]*) of [0-9,]* entries",
                                         "\\1"))

    if(n > m) n <- m
    sapply(sample(1:m, n), function(x) {
      row_e <- remDr$findElement("css selector",
                               paste0("div[data-value = '", trans, "'] * tbody > tr:nth-child(", x, ")"))
      row_e <- row_e$getElementText()
      row_d <- data[x, as.vector(!is.na(data[x, ]))]
      expect_row_equal(row_e, row_d, info = paste0("Comparing ", trans))
    })
  }
}

test_preview <- function(preview, output) {
  preview <- unlist(stringr::str_split(preview, "\n"))
  for(x in 1:length(preview)) expect_row_equal(preview[x], output[x,], info = "test preview against output")
}

expect_row_equal <- function(row_e, row_d, info = "") {
  expect_equal(paste0(sapply(row_d, as.character), collapse = " "),
               unlist(row_e), info = info)
}

msg_table <- function(t){
 paste0(capture.output(t), collapse = "\n")
}

select_files <- function(remDr, files){
  e <- remDr$findElement("css", "[id $= 'file1']")
  #for(f in files) e$sendKeysToElement(list(f))
  e$sendKeysToElement(list(paste0(files, collapse = "\n")))
  data_loaded(remDr)
  Sys.sleep(0.5)
  expect_false(test_error(remDr))
}

download_files <- function(remDr, files, preview = NULL, type = "preformat", time_format = "ymd HMS") {
  # Import
  click_button(remDr, "get_data")
  Sys.sleep(0.5)
  expect_false(test_error(remDr))

  # Compare to expected
  if(type == "preformat") {
    suppressWarnings(i1 <- load_format(dplyr::bind_rows(lapply(files, function(x) load_format(utils::read.csv(x), time_format = time_format, verbose = FALSE))), verbose = FALSE))
    if(!is.null(preview)) ip <- load_format(utils::read.csv(files[1]), time_format = time_format, verbose = FALSE)
  } else if (type == "logger") {
    suppressWarnings(i1 <- load_format(dplyr::bind_rows(lapply(files, load_raw, logger_pattern = NA, time_format = time_format)), verbose = FALSE))
    if(!is.null(preview)) ip <- load_format(load_raw(files[1], logger_pattern = NA, time_format = time_format), verbose = FALSE)
  }
  i2 <- load_format(utils::read.csv(paste0(test_dir, "/downloads/output.csv")), verbose = FALSE)
  expect_equivalent(i1, i2, info = paste0("Comparing: ", paste0(files, collapse = "\n")))

  # Compare to preview
  if(!is.null(preview)) test_preview(preview, ip)
}

test_db_site <- function(remDr, site = "Kamloops, BC") {
  # Select Site
  remDr$findElement("css selector", "[class = 'selectize-control single']")$clickElement()
  Sys.sleep(0.25)
  remDr$findElement("css selector", paste0("[data-value = '", site, "']"))$clickElement()
  expect_false(test_error(remDr))
}

test_db_dates <- function(remDr, dates = NULL){
    # Select Dates
    e <- remDr$findElements(using = 'css', value = "[data-initial-date]")
    sapply(e, function(x) x$clearElement())
    e[[1]]$clickElement()
    e[[1]]$sendKeysToElement(list(dates[1]))
    e[[1]]$sendKeysToElement(list("", key = "escape"))
    e[[2]]$clickElement()
    e[[2]]$sendKeysToElement(list(dates[2]))
    e[[2]]$sendKeysToElement(list("", key = "escape"))
    expect_false(test_error(remDr))
}

test_db_species <- function(remDr, species = NULL){
  # Select Species
  sp <- remDr$findElements(using = 'css selector', value = "[name $='data_species']")
  for(s in sp){
    if(is.null(species)) {
      if(!unlist(s$isElementSelected())) s$clickElement()
    } else {
      if(unlist(s$getElementAttribute("value")) %in% species) {
        if(!unlist(s$isElementSelected())) s$clickElement()
      } else {
        if(unlist(s$isElementSelected())) s$clickElement()
      }
    }
    Sys.sleep(0.5)
    expect_false(test_error(remDr))
  }
}

test_db_n <- function(remDr, species, n){
  t <- remDr$findElements(using = 'css selector', value = "[id $= 'data_selection'] * td")
  t <- unlist(sapply(t, function(x) x$getElementText()))
  t <- data.frame(species = t[seq(1, length(t), 2)], n = t[seq(2, length(t), 2)])
  expect_equal(nrow(t), length(species), info = "No. species match")
  expect_true(setequal(t$species, species), info = paste0(species, " present"))
  expect_true(setequal(t$n, n), info = paste0(paste0(species, "; n = ", n), collapse = "\n"))
}

test_error <- function(remDr){
  e <- remDr$findElements(using = "css selector", value = "[class ~= 'shiny-output-error']")

  if(length(e) > 0) {
    cls <- sapply(e, function(x) x$getElementAttribute("class"))
    e <- e[sapply(cls, function(x) !stringr::str_detect(x, "shiny-output-error-validation"))]
    if(length(e) > 0) {
      return(unlist(sapply(e, function(x) x$getElementText())))
    } else return(FALSE)
  } else return(FALSE)
}

test_present <- function(remDr, selector){
  length(remDr$findElements(using = 'css selector', value = selector)) > 0
}

test_msg <- function(remDr){
  e <- remDr$findElements("css", "[class ~= 'shiny-output-error-validation']")
  return(unlist(lapply(e, function(x) x$getElementText())))
}

take_screenshot <- function(remDr, file, ref = FALSE){
  # Take screenshot
  if(!ref) file <- paste0(file, Sys.Date(), ".png") else file <- paste0(file, "ref.png")
  remDr$maxWindowSize(); Sys.sleep(1)
  remDr$screenshot(file = file)
}

compare_screenshot <- function(file){
  current <- png::readPNG(paste0(file, Sys.Date(), ".png"))
  ref <- png::readPNG(paste0(file, "ref.png"))
  if(length(current) == length(ref)) {
    diff <- 100 * sum(current == ref) / length(ref)
  }
  return(diff)
}

test_time_formats <- function(f, file, format = "ymd") {
  test_that(paste0("Date/Time format - ", format, " (", file, ")"), {
    remDr <- shiny_test_startup(f, appURL, browserName = "chrome")

    # Select file
    select_files(remDr, file)

    if(stringr::str_detect(file, "logger")) {
      type <- "logger"
      remDr$findElement("css", "[type = 'radio'][value = 'logger']")$clickElement()
      Sys.sleep(0.5)
    } else type <- "preformat"

    fs <- c("ymd", "mdy", "dmy")
    fs <- fs[order(fs == format)]
    for(h in fs){
      remDr$findElement("css", "[data-value $= 'HMS']")$clickElement()
      Sys.sleep(0.5)
      remDr$findElement("css", paste0("[data-value = '", h, " HMS']"))$clickElement()
      Sys.sleep(1)

      if(format == h) {
        expect_null(test_msg(remDr))
      } else {
        expect_match(test_msg(remDr), "Cannot proceed: NA times detected, check your time format")
      }
    }

    # Test output
    download_files(remDr, file, type = type, time_format = paste0(format, " HMS"))

    # Clean up
    shiny_test_cleanup(remDr, f)
  })
}

rand_opts <- function(){
  opts <- list('sum' = c("cumulative", "instant"),
               'id' = c("all", "041868D396", "041868D861", "062000043E", "06200004F8", "0620000514"),
               'sum_type' = c("sum", "sum_indiv"),
               #'time_range' = c(0, 0.5),
               'res' = c(5, 15, 30, 60, 180, 360, 1440), #720
               'anim_speed' = seq(0, 1, 0.2),
               'sun' = c(TRUE, FALSE))

  ran_opts <- lapply(opts, sample, 1)

  if(ran_opts$id != "all") ran_opts$sum_type <- "sum"
  if(ran_opts$res == 1440) ran_opts$sun <- FALSE
  return(ran_opts)
}
