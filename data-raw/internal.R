## Must have package built (Build and Reload), not just in memory

## feedr Manual Entries for ui_settings and ui_trans

library(magrittr)

types <- list("Logical" = c("logical", "logic", "boolean", "bool"),
              "Data frame" = c("data frame", "data.frame", "dataframe"),
              "Numeric" = c("numerical", "numeric", "number"),
              "Character" = c("text", "character", "string"),
              "Deprecated" = c("deprecated", "depreciated"))
types <- lapply(types, FUN = function(x) paste0("(^", paste0(x, collapse = ")|(^"), ")"))

get_argument <- function(x) {
  x <- unlist(x[[1]][1])
  if(x != "\n") return(x)
}
get_description <- function(x) {
  if(x[1] != "\n"){
    x <- x[-1]
    x <- paste0(unlist(x), collapse = "")
    x <- stringr::str_replace(x, "\n", " ")
    return(x)
  }
}
get_class <- function(x) {
  dots <- sapply(types, function(x) return(formula(paste0("~stringr::str_detect(tolower(desc), '", x, "')"))))
  x <- dplyr::mutate_(x, .dots = setNames(dots, names(types)))
  return(x)
}
omit_class <- function(x) {
  t <- paste0(types, collapse = "|")
  x <- stringr::str_replace(x, stringr::regex(t, ignore_case = TRUE), "")
  x <- stringr::str_replace(x, "^[^a-zA-Z0-9()]+", "")
  x <- stringr::str_replace(x, "^vector(s)*", "")
  x <- stringr::str_replace(x, "^[^a-zA-Z0-9()]+", "")
  return(x)
}

get_defaults <- function(x) {
  e <- environment(feedr::visits)
  f <- formals(get(x = x$f, envir = e))
  tibble::tibble(arg = names(f),
                 value = as.character(f)) %>%
    dplyr::filter(value != "")
}

get_manual_single <- function(func){
  tags <- tools:::RdTags(func)
  if(any(tags == "\\arguments")) {
    args <- func[which(tags == "\\arguments")][[1]]

    d <- tibble::tibble(arg = unlist(sapply(args, get_argument)),
                        desc = unlist(sapply(args, get_description))) %>%
      get_class(.) %>%
      tidyr::gather(key = class, value = value, -desc, -arg) %>%
      dplyr::filter(value) %>%
      dplyr::select(-value) %>%
      dplyr::mutate(desc = omit_class(desc),
                    f = as.character(func[which(tags == "\\name")][[1]][[1]]),
                    title = as.character(func[which(tags == "\\title")][[1]][[1]]),
                    desc = replace(desc, class == "Deprecated", "")) %>%
      dplyr::select(f, title, arg, desc, class)

    col_split <- stringr::str_count(d$arg, ",")
    while(any(col_split > 0)) {
      d <- tidyr::separate(d, arg, into = c("arg1", "arg2"), sep = ",", fill = "right", extra = "merge") %>%
        tidyr::gather(key = "type", value = "arg", arg1, arg2) %>%
        dplyr::filter(!is.na(arg)) %>%
        dplyr::select(f, title, arg, desc, class)
      col_split <- stringr::str_count(d$arg, ",")
    }
    if(nrow(d) > 0) {
      d <- dplyr::left_join(d, get_defaults(d), by = "arg") %>%
        dplyr::mutate(id = paste0("set_", f, "_", arg))
      #message(d$f[1])
      return(d)
    }
  }
}

get_manual <- function(x = "feedr"){
  db <- tools::Rd_db(x)
  do.call('rbind', lapply(db, get_manual_single))
}

labs <- data.frame(
  rbind(cbind(f = "activity",
              arg = c("by_logger", "sun", "keep_all", "res"),
              lab = c("Calculate activity by logger", "Calculate sunrise/sunset times",
                      "Keep all individuals", "Time resolution in minutes")),
        cbind(f = "disp", arg = "bw", lab = "Max seconds between two events"),
        cbind(f = "dom",
              arg = c("tries", "omit_cutoff"),
              lab = c("Number of iterations", "Min number of interactions per individual")),
        cbind(f = "move", arg = "all", lab = "Keep individuals which didn't move?"),
        cbind(f = "presence", arg = "bw", lab = "Max minutes between visits"),
        cbind(f = "visits",
              arg = c("allow_imp", "na_rm", "bw", "bw_imp"),
              lab = c("Allow impossible visits?", "Remove data with missing values", "Min seconds between visits", "Min seconds to travel between loggers"))), stringsAsFactors = FALSE)

man <- get_manual() %>%
  dplyr::left_join(labs, by = c("f", "arg"))

############################################################################
url <- "http://gaia.tru.ca/birdMOVES/rscripts/anquery.csv"
url_count <- "http://gaia.tru.ca/birdMOVES/rscripts/anInit.csv"
url_loggers <- "http://gaia.tru.ca/birdMOVES/rscripts/anloggers.csv"

# Get species list
species_list <- RCurl::getForm(url_count, key = feedr:::check_db()) %>%
  utils::read.csv(text = ., strip.white = TRUE, colClasses = "character") %>%
  dplyr::select(species = engl_name) %>%
  unique() %>%
  unlist(., use.names = FALSE)

if(nrow(man) > 0) {
  devtools::use_data(man, url, url_count, url_loggers, species_list, internal = TRUE, overwrite = TRUE)
} else stop("Manual entries didn't compile")
