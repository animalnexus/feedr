# Summaries
summaries <- function(d, summary = "sum_indiv") {

  if(!(summary %in% c("sum", "sum_indiv", "indiv"))) stop("Summary type not valid, must be one of 'sum', 'sum_indiv', or 'indiv'", call. = FALSE)
  # Grouping
  g <- c("logger_id", "lat", "lon")
  if("move_path" %in% names(d)) g <- c("move_path", g)

  # Individual summaries
  if(summary == "indiv") g <- c("animal_id", g)

  # Summaries
  if("move_path" %in% g) {
    s <- "length(move_path)"
    n <- "path_use"
  }
  if(!("move_path" %in% g)) {
    s <- "sum(length)"
    n <- "amount"
  }

  # By totals by individual
  if(summary == "sum_indiv") s <- paste0(s, " / animal_n[1]")

  d_sum <- d %>%
    dplyr::group_by_(.dots = g) %>%
    dplyr::summarize_(.dots = stats::setNames(s, n))

 return(d_sum)
}
