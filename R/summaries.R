# Summaries
summaries <- function(d, summary = "sum_indiv") {

  if(!(summary %in% c("sum", "sum_indiv", "indiv"))) {
    stop("Summary type not valid, must be one of 'sum', ",
         "'sum_indiv', or 'indiv'", call. = FALSE)
  }

  # Grouping
  if("move_path" %in% names(d)) {
    g <- c("logger_id", "move_path")
  } else {
    g <- "logger_id"
  }
  g <- c(g, "lat", "lon", "animal_n")

  # Individual summaries
  if(summary == "indiv") g <- c("animal_id", g)

  # Summaries
  d <- dplyr::group_by(d, dplyr::across(g))

  if("move_path" %in% g) {
    d <- dplyr::summarize(d, s = length(.data$move_path),
                          .groups = "drop_last") %>%
      dplyr::select("logger_id", dplyr::everything())
    n <- "path_use"
  } else {
    d <- dplyr::summarize(d, s = sum(.data$length), .groups = "drop_last")
    n <- "amount"
  }

  # By totals by individual
  if(summary == "sum_indiv") {
    d <- dplyr::summarize(d, s = .data$s / .data$animal_n, .groups = "drop")
  } else {
    d <- dplyr::summarize(d, s = unique(.data$s), .groups = "drop")
  }

  dplyr::rename(d, !!n := s)
}
