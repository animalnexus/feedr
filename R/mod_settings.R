# Launch settings

ui_settings <- function(){
  ui_app(name = "settings")
}


getHTMLhelp <- function(x){
  capture.output(
    tools:::Rd2txt(utils:::.getHelpFile(utils::help(x)))
  )
}

get_manual <- function(x){
  ?tools::Rd2ex
  db <- tools::Rd_db('feedr')

 # arguments <- lapply(db, function(x) {
  x <- db[[27]]
    tags <- tools:::RdTags(db[[27]])
#    if("\\arguments" %in% tags){
      # return a crazy list of results
      #out <- x[which(tmp=="\\author")]
      # return something a little cleaner

      out <- x[which(tags=="\\arguments")][[1]]

      o <- lapply(out, function(t) stringr::str_replace(paste0(sapply(t, as.character), collapse = ""), "\\n", " "))
  #  }
  #  else
  #    out <- NULL
  #  invisible(out)
  #})



  x <- utils::help("visits", package = "feedr")
  utils:::.getHelpFile(x)
  x.arg <- lapply(x, FUN = function(x) attr(x, which = "srcref"))

  x.type <- sapply(x.arg, function(x) {
    x <- stringr::str_extract(x, "^\\\\[a-zA-Z]+\\{")[1]
    x <- stringr::str_extract(x, "[a-zA-Z]+")
  })


  y <- paste0(capture.output(x), collapse = " ")

  y.arg <- stringr::str_extract(y, "\\\\arguments\\{.+\\}")

  ## Omit cods and links
  y.arg2 <- stringr::str_replace_all(y.arg, c("\\\\arguments\\{" = "",
                                              "\\\\code\\{(.+?)\\}" = "\\1",
                                              "\\\\item " = " ",
                                              "\\\\itemize\\{" = " "))


}


## Settings
#' @import shiny
#' @import magrittr
mod_UI_settings <- function(id) {
  ns <- NS(id)

  tagList(
    h4("Vists"),
    numericInput(ns("visits_bw"), "bw", min = 0, value = 3, width = "50pt"),
    radioButtons(ns("visits_allow_imp"), "Allow impossible visits", choices = c("Yes" = TRUE, "No" = FALSE), selected = FALSE, inline = TRUE),


    h4("Presence"),

    h4("Movements"),

    h4("Displacements"),

    h4("Dominance"),

    h4("Activity"),

    h4("Daily Activity")
  )
}


#' @import shiny
#' @import magrittr
mod_settings <- function(input, output, session, db) {

  # Setup -------------------------------------------------------------------
  ns <- session$ns

}
