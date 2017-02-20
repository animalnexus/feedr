# ui_indiv(animals = unique(finches[, c("animal_id", "species")]))

ui_indiv <- function(r){
  force(r)
  ui_app(name = "indiv", r = reactive({r}))
}

## Get current data
#' @import shiny
#' @import magrittr
mod_UI_indiv <- function(id) {
  ns <- NS(id)

  tagList(column(9,
                 fluidRow(DT::dataTableOutput(ns("dt_animals")))),
          column(3,
                 div(style = "position: fixed",
                 h4("Click on a row for photos"),
                 htmlOutput(ns("img_animals"))))
  )

}

#' @import shiny
#' @import magrittr
mod_indiv <- function(input, output, session, r) {

  ns <- session$ns

  ## Animals of current data
  animals <- reactive({
    req(r())

    a <- try(keep_extra(r(), n = c("date", "time", "lat", "lon", "logger_id", "dataaccess"), only = "animal_id")[['animal_id']])
    if(class(a) == "try-error") {
      cols <- names(r())[names(r()) %in% c("animal_id", "species", "age", "sex", "tagged_on", "site_name")]
      a <- r() %>%
        dplyr::select_(.dots = cols) %>%
        unique(.)
    } else if(is.null(a)) a <- tibble::tibble(animal_id = unique(r()$animal_id))
    return(a)
  })

  ## Look at animals
  output$img_animals <- renderText({
    req(animals())
    # Don't actually know what STRH stands for, assuming Sapphire-throated Humminganimal
    #paste0("<div class = \"animal-img\">",
    get_image(animals(), input$dt_animals_rows_selected, "300px")#,
    #       "</div>")
  })

  output$dt_animals <- DT::renderDataTable({
    validate(need(try(nrow(animals()) > 0, silent = TRUE), "No data on individuals: Either no data selected or no individuals"))
    req(animals())

    DT::datatable(animals(),
                  filter = "top",
                  options = list(pageLength = 100),
                  rownames = FALSE,
                  colnames = gsub("_", " ", names(animals())) %>% gsub("\\b(\\w)", "\\U\\1", ., perl=TRUE),
                  selection = "single")
  }, server = FALSE)

}
