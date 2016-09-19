## Data and Text Outputs

## Get only publically available data
birds_dl <- reactive({
  birds()
})

raw_dl <- reactive({
  raw() %>%
    dplyr::filter(dataaccess == 0) %>%
    dplyr::select(-dataaccess)
})

v_dl <- reactive({
  v() %>%
    dplyr::filter(dataaccess == 0) %>%
    dplyr::select(-dataaccess)
})

f_dl <- reactive({
  f() %>%
    dplyr::filter(dataaccess == 0) %>%
    dplyr::select(-dataaccess)
})

m_dl <- reactive({
  m() %>%
    dplyr::filter(dataaccess == 0) %>%
    dplyr::select(-dataaccess)
  })

#disp <- reactive({})

#dom_dl <- reactive({})

#a_dl <- reactive({})

#da_dl <- reactive({})

all <- reactive({
  list(raw = raw_dl(),
       visits = v_dl(),
       feeding = f_dl(),
       movements = m_dl())
})

## Activate/deactivate buttons depending on whether there is any data to download:
observe({
  req(raw_dl)
  if(is.null(raw_dl()) || nrow(raw_dl()) == 0){
    shinyjs::disable(id = "data_dl_raw")
    shinyjs::disable(id = "data_dl_visits")
    shinyjs::disable(id = "data_dl_feeding")
    shinyjs::disable(id = "data_dl_move")
    shinyjs::disable(id = "data_dl")
  } else {
    shinyjs::enable(id = "data_dl_raw")
    shinyjs::enable(id = "data_dl_visits")
    shinyjs::enable(id = "data_dl_feeding")
    shinyjs::enable(id = "data_dl_move")
    shinyjs::enable(id = "data_dl")
  }
})

msg_select <- c("Please select data through the Database or by Importing")

msg_private <- "None of the currently selected data is available for download.\n
Some of data in our Database is restricted to visualizations only to protect the hard work of scientists until they've had a chance to publish their findings."


## Raw
output$dt_raw <- DT::renderDataTable({
  validate(need(try(nrow(raw()) > 0, silent = TRUE), msg_select))
  req(raw())
  validate(need(sum(raw()$dataaccess==0) > 0, msg_private))

  DT::datatable(raw_dl(),
                filter = "top",
                options = list(pageLength = 100),
                rownames = FALSE,
                colnames = gsub("_", " ", names(raw_dl())) %>% gsub("\\b(\\w)", "\\U\\1", ., perl=TRUE))
})

output$data_dl_raw <- downloadHandler(
  filename = paste0('raw_', Sys.Date(), '.csv'),
  content = function(file) {
    write.csv(raw_dl(), file, row.names = FALSE)
  }
)

## Birds
output$dt_birds <- DT::renderDataTable({
  validate(need(try(nrow(raw()) > 0, silent = TRUE), msg_select))
  validate(need(try(nrow(birds()) > 0, silent = TRUE), "No data on individuals"))
  req(birds())

  DT::datatable(birds_dl(),
                filter = "top",
                options = list(pageLength = 100),
                rownames = FALSE,
                colnames = gsub("_", " ", names(birds_dl())) %>% gsub("\\b(\\w)", "\\U\\1", ., perl=TRUE),
                selection = "single")
}, server = FALSE)


## Visits
output$dt_v <- DT::renderDataTable({
  validate(need(try(nrow(raw()) > 0, silent = TRUE), msg_select))
  validate(need(try(nrow(v()) > 0, silent = TRUE), "No data on visits"))
  req(raw())
  validate(need(sum(raw()$dataaccess==0) > 0, msg_private))

  DT::datatable(v_dl(),
                filter = "top",
                options = list(pageLength = 100),
                rownames = FALSE,
                colnames = gsub("_", " ", names(v_dl())) %>% gsub("\\b(\\w)", "\\U\\1", ., perl=TRUE))
})

output$data_dl_visits <- downloadHandler(
  filename = paste0('visits_', Sys.Date(), '.csv'),
  content = function(file) {
    write.csv(v_dl(), file, row.names = FALSE)
  }
)


## Feeding
output$dt_f <- DT::renderDataTable({
  validate(need(try(nrow(raw()) > 0, silent = TRUE), msg_select))
  validate(need(try(nrow(f()) > 0, silent = TRUE), "No data on feeding bouts"))
  req(raw())
  validate(need(sum(raw()$dataaccess==0) > 0, msg_private))

  DT::datatable(f_dl(),
                filter = "top",
                options = list(pageLength = 100),
                rownames = FALSE,
                colnames = gsub("_", " ", names(f_dl())) %>% gsub("\\b(\\w)", "\\U\\1", ., perl=TRUE))
})


output$data_dl_feeding <- downloadHandler(
  filename = paste0('feeding_', Sys.Date(), '.csv'),
  content = function(file) {
    write.csv(f_dl(), file, row.names = FALSE)
  }
)

## Movements
output$dt_m <- DT::renderDataTable({
  validate(need(try(nrow(raw()) > 0, silent = TRUE), msg_select))
  validate(need(try(nrow(m()) > 0, silent = TRUE), "No data on movements"))
  req(raw())
  validate(need(sum(raw()$dataaccess == 0) > 0, msg_private))

  DT::datatable(m_dl(),
                filter = "top",
                options = list(pageLength = 100),
                rownames = FALSE,
                colnames = gsub("_", " ", names(m_dl())) %>% gsub("\\b(\\w)", "\\U\\1", ., perl=TRUE))
})


output$data_dl_move <- downloadHandler(
  filename = paste0('movements_', Sys.Date(), '.csv'),
  content = function(file) {
    write.csv(m_dl(), file, row.names = FALSE)
  }
)

## Download All

output$data_dl <- downloadHandler(
  filename = paste0("feedr_all_", Sys.Date(), ".zip"),
  content = function(file) {
    tmpdir <- tempdir()
    setwd(tempdir())
    cat(tempdir())

    fs <- paste0(names(all()), "_", Sys.Date(), ".csv")
    for(d in 1:length(all())){
      write.csv(all()[[d]], file = fs[d], row.names = FALSE)
    }
    cat(fs)

    zip(zipfile = file, files = fs)
  },
  contentType = "application/zip"
)

## Data Descriptions

output$data_desc <- renderText({
  req(input$data_tabs)
  if(input$data_tabs == "Raw Data") t <- "<h3>Raw RFID data</h3> <p>Each row corresponds to an RFID 'read' event.</p>"
  if(input$data_tabs == "Visits Data") t <- "<h3>Visits</h3> <p>Each row corresponds to a single 'visit' to the reader. Visits are defined as a series of consecutive RFID reads, with each read occurring within 3s of the next. See the visits() function in the feedr package for R to fine tune these settings.</p><p>Bird N and Feeder N refer to the total number of individuals and readers in the data, respectively.</p>"
  if(input$data_tabs == "Feeding Data") t <- "<h3>Feeding bouts</h3> <p>Each row corresponds to a single 'feeding bout' at the reader if the reader is a feeder, or a period of time spent near the reader otherwise. Feeding bouts are defined as a series of visits at a single feeder separated by no more than 15min. See the feeding() function in the feedr package for R to fine tune these settings.</p><p>Feed start and end reflect the start and end of the feeding bout and feed length refers to the length in minutes of the feeding bout.</p><p>Bird N and Feeder N refer to the total number of individuals and readers in the data, respectively.</p>"
    if(input$data_tabs == "Movement Data") t <- "<h3>Movements</h3> <p>Each two rows correspond to a single 'movement' from one reader to another. A movement is defined as the last and first consecutive visits made by an individual to two different readers.</p><p>Move Id refers to the unique identifier of each movement made by an individual. Move Path reflects the unique path between readers (without accounting for direction) whereas Move Dir reflect s the unique path between readers, including direction. Strength is a measure of how connected two readers are and is calculated as the inverse of time taken to move between the readers.</p>"
    return(t)
})
