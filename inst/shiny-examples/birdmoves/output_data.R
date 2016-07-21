## Data and Text Outputs

## Get only publically available data
birds_dl <- reactive({
  birds()
})

raw_dl <- reactive({
  raw()[raw()$dataaccess == 0, ]
})

v_dl <- reactive({
  v()[v()$dataaccess == 0, ]
})

f_dl <- reactive({
  f()[f()$dataaccess == 0, ]
})

m_dl <- reactive({
  m()[m()$dataaccess == 0, ]
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

msg_select <- c("Please select data first:\n
                - On the 'Select Data' tab, make your selection\n
                - Be sure to click on the 'Get Data' button\n
                - Data is available when the the 'Retrieving Data' message has disappeared")

msg_private <- "None of the currently selected data is available for download.\n
Some of our data is restricted to visualizations only to protect the hard work of scientists until they've had a chance to publish their findings."


## Raw
output$dt_raw <- DT::renderDataTable({
  validate(need(input$data_get > 0, msg_select))
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
  validate(need(input$data_get > 0, msg_select))
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
  validate(need(input$data_get > 0, msg_select))
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
  validate(need(input$data_get > 0, msg_select))
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
  validate(need(input$data_get > 0, msg_select))
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
