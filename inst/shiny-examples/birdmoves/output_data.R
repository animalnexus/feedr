## Data and Text Outputs

## Get only publically available data
birds_dl <- reactive({
  birds()#[birds()$site_name %in% unique(feeders_all$site_name[feeders_all$dataaccess == 0]),]
})

raw_dl <- reactive({
  raw()[raw()$site_name %in% unique(feeders_all$site_name[feeders_all$dataaccess == 0]),]
})

v_dl <- reactive({
  v()[v()$site_name %in% unique(feeders_all$site_name[feeders_all$dataaccess == 0]),]
})

f_dl <- reactive({
  f()[f()$site_name %in% unique(feeders_all$site_name[feeders_all$dataaccess == 0]),]
})

m_dl <- reactive({
  m()[m()$site_name %in% unique(feeders_all$site_name[feeders_all$dataaccess == 0]),]
  })

#disp <- reactive({})

#dom_dl <- reactive({})

#a_dl <- reactive({})

#da_dl <- reactive({})

## Raw
output$dt_raw <- DT::renderDataTable({
  validate(need(input$data_get > 0, "Please select data first"))
  req(raw())
  validate(need(sum(raw()$dataaccess==0) > 0, "None of the selected data is available for download."))

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
  validate(need(input$data_get > 0, "Please select data first"))
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
  validate(need(input$data_get > 0, "Please select data first"))
  req(raw())
  validate(need(sum(raw()$dataaccess==0) > 0, "None of the selected data is available for download."))

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
  validate(need(input$data_get > 0, "Please select data first"))
  req(raw())
  validate(need(sum(raw()$dataaccess==0) > 0, "None of the selected data is available for download."))

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
  validate(need(input$data_get > 0, "Please select data first"))
  req(raw())
  validate(need(sum(raw()$dataaccess == 0) > 0, "None of the selected data is available for download."))

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
