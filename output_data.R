## Data and Text Outputs

output$dt_data <- DT::renderDataTable(
  DT::datatable(data(), 
                filter = "top",
                options = list(pageLength = 100),
                rownames = FALSE,
                colnames = gsub("_", " ", names(data())) %>% gsub("\\b(\\w)", "\\U\\1", ., perl=TRUE)
                )
)

output$dt_birds <- DT::renderDataTable(
  DT::datatable(birds_sub(), 
                filter = "top",
                options = list(pageLength = 100), 
                rownames = FALSE, 
                colnames = gsub("_", " ", names(birds_sub())) %>% gsub("\\b(\\w)", "\\U\\1", ., perl=TRUE),
                selection = "single"
  ), server = FALSE
)

output$dt_v <- DT::renderDataTable(
  DT::datatable(v(), 
                filter = "top",
                options = list(pageLength = 100),
                rownames = FALSE,
                colnames = gsub("_", " ", names(v())) %>% gsub("\\b(\\w)", "\\U\\1", ., perl=TRUE)
  )
)

output$dt_points <- DT::renderDataTable(
  DT::datatable(v_points(), 
                filter = "top",
                options = list(pageLength = 100),
                rownames = FALSE),
                colnames = gsub("_", " ", names(v_points())) %>% gsub("\\b(\\w)", "\\U\\1", ., perl=TRUE)
)

output$dt_paths <- DT::renderDataTable(
  DT::datatable(m_paths(), 
                filter = "top",
                options = list(pageLength = 100),
                rownames = FALSE),
                colnames = gsub("_", " ", names(m_paths())) %>% gsub("\\b(\\w)", "\\U\\1", ., perl=TRUE)
)

#output$selection <- renderText({input$birds})
