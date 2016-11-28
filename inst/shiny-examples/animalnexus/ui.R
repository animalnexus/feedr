library(feedr, lib.loc = "/usr/local/lib/R_exp/site-library/")
library(shiny)
library(shinyjs)
library(shinyBS)

shinyUI(

  tagList(
    shinyjs::useShinyjs(),
    includeCSS("../../extra/style.css"),
    tags$head(tags$script("


        window.onload = function() {
            $('#main a:contains(\"Database\")').parent().addClass('hidden');
            $('#main a:contains(\"Import\")').parent().addClass('hidden');
            $('#main a:contains(\"Visualizations\")').parent().addClass('hidden');
            $('#main a:contains(\"Individuals\")').parent().addClass('hidden');
            $('#main a:contains(\"Transformations\")').parent().addClass('hidden');
            $('#main a:contains(\"Help\")').parent().addClass('hidden');
        };

        Shiny.addCustomMessageHandler('activeNavs', function(nav_label) {
            $('#main a:contains(\"' + nav_label + '\")').parent().removeClass('hidden');
        });

   ")),
    navbarPage(title = a(href = "http://animalnexus.ca", HTML("animal<strong>nexus</strong>")),
               id = "main",
               position = "fixed-top",
               collapsible = TRUE,
               windowTitle = "animalnexus",
               footer = column(12,
                               hr(),
                               div(class = "data-status", textOutput("data_info")),
                               div(class = "package-version", htmlOutput("package_version"))),

             #################
             ## Watch Now
             #################
             tabPanel("Home",
                      fluidRow(
                        div(style = "font-size: 200%; padding: 10px; max-width: 350px; margin: auto; text-align:center; border-style: solid; border-radius: 5px; box-shadow: 10px 10px 5px #888888; color: red;", "This is EXPERIMENTAL"),
                        div(style = "text-align:center", HTML("<h1>Welcome to animal<strong>nexus</strong></h1>")),
                        h4(style = "text-align:center", "This webapp is based on R shiny and uses the 'feedr' package to transform, summarize and visualize animal movement data collected from RFID stations."),
                        h4(style = "text-align:center", "To get started, ", actionLink("link_db", "select data from our data base"), "or", actionLink("link_import", "import your own.")),
                        #actionButton("pause", "Pause"),

                        div(class = "alert", id = "loading_app", "Please wait while the app loads..."),

                        hr(),
                        h4(style = "text-align:center", "Current activity at feeders on Thompson Rivers University Campus")),
                      fluidRow(
                        column(10, offset = 1,
                               div(style = "max-width: 800px; margin-left: auto; margin-right:auto;", feedr:::mod_UI_map_current("current"))
                        ))
             ),

             #################
             ## SETUP
             #################
             tabPanel(title = "Database", icon = icon("database"),
                      feedr:::mod_UI_data_db("access")),
             tabPanel(title = "Import", icon = icon("upload"),
                      feedr:::mod_UI_data_import("import")),

            #################
            ## Visualization
            #################
            tabPanel("Visualizations", icon = icon("eye"),
                     column(12,
                            mod_UI_map_animate("anim")
                     )
            ),
            #################
            ## BIRDS
            #################
            tabPanel("Individuals",
                     column(9,
                            fluidRow(DT::dataTableOutput("dt_birds"))),
                     column(3,
                            h4("Click on a row for more information"),
                            htmlOutput("img_birds", class = "bird-img"))
            ),
            #################
            ## DATA
            #################
            tabPanel("Transformations", icon = icon("exchange"),
                     column(3,
                            htmlOutput("data_desc"),
                            hr(),
                            h3("Downloads"),
                            p(shinyjs::disabled(downloadButton('data_dl', 'All'))),
                            p(shinyjs::disabled(downloadButton("data_dl_raw", "Raw"))),
                            p(shinyjs::disabled(downloadButton("data_dl_visits", "Visits"))),
                            p(shinyjs::disabled(downloadButton("data_dl_feeding", "Feeding"))),
                            p(shinyjs::disabled(downloadButton("data_dl_move", "Movements"))),
                            p(shinyjs::disabled(downloadButton("data_dl_disp", "Displacements"))),
                            p(shinyjs::disabled(downloadButton("data_dl_dom", "Dominance")))
                     ),
                     column(9,
                            tabsetPanel(type = "tabs", id = "data_tabs",
                                        tabPanel("Raw Data", DT::dataTableOutput("dt_raw")),
                                        tabPanel("Visits Data", DT::dataTableOutput("dt_v")),
                                        tabPanel("Feeding Data", DT::dataTableOutput("dt_f")),
                                        tabPanel("Movement Data", DT::dataTableOutput("dt_m")),
                                        tabPanel("Displacement Data", DT::dataTableOutput("dt_disp"))
                            )
                     )
            ),
            tabPanel("Help",
                     navlistPanel(widths = c(2, 10),
                                  HTML("About animal<strong>nexus</strong>"),
                                  tabPanel("About", includeMarkdown("about.md")),
                                  tabPanel("Contact us", includeMarkdown("contact.md")),
                                  tabPanel("References (in progress)"),
                                  HTML("Using animal<strong>nexus</strong>"),
                                  tabPanel("FAQ", includeMarkdown("faq.md")),
                                  tabPanel("Tutorials (in progress)"),
                                  tabPanel("Using the feedr package (in progress)")
                     )
            )

  )
))
