cat("Starting UI...\n")
library(feedr)
library(shiny)
library(shinyjs)
library(shinyBS)

addResourcePath("assets", system.file("shiny-examples", "app_files", package = "feedr"))

shinyUI(
  tagList(
    shinyjs::useShinyjs(),
    tags$head(
      #tags$link(rel = "stylesheet", href = "assets/style.css"),
      cat("UI - Javascripts...\n"),

      # Hide tabs on load -------------------------------------------------------
      tags$script("
        window.onload = function() {
            $('#main a:contains(\"Database\")').parent().addClass('hidden');
            $('#main a:contains(\"Import\")').parent().addClass('hidden');
            $('#main a:contains(\"Visualizations\")').parent().addClass('hidden');
            $('#main a:contains(\"Individuals\")').parent().addClass('hidden');
            $('#main a:contains(\"Transformations\")').parent().addClass('hidden');
            $('#main a:contains(\"Settings\")').parent().addClass('hidden');
            $('#main a:contains(\"Help\")').parent().addClass('hidden');
        };

        Shiny.addCustomMessageHandler('activeNavs', function(nav_label) {
            $('#main a:contains(\"' + nav_label + '\")').parent().removeClass('hidden');
        });

   ")),

    # Load navbar -------------------------------------------------------------
    cat("UI - navbarPage...\n"),
    navbarPage(title = a(href = "http://animalnexus.ca", HTML("animal<strong>nexus</strong>")),
               id = "main",
               position = "fixed-top",
               collapsible = TRUE,
               windowTitle = "animalnexus",
               header = includeCSS(system.file("shiny-examples", "app_files", "style.css", package = "feedr")),
               footer = column(12,
                               hr(),
                               div(class = "data-status", textOutput("data_info")),
                               div(class = "package-version", htmlOutput("package_version"))),



               # Greeting ----------------------------------------------------------------

               tabPanel("Home",
                        fluidRow(
                          div(style = "text-align:center", HTML("<h1>Welcome to animal<strong>nexus</strong></h1>")),
                          h4(style = "text-align:center", "This is a webapp built with R shiny and using the ", code(a(href = "http://github.com/animalnexus/feedr", target = "_blank", "feedr")), " package to transform, summarize and visualize animal movement data collected from RFID stations."),
                          shinyjs::hidden(h4(id = "get-started", style = "text-align:center", "To get started, ", actionLink("link_db", "select data from our data base"), "or", actionLink("link_import", "import your own."),
                                             " See also the ", a(href = "http://animalnexus.github.io", target = "_blank", "About page"), "for more information, or click on any help button (", actionButton("h", "?", class = "help"), ") for assistance.")),

                          #actionButton("pause", "Pause"),

                          div(class = "alert", id = "loading_app", "Please wait while the app loads..."),
                          hr(),
                          h4(style = "text-align:center", "Current activity at RFID-enabled feeders on Thompson Rivers University Campus")),
                        # Current Map -------------------------------------------------------------
                        fluidRow(
                          column(10, offset = 1,
                                 div(style = "max-width: 800px; margin-left: auto; margin-right:auto;", feedr:::mod_UI_map_current("current"))
                          ))
               ),


               # Modules -------------------------------------------------------------------
               tabPanel("Database", icon = icon("database"),
                        feedr:::mod_UI_data_db("access")),
               tabPanel("Import", icon = icon("upload"),
                        feedr:::mod_UI_data_import("import")),
               tabPanel("Visualizations", icon = icon("eye"),
                        feedr:::mod_UI_map_animate("anim")),
               tabPanel("Individuals",
                        feedr:::mod_UI_indiv("indiv")),
               tabPanel("Transformations", icon = icon("exchange"),
                        feedr:::mod_UI_trans("trans")),
               tabPanel("Settings", icon = icon("cog"),
                        feedr:::mod_UI_settings("settings"))

               # Help --------------------------------------------------------------------
               # tabPanel("Help",
               #          navlistPanel(widths = c(2, 10),
               #                       HTML("About animal<strong>nexus</strong>"),
               #                       tabPanel("About", includeMarkdown("about.md")),
               #                       tabPanel("Contact us", includeMarkdown("contact.md")),
               #                       #tabPanel("References (in progress)"),
               #                       HTML("Using animal<strong>nexus</strong>"),
               #                       tabPanel("Using animalnexus", includeMarkdown("help.md")),
               #                       tabPanel("FAQ", includeMarkdown("faq.md"))
               #                       #tabPanel("Tutorials (in progress)"),
               #                       #tabPanel("Using the feedr package (in progress)")
               #          )
               # )
    )
  ))
