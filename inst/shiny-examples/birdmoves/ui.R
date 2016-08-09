library(shiny)
library(shinyjs)
library(shinyBS)
library(leaflet)
library(feedr)
#library(ggvis)

shinyUI(fluidPage(

  shinyjs::useShinyjs(),

  ## CSS Styling

  tags$head(
    tags$title("birdmoves"),
    tags$style(HTML("
      .shiny-output-error-validation {
        padding: 1em;
        font-weight: bold;
        color: green;
      }
      .shiny-progress .progress-text {
        background-color: orange;
      }
      .shiny-progress .progress-text .progress-message {
        font-size: 100%;
        color: #03F;
      }
     img {
        border: 1;
        max-width: 100%;
     }
     element.style {
        width: 33.33%;
     }

     .bird-img {
        position:relative;
        width:100%;
     }
     .wiki-watermark {
        background: rgb(0, 0, 0); /* fallback color */
        background: rgba(0, 0, 0, 0.7);
        position:absolute;
        left:0px;
        bottom:0px;
        padding: 5px;
        color:white;
        font-weight:bold;
        font-size: 75%;
     }
     .wiki-watermark a:link, .wiki-watermark a:visited, .wiki-watermark a:hover, .wiki-watermark a:active, .wiki-watermark a:focus {
        color:white;
     }
    "))
  ),

  navbarPage(title = a(href = "http://gaia.tru.ca:8080/animalnexus/", HTML("animal<strong>nexus</strong>")),

             #################
             ## SETUP
             #################
             tabPanel("Select Data",
                      feedr:::mod_UI_db_data("access")
              ),

            #################
            ## Watch Now
            #################
            tabPanel("Current Activity",
                     feedr:::mod_UI_map_current("current")
                     # fluidRow(
                     #   column(2,
                     #          h3("Summary"),
                     #          bsButton("current_update", "Update Now"),
                     #          htmlOutput("summary_current")),
                     #   column(10,
                     #          leafletOutput("map_current", height = 600),
                     #          htmlOutput('current_time')
                     #   )
                     # )

            ),

            #################
            ## Visualization
            #################
            tabPanel("Visualizations",
                     mod_UI_map_animate("anim")
            ),

            # tabPanel("Paths (Static)",
            #          column(3,
            #                 h1("Data"),
            #                 uiOutput("UI_static_bird_id"),
            #                 h3("To Do:"),
            #                 div("Based on feedr package visualizations"),
            #                 div("Change feedr functions to have map and layers separate, so doesn't refresh entire map for each bird")
            #                 #sliderInput("speed", "Speed",
            #                 #            min = 0, max = 100,
            #                 #            post = "%",
            #                 #            value = 50),
            #                 #sliderInput("interval", "Interval",
            #                 #            min = 1,
            #                 #            max = 24,
            #                 #            value = 1,
            #                 #            post = " hour(s)")
            #          ),
            #          column(9,
            #                 fluidRow(leafletOutput("map_static", height = 600))
            #          ),
            #          fluidRow(
            #                   #DT::dataTableOutput("dt_paths")
            #                   )
            #
            # ),
            #################
            ## BIRDS
            #################
            tabPanel("Birds",
                     fluidRow(column(6,
                                     h3("Click on a row for more information")),
                              column(6, htmlOutput("img_birds"))),
                     fluidRow(DT::dataTableOutput("dt_birds"))
            ),
            #################
            ## DATA
            #################
            tabPanel("Data Transformations",
                     column(3,
                            h2("Downloads"),
                            p(shinyjs::disabled(downloadButton('data_dl', 'Download All'))),
                            p(shinyjs::disabled(downloadButton("data_dl_raw", "Raw"))),
                            p(shinyjs::disabled(downloadButton("data_dl_visits", "Visits"))),
                            p(shinyjs::disabled(downloadButton("data_dl_feeding", "Feeding"))),
                            p(shinyjs::disabled(downloadButton("data_dl_move", "Movements")))
                     ),
                     column(9,
                            tabsetPanel(type = "tabs",
                                        tabPanel("Raw Data", DT::dataTableOutput("dt_raw")),
                                        tabPanel("Visits Data", DT::dataTableOutput("dt_v")),
                                        tabPanel("Feeding Data", DT::dataTableOutput("dt_f")),
                                        tabPanel("Movement Data", DT::dataTableOutput("dt_m"))
                            )
                     )
            ),
            tabPanel("About",
                     h1("Overall Project"),
                     p(HTML("The animal<strong>nexus</strong> project is large collaborative effort to develop tools for the observation, visualization, and analysis of animal movements registered by RFID feeders or other static recording stations.")),

                     p("We hope that this project will be useful to scientists, citizen scientists, students and educators alike."),
                     p("Our goals are to provide a tool that is easy to use, yet powerful."),
                     p(),
                     "If you're interested in contruting functionality and/or coding to this project, check out our project on ", HTML(paste0("<a href = 'http://github.org/steffilazerte/feedr/' target = 'blank'>GitHub <img src = 'GitHub-Mark-32px.png'></a>")), "We welcome all involvement!",

                     h1("People and Individual Projects"),

                     h3("TRU campus monitoring"),
                     div(strong("Thompson Rivers University")),
                     div(strong("Matt Reudink, Mark Paetkau, David J. Hill, Jackson Kusack, Alistair Sutter, Jerin Roberts, Jamie Shippit, Brandon Turner, Steffi LaZerte")),
                     div("Wifi enabled feeders automatically upload data on feeder use of common city birds (house finches, mountain chickadees, etc.). We're currently examining barriers to movement in an urban setting"),

                     h3("UNBC gap-crossing in chickadees"),
                     div(strong("University of Northern British Columbia")),
                     div(strong("Ken Otter, Jacob Baily, Steffi LaZerte")),
                     div("Habitats gaps have been well documented to act as habitat barriers to bird movement, negatively effecting the dispersal of juveniles, and reducing genetic connectivity between populations. Our work uses a novel approach (radio-frequency identification - RFID) to assess the permeability of habitat gaps, and investigate what factors influence avian gap crossing decisions. Using bird feeders outfitted with RFID readers, we tracked the movement patterns of resident, wintering black-capped chickadees individually banded with passive integrated transponder (PIT) tags. Preliminary results support the prediction that gaps are perceived as movement barriers; bird movements between feeders on opposite sides of gaps were reduced compared to feeders on the same sides of habitat gaps."),

                     h3("SLU behaviour and movements of wintering birds"),
                     div(strong("Swedish University for Agricultural Sciences (SLU)")),
                     div(strong("Adriaan De Jong, Bo-Sören Wiklund, Holger Dettki, Ola Engman and students from Dragonskolan College")),

                     div("At SLU campus in northern Sweden, studies of behaviour and movements of wintering birds are combined with the development of rugged high-tech RFID feeder/reader units for long-term monitoring under harsh climatic conditions."),

                     h3("OSU Community ecology of tropical pollinators"),
                     div(strong("Oregon State University")),
                     div(strong("Matt Betts, Evan Jackson")),
                     div("Pollination is essential to human wellbeing, as well as fundamental to maintenance of biological diversity. Research showing large-scale declines of plants and pollinators highlights the concern that pollination is an ecosystem service at risk. Habitat loss and fragmentation have been implicated as primary drivers of pollination declines, but these observations contradict most model predictions indicating that pollinator networks should be resilient. Very recent theoretical models predict that if ‘keystone’ species are lost, pollination webs may be more vulnerable, resulting in sudden network collapse. However, lack of experimental studies examining the consequences of plant or pollinator extinction means that we know little about whether such collapses occur in nature."),
                     div("This research will focus on the idea that plant-pollinator interactions can be lost if pollinator movement is restricted due to landscape fragmentation. The tropical hummingbird-plant system is ideal for this because the large size of hummingbirds in relation to other pollinators enables tracking of movements across whole landscapes."),
                     div("This research will address two overarching questions: (1) What are the consequences of keystone plant decline and extinction to the broader pollination network? (2) Can pollinator movement and landscape connectivity buffer pollination networks against network collapse?"),
                     div("To address these questions the research team will experimentally remove H. tortuosa in isolated and connected forest fragments to determine whether altered densities of this species affects (a) hummingbird movement, (b) plant reproduction and genetic diversity, and (c) the structure of the pollination network. Hummingbird movement will be quantified for all species in the network using Radio-Frequency Identification Devices (RFID). This method, already successfully demonstrated in this project, will enable observations of the entire hummingbird pollinator community at an unprecedented temporal resolution (seconds) and spatial extent (1000 m radius landscapes).")

            ),
            tabPanel("References",
                     "Coming soon!",
                     h1("FAQ"),
                     h1("Caveats and Disclaimers"),
                     h1("Tutorials"),
                     h1("Glossary")
            )

  )
))
