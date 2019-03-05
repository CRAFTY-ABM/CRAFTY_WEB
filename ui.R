#load libraries
library(shiny)
library(leaflet)
library(dplyr)
library(leaflet.extras)
library(RColorBrewer)
library(gplots)
library(markdown)

source("Functions_CRAFTY_WEB.R")
# 
# 
# absolutePanel(
#   top = 0, left = 0, right = 0,
#   fixed = TRUE,
#   div(
#     style="padding: 8px; border-bottom: 1px solid #CCC; background: #FFFFEE;",
#     HTML(markdownToHTML(fragment.only=TRUE, text=c(
#       "This absolutePanel is docked to the top of the screen
#                  using `top`, `left`, and `right` attributes.
#                  Because `fixed=TRUE`, it won't scroll with the page."
#     )))
#   )
# )


fluidPage("CRAFTY interactive web-interface",   
          titlePanel("CRAFTY-EU"), 
          
          # Sidebar layout with input and output definitions ----
          sidebarLayout(
            sidebarPanel(width=3,
                         sliderInput("year",
                                     "Year:",
                                     min = 2016,
                                     max = 2096,
                                     value = 2016, step=10, animate =animationOptions(interval = 3000, loop = FALSE, playButton = NULL, pauseButton = NULL)),
                         selectInput("paramset", label = "Paramset",
                                     choices = paramsets, selected = paramsets[1] 
                         ),
                         selectInput("scenario", "Scenario",
                                     scenario.names, selected = scenario.names[1]
                         ),
                         selectInput("indicator", "Indicator", 
                                     indicator.names[-c(16, 18)], selected=indicator.names[17]
                         ), 
                           selectInput("background", "Background tiles", choices = 
                                         as.character(providers), selected=providers$OpenStreetMap.Mapnik
                           ) 
            ), 
     
            # Main panel for displaying outputs ----
            mainPanel(
              tabsetPanel(
                tabPanel("Map", 
                         leafletOutput("MapPane",  height = 600)
                         # Run info
                         , verbatimTextOutput("PaneRuninfo")
                         # Time series info 
                         , plotOutput("Tab1_SubplotPane")
                         # Map view options
                         , absolutePanel(
                           top = 380, left = 20, width = 150,
                           draggable = TRUE,
                           
                           
                           wellPanel(
                             # HTML(markdownToHTML(fragment.only=TRUE, text=c("contents"
                             #   # "This is an absolutePanel that uses `bottom` and `right` attributes.
                             #   # It also has `draggable = TRUE`, so you can drag it to move it around the page.
                             #   # The slight transparency is due to `style = 'opacity: 0.92'`.
                             #   # You can put anything in absolutePanel, including inputs and outputs:"
                             # ))),
                             
                             sliderInput("alpha", "Transparency",0, 1,
                                         value = 0.8, step = 0.1
                             ),
                             selectInput("colors", "Color palette",
                                         rownames(subset(brewer.pal.info, category %in% c("seq", "div")))
                             ), 
                             
                             # , radioButtons("plotType", "Plot type",
                             #              c("Scatter"="p", "Line"="l")
                             # )
                             # , checkboxInput("legend", "Show legend", TRUE)
                             # # )
                             # sliderInput("n", "", min=3, max=20, value=5),
                             # plotOutput("plot2", height="50px")
                             downloadLink("downloadData", "Download Map (GeoTIFF)")
                           ),
                           style = "opacity: 0.5"
                           
                           
                         )
                ),
                tabPanel("Summary plot",
                         # Show a plot of the generated distribution
                         plotOutput("PlotPane")
                         , verbatimTextOutput("PaneRuninfo2")
                ), 
                
                # tabPanel("Transition plot (working on)",
                #          sidebarLayout(
                #          #   sidebarPanel(width=3,
                #          #                
                #          #                sliderInput("year2",
                #          #                            "Year:",
                #          #                            min = 2016,
                #          #                            max = 2096,
                #          #                            value = 2016, step=10, animate =F),
                #          #                
                #          #                selectInput("paramset2", label = "Paramset",
                #          #                            choices = paramsets, selected = paramsets[1] 
                #          #                ),
                #          #                selectInput("scenario2", "Scenario",
                #          #                            scenario.names, selected = scenario.names[1]
                #          #                ),
                #          #                
                #          #                selectInput("indicator2", "Indicator", 
                #          #                            indicator.names[-c(17:18)], selected=indicator.names[16]
                #          #                )
                #          #   ),
                #          #   mainPanel(
                #          #     # Show a plot of the generated distribution
                #          #     # plotOutput("PlotPane")
                #          #     # , verbatimTextOutput("PaneRuninfo2")
                #          #   )
                #          )
                # )
                # , tabPanel("Behaviora Table (working on)",
                # DT::dataTableOutput("table")
                # )
                # , navbarMenu("More",
                tabPanel("About",
                         fluidRow(
                           column(12,
                                  includeMarkdown("crafty_about.md")
                           )
                           # ,
                           # column(3,
                           #        img(class="img-polaroid",
                           #            src=paste0("http://upload.wikimedia.org/",
                           #                       "wikipedia/commons/9/92/",
                           #                       "1919_Ford_Model_T_Highboy_Coupe.jpg")),
                           #        tags$small(
                           #          "Source: Photographed at the Bay State Antique ",
                           #          "Automobile Club's July 10, 2005 show at the ",
                           #          "Endicott Estate in Dedham, MA by ",
                           #          a(href="http://commons.wikimedia.org/wiki/User:Sfoskett",
                           #            "User:Sfoskett")
                           #        )
                         )
                )
                # , absolutePanel(
                #   bottom = 12, left = 15, width = 200, height = 'auto',
                #   draggable = TRUE,
                #   
                #   wellPanel(
                #     HTML(markdownToHTML(fragment.only=TRUE, text=c("contents"
                #                                                    # "This is an absolutePanel that uses `bottom` and `right` attributes.
                #                                                    # It also has `draggable = TRUE`, so you can drag it to move it around the page.
                #                                                    # The slight transparency is due to `style = 'opacity: 0.92'`.
                #                                                    # You can put anything in absolutePanel, including inputs and outputs:"
                #     )))
                #     
                #   ),
                #   style = "opacity: 0.5"
                # )
                
              )
            )
          )
)
