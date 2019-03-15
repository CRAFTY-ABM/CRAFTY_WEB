#load libraries
library(shiny)
library(leaflet)
library(dplyr)
library(leaflet.extras)
library(RColorBrewer)
library(gplots)
library(markdown)
library(rgdal) 
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

# https://www.cultureofinsight.com/blog/2018/03/15/2018-03-15-responsive-iframes-for-shiny-apps/
  
  
fluidPage("CRAFTY interactive web-interface",   
          titlePanel("CRAFTY-EU"), 
          
          # Sidebar layout with input and output definitions ----
          sidebarLayout(
            sidebarPanel(width=4,
                         sliderInput("year",
                                     "Year:",
                                     min = 2016,
                                     max = 2096, sep = "", 
                                     value = 2016, step=10, animate =animationOptions(interval = 3000, loop = FALSE, playButton = NULL, pauseButton = NULL)),
                         selectInput("paramset_full", label = "Behavioural parameters",
                                     choices = paramsets.fullnames, selected = paramsets.fullnames[1] 
                         ),
                         selectInput("scenario", "Climate and socio-economic scenario",
                                     scenario.names, selected = scenario.names[1]
                         ),
                         selectInput("inputlayer", "Model Input", 
                                     indicator.names[8:14], selected=indicator.names[9]
                         ), 
                         selectInput("outputlayer", "Model Output", 
                                     indicator.names[c(20, 17, 1:7, 19)], selected=indicator.names[20]
                         ), 
                           selectInput("background", "Background tiles", choices = 
                                         as.character(providers), selected=providers$OpenStreetMap.Mapnik
                           ),
                         actionLink("deleteCache", "Delete cached files")
                         
            ), 
     
            # Main panel for displaying outputs ----
            mainPanel(
              tabsetPanel(
                tabPanel("Map", 
                         leafletOutput("MapPane",  height = 600)
                         # Run info
                         , verbatimTextOutput("PaneRuninfo")
                         # Time series info 
                         , plotOutput("Tab1_SubPlotPane")
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
                tabPanel("Time Series", 
                         # Show a plot of the generated distribution
                         plotOutput("Tab2_SummaryPlotPane", height = 600)
                         , verbatimTextOutput("PaneRuninfo2")
                ), 
                
                tabPanel("Transition plot (working on)",
                         # Show a transition plot of the selected
                         plotOutput("Tab3_TransitionPlotPane")
                ), 
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
