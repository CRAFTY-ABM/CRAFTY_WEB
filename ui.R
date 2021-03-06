# load libraries
library(shiny)
library(leaflet)
library(dplyr)
library(leaflet.extras)
library(RColorBrewer)
library(gplots)
library(markdown)
library(DT)

source("RScripts/Functions_CRAFTY_WEB.R")
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


navbarPage("CRAFTY interactive web-interface", windowTitle =  "CRAFTY interactive web-interface (Land Use and Ecosystem Change Summer School 2019)", fluid = T, 
           
           
           # titlePanel("CRAFTY-EU"), 
           tabPanel("Main", 
                    
                    # Sidebar layout with input and output definitions ----
                    sidebarLayout(
                      sidebarPanel(width=SIDEBAR_WIDTH,
                                   fluidPage(br(), h4("Scenario customisation"))
                                   
                                   , sliderInput("year",
                                                 "Year:",
                                                 min = 2016,
                                                 max = 2086, sep = "",
                                                 value = 2016, step=10, animate =animationOptions(interval = 3000, loop = FALSE, playButton = NULL, pauseButton = NULL)),
                                   selectInput("paramset_full", label = "Behavioural parameter set-up",
                                               choices = paramsets.fullnames, selected = paramsets.fullnames[1]
                                   ),
                                   selectInput("scenario", "Climate and socio-economic scenario",
                                               scenario.names[], selected = scenario.names[1]
                                   ),
                                   selectInput("foodprice", "Food price",
                                               foodprice.names, selected = foodprice.names[1]
                                   ),
                                   selectInput("fooddemand", "Meat demand (applies to RCP8.5-SSP3)",
                                               fooddemand.names, selected = fooddemand.names[1]
                                   ),
                                   fluidPage(br(), h4("Map customisation"))
                                   , selectInput("outputlayer", "Model Output",
                                                 indicator.names[c(20, 17, 1:7)], selected=indicator.names[20]
                                   )                                  
                                   , selectInput("inputlayer", "Model Input",
                                                 indicator.names[8:14], selected=indicator.names[9]
                                   ),
                                   
                                   selectInput("background", "Background tiles", choices =
                                                 provider_names, selected=providers$OpenStreetMap.Mapnik
                                   )
                                   , htmlOutput("ReferenceToScenarios")
                                   
                                   # , actionLink("deleteCache", "Delete cached files")
                                   
                      ),
                      # 
                      # # Main panel for displaying outputs ----
                      mainPanel(width=MAINPANEL_WIDTH, 
                                tabsetPanel(
                                  tabPanel("Map", 
                                           leafletOutput("Tab1_MapPane", height = PLOT_HEIGHT)
                                           # Run info
                                           , verbatimTextOutput("PaneRuninfo")
                                           
                                           , absolutePanel(
                                             top = 380, left = 20, width = 180,
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
                                               selectInput("colors", "Color palette (cont.)",
                                                           rownames(subset(brewer.pal.info, category %in% c("seq", "div")))
                                               ),
                                               
                                               # , radioButtons("plotType", "Plot type",
                                               #              c("Scatter"="p", "Line"="l")
                                               # )
                                               # , checkboxInput("legend", "Show legend", TRUE)
                                               # # )
                                               # sliderInput("n", "", min=3, max=20, value=5),
                                               # plotOutput("plot2", height="50px")
                                               downloadLink("downloadData", "Download output map (GeoTIFF)")
                                             ),
                                             style = "opacity: 0.5"
                                             
                                             
                                           )
                                  ), 
                                  
                                  tabPanel("Behavioural parameters", 
                                           # Time series info
                                           dataTableOutput('Tab1_BehaviouralTablePane')
                                           , htmlOutput("ReferenceToParameters")
                                           
                                           # Map view options
                                  ), 
                                  tabPanel("Statistics", 
                                           # Time series info
                                           plotOutput("Tab1_StatisticsPane")
                                           # Map view options
                                  )
                                )))
           ),
           
           tabPanel("Time-series",
                    
                    # Sidebar layout with input and output definitions ----
                    sidebarLayout(
                      sidebarPanel(width=SIDEBAR_WIDTH,
                                   selectInput("paramset_full_ts", label = "Behavioural parameters",
                                               choices = paramsets.fullnames, selected = paramsets.fullnames[1]
                                   ),
                                   selectInput("scenario_ts", "Climate and socio-economic scenario",
                                               scenario.names[], selected = scenario.names[1]
                                   )
                                   , selectInput("foodprice_ts", "Food price",
                                                 foodprice.names, selected = foodprice.names[1]
                                   )
                                   , selectInput("fooddemand_ts", "Meat demand (applies to RCP8.5-SSP3)",
                                                 fooddemand.names, selected = fooddemand.names[1]
                                   )
                                   # ,
                                   # selectInput("inputlayer_ts", "Model Input",
                                   #             indicator.names[8:14], selected=indicator.names[9]
                                   # ),
                                   # selectInput("outputlayer_ts", "Model Output",
                                   #             indicator.names[c(20, 17, 1:7, 19)], selected=indicator.names[20]
                                   # )
                                   
                      ),
                      # 
                      # # Main panel for displaying outputs ----
                      mainPanel(width = MAINPANEL_WIDTH,
                                # Show a plot of the generated distribution
                                plotOutput("Tab2_TimeseriesPlotPane", height = PLOT_HEIGHT)
                                # , verbatimTextOutput("PaneRuninfo_ts")
                      )
                    )
           ),
           #       
           tabPanel("Transition",
                    # Show a transition plot of the selected
                    
                    # Sidebar layout with input and output definitions ----
                    sidebarLayout(
                      sidebarPanel(width=SIDEBAR_WIDTH, height=PLOT_HEIGHT, 
                                   # 
                                   selectInput("outputlayer_transition", "Model Output (LU)",
                                               indicator.names[c(17, 20)], selected=indicator.names[20]
                                   )
                                   ,
                                   fluidPage(br(), h4("Land use transition from"))
                                   
                                   , sliderInput("year_from",
                                                 "Year",
                                                 min = 2016,
                                                 max = 2086, sep = "",
                                                 value = 2016, step=10),
                                   selectInput("paramset_full_from", label = "Behavioural parameters",
                                               choices = paramsets.fullnames, selected = paramsets.fullnames[1]
                                   ),
                                   selectInput("scenario_from", "Climate and socio-economic scenario",
                                               scenario.names[], selected = scenario.names[1]
                                   )
                                   , selectInput("foodprice_from", "Food price",
                                                 foodprice.names, selected = foodprice.names[1]
                                   )
                                   , selectInput("fooddemand_from","Meat demand (applies to RCP8.5-SSP3)",
                                                 fooddemand.names, selected = fooddemand.names[1]
                                   )
                                   # selectInput("inputlayer", "Model Input",
                                   #             indicator.names[8:14], selected=indicator.names[9]
                                   # ),
                                   , fluidPage(br(), h4("To"))
                                   
                                   ,  sliderInput("year_to",
                                                  "Year",
                                                  min = 2016,
                                                  max = 2086, sep = "",
                                                  value = 2086, step=10),
                                   selectInput("paramset_full_to", label = "Behavioural parameters",
                                               choices = paramsets.fullnames, selected = paramsets.fullnames[1]
                                   ),
                                   selectInput("scenario_to", "Climate and socio-economic scenario",
                                               scenario.names[], selected = scenario.names[1]
                                   )
                                   , selectInput("foodprice_to", "Food price",
                                                 foodprice.names, selected = foodprice.names[3]
                                   )
                                   , selectInput("fooddemand_to","Meat demand (applies to RCP8.5-SSP3)",
                                                 fooddemand.names, selected = fooddemand.names[1]
                                   )
                                   # selectInput("inputlayer", "Model Input",
                                   #             indicator.names[8:14], selected=indicator.names[9]
                                   # ),
                                   
                                   
                      ),
                      # 
                      # # Main panel for displaying outputs ----
                      mainPanel(
                        
                        tabPanel("Tab3_TransitionPlotPane", 
                                 plotOutput("Tab3_TransitionPlotPane")
                        )
                      )
                    )
           )
           , tabPanel("About",
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
           ,  
           tabPanel("New social network (experimental)", 
                    # Run info
                    # , verbatimTextOutput("PaneRuninfo")
                    
                    # Sidebar layout with input and output definitions ----
                    sidebarLayout(
                      sidebarPanel(width=SIDEBAR_WIDTH + 1 ,
                                   fluidPage(br(), h4("Scenario customisation"))
                                   
                                   , sliderInput("year_sn",
                                                 "Year:",
                                                 min = 2016,
                                                 max = 2086, sep = "",
                                                 value = 2016, step=10, animate =animationOptions(interval = 3000, loop = FALSE, playButton = NULL, pauseButton = NULL))

                                   , selectInput("paramset_sn", label = "Behavioural parameter set-up",
                                                 choices = paramsets.fullnames, selected = paramsets.fullnames[1]
                                   ),
                                   selectInput("scenario_sn", "Climate and socio-economic scenario",
                                               scenario.names[], selected = scenario.names[1]
                                   ),
                                   selectInput("foodprice_sn", "Food price",
                                               foodprice.names, selected = foodprice.names[1]
                                   ),
                                   selectInput("fooddemand_sn", "Meat demand (applies to RCP8.5-SSP3)",
                                               fooddemand.names, selected = fooddemand.names[1]
                                   ), 
                                   
                                   fluidPage(br(), h4("SN model customisation"))
                                   , selectInput("outputlayer_sn", "Output layer",
                                                 c("AFT density","Social.capital"), selected="AFT density")   
                                   , selectInput("type_sn", "Target AFT",
                                                 aft.names.fromzero, selected = aft.names.fromzero[2]
                                   )
                                   , fluidPage(br(), h5("Adjusted C (C')"),  h5("C' = C_0 + C_0 * (1+D) * alpha + beta"), h5(", where C_0 is the default capital value, D is the density of the same agent types within the buffer in [0,1]"))
                                   , sliderInput("socialnet_width", 
                                                 "Size of Social Network Buffer (15-200 km):",
                                                 min = 15, 
                                                 max = 200, sep = "", 
                                                 value = 30, step = 5,  animate =animationOptions(interval = 3000, loop = FALSE, playButton = NULL, pauseButton = NULL))


                                   
                                   , sliderInput("sn_alpha", 
                                                 "alpha [-1, 1]",
                                                 min = -1E1, 
                                                 max = 1E1, sep = "", 
                                                 value = 1, step = 0.1)
                                   , sliderInput("sn_beta", 
                                                 "beta [-10, 10]",
                                                 min = -10, 
                                                 max = 10, sep = "", 
                                                 value = 0, step = 0.1)
                                   # fluidPage(br(), h4("Map customisation"))
                                   # , selectInput("outputlayer2", "Model Output",
                                   #               indicator.names[c(20, 17, 1:7)], selected=indicator.names[20]
                                   # )                                  
                                   # , selectInput("inputlayer2", "Model Input",
                                   #               indicator.names[8:14], selected=indicator.names[9]
                                   # ),
                                   # 
                                   # selectInput("background2", "Background tiles", choices =
                                   #               provider_names, selected=providers$OpenStreetMap.Mapnik
                                   # )
                                   
                                   # , actionLink("deleteCache", "Delete cached files")
                                   
                      ), 
                      mainPanel(width=MAINPANEL_WIDTH - 1 , 
                                tabPanel("MapUK", leafletOutput("TabUK_MapPane", height = PLOT_HEIGHT)
                                )
                                , textOutput("PaneSNinfo")
                                , textOutput("PaneSNinfoGugi")
                                
                                , absolutePanel(
                                  top = 380, left = 20, width = 180,
                                  draggable = TRUE,
                                   
                                  wellPanel(
                                    # HTML(markdownToHTML(fragment.only=TRUE, text=c("contents"
                                    #   # "This is an absolutePanel that uses `bottom` and `right` attributes.
                                    #   # It also has `draggable = TRUE`, so you can drag it to move it around the page.
                                    #   # The slight transparency is due to `style = 'opacity: 0.92'`.
                                    #   # You can put anything in absolutePanel, including inputs and outputs:"
                                    # ))),
                                    
                                    sliderInput("alpha_sn", "Transparency",0, 1,
                                                value = 0.8, step = 0.1
                                    ),
                                    selectInput("colors_sn", label = "Color palette (cont.)", selected = "RdYlGn", choices =        rownames(subset(brewer.pal.info, category %in% c("seq", "div")))
                                    ), 
                                    selectInput("background_sn", "Background tiles", choices =
                                                  provider_names, selected=providers$OpenStreetMap.Mapnik
                                    )
                                    
                                    # , radioButtons("plotType", "Plot type",
                                    #              c("Scatter"="p", "Line"="l")
                                    # )
                                    # , checkboxInput("legend", "Show legend", TRUE)
                                    # # )
                                    # sliderInput("n", "", min=3, max=20, value=5),
                                    # plotOutput("plot2", height="50px")
                                  ),
                                  style = "opacity: 0.5"
                                )
                      )
                    )
                    
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
           
           #       # , absolutePanel(
           #       #   bottom = 12, left = 15, width = 200, height = 'auto',
           #       #   draggable = TRUE,
           #       #   
           #       #   wellPanel(
           #       #     HTML(markdownToHTML(fragment.only=TRUE, text=c("contents"
           #       #                                                    # "This is an absolutePanel that uses `bottom` and `right` attributes.
           #       #                                                    # It also has `draggable = TRUE`, so you can drag it to move it around the page.
           #       #                                                    # The slight transparency is due to `style = 'opacity: 0.92'`.
           #       #                                                    # You can put anything in absolutePanel, including inputs and outputs:"
           #       #     )))
           #       #     
           #       #   ),
           #       #   style = "opacity: 0.5"
           #       # )
           #       
           #     )
           #   )
)
