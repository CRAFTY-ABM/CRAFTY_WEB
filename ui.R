# load libraries
library(shiny)


source("RScripts/Functions_CRAFTY_WEB.R")

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


navbarPage("CRAFTY-GB", windowTitle =  "CRAFTY-GB interactive web-interface", fluid = T, 
           
           
           # # titlePanel("CRAFTY-EU"), 
           tabPanel("Home", 
                    fluidRow(
                      column(10, includeMarkdown("LandingPage.md"))
                      #, column(2,  htmlOutput("ReferenceToScenarios"))
                    )
                    
           ),
           tabPanel("Model info", 
                    tabsetPanel(
                      tabPanel("Model structure", 
                               fluidRow(
                                 column(10, includeMarkdown("ModelStructure.md"))
                               )
                      )
                      , tabPanel("Land use types",
                                 # AFT info
                                 # dataTableOutput("Tab1_AFTTablePane")
                                 fluidRow(
                                   column(10, includeMarkdown("AFT_Description.md"))
                                 )
                      )
                      , tabPanel("Capitals",
                                 # AFT info
                                 fluidRow(
                                   column(10, dataTableOutput("Tab1_CapTablePane"))
                                 )
                                 
                      ), tabPanel("Services",
                                  # Services info
                                  fluidRow(
                                    column(10, dataTableOutput("Tab1_ServiceTablePane"))
                                  )
                      )
                    )
           ),
           
           tabPanel("Model result",
                    tabsetPanel(
                      tabPanel("Maps", 
                               
                               # Sidebar layout with input and output definitions ----
                               sidebarLayout(
                                 sidebarPanel(width=SIDEBAR_WIDTH,
                                              
                                              # selectInput("version", "Version",
                                              #             version_names, selected = version_names[version_default_idx]
                                              # ),
                                              # , fluidPage(br(), h4("Scenario"))
                                              selectInput("scenario", "Climate and socio-economic scenario",
                                                          scenario_names[], selected = selected_scenario_current
                                              )
                                              , sliderInput("year",
                                                            "Year:",
                                                            min = min(target_years_other),
                                                            max = max(target_years_other), sep = "",
                                                            value = min(target_years_other), step=10, animate =animationOptions(interval = 5000, loop = FALSE, playButton = NULL, pauseButton = NULL))
                                              
                                              , radioButtons("outputGroup", "Show layer",
                                                             c("Model output"="print_out", "Model input"="print_in")
                                              )
                                              
                                              , selectInput("outputlayer", "Output", 
                                                            indicator_names[c(29, 1:14)], selected=indicator_names[28]
                                              )                                  
                                              , selectInput("inputlayer", "Input",
                                                            indicator_names[15:28], selected=indicator_names[28]
                                              )
                                              , htmlOutput("ReferenceToScenarios")
                                              
                                              
                                 ),
                                 # 
                                 # # Main panel for displaying outputs ----
                                 mainPanel(width=MAINPANEL_WIDTH, 
                                           
                                           leafletOutput("Tab1_MapPane", height = 800)
                                           # Run info
                                           , verbatimTextOutput("PaneRuninfo")
                                           
                                           , absolutePanel(
                                             top = 380, left = 20, width = 180,
                                             draggable = TRUE,
                                             
                                             
                                             wellPanel(
                                               
                                               sliderInput("alpha", "Transparency",0, 1,
                                                           value = TRANSPARENCY_DEFAULT, step = 0.1
                                               ),
                                               selectInput("colorsGroup", "Color palette (AFT)",
                                                           choices = c("Shaded (n=14)", "Reduced (n=7)")
                                               ),
                                               selectInput("colors", "Color palette (cont.)",
                                                           rownames(subset(brewer.pal.info, category %in% c("seq", "div")))
                                               )
                                               # , radioButtons("plotType", "Plot type",
                                               #              c("Scatter"="p", "Line"="l")
                                               # )
                                               , checkboxInput("InvertColour", "Invert colour", FALSE)
                                               , checkboxInput("legend", "Show legend", TRUE)
                                               ,
                                               
                                               selectInput("background", "Basemap", choices =
                                                             provider_names, selected=providers$OpenStreetMap.Mapnik
                                               )
                                               # sliderInput("n", "", min=3, max=20, value=5),
                                               # plotOutput("plot2", height="50px")
                                               , downloadLink("downloadData", "Download output map (GeoTIFF)")
                                             ),
                                             style = "opacity: 0.8" # 0.5 previously
                                             
                                             
                                           )
                                           
                                           
                                 )
                               )
                      )
                      ,
                      
                      # tabPanel("Time-series",
                      #            plotOutput("Tab2_TimeseriesPlotPane", height = PLOT_HEIGHT)
                      # )
                      # 
                      # 
                      # 
                      
                      
                      tabPanel("Time-series", 
                               
                               sidebarLayout(
                                 sidebarPanel(width=SIDEBAR_WIDTH,
                                              
                                              # selectInput("version", "Version",
                                              #             version_names, selected = version_names[version_default_idx]
                                              # ),
                                              # , fluidPage(br(), h4("Scenario"))
                                              selectInput("scenario_ts", "Climate and socio-economic scenario",
                                                          scenario_names[], selected = selected_scenario_current
                                              )
                                              
                                              
                                 ), 
                                 mainPanel(width=MAINPANEL_WIDTH, 
                                           
                                           plotOutput("Tab2_TimeseriesPlotPane", height = PLOT_HEIGHT)
                                 )
                               )
                               
                      )
                      
                      
                      
                      
                      , tabPanel("Land Use Transition",
                                 # Show a transition plot of the selected
                                 
                                 # Sidebar layout with input and output definitions ----
                                 sidebarLayout(
                                   sidebarPanel(width=SIDEBAR_WIDTH, height=PLOT_HEIGHT,
                                                
                                                fluidPage(br(), h4("Land use transition from"))
                                                
                                                # , selectInput("version_from", "Version",
                                                #             version_names, selected = version_names[1]
                                                # )
                                                , selectInput("scenario_from", "Scenario",
                                                              scenario_names[], selected = scenario_names[1]
                                                              
                                                )
                                                , sliderInput("year_from",
                                                              "Year",
                                                              min = 2020,
                                                              max = 2080, sep = "",
                                                              value = 2020, step=10)
                                                # selectInput("paramset_full_from", label = "Behavioural parameter set-up",
                                                #             choices = paramsets_fullnames, selected = paramsets_fullnames[1]
                                                # ),
                                                
                                                , fluidPage(br(), h4("to"))
                                                
                                                # , selectInput("version_to", "Version",
                                                #             version_names, selected = version_names[1]
                                                # ), 
                                                , selectInput("scenario_to", "Scenario",
                                                              scenario_names[], selected = scenario_names[4]
                                                ) 
                                                , sliderInput("year_to",
                                                              "Year",
                                                              min = 2020,
                                                              max = 2080, sep = "",
                                                              value = 2070, step=10)
                                                # selectInput("paramset_full_to", label = "Behavioural parameter set-up",
                                                #             choices = paramsets_fullnames, selected = paramsets_fullnames[1]
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
                      
                      
                    )
           )
           , tabPanel("About",
                      fluidRow(
                        column(10,
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
                        , actionButton(inputId = "deleteCache", label = "Delete cache (admin only)")
                        , actionButton(inputId = "createCache", label = "Create cache (admin only)")
                        
                      )
           )
           
           
           
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
