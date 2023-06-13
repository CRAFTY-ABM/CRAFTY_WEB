# load libraries
library(shiny)
library(knitr)
library(shinythemes)
library(bslib)

source("RScripts/Functions_CRAFTY_WEB.R")


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

# Ref: https://www.cultureofinsight.com/blog/2018/03/15/2018-03-15-responsive-iframes-for-shiny-apps/
####user interface####
ui <- shinyUI(
  fluidPage(
    
    
    ####make the navbar page####
    navbarPage(MODEL_INFO$NAME, windowTitle = MODEL_INFO$TITLE, fluid = F, 
               #theme =  shinytheme("simplex"),
               theme = bs_theme(bootswatch = "simplex", primary = "darkgreen")
               
               
               ,
               
               # # titlePanel("CRAFTY-EU"), 
               tabPanel("Home", value = "home",
                        fluidRow(
                          column(1),
                          column(10, includeMarkdown("LandingPage.md")), 
                          #, column(2,  htmlOutput("ReferenceToScenarios"))
                          column(1)
                        )
               ),
               tabPanel("Model info", 
                        
                        fluidRow(column(1),
                                 column(10, includeMarkdown("crafty_about.md"), 
                                        column(1)))
                        
                        
                        # fluidRow(
                        #   column(1),
                        #   column(10, tabsetPanel(
                        #     tabPanel("Model structure", 
                        #              fluidRow(
                        #                column(10, includeMarkdown("ModelStructure.md"))
                        #              )
                        #     )
                        #     , tabPanel("Land use types",
                        #                # AFT info
                        #                # dataTableOutput("Tab1_AFTTablePane")
                        #                fluidRow(
                        #                  column(10, includeMarkdown("AFT_Description.md"))
                        #                )
                        #     )
                        #     , tabPanel("Capitals",
                        #                # AFT info
                        #                fluidRow(
                        #                  column(10, dataTableOutput("Tab1_CapTablePane"))
                        #                )
                        #                
                        #     ), tabPanel("Services",
                        #                 # Services info
                        #                 fluidRow(
                        #                   column(10, dataTableOutput("Tab1_ServiceTablePane"))
                        #                 )
                        #     ))
                        #   ),
                        # column(1)
                        
               ),
               
               tabPanel("Model results",
                        tabsetPanel(id = "tabset_results",
                                    tabPanel("Maps", 
                                             
                                             # Sidebar layout with input and output definitions ----
                                             sidebarLayout(
                                               sidebarPanel(width=SIDEBAR_WIDTH,
                                                            
                                                            # selectInput("version", "Version",
                                                            #             version_names, selected = version_names[version_default_idx]
                                                            # ),
                                                            # , fluidPage(br(), h4("Scenario"))
                                                            selectInput("scenario", "Climate and socio-economic scenario (+PA)",
                                                                        MODEL_INFO$scenario_names_full[], selected = selected_scenario_current
                                                            )
                                                            # , selectInput("scenario_pa", "Protected area scenario",
                                                            #                                                                 MODEL_INFO$pa_scenario_names_full[], selected = selected_pascenario_current
                                                            #  )
                                                            
                                                            , sliderInput("year",
                                                                          "Year:",
                                                                          min = min(target_years_other),
                                                                          max = max(target_years_other), sep = "",
                                                                          value = min(target_years_other), step=10, animate =animationOptions(interval = 5000, loop = FALSE, playButton = NULL, pauseButton = NULL))
                                                            
                                                            , radioButtons("outputGroup", "Show", selected = "print_out", c( "Model input"="print_in", "Model output"="print_out")
                                                            )
                                                            , conditionalPanel(
                                                              condition="input.outputGroup=='print_out'"
                                                              , selectInput("outputlayer", "Output", 
                                                                            indicator_names_full[c(16, 1:7)], selected=indicator_names_full[16]
                                                              ))                                  
                                                            , conditionalPanel(
                                                              condition="input.outputGroup=='print_in'"
                                                              , selectInput("inputlayer", "Input",
                                                                            indicator_names_full[8:15], selected=indicator_names_full[9]
                                                              ))
                                                            , htmlOutput("ReferenceToScenarios")
                                                            
                                                            
                                               ),
                                               # 
                                               # # Main panel for displaying outputs ----
                                               mainPanel(width=MAINPANEL_WIDTH, 
                                                         
                                                         leafletOutput("Tab1_MapPane", height = 800)
                                                         # Run info
                                                         , verbatimTextOutput("PaneRuninfo")
                                                         
                                                         , absolutePanel(
                                                           top = 180, left = 20, width = 160,
                                                           draggable = TRUE,
                                                           
                                                           
                                                           wellPanel(
                                                             
                                                             sliderInput("alpha", "Transparency",0, 1,
                                                                         value = TRANSPARENCY_DEFAULT, step = 0.1
                                                             )
                                                             
                                                             , conditionalPanel(
                                                               condition=paste0("input.outputGroup=='print_out' && input.outputlayer=='", indicator_names_full[length(indicator_names_full)], "'"), 
                                                               selectInput("colorsGroup", "Colour palette",
                                                                           choices = c("Land use (17 AFTs)")
                                                               ))
                                                             
                                                             , conditionalPanel(
                                                               condition=paste0("input.outputGroup!='print_out' || input.outputlayer!='", indicator_names_full[length(indicator_names_full)], "'"),
                                                               selectInput("colors", "Colour palette",
                                                                           rownames(subset(brewer.pal.info, category %in% c("seq", "div")))
                                                               )
                                                               , checkboxInput("InvertColour", "Invert colour", FALSE)
                                                             )
                                                             # , radioButtons("plotType", "Plot type",
                                                             #              c("Scatter"="p", "Line"="l")
                                                             # )
                                                             , checkboxInput("legend", "Show legend", TRUE)
                                                             ,
                                                             
                                                             selectInput("background", "Basemap", choices =
                                                                           provider_names, selected=providers$OpenStreetMap.Mapnik
                                                             )
                                                             # sliderInput("n", "", min=3, max=20, value=5),
                                                             # plotOutput("plot2", height="50px")
                                                             # , downloadLink("downloadData", "Download output map (GeoTIFF)")
                                                           ),
                                                           style = "opacity: 0.8" # 0.5 previously
                                                           
                                                           
                                                         )
                                                         
                                                         
                                               )
                                             )
                                    )
                                    ,
                                    
                                    
                                    
                                    
                                    tabPanel("Time-series"
                                             
                                             , sidebarLayout(
                                               sidebarPanel(width=SIDEBAR_WIDTH_TS,id = "tsmenu",
                                                            
                                                            # selectInput("version", "Version",
                                                            #             version_names, selected = version_names[version_default_idx]
                                                            # ),
                                                            # , fluidPage(br(), h4("Scenario"))
                                                            selectInput("scenario_ts", "Climate and socio-economic scenario",
                                                                        MODEL_INFO$scenario_names_full[], selected = MODEL_INFO$scenario_default  
                                                            )
                                                            
                                                            
                                               )
                                               , mainPanel(width=MAINPANEL_WIDTH_TS,
                                                           
                                                           plotOutput("Tab2_TimeseriesPlotPane", height = PLOT_HEIGHT)
                                               )
                                             )
                                             
                                    )
                                    
                                    
                                    
                                    
                                    , tabPanel("Land Use Transition", 
                                               # Show a transition plot of the selected
                                               
                                               # Sidebar layout with input and output definitions ----
                                               sidebarLayout(
                                                 sidebarPanel(width=SIDEBAR_WIDTH,
                                                              
                                                              fluidRow("Land use transition", br(), "from:")
                                                              
                                                              # , selectInput("version_from", "Version",
                                                              #             version_names, selected = version_names[1]
                                                              # )
                                                              , selectInput("scenario_from", "Scenario",
                                                                            MODEL_INFO$scenario_names_full[], selected = selected_scenario_current
                                                                            
                                                              )
                                                              , sliderInput("year_from",
                                                                            "Year",
                                                                            min = MODEL_INFO$YEAR_START,
                                                                            max = MODEL_INFO$YEAR_END, sep = "",
                                                                            value = MODEL_INFO$YEAR_DEFAULT, step=10)
                                                              # selectInput("paramset_full_from", label = "Behavioural parameter set-up",
                                                              #             choices = paramsets_fullnames, selected = paramsets_fullnames[1]
                                                              # ),
                                                              
                                                              , fluidRow(  h5("to:"))
                                                              
                                                              # , selectInput("version_to", "Version",
                                                              #             version_names, selected = version_names[1]
                                                              # ),
                                                              , selectInput("scenario_to", "Scenario",
                                                                            MODEL_INFO$scenario_names_full[], selected = selected_scenario_future
                                                              )
                                                              , sliderInput("year_to",
                                                                            "Year",
                                                                            min = MODEL_INFO$YEAR_START,
                                                                            max = MODEL_INFO$YEAR_END, sep = "",
                                                                            value = MODEL_INFO$YEAR_END, step=10)
                                                              
                                                              # selectInput("paramset_full_to", label = "Behavioural parameter set-up",
                                                              #             choices = paramsets_fullnames, selected = paramsets_fullnames[1]
                                                              # ),
                                                              
                                                              
                                                 ),
                                                 #
                                                 # # Main panel for displaying outputs ----
                                                 mainPanel(
                                                   
                                                   tabPanel("Tab3_TransitionPlotPane", height=PLOT_HEIGHT,
                                                            plotOutput("Tab3_TransitionPlotPane",  height = PLOT_HEIGHT)
                                                   )
                                                 )
                                               )
                                    )
                                    
                                    
                                    , tabPanel("Download results"
                                               ,             fluidRow(
                                                 column(10, includeMarkdown("DownloadResults.md"))
                                               )
                                               
                                               
                                    )
                                    
                                    
                        )
               )
               
    )
    , fluidRow(column(1),
               column(10, htmlOutput("Footer")) # (includeMarkdown("Footer_disclaimer.md")))
               , column(1)
               
    )
    
  )
)

