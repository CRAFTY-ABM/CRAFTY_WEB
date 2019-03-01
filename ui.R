#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

#load libraries
library(shiny)
library(leaflet)
library(dplyr)
library(leaflet.extras)
library(RColorBrewer)
library(gplots)


source("Functions_CRAFTY_WEB.R")


# Define UI for application that draws a histogram
shinyUI(
  fluidPage(
    tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
    # Application title
    titlePanel("CRAFTY-EU"),
    # headerPanel('Iris k-means clustering'),
    
    # p(),
    # actionButton("redraw", "Update plot"),
    # Sidebar with a slider input for number of bins
    # actionButton("redraw", "Update plot"),
    
    fluidRow(position = "left",
             column(4,
                    # sidebarPanel(
                    
                    sliderInput("Year",
                                "Year:",
                                min = 2016,
                                max = 2096,
                                value = 2016, step=10, animate =F),
                    sliderInput("alpha", "Opaqueness",0, 1,
                                value = 0.8, step = 0.05
                    ),
                    selectInput("paramset", "Paramset",
                                paramsets, selected = paramsets[1]
                    ),
                    selectInput("scenario", "Scenario",
                                scenario.names, selected = scenario.names[1]
                    ),
                    
                    selectInput("indicator", "Indicator", 
                                indicator.names[-c(17:18)], selected=indicator.names[16]
                    ), 
                    selectInput("colors", "Color Scheme",
                                rownames(subset(brewer.pal.info, category %in% c("seq", "div")))
                    )
                    # , checkboxInput("legend", "Show legend", TRUE)
                    # # )
                    
             ),
             column(8,
                    leafletOutput("MapPane",  width = "100%", height = 600)),
             column(8,
                    # Show a plot of the generated distribution
                    mainPanel(
                      plotOutput("PlotPane")
                    )
             )
    )            
  )
)

