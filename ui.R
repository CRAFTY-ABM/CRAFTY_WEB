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

#import data
# data <- read.csv("worldearthquakes.csv")

# Define UI for application that draws a histogram
shinyUI(
  fluidPage(
    tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
    # Application title
    titlePanel("CRAFTY-EU"),
    # p(),
    actionButton("recalc", "Year"),
    # Sidebar with a slider input for number of bins
    
    sidebarLayout(position = "left",
                  leafletOutput("mymap",  width = "100%", height = 500),
                  
                  sidebarPanel(
                    sliderInput("Year",
                                "Year:",
                                min = 2016,
                                max = 2100,
                                value = 2100-2016 + 1, step=1, animate = T),
                    sliderInput("range", "Magnitudes", min(quakes$mag), max(quakes$mag),
                                value = range(quakes$mag), step = 0.1
                    ),
                    selectInput("colors", "Color Scheme",
                                rownames(subset(brewer.pal.info, category %in% c("seq", "div")))
                    ),
                    checkboxInput("legend", "Show legend", TRUE)
                  )
                  
                  # Show a plot of the generated distribution
                  # mainPanel(
                  #     plotOutput("distPlot")
                  # )
    )
  )
)
