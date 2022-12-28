#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinycssloaders)
library(shinyWidgets)
library(dplyr)

# Load data
veg <- readRDS("veg.RDS")
uni.sites <- unique(veg$site_name)

# Define UI for application that draws a histogram
shinyUI(navbarPage("Pinyon-Juniper sapflux",
                   # in-code CSS to manipulate the base slider
                   # I got the css class for this by right-clicking on the handle and clicking
                   # inspect element then finding the appropriate class and fooling around with
                   # it in the inspector - trial and error
                   tags$head(
                     tags$style(type = "text/css",
                              ".irs--shiny .irs-handle {width: 14px; height: 14px; top: 22px;}")
                     ),

                   

    # Tab panel 1 - Explore
    tabPanel("Explore raw data",
             # Application title
             titlePanel("All sites"),
             # Can use a different theme provided by shinyWidgets package that axes
             # the bubble
             # chooseSliderSkin("Flat"),
             # Sidebar with a slider input for number of bins
             sidebarLayout(
               sidebarPanel(
                 selectInput("site",
                             "Select site:",
                             choices = uni.sites,
                             selected = "left"),
                 uiOutput("dyn_species"),
                 uiOutput("dyn_individuals"),
                 uiOutput("dyn_year_slider"),
                 actionButton("refresh", "Refresh Plot")
                 ),
               # Show a plot of the generated distribution
               mainPanel(
                 tabsetPanel(
                   tabPanel("Trimmed",
                            sliderInput(inputId = "day_range",
                                        label = "Select Day Range:",
                                        min = 1,
                                        max = 366,
                                        value = c(1, 366),
                                        width = '100%'),
                            plotOutput("vPlot",
                                       height = "600px") %>%
                              withSpinner(type = 8)) #loading indicator for plot),
                
                 )
                 
                 )
             )
    ),
    
    # Tab panel 2 - Baseline
    tabPanel("Baseline data",
             # Application title
             titlePanel("Pinyon-Juniper sites"),
             sidebarLayout(
               sidebarPanel(
                 selectInput("site2",
                             "Select site:",
                             choices = c("left", "right", "upland", "valley"),
                             selected = "left"),
                 uiOutput("dyn_species2"),
                 uiOutput("dyn_individuals2"),
                 uiOutput("dyn_sensor"),
                 sliderInput(inputId = "year2", 
                             label = "Select Year:", 
                             min = 2013,
                             max = 2018,
                             value = 2013,
                             sep = "",
                             step = 1,
                             ticks = TRUE),
                 sliderInput(inputId = "dr_interval",
                             label = "Select double regression interval:",
                             min = 1,
                             max = 7,
                             value = 3, 
                             step = 1,
                             ticks = FALSE),
                 actionButton("refresh2", "Refresh Plot")
               ),
               # Show a plot of the generated distribution
               mainPanel(
                 h4(textOutput("Baselining with double regression from TREX")),
                 uiOutput("dyn_dayrange2"),
                 plotOutput("drPlot",
                            height = "600px") %>%
                   withSpinner(type = 8)
                 )
             )
    )
))
