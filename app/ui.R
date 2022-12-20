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

                   

    # Tab panel
    tabPanel("Explore raw data",
             # Application title
             titlePanel("Pinyon-Juniper"),
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
                              withSpinner(type = 8)), #loading indicator for plot),
                   tabPanel("Baselined",
                            sliderInput(inputId = "day_range2",
                                        label = "Select Day Range:",
                                        min = 1,
                                        max = 366,
                                        value = c(1, 366),
                                        width = '100%'))
                 )
                 
                 )
             )
    )
))
