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

# Load data
readRDS("veg.RDS")
# readRDS("probe.RDS")

# Define UI for application that draws a histogram
shinyUI(navbarPage("Pinyon-Juniper sapflux",

    # Tab panel
    tabPanel("Explore raw data",
             # Application title
             titlePanel("Pinyon-Juniper"),
             # Sidebar with a slider input for number of bins
             sidebarLayout(
               sidebarPanel(
                 selectInput("site",
                             "Select site:",
                             choices = unique(veg$site_name),
                             selected = "left"),
                 uiOutput("dyn_species"),
                 uiOutput("dyn_individuals"),
                 uiOutput("dyn_year_slider"),
                 actionButton("refresh", "Refresh Plot")
                 ),
               # Show a plot of the generated distribution
               mainPanel(
                 tabsetPanel(
                   tabPanel("All",
                            sliderInput(inputId = "day_range",
                                        label = "Select Day Range:",
                                        min = 1,
                                        max = 366,
                                        value = c(1, 366),
                                        width = '100%'),
                            plotOutput("vPlot")%>%
                              withSpinner(type = 8)), #loading indicator for plot),
                   tabPanel("Trimmed")
                 )
                 
                 )
             )
    )
))
