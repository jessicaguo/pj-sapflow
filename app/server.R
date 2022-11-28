#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Load data
readRDS("veg.RDS")
readRDS("probe.RDS")

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  # Render a UI for selecting species
  output$dyn_species <- renderUI({
    temp <- veg %>%
      filter(site_name == input$site) %>%
      pull(veg_type)
    
    selectInput(inputId = "species",
                label = "Select species:", 
                choices = temp)
  })
  
  # Render a UI for selecting individuals
  output$dyn_individuals <- renderUI({
    temp <- veg %>%
      filter(site_name == input$site,
             veg_type == input$species) %>%
      pull(veg_id)
    
    checkboxGroupInput(inputId = "individuals",
                label = "Select individuals:", 
                choices = temp,
                selected = temp)
  })

    output$distPlot <- renderPlot({

        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white',
             xlab = 'Waiting time to next eruption (in mins)',
             main = 'Histogram of waiting times')

    })

})
