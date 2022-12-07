#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(lubridate)
library(dplyr)
library(ggplot2)
library(cowplot)

# Load tables
veg <- readRDS("veg.RDS")
# Load processed data
met <- readRDS("met.RDS")
sapflux <- readRDS("sapflux.RDS")


# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  # Render a UI for selecting species
  output$dyn_species <- renderUI({
    temp <- veg %>%
      filter(site_name == input$site) %>%
      pull(veg_type)
    
    selectInput(inputId = "species",
                label = "Select species:", 
                choices = temp,
                selected = temp[1],
                multiple = TRUE)
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
  
  # Render a UI for selecting date range
  output$dyn_year_slider <- renderUI({
    temp <- sapflux %>%
      filter(veg_id %in% input$individuals) %>%
      pull(year)
    
    sliderInput(inputId = "year", 
                label = "Select Year:", 
                min = min(temp),
                max = max(temp),
                value = 2013,
                sep = "",
                step = 1,
                ticks = FALSE)
    
  })
  
  # Render a UI for selecting date range
  # output$dyn_date_slider <- renderUI({
  #   temp <- sap_all %>%
  #     filter(veg_id %in% input$individuals,
  #            year == input$year) %>%
  #     pull(date)
  #   
  #   sliderInput(inputId = "date_range", 
  #               label = "Select Date Range:", 
  #               min = min(temp),
  #               max = max(temp),
  #               value = c(min(temp),
  #                         max(temp)),
  #               width = '100%')
  # })
  
  # Filter data by inputs
  # Filter sapflux dataset based on inputs
  sap_filter <- reactive(sapflux %>%
    filter(site_name == input$site,
           veg_type %in% input$species,
           veg_id %in% input$individuals,
           year == input$year))
  
  # Filter met dataset based on inputs
  met_filter <- reactive(met %>%
    filter(site_name == input$site,
           year == input$year))

    output$vPlot <- renderPlot({
      
      #include the refresh button so plot updates when refresh is clicked
      input$refresh
      
      #use isolate() so changes to sap_sub() don't trigger the plot to update
      sap_sub <- isolate(sap_filter()) 
      met_sub <- isolate(met_filter())

        # Plot timeseries of raw voltages differences
        fig1 <- sap_sub %>%
          filter(doy >= input$day_range[1], 
                 doy <= input$day_range[2]) %>%
          ggplot() +
          geom_point(aes(x = timestamp,
                         y = vdelta,
                         color = probe_id,
                         shape = veg_type),
                     size =  0.2) +
          scale_x_datetime("Date") +
          scale_y_continuous(expression(paste(Delta, " V (mV)"))) +
          theme_bw(base_size = 14) +
          theme(legend.position = "top")
        
        # Plot timeseries of VPD
        fig2 <- met_sub %>%
          filter(doy >= input$day_range[1], 
                 doy <= input$day_range[2]) %>%
          ggplot() +
          geom_line(aes(x = timestamp,
                         y = vpd)) +
          scale_x_datetime("Date") +
          scale_y_continuous("VPD (kPa)") +
          theme_bw(base_size = 14)
        
       plot_grid(fig1, fig2, 
                 nrow = 2,
                 rel_heights = c(1.5, 1),
                 align = "v") 

    })
    
    

})
