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
library(TREXr)

# Load tables
veg <- readRDS("veg.RDS")
# Load processed data
met <- readRDS("met_composite.RDS")
sapflux <- readRDS("sapflux.RDS")
# Load data to baseline
baseline <- readRDS("baseline.RDS")


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
  
  # Render a UI for selecting species on tab 2
  output$dyn_species2 <- renderUI({
    temp <- veg %>%
      filter(site_name == input$site2) %>%
      pull(veg_type)
    
    selectInput(inputId = "species2",
                label = "Select species:", 
                choices = temp,
                selected = temp[1],
                multiple = TRUE)
  })
  
  # Render a UI for selecting individuals on tab 2
  output$dyn_individuals2 <- renderUI({
    req(input$species2)
    temp <- veg %>%
      filter(site_name == input$site2,
             veg_type == input$species2,
             !is.na(veg_type)) %>%
      pull(veg_id)
    
    selectInput(inputId = "individuals2",
                label = "Select individuals:",
                choices = temp,
                selected = temp,
                multiple = FALSE)
  })
  
  # Render a UI for selecting sensors  on tab 2
  output$dyn_sensor <- renderUI({
    req(input$individuals2)
    temp <- sapflux %>%
      filter(site_name == input$site2,
             veg_type == input$species2,
             veg_id == input$individuals2) %>%
      select(probe_id) %>%
      unique() %>%
      pull()
    
    selectInput(inputId = "sensor", # only one at a time
                label = "Select sensor:",
                choices = temp,
                selected = temp[1],
                multiple = FALSE)
  })
  
  # Render a UI for selecting day of year range
  
  output$dyn_dayrange2 <- renderUI({
    
    # updates when refresh is clicked
    input$refresh2
    
    temp <- trex() %>%
      select(doy) %>%
      range() 
    
    sliderInput(inputId = "day_range2",
                label = "Select Day Range:",
                min = temp[1],
                max = temp[2],
                value = temp,
                width = '100%')
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
    filter(year == input$year))

    output$vPlot <- renderPlot({
      req(input$refresh)
      
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
          theme_bw(base_size = 16) +
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
          theme_bw(base_size = 16)
        
       plot_grid(fig1, fig2, 
                 nrow = 2,
                 rel_heights = c(1.75, 1),
                 align = "v") 

    })
    
    # Reactive function to extract list of is.trex objects by sensor name, baseline
    trex <- reactive({
      req(input$sensor)
      
      dr_list <- list()
      for(i in 1:length(baseline[[input$sensor]])) { 
        dr_out <- tdm_dt.max(as.data.frame(baseline[[input$sensor]][[i]]), 
                                methods = c("pd", "mw", "dr"),
                                interpolate = FALSE,
                                max.days = input$dr_interval, 
                                df = TRUE)
        
        dr_list[[i]] <- cbind.data.frame(timestamp = as.POSIXct(dr_out$input$timestamp, tz = "GMT"),
                                         vdelta = dr_out$input$value, 
                                         max.dr = dr_out$max.dr$value,
                                         max.mw = dr_out$max.mw$value,
                                         max.pd = dr_out$max.pd$value)
      }
      
      do.call(rbind, dr_list) %>%
        mutate(timestamp = with_tz(timestamp, tzone = "America/Los_Angeles"),
               doy = yday(timestamp),
               year = year(timestamp)) %>%
        filter(year == input$year2)
      
    })
    
    # Filter met dataset based on inputs
    # Currently using composite data, averaged between right and left
    met_filter2 <- reactive(met %>%
                              year == input$year2)
    
    # Plot baseline with double regression and raw data
    
    output$drPlot <- renderPlot({
      req(input$refresh2)
      
      #include the refresh button so plot updates when refresh is clicked
      input$refresh2
      
      #use isolate() so changes to trex() don't trigger the plot to update
      dr <- isolate(trex()) 
      met_sub <- isolate(met_filter2())
      
      
      # Plot timeseries of raw voltages differences
      fig1 <- dr %>%
        filter(doy >= input$day_range2[1], 
               doy <= input$day_range2[2]) %>%
        ggplot() +
        geom_point(aes(x = timestamp,
                       y = vdelta),
                   size =  0.2) +
        geom_step(aes(x = timestamp,
                      y = max.pd, 
                      col = "Predawn")) +
        geom_step(aes(x = timestamp,
                      y = max.dr, 
                      col = "Double regression")) +
        geom_step(aes(x = timestamp,
                      y = max.mw, 
                      col = "Moving window")) +
        scale_x_datetime("Date") +
        scale_y_continuous(expression(paste(Delta, " V (mV)"))) +
        theme_bw(base_size = 16) +
        theme(legend.position = "top")
      
      # Plot timeseries of VPD
      fig2 <- met_sub %>%
        filter(doy >= input$day_range2[1], 
               doy <= input$day_range2[2]) %>%
        ggplot() +
        geom_line(aes(x = timestamp,
                      y = vpd)) +
        scale_x_datetime("Date") +
        scale_y_continuous("VPD (kPa)") +
        theme_bw(base_size = 16)
      
      plot_grid(fig1, fig2, 
                nrow = 2,
                rel_heights = c(1.25, 1),
                align = "v") 
      
    })
    

})
