server <- function(input, output, session) { 
  ## INTERACTIVE MAP -----------------------------------------------------------
  
  
  #### Create data sets and update UI components--------------------------------
  rvs <- reactiveValues()
  
  # Filter data for the DATA type chosen
  observe({
    req(input$mapdata_name)

    rvs$mapdata_name <- co2_emissions %>% filter(data_name == input$mapdata_name)
  })
  
  # update CATEGORY dropdown choices based on selected data type
  observe({
    req(rvs$mapdata_name)
    req(input$mapdata_name)

    choices  <-  unique(rvs$mapdata_name$category)
    selected <- isolate(input$mapdata_cat)
    updateSelectizeInput(session, 'mapdata_cat', selected = selected, choices = choices, server = T)
  })
  
  # Filter data for the CATEGORY type chosen
  observe({ 
    req(rvs$mapdata_name)
    req(input$mapdata_cat)
    
    rvs$mapdata_cat <- rvs$mapdata_name %>% 
      filter(category == input$mapdata_cat)
  })
  
  # update SERIES dropdown choices based on selected category type
  observe({
    req(input$mapdata_cat)
    req(rvs$mapdata_cat)

    choices  <-  unique(rvs$mapdata_cat$series)
    selected <- isolate(input$mapdata_series)
    updateSelectizeInput(session, 'mapdata_series', selected = selected, choices = choices, server = T)
  })
  
  # Filter data for the SERIES type chosen
  observe({ 
    req(input$mapdata_series)
    
    rvs$mapdata_series <- rvs$mapdata_cat %>% 
      filter(series == input$mapdata_series) 
  })
  
  
  #### Map ---------------------------------------------------------------------
  output$map <- renderMapdeck({ 
    token = "pk.eyJ1IjoiYWtzaW5naGFsODgiLCJhIjoiY2tuZHgyeWxyMWEycDJwbzB1dDBqMGR0NiJ9.XFjK_TTS-nKfFYkQY70wIQ"
    mapdeck(style = mapdeck_style('dark'), token = token) %>%
      mapdeck_view(location = c(-100, 40), zoom = 4, pitch = 10)
  })
  
  observe({
    req(input$mapdata_series)
    
    # Palette
    pal <- colorRamp(c("#00AFBB", "#E7B800", "#FC4E07"), alpha = T)((1:256)/256)
    pal[, 4] <- pal[, 4]*0.8
    
    # Data for mapping
    mapdata <- rvs$mapdata_series %>% 
      filter(period == max(period)) %>% 
      left_join(sf_states, by = 'state_abb') 
      
    sf::st_geometry(mapdata) <- mapdata$geometry
    
    # Variables
    if (input$mapdata_smry == "Per capita") {
      fill_colour <- 'per_capita' 
      mapdata <- mapdata %>%
        arrange(desc(per_capita)) %>% 
        mutate(rank = 1:n()) %>% 
        mutate(tooltip = paste0('<i>', state, '</i>', '<br>',
                                ' CO2 emissions in ', period, '<br>',
                                input$mapdata_cat, '; ', input$mapdata_series, '<br>',
                                round(per_capita, 1), ' metric tons per capita (#', rank, ')'))
      legend_options <- list(title = paste0(unique(mapdata$per_capita_units)))
    } else if (input$mapdata_smry == 'Total quantity') {
      fill_colour <- 'value'
      mapdata <- mapdata %>% 
        arrange(desc(value)) %>% 
        mutate(rank = 1:n()) %>% 
        mutate(tooltip = paste0('<i>', state, '</i>', '<br>',
                                ' CO2 emissions in ', period, '<br>',
                                input$mapdata_cat, '; ', input$mapdata_series, '<br>',
                                round(value, 1), ' million metric tons', ' (#', rank, ')'))
      legend_options <- list(title = paste0('Total ', unique(mapdata$units)))
    } else {
      stop('This option does not exist.')
    }
    
    
    # Map
    mapdeck_update(map_id = 'map') %>%
      clear_polygon('statefill') %>%
      add_polygon(
        mapdata,
        fill_colour = fill_colour, 
        stroke_colour = '#FFFFFF',
        stroke_width = 2000,
        auto_highlight = T,
        highlight_colour = '#FFFFFF26',
        # fill_opacity = 0.8,
        palette = pal, 
        tooltip = "tooltip", 
        update_view = F, 
        legend = list(fill_colour = T, stroke_colour = F),
        legend_options = legend_options,
        layer_id = 'statefill'
      )
  })
  
  
  
  ## SUMMARY CHARTS-------------------------------------------------------------
  productionServer('production', production)
  co2EmissionsServer('co2_emissions', co2_emissions)
}