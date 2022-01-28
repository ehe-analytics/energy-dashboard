server <- function(input, output, session) { 
  ## INTERACTIVE MAP -----------------------------------------------------------
  
  
  #### Create data sets and update UI components--------------------------------
  rvs <- reactiveValues()
  
  # Filter data for the DATA type chosen
  observe({
    req(input$mapdata_name)

    rvs$mapdata_name <- all_data %>% filter(data_name == input$mapdata_name)
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
      # left_join(state_pop, by = 'geoid') %>% 
      # mutate(value_by_pop = value*1e6/population)
    sf::st_geometry(mapdata) <- mapdata$geometry
    
    # Variables
    if (input$mapdata_smry == "Per capita") {
      fill_colour <- 'per_capita' 
      mapdata <- mapdata %>% 
        mutate(tooltip = paste(round(per_capita, 1), per_capita_units, 'per capita in', unique(period)))
      legend_options <- list(title = paste0(unique(mapdata$per_capita_units)))
    } else if (input$mapdata_smry == 'Total quantity') {
      fill_colour <- 'value'
      mapdata <- mapdata %>% 
        mutate(tooltip = paste(round(value, 1), units, 'in', unique(period)))
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
  
  observe({ 
    req(input$smryplot_trendby) 
    
    choices <- if (input$smryplot_trendby == 'Fuel') {
      unique(all_data$category)
    } else if (input$smryplot_trendby == 'Sector') { 
      unique(all_data$series)
    }
    selected <- isolate(input$smryplot_viewby)
    updateSelectizeInput(session, 'smryplot_viewby', selected = selected, choices = choices, server = T)
  })
  
  output$smryplot_co2 <- renderGirafe({
    req(input$smryplot_trendby)
    req(input$smryplot_viewby)
    req(input$smryplot_state)
    
    col_to_filter <- case_when(
      input$smryplot_trendby == 'Fuel' ~ 'category', 
      input$smryplot_trendby == 'Sector' ~ 'series') 
    
    col_to_group <- case_when(
      input$smryplot_trendby == 'Fuel' ~ 'series', 
      input$smryplot_trendby == 'Sector' ~ 'category') 

    plotdata <- all_data %>% 
      filter(data_name == 'CO2 emissions') %>% 
      filter(.data[[col_to_filter]] == input$smryplot_viewby) %>% 
      filter(state == input$smryplot_state) 
      
    plt <- ggplot(plotdata, aes(x = period, y = value, color = .data[[col_to_group]] )) + 
      geom_point_interactive(aes(tooltip = paste(round(value, 1), unitsshort), data_id = .data[[col_to_group]] ), alpha = 0.6) + 
      geom_textsmooth(aes(label = .data[[col_to_group]] ), method = 'loess', formula = y ~ x, se = F, linewidth = 1, span = 0.3, hjust = 'auto') +
      labs(x = 'Year', y = 'CO2 (million metric tons)', 
           title = paste0('CO2 Emissions For ', input$smryplot_viewby, ' Usage'), 
           subtitle = input$smryplot_state) +
      theme_bw(base_family = 'Arial') + 
      theme(legend.position = 'none', 
            text = element_text(size = 12))
    
    girafe(
      ggobj = plt, 
      options = list(
        opts_hover_inv(css = 'opacity:0.3;'), 
        opts_selection(type = 'single')
      )
    )
  })
  
  

}