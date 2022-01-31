ui <- navbarPage(
  'EH&E Energy Dashboard', 
  id = 'nav', 
  
  ## INTERACTIVE MAP ---------------------------------------------------------
  tabPanel(
    'Interactive Map', icon = icon('map'), 
    
    # useShinyjs(), 
    # includeScript('script.js'),
    
    div(
      class = 'outer',
      tags$head(includeCSS('styles.css')),
      
      mapdeckOutput('map', width = '100%', height = '100%')
    ),
    
    
    #### Map Navigation Panel ####
    absolutePanel(
      id = 'controls', draggable = T, 
      left = 'auto', right = 20, bottom = 'auto', width = 250, height = 'auto', 
      
      h3('Map Tools', icon('map')), 
      selectizeInput('mapdata_name', label = NULL, choices = c('Select data type' = '', unique(co2_emissions$data_name))), 
      selectizeInput('mapdata_cat', label = NULL, choices = c('Select data category' = '')),
      selectizeInput('mapdata_series', label = NULL, choices = c('Select data series' = '')), 
      radioButtons('mapdata_smry', label = NULL, choices = c("Per capita", "Total quantity"), selected = "Per capita", inline = T)
    )
  
  ), 
  
  ## SUMMARY CHARTS ----------------------------------------------------------
  tabPanel(
    'Summary Charts', icon = icon('chart-bar'),
    tags$h4("Plots for each of the data categories are provided in the tabs below. ", 
            "Select the appropriate tab and select options therein for the plot to be generated. "), 
    tags$h4("Plots can be downloaded by clicking on the download button that appears on the far top right corner when hovering above the plot."), 
    tags$br(), 
    
    tabsetPanel(
      id = 'smrytabs', 
      
      co2EmissionsUI('co2_emissions', states = unique(co2_emissions$state)),
      # tabPanel(
      #   'CO2 Emissions',
      #   tags$h5('Choose a state below to see carbon dioxide emissions trends. You may also select US for the whole country.'),
      #   tags$h5("Carbon dioxide emissions in the EIA databsae are reported by five different sectors: ",
      #           "commercial, electric power, industrial, residential, transportation, and from all sectors combined."),
      #   column(3, selectizeInput('smryplot_trendby', label = NULL, choices = c('Trend by' = '', 'Fuel', 'Sector'))),
      #   column(3, selectizeInput('smryplot_viewby', label = NULL, choices = c('Select filter' = ''))),
      #   column(3, selectizeInput('smryplot_state', label = NULL, choices = c('Select state' = '', unique(co2_emissions$state)))),
      #   ggiraphOutput('smryplot_co2', width = '100%', height = '600px')
      # ),
      
      tabPanel(
        'Tab 2'
      )
    )
  ), 
  
  ## ABOUT ---------------------------------------------------------------------
  tabPanel(
    'About', icon = icon('info'), 
    tags$h3("TEMPORARY PLACEHOLDER..."),
    tags$p('Created by a team of air quality junkies and data science nerds at Environmental Health & Engineering, Inc.'), 
    tags$p('The code for this dashboard is available on our', tags$a('Github', href='https://github.com/ehe-analytics/energy-dashboard'), 'page.')
  )
)
