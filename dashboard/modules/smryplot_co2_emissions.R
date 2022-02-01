# Summary tab CO2 emissions module

#' Inputs for CO2 summary plots interface
#' 
#' @param id, character used to specify name space
#' @param states, state choices to display
#' 
#' \describe{
#'   \item{smryplot_trendby}{user input for trends options between Fuel and Sector}
#'   \item{smryplot_viewby}{user input filter based on trends options selected. This is updated in the server function}
#'   \item{smryplot_state}{list of states/regions}
#'   \item{smryplot}{CO2 emissions summary plot output based on the parameters provided by user}
#' } 
#' 
co2EmissionsUI <- function(id, states) { 
  ns <- NS(id)
  
  tabPanel(
    'CO2 Emissions', 
    tags$h5('Choose a state below to see carbon dioxide emissions trends. You may also select US for the whole country.'), 
    tags$h5("Carbon dioxide emissions in the EIA databsae are reported by five different sectors: ", 
            "commercial, electric power, industrial, residential, transportation, and from all sectors combined."), 
    
    # inputs
    column(3, selectizeInput(ns('smryplot_trendby'), label = NULL, choices = c('Trend by' = '', 'Fuel', 'Sector'))),
    column(3, selectizeInput(ns('smryplot_viewby'), label = NULL, choices = c('Select filter' = ''))), 
    column(3, selectizeInput(ns('smryplot_state'), label = NULL, choices = c('Select state' = '', states))), 
    
    # output
    ggiraphOutput(ns('smryplot'), width = '100%', height = '600px')
  )
}


#' Server-side processing for updating inputs and generating co2 emissions summary plot
co2EmissionsServer <- function(id, dat) { 

  moduleServer(
    id, 
    function(input, output, session) { 
      
      # Update drop down menus based on user selection
      observe({ 
        req(input$smryplot_trendby) 

        choices <- if (input$smryplot_trendby == 'Fuel') {
          unique(dat$category)
        } else if (input$smryplot_trendby == 'Sector') { 
          unique(dat$series)
        }
        selected <- isolate(input$smryplot_viewby)
        updateSelectizeInput(session, 'smryplot_viewby', selected = selected, choices = choices, server = T)
      })
      
      
      # Plot output
      output$smryplot <- renderGirafe({
        req(input$smryplot_trendby)
        req(input$smryplot_viewby)
        req(input$smryplot_state)
        
        col_to_filter <- case_when(
          input$smryplot_trendby == 'Fuel' ~ 'category', 
          input$smryplot_trendby == 'Sector' ~ 'series') 
        
        col_to_group <- case_when(
          input$smryplot_trendby == 'Fuel' ~ 'series', 
          input$smryplot_trendby == 'Sector' ~ 'category') 
        
        plotdata <- dat %>%
          filter(.data[[col_to_filter]] == input$smryplot_viewby, 
                 state == input$smryplot_state) %>% 
          mutate(tooltip = paste0(round(per_capita, 1), ' metric tons CO2 (', period, ')'))
        
        plt <- ggplot(plotdata, aes(x = period, y = per_capita, color = .data[[col_to_group]] )) + 
          geom_point_interactive(aes(tooltip = tooltip, data_id = .data[[col_to_group]] ), alpha = 0.6) +
          geom_textsmooth(aes(label = .data[[col_to_group]]), 
                          method = 'loess', formula = y ~ x, se = F, span = 0.3, 
                          linewidth = 1, hjust = 'auto') +
          labs(x = 'Year', y = 'Metric tons', 
               title = paste0('Per Capita CO2 Emissions For ', input$smryplot_viewby, ' Usage'), 
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
  )
}