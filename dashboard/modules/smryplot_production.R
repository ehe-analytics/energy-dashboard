# Summary tab Energy Production module

#' Inputs for Energy Production plots interface
#' 
#' @param id, character used to specify name space
#' @param view_choices, series choices to display
#' @param states, state choices to display
#' 
#' \describe{
#'   \item{smryplot_viewby}{user input filter for series}
#'   \item{smryplot_state}{list of states/regions}
#'   \item{smryplot}{Energy production summary plot output based on the parameters provided by user}
#' } 
#' 
productionUI <- function(id, view_choices, states) { 
  ns <- NS(id)
  
  tabPanel(
    'Energy Production', 
    tags$h5('Choose a state to see energy production for that state.'), 
    
    # inputs
    column(3, selectizeInput(ns('smryplot_state'), label = NULL, choices = c('Select state' = '', states))),
    column(3, selectizeInput(ns('smryplot_viewby'), label = NULL, choices = c('Select one or more filters' = '', view_choices), multiple = T)), 
    
    # output
    ggiraphOutput(ns('smryplot'), width = '100%', height = '600px')
  )
}

#' Server-side processing for generating energy production summary plot
productionServer <- function(id, dat) { 
  
  moduleServer(
    id, 
    function(input, output, session) {
      
      # Plot output
      output$smryplot <- renderGirafe({ 
        req(input$smryplot_viewby)
        req(input$smryplot_state)
        
        plotdata <- dat %>% 
          filter(series %in% input$smryplot_viewby, 
                 state == input$smryplot_state) %>% 
          mutate(tooltip = paste0(round(per_capita, 1), ' ', units, ' (', period, ')'))
        
        plt <- ggplot(plotdata, aes(x = period, y = per_capita, color = series)) + 
          geom_point_interactive(aes(tooltip = tooltip, data_id = series), alpha = 0.6) +
          geom_textsmooth(aes(label = series), 
                          method = 'loess', formula = y ~ x, se = F, span = 0.3, 
                          linewidth = 1, hjust = 'auto', alpha = 0.8) +
          labs(x = 'Year', y = 'Billion Btu', 
               title = paste0('Per Capita ', input$smryplot_viewby, ' Energy production'), 
               subtitle = input$smryplot_state) +
          theme_bw(base_family = 'Arial') + 
          theme(legend.position = 'none', 
                text = element_text(size = 12))
        
        girafe(
          ggobj = plt, 
          options = list(opts_hover_inv(css = 'opacity:0.3;'), 
                         opts_selection(type = 'single'))
        )
      })
    }
  )  
}