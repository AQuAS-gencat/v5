################################################################################
#
## Funcions per la creació de gràfics
#
################################################################################


#2. data unavailable function (for plotly charts )-----------------------------
# purpose: create a plot saying 'no data available' when nothing to plot
plot_nodata <- function(height_plot = 450) {
  text_na <-
    list(x = 5,y = 5,
         text = "La selecció actual no permet mostrar cap gràfic." ,
         size = 40,
         xref = "x",
         yref = "y",
         showarrow = FALSE
    )
  
  plot_ly(height = height_plot) %>%
    layout(
      annotations = text_na,
      #empty layout
      yaxis = list(
        showline = FALSE,
        showticklabels = FALSE,
        showgrid = FALSE,
        fixedrange = TRUE
      ),
      xaxis = list(
        showline = FALSE,
        showticklabels = FALSE,
        showgrid = FALSE,
        fixedrange = TRUE
      ),
      font = list(family = '"Helvetica Neue", Helvetica, Arial, sans-serif')
    ) %>%
    config(displayModeBar = FALSE) # taking out plotly logo and collaborate button
} 




#4. saving charts  ---------------------------------------------------------
# purpose: save charts as png
#savechart_button <- function(outputId, label = "Descarrega el gràfic", class=NULL, disabled = FALSE){
#  
#  tags$a(id = outputId, class = paste("btn btn-default shiny-download-link", class),
#         href = "", target = "_blank", download = NA, icon("image"), label)
#  
#}

#chart_controls_icon <- function(size = "1em") {
#  shiny::tags$span(
#    title = "Click here to customise chart options",  # HTML tooltip
#    bsicons::bs_icon(
#      name = "gear-fill",
#      size = size,
#      class = "chart-controls-icon"
#    )
#  )
#}


# Reusable function to handle PNG downloads for any chart
handlePlotlyDownload <- function(input_id, session, options = list()) {
  observeEvent(input[[input_id]], {
    # Default options
    default_options <- list(
      selector = '.js-plotly-plot',  # Default selector
      format = 'png',                # Default format
      filename = 'grafic',           # Default filename
      scale = 3                      # Default scale
    )
    
    # Merge provided options with defaults
    download_options <- modifyList(default_options, options)
    
    # Send the message with all options
    session$sendCustomMessage(type = "clickPlotlyDownload", message = download_options)
  })
}
