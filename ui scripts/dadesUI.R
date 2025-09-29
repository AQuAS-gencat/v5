###############################################
#
# UI for data tab
#
###############################################

descarreguesTab <- nav_panel(
  title = "Dades",  # Simplified title without icon
  value = "dades",
  
  
  div(style = "margin:10px;",
      
      # Main Layout: Sidebar on Left, Main Content on Right
      layout_sidebar(
        fillable = F,
        
        sidebar = sidebar(
          width = '25%',
          
          accordion(
            
            multiple = TRUE,  # allows multiple panels to stay open
            open = c("que_panel_dades", "geo_panel_dades", "interval_panel_dades"),
            
            accordion_panel(
              value = "que_panel_dades",
              "Pas 1: Selecciona els indicadors o àmbits que vulguis mostrar a la taula",
              awesomeRadio("product_filter", label=NULL, choices = c("Indicador", "Àmbit", "Dimensió"), selected = NULL, inline = FALSE,
                           status = "primary", checkbox = TRUE),
              conditionalPanel(condition="input.product_filter=='Indicador'",
                               selectizeInput("indicator_filter", label = NULL,
                                              choices = indicadors, selected = NULL,
                                              multiple=TRUE, options = list(maxOptions = 1000, placeholder = "Selecciona o escriu indicadors"))
                               
              ),
              conditionalPanel(condition="input.product_filter=='Àmbit'",
                               selectizeInput("ambit_filter", label = NULL,
                                              choices = ambits, selected = NULL,
                                              multiple=TRUE, options = list(maxOptions = 1000, placeholder = "Selecciona o escriu àmbits"))    
              ),
              conditionalPanel(condition="input.product_filter=='Dimensió'",
                               selectizeInput("dimensio_filter", label = NULL,
                                              choices = dimensions, selected = NULL,
                                              multiple=TRUE, options = list(maxOptions = 1000, placeholder = "Selecciona o escriu dimensions"))    
              )
            ),
            
            accordion_panel(
              value = "geo_panel_dades",
              "Pas 2: Selecciona les unitats geogràfiques d'anàlisi",
              awesomeCheckbox("catalunya",label = "Catalunya", value = FALSE),
              # Panel for RS selections
              awesomeCheckbox("rs",label = "Regió Sanitària", value = FALSE),
              conditionalPanel(
                condition = "input.rs == true",
                selectizeInput("rs_true", label = NULL,
                               choices = rs, selected = NULL, multiple=TRUE),
                options = list(placeholder = "Selecciona o escriu les regions sanitàries que vulguis mostrar")),
              # Panel for AGA selections
              awesomeCheckbox("aga", label = "Àrea de Gestió Assistencial", value = FALSE),
              conditionalPanel(
                condition = "input.aga == true",
                selectizeInput("aga_true", label = NULL,
                               choices = aga, selected = NULL, multiple=TRUE, 
                               options = list(placeholder = "Selecciona o escriu les AGA que vulguis mostrar"))),
              # Panel for ABS selections
              awesomeCheckbox("abs",label = "Àrea Bàsica de Salut", value = FALSE),
              conditionalPanel(
                condition = "input.abs == true",
                selectizeInput("abs_true", label = NULL,
                               choices = abs, selected = NULL, multiple=TRUE,
                               options = list(placeholder = "Selecciona o escriu les ABS que vulguis mostrar"))),
              # Panel for centre selections
              awesomeCheckbox("centre",label = "Centre (Unitat proveïdora)", value = FALSE),
              conditionalPanel(
                condition = "input.centre == true",
                selectizeInput("centre_true", label = NULL, choices = centre, 
                               selected = NULL, multiple=TRUE)),
              # To select all available geographies
              awesomeCheckbox("all_geo",label = "Tots els nivells geogràfics disponibles", value = FALSE)
            ),
            
            accordion_panel(
              value = "interval_panel_dades",
              "Si ho vols, selecciona un període temporal concret",
              #sliderInput(
              #  inputId = "year_range_estrat",
              #  label = NULL,
              #  #label = "Tria un interval:",
              #  #shiny::HTML("<p class='step-text'>Tria un interval</p>"), 
              #  min = 2016,  # Placeholder value
              #  max = 2024,  # Placeholder value
              #  value = c(2016, 2024),  # Placeholder value
              #  step = 1,
              #  ticks = F,
              #  sep = ""
              #),
              uiOutput("date_slider_ui")
            )
            
          ),
          
          actionLink(
            inputId = "clear",
            label = HTML('<i class="fas fa-eraser"></i> Esborra tots els filtres'),
          ),
          actionLink(
            inputId = "download_table_excel",
            label = "Descarrega la taula filtrada",
          ),
          actionLink(
            inputId = "download_all_csv",
            label = "Descarrega la base dades completa",
          )
          
          #br(),
          
        ),
        
        
        fluidRow(  
          br(),
          column(12, div(DTOutput("table_filtered"), 
                         style = "font-size: 98%; width: 98%"))
        )
        

        
        
      )
      
      
  )
  
  
)

