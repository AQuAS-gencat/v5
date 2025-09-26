###############################################
#
# UI for data tab
#
###############################################


descarreguesTab <- 
  
  nav_panel(
    #div(
    #div(role = "navigation"),
    #"Dades"),
    title = "Descàrregues",
    value = "descarregues",
    #icon = icon("download"),
    
    div(style = "margin:10px;",
        h2("Dades", 
           style = "font-weight: bold;"),
        
        # Upper part: selectors, explanation, and buttons
        fluidRow(
          # Left column for selectors
        ),
        
        # Main panel for table display
        mainPanel(
          width = 12,
          class = "tableContainer",
          style = "margin-left:10px; margin-right:10px;",
          
        )
    ),
    br()
    #  ) # well panel
  )

#    #Sidepanel for filtering data
#    mainPanel(
#      width = 12, style="margin-left:0.5%; margin-right:0.5%",
#      #Row 1 for intro  
#      fluidRow(
#        h2("Explora les dades de la plataforma en format taula i descarrega-les", 
#           style = "font-weight: bold;"),
#        br(),
#        column(4,
#               div(
#                 p(style = "font-size: 14px; padding: 10px; background-color: #FFE7CE; border-left: 5px solid #FFA500; margin-bottom: 15px;", 
#                   'Fes servir els filtres de sota per triar les dades que vulguis previsualitzar.'),
#               ),
#               ),
#        column(8),
#
#        br()
#      ),
#      #Row 2 for selections
#      fluidRow(
#        column(3,
#               p("Pas 1: Selecciona els indicadors o àmbits que vulguis mostrar a la taula", style = "font-weight: bold; color: black;"),  
#               #               div("All available indicators will be displayed for
#               #                 selected geography if none specified"),
#               awesomeRadio("product_filter", label=NULL, choices = c("Indicador", "Àmbit", "Dimensió"), selected = NULL, inline = FALSE,
#                            status = "primary", checkbox = TRUE),
#               conditionalPanel(condition="input.product_filter=='Indicador'",
#                                selectizeInput("indicator_filter", label = NULL,
#                                               choices = indicadors, selected = NULL,
#                                               multiple=TRUE, options = list(maxOptions = 1000, placeholder = "Selecciona o escriu indicadors"))
#                                
#               ),
#               conditionalPanel(condition="input.product_filter=='Àmbit'",
#                                selectizeInput("ambit_filter", label = NULL,
#                                               choices = ambits, selected = NULL,
#                                               multiple=TRUE, options = list(maxOptions = 1000, placeholder = "Selecciona o escriu àmbits"))    
#               ),
#               conditionalPanel(condition="input.product_filter=='Dimensió'",
#                                selectizeInput("dimensio_filter", label = NULL,
#                                               choices = dimensions, selected = NULL,
#                                               multiple=TRUE, options = list(maxOptions = 1000, placeholder = "Selecciona o escriu dimensions"))    
#               )
#        ),# column bracket
#        
#        
#        
#        
#        column(3,
#               p("Pas 2: Selecciona les unitats geogràfiques d'anàlisi", style = "font-weight: bold; color: black;"),
#               # Catalunya selections
#               awesomeCheckbox("catalunya",label = "Catalunya", value = FALSE),
#               # Panel for RS selections
#               awesomeCheckbox("rs",label = "Regió Sanitària", value = FALSE),
#               conditionalPanel(
#                 condition = "input.rs == true",
#                 selectizeInput("rs_true", label = NULL,
#                                choices = rs, selected = NULL, multiple=TRUE),
#                 options = list(placeholder = "Selecciona o escriu les regions sanitàries que vulguis mostrar")),
#               # Panel for AGA selections
#               awesomeCheckbox("aga", label = "Àrea de Gestió Assistencial", value = FALSE),
#               conditionalPanel(
#                 condition = "input.aga == true",
#                 selectizeInput("aga_true", label = NULL,
#                                choices = aga, selected = NULL, multiple=TRUE, 
#                                options = list(placeholder = "Selecciona o escriu les AGA que vulguis mostrar"))),
#               # Panel for ABS selections
#               awesomeCheckbox("abs",label = "Àrea Bàsica de Salut", value = FALSE),
#               conditionalPanel(
#                 condition = "input.abs == true",
#                 selectizeInput("abs_true", label = NULL,
#                                choices = abs, selected = NULL, multiple=TRUE,
#                                options = list(placeholder = "Selecciona o escriu les ABS que vulguis mostrar"))),
#               # Panel for centre selections
#               awesomeCheckbox("centre",label = "Centre (Unitat proveïdora)", value = FALSE),
#               conditionalPanel(
#                 condition = "input.centre == true",
#                 selectizeInput("centre_true", label = NULL, choices = centre, 
#                                selected = NULL, multiple=TRUE)),
#               # Panel for UT selections
#               awesomeCheckbox("ut",label = "Unitat territorial (protecció de la salut)", value = FALSE),
#               conditionalPanel(
#                 condition = "input.ut == true",
#                 selectizeInput("ut_true", label = NULL, choices = ut, 
#                                selected = NULL, multiple=TRUE)),
#               # Panel for USL selections
#               awesomeCheckbox("usl",label = "Unitat de Salut Laboral", value = FALSE),
#               conditionalPanel(
#                 condition = "input.usl == true",
#                 selectizeInput("usl_true", label = NULL, choices = usl, 
#                                selected = NULL, multiple=TRUE)),
#               # To select all available geographies
#               awesomeCheckbox("all_geo",label = "Tots els nivells geogràfics disponibles", value = FALSE)
#               
#               
#        ), # column bracket
#        column(3,
#               p("Si ho vols, selecciona un període temporal concret", style = "font-weight: bold; color: black;"),
#               uiOutput("date_slider_ui")
#               #sliderInput("date_from",label = NULL, min = min_year, 
#              #             max = max_year, value = c(min_year,max_year), 
#              #             step = 1, sep="", round = TRUE, 
#              #             ticks = TRUE, dragRange = FALSE)
#
#               
#        ), #column bracket
#        column(3, style = "width:20%",
#               br(),
#               div(title = "Esborra tots els filtres",
#                   actionButton("clear", label = "Esborra tots els filtres",  icon ("eraser"), class = "qia-down")),
#               div(title = "Descarrega la taula en format Excel",
#                   downloadButton("download_table_excel", 'Descarrega la taula filtrada', class = "qia-down")),
#               div(title = "Descarrega la base de dades completa en format CSV (excedeix el nombre màxim de columnes permeses en Excel, de manera que s'aconsella obrir-lo amb programari específic d'anàlisi de dades)",
#                   downloadButton("download_all_csv", 'Descarrega la base dades completa', class = "qia-down")),
#               br()
#               
#        ) #column bracket
#      ), #filters fluid row bracket
#      #Row 3- Table
#      fluidRow(  
#        br(),
#        column(12, div(reactableOutput("table_filtered"), 
#                       style = "font-size: 98%; width: 98%"))
#      )
#    ) # main panel bracket
#  ) #Tab panel bracket#