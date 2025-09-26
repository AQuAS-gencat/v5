###############################################
#
## UI de la pestanya definicions ----
#
###############################################


defTab <- 
  
  nav_panel(
    title = "Documentació",
    value = "defs",
    #icon = icon("file-lines"),
    #div(value = "defs", role = "navigation"),
    mainPanel(
      width = 12, style="margin-left:1.5%; margin-right:1.5%",
      
      
      #Row 1 for intro  
#      fluidRow(
#        h2("Fitxes metodològiques", 
#           style = "font-weight: bold;"),
#        br(),
#        br(),
#        column(9,
#               div(
#                 p(style = "font-size: 14px; padding: 10px; background-color: #FFE7CE; border-left: 5px solid #FFA500; margin-bottom: 15px;", 
#                   "En aquesta pàgina pots consultar les fitxes metodològiques de tots els indicadors que es mostren a l'aplicació. Clicant cada indicador es desplega tota la informació relacionada amb aquest.
#                   També es poden cercar paraules clau mitjançant la barra de cerca i ordenar la taula clicant sobre els títols de les columnes."),
#               ),
#               div(
#                 p(style = "font-size: 14px; padding: 10px; background-color: #FFE7CE; border-left: 5px solid #FFA500; margin-bottom: 15px;", 
#                   "Les fitxes de tots els indicadors es poden descarregar en format Excel prement el botó de descàrrega."),
#               ),
#        ),
#        column(3,
#               div(class = "tech-doc-download",
#                   title = "Descarrega l'arxiu de fitxes metodològiques en format Excel.",
#                   downloadButton('docu_download', "Descarrega les fitxes", class = "qia-down")
#               ),
#               div(
#                 title = "Document metodològic",
#                 actionButton("docu_meto", label = "Document metodològic", icon = icon('question-circle'), class = "qia-down")
#               )
#               ),
#        
#        br(),
#        div(
#          fluidRow(reactableOutput("ind_search_results") %>% withSpinner(color = "orange"))
#        )
#      )
#      
      ) # close main panel

  ) # close tab panel
