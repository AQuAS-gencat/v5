###############################################
#
# UI for about us tab ----
#
###############################################

aboutTab <- 
  
  nav_panel(
    title = "Sobre la Central",
    value = "about",
    #icon = icon("file-lines"),
    #div(value = "defs", role = "navigation"),
      #width = 12, style="margin-left:1.5%; margin-right:1.5%",
      
      bslib::card(
        full_screen = FALSE,
#        style = "padding: 20px; max-width: 80%; background-color: #f9f9f9; 
#                 border: 1px solid #e0e0e0; border-radius: 8px;",
        
#        div(
          p("La Central de Resultats (CdR) mesura, avalua i difon els resultats en salut i qualitat assolits pels diferents agents que integren 
            el sistema sanitari català. "),
          p("Es tracta d'un producte quantitatiu creat l'any 2003 per l'Agència de Qualitat i Avaluació Sanitàries de Catalunya (AQuAS), 
            que vol fomentar i permetre als i les professionals i gestors dels serveis sanitaris la comparació i el monitoratge de la qualitat 
            i l'eficiència per a la millora contínua de l'assistència. "),
          br(),
          
          h4("Sobre l'AQuAS"),
          p("L'Agència de Qualitat i Avaluació Sanitàries de Catalunya (AQuAS) té la missió de generar coneixement rellevant, mitjançant l'avaluació i l'anàlisi 
            de dades per a la presa de decisions, amb la finalitat de contribuir a la millora de la salut de la ciutadania i a la sostenibilitat del sistema de 
            salut de Catalunya. "),
          
          h4("Contacta amb nosaltres"),
          p("Si detecteu algun error en les dades o teniu qualsevol dubte, comentari o aportació sobre l'eina o els indicadors que ens vulgueu transmetre podeu enviar un correu a ",
            tags$a(href="mailto:cdr.aquas@gencat.cat", "cdr.aquas@gencat.cat", class="externallink"),
            ". La vostra col·laboració ens servirà per seguir millorant la plataforma.")
          
          
#        )
      )
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

    
  ) # close tab panel


