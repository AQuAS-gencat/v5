selectorTab <- tabPanel(
  title = "Inici",
  value = "inici-visor", # tab ID
  
  # Title aligned to the left in black

  
  # Main fluidRow with the 9/12 and 3/12 columns
  fluidRow(
    
    div(style = "margin: 10px;",
        h1("Visor de centres i territoris", style = "font-weight: bold; text-align: left; margin: 10px;")
    ),
    
    column(8,
           bslib::card(
             style = "background-color: #e7f3fe; border-left: 5px solid #2196F3;",
             p("Explora 191 indicadors dels 6 àmbits la Central de Resultats des de diferents perspectives i en una sola plataforma.",
               style = "font-size: 20px;")
           ),
           bslib::card(
             style = "background-color: #e7f3fe; border-left: 5px solid #2196F3;",
             p("Obtén una visió global, contextualitzada i interactiva de l'acompliment dels centres o unitats territorials.",
               style = "font-size: 20px;")
           ),
           bslib::card(
             style = "background-color: #e7f3fe; border-left: 5px solid #2196F3;",
             p("Descarrega fàcilment les dades i gràfics segons els filtres seleccionats.",
               style = "font-size: 20px;")
           )
           
           
           
           ),
    
    
    column(4, 
           div(
             style = "text-align: center;", # Center the contents of this div
             h4("Selecciona un centre o territori per navegar pel visor:",
                style = "font-size: 20px; font-weight: bold; padding: 10px; 
                     font-color: #000000;")
           ),
           div(
             style = "text-align: left; max-width: 80%; margin: 0 auto;", # Center-aligns and sets a max width for the input elements
             selectInput("geography_level", "Selecciona una unitat geogràfica:", 
                         choices = c("Regió Sanitària", 
                                     "Àrea de Gestió Assistencial",
                                     "Àrea Bàsica de Salut",
                                     "Centre (Unitat proveïdora)",
                                     "Unitat territorial (protecció de la salut)",
                                     "Unitat de Salut Laboral")),
             br(),
             uiOutput("center_selector"),
             br()
           )
    ),
    
    
    fluidRow(
      
      column(width = 4,
             # Resum Box with link to "resum" subtab
             tags$div(
               class = "card-button",
               
               tags$button(
                 onclick = "
      // First, click on the Visor tab
      $('#nav li:contains(\"Visor\") a').click();
      // Then, use a timeout to ensure Visor loads, then click on the Resum subtab
      setTimeout(function() {
        // Target the Resum subtab specifically within Visor
        $('#nav_visor li:contains(\"Resum\") a').click();
      }, 50);  // Adjust the delay if needed
    ",
                 tags$i(class = "fa-solid fa-list-alt"),
                 br(),
                 tags$span("Resum"),
                 p("Quin són els resultats del centre o territori?"#,
                   #class = "small-black-text"
                 )
               )
             )
      ),
      
      column(width = 4,
             tags$div(
               class = "card-button",
               # Variabilitat Box with link to "variabilitat" subtab  
               tags$button(
                 
                 onclick = "
      // First, click on the Visor tab
      $('#nav li:contains(\"Visor\") a').click();
      // Then, use a timeout to ensure Visor loads, then click on the Resum subtab
      setTimeout(function() {
        // Target the Resum subtab specifically within Visor
        $('#nav_visor li:contains(\"Variabilitat\") a').click();
      }, 50);  // Adjust the delay if needed
    ",
                 #role = "button",
                 tags$i(class = "fa-solid fa-bar-chart"),
                 br(),
                 tags$span("Variabilitat"),
                 p("Com se situen els resultats del centre o territori en relació als altres?"
                 )
               )
             )
      ),
      column(width = 4,
             # Evolució temporal Box with link to "evolutiu" subtab
             tags$div(
               class = "card-button",
               
               tags$button(
                 onclick = "
      // First, click on the Visor tab
      $('#nav li:contains(\"Visor\") a').click();
      // Then, use a timeout to ensure Visor loads, then click on the Resum subtab
      setTimeout(function() {
        // Target the Resum subtab specifically within Visor
        $('#nav_visor li:contains(\"Evolució temporal\") a').click();
      }, 50);  // Adjust the delay if needed
    ",
                 #role = "button",
                 tags$i(class = "fa-solid fa-line-chart"),
                 br(),
                 tags$span("Evolució temporal"),
                 p("Com evoluciona el centre o territori en relació als altres?"
                 )
               )
             ),
      )
      
    ),
    br(),
    hr()
    
  )
  
)
    

    
