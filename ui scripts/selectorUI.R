selectorTab <- nav_panel(
  title = "Inici",
  value = "inici-visor", # tab ID
  #icon = icon("laptop-medical"),
  icon = icon("home"),
  
  page_fluid(
#    div(style = "margin: 10px;",
#        h1("Visor de centres", style = "font-weight: bold; text-align: left; margin: 10px;")
#    ),
    
    layout_columns(
      col_widths = c(4, 8),
      
      # Left column with selector
      card(
        card_header("Visor de centres",
                    style = "font-size: 30px; font-weight: bold; font-color: #000000; margin: 10px;"),
        card_body(
          uiOutput("center_selector")
        )
      ),
      
      # Right column with 2x2 card grid
      layout_columns(
        col_widths = c(6, 6),
        
        # First row, first card
        div(
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
            tags$img(src = "icon_resum_cr.svg", width="40px"),
            br(),
            tags$b("Resum"),
            p("Quins són els resultats del centre?")
          )
        ),
        
        # First row, second card
        div(
          class = "card-button",
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
            tags$img(src = "icon_variabilitat_cr.svg", width="40px"),
            br(),
            tags$b("Variabilitat"),
            p("Com es comparen els resultats del centre?")
          )
        ),
        
        # Second row, first card
        div(
          class = "card-button",
          tags$button(
            onclick = "
              // First, click on the Visor tab
              $('#nav li:contains(\"Visor\") a').click();
              // Then, use a timeout to ensure Visor loads, then click on the estratificació subtab
              setTimeout(function() {
                // Target the Estratificació subtab specifically within Visor
                $('#nav_visor li:contains(\"Evolució estratificada\") a').click();
              }, 50);  // Adjust the delay if needed
            ",
            icon("layer-group"),
            #tags$img(src = "icon_estratificacio_cr.svg", width="40px"),
            br(),
            tags$b("Evolució estratificada"),
            p("Com evolucionen els resultats segons el sexe i l'edat?")
          )
        ),

        # Second row, second card
        div(
          class = "card-button",
          tags$button(
            onclick = "
              // First, click on the Visor tab
              $('#nav li:contains(\"Visor\") a').click();
              // Then, use a timeout to ensure Visor loads, then click on the Resum subtab
              setTimeout(function() {
                // Target the Resum subtab specifically within Visor
                $('#nav_visor li:contains(\"Evolució comparada\") a').click();
              }, 50);  // Adjust the delay if needed
            ",
            tags$img(src = "icon_evolucio_cr.svg", width="40px"),
            br(),
            tags$b("Evolució comparada"),
            p("Com evoluciona el centre en relació als altres?")
          )
        )
      )
    )
  )
)