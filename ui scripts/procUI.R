procTab <- nav_panel(
  title = "Processos assistencials",  # Simplified title without icon
  value = "proc",
  #icon = icon("arrows-spin"),
  
  
  # Wrapper to style the entire wellPanel section
  #      wellPanel(
  #style = "background: white;",
  
  div(style = "margin:10px;",
      h2("Processos assistencials", 
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
