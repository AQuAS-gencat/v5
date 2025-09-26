###############################################
#
# UI de la pàgina d'inici
#
###############################################

iniciTab <- 
  
  nav_panel(
    title = "Inici",
    #title = div(role = "navigation", "Inici"), # wrap in div for screenreader / accessibility purposes 
    value = "inici", # tab ID
    #icon = icon("home"),
    
    # Main content
    page_fluid(
        #p("Prova")
        includeHTML("index25.html")

    )
  )