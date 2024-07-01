filtered_data <- reactive({
  req(input$date)
  
  selected_date <- as.Date(input$date)
  selected_year <- year(selected_date)
  selected_month <- month(selected_date)
  
  print(paste("Date sélectionnée :", selected_date))
  print(paste("Année sélectionnée :", selected_year))
  print(paste("Mois sélectionné :", selected_month))
  
  if(is.na(selected_year) || is.na(selected_month)) {
    print("Année ou mois sélectionné est NA")
    return(NULL)
  }
  
  filtered_df <- pax_apt_all %>%
    mutate(date = as.Date(paste(anmois, "01", sep=""), format="%Y%m%d")) %>%
    filter(year(date) == selected_year, month(date) == selected_month)
  
  print("Données filtrées :")
  print(filtered_df)
  
  return(filtered_df)
})

#Value Box: Nombre Total de Passagers
output$total_passengers <- renderValueBox({
  total_passengers <- filtered_data() %>%
    summarize(total = sum(apt_pax_dep + apt_pax_arr + apt_pax_tr, na.rm = TRUE)) %>%
    pull(total)
  
  month_year <- format(input$date, "%m/%y")
  
  valueBox(
    format(total_passengers, big.mark = " "),
    paste("Nombre Total de Passagers en France (", month_year, ")"),
    icon = icon("users"),
    color = "blue"
  )
})

# Value Box: Aéroport le Plus Fréquenté
output$busiest_airport <- renderValueBox({
  busiest_airport <- filtered_data() %>%
    group_by(apt_nom) %>%
    summarize(total = sum(apt_pax_dep + apt_pax_arr + apt_pax_tr, na.rm = TRUE)) %>%
    arrange(desc(total)) %>%
    slice(1) %>%
    pull(apt_nom)
  
  month_year <- format(input$date, "%m/%y")
  
  valueBox(
    busiest_airport,
    paste("Aéroport le Plus Fréquenté (", month_year, ")"),
    icon = icon("plane"),
    color = "green"
  )
})

# Value Box: Aéroport le Moins Fréquenté
output$least_busy_airport <- renderValueBox({
  least_busy_airport <- filtered_data() %>%
    group_by(apt_nom) %>%
    summarize(total = sum(apt_pax_dep + apt_pax_arr + apt_pax_tr, na.rm = TRUE)) %>%
    arrange(total) %>%
    slice(1) %>%
    pull(apt_nom)
  
  month_year <- format(input$date, "%m/%y")
  
  valueBox(
    least_busy_airport,
    paste("Aéroport le Moins Fréquenté (", month_year, ")"),
    icon = icon("plane"),
    color = "red"
  )
})

# Value Box: Variation Mois sur Mois
output$monthly_variation <- renderValueBox({
  selected_year <- as.numeric(format(input$date, "%Y"))
  selected_month <- as.numeric(format(input$date, "%m"))
  
  previous_month_df <- pax_apt_all %>%
    mutate(date = as.Date(paste(anmois, "01", sep=""), format="%Y%m%d")) %>%
    filter(year(date) == selected_year, month(date) == (selected_month - 1))
  
  current_month_passengers <- filtered_data() %>%
    summarize(total = sum(apt_pax_dep + apt_pax_arr + apt_pax_tr, na.rm = TRUE)) %>%
    pull(total)
  
  previous_month_passengers <- previous_month_df %>%
    summarize(total = sum(apt_pax_dep + apt_pax_arr + apt_pax_tr, na.rm = TRUE)) %>%
    pull(total)
  
  variation <- ((current_month_passengers - previous_month_passengers) / previous_month_passengers) * 100
  
  month_year <- format(input$date, "%m/%y")
  previous_month_year <- format(as.Date(paste(selected_year, selected_month - 1, "01", sep = "-")), "%m/%y")
  
  valueBox(
    sprintf("%.1f%%", variation),
    paste("Variation Mois sur Mois des Passagers (", previous_month_year, " => ", month_year, ")"),
    icon = icon("chart-line"),
    color = "purple"
  )
})

# Value Box: Variation Annuelle
output$annual_variation <- renderValueBox({
  selected_year <- as.numeric(format(input$date, "%Y"))
  selected_month <- as.numeric(format(input$date, "%m"))
  
  previous_year_df <- pax_apt_all %>%
    mutate(date = as.Date(paste(anmois, "01", sep=""), format="%Y%m%d")) %>%
    filter(year(date) == (selected_year - 1), month(date) == selected_month)
  
  current_year_passengers <- filtered_data() %>%
    summarize(total = sum(apt_pax_dep + apt_pax_arr + apt_pax_tr, na.rm = TRUE)) %>%
    pull(total)
  
  previous_year_passengers <- previous_year_df %>%
    summarize(total = sum(apt_pax_dep + apt_pax_arr + apt_pax_tr, na.rm = TRUE)) %>%
    pull(total)
  
  variation <- ((current_year_passengers - previous_year_passengers) / previous_year_passengers) * 100
  
  month_year <- format(input$date, "%m/%y")
  previous_year_month_year <- format(as.Date(paste(selected_year - 1, selected_month, "01", sep = "-")), "%m/%y")
  
  valueBox(
    sprintf("%.1f%%", variation),
    paste("Variation Annuelle des Passagers (", previous_year_month_year, " => ", month_year, ")"),
    icon = icon("chart-line"),
    color = "orange"
  )
})

# Value Box: Nombre Total de Vols
output$total_flights <- renderValueBox({
  total_flights <- filtered_data() %>%
    summarize(total = sum(apt_nmvt_mxt + apt_nmvt_cgo, na.rm = TRUE)) %>%
    pull(total)
  
  month_year <- format(input$date, "%m/%y")
  
  valueBox(
    format(total_flights, big.mark = " "),
    paste("Nombre Total de Vols en France (", month_year, ")"),
    icon = icon("plane"),
    color = "yellow"
  )
})

output$selected_period_text <- renderText({
  if (is.null(input$selected_month)) {
    return("Sélectionner une période")
  } else {
    paste("Mois sélectionné :", input$selected_month)
  }
})

# Tableau des liaisons
output$table_mois <- renderDT({
  df <- filtered_data() %>%
    select(
      Aéroport = apt,
      `Nom de l'aéroport` = apt_nom,
      `Passagers Départ` = apt_pax_dep,
      `Passagers Arrivée` = apt_pax_arr,
      `Passagers Transit` = apt_pax_tr
    ) %>%
    arrange(desc(`Passagers Départ`))
  
  datatable(df, options = list(pageLength = 10, searchHighlight = TRUE, autoWidth = TRUE,
                               rowCallback = JS(
                                 "function(row, data, index) {",
                                 "  if (index === 0) {",
                                 "    $('td', row).css('background-color', '#FFB6C1');", # Rouge pastel pour le 1er
                                 "  } else if (index === 1) {",
                                 "    $('td', row).css('background-color', '#FFA07A');", # Orange pastel pour le 2e
                                 "  } else if (index >= 2 && index <= 4) {",
                                 "    $('td', row).css('background-color', '#FFFF99');", # Jaune pastel pour les 3-5e
                                 "  }",
                                 "}"
                               )), filter = 'top', rownames = FALSE) %>%
    formatRound(columns = c('Passagers Départ', 'Passagers Arrivée', 'Passagers Transit'), digits = 0, mark = ' ')
})

output$table_annee <- renderDT({
  selected_year <- input$select_year
  
  df_annee <- pax_apt_all %>%
    mutate(Year = as.numeric(format(as.Date(paste(anmois, "01", sep=""), format="%Y%m%d"), "%Y"))) %>%
    filter(Year == selected_year) %>%
    group_by(Year, apt, apt_nom) %>%
    summarize(
      `Passagers Départ` = sum(apt_pax_dep, na.rm = TRUE),
      `Passagers Arrivée` = sum(apt_pax_arr, na.rm = TRUE),
      `Passagers Transit` = sum(apt_pax_tr, na.rm = TRUE)
    ) %>%
    ungroup() %>% 
    arrange(desc(`Passagers Départ`))
  
  datatable(df_annee, options = list(pageLength = 10, searchHighlight = TRUE, autoWidth = TRUE,
                                     rowCallback = JS(
                                       "function(row, data, index) {",
                                       "  if (index === 0) {",
                                       "    $('td', row).css('background-color', '#FFB6C1');", # Rouge pastel pour le 1er
                                       "  } else if (index === 1) {",
                                       "    $('td', row).css('background-color', '#FFA07A');", # Orange pastel pour le 2e
                                       "  } else if (index >= 2 && index <= 4) {",
                                       "    $('td', row).css('background-color', '#FFFF99');", # Jaune pastel pour les 3-5e
                                       "  }",
                                       "}"
                                     )), filter = 'top', rownames = FALSE) %>%
    formatRound(columns = c('Passagers Départ', 'Passagers Arrivée', 'Passagers Transit'), digits = 0, mark = ' ')
})

# Création carte Leaflet initiale
output$carte <- renderLeaflet({
  leaflet() %>%
    # Ajoute une couche de tuiles par défaut à la carte (OpenStreetMap)
    addTiles() %>%
    # Définit le centre de la carte et le niveau de zoom initial - focus FR métropolitaine
    setView(lng = 1.888334, lat = 46.603354, zoom = 5) #%>%
    # addMarkers(lng = 2.3522, lat = 48.8566, popup = "Paris") # test ce marker précis pour débug
})


# Observer to update map based on filtered data
observe({
  req(input$date)
  
  # Filtrer les données basées sur la date sélectionnée
  trafic_aeroports <- filtered_data()
  
  # Vérifiez si les données filtrées sont disponibles
  if (is.null(trafic_aeroports) || nrow(trafic_aeroports) == 0) {
    print("Aucune donnée disponible pour la date sélectionnée")
    return()
  }
  
  # Afficher les données filtrées
  print("Données filtrées:")
  print(head(trafic_aeroports))
  
  # Fusionner avec les informations de localisation des aéroports
  trafic_aeroports <- trafic_aeroports %>%
    inner_join(airports_location, by = c("apt" = "Code.OACI"))
  
  # Vérifiez si la fusion a réussi
  if (nrow(trafic_aeroports) == 0) {
    print("Aucune donnée après la jointure")
    return()
  }
  
  print("Données après la jointure avec les informations de localisation des aéroports:")
  print(head(trafic_aeroports))
  
  # Gérer les valeurs NA après la fusion
  trafic_aeroports <- trafic_aeroports %>%
    mutate(
      apt_pax_dep = coalesce(apt_pax_dep.x, apt_pax_dep.y, 0),
      apt_pax_arr = coalesce(apt_pax_arr.x, apt_pax_arr.y, 0),
      apt_pax_tr = coalesce(apt_pax_tr.x, apt_pax_tr.y, 0),
      trafic = apt_pax_dep + apt_pax_arr + apt_pax_tr
    )
  
  print("Données après le calcul du trafic total:")
  print(head(trafic_aeroports))
  
  # Extraire les coordonnées
  coords <- st_coordinates(trafic_aeroports$geometry)
  trafic_aeroports$longitude <- coords[,1]
  trafic_aeroports$latitude <- coords[,2]
  
  print("Coordonnées extraites:")
  print(head(coords))
  
  # Ajouter les couleurs pour les marqueurs
  trafic_aeroports <- trafic_aeroports %>%
    mutate(volume = ntile(trafic, 3)) %>%
    mutate(color = c("green", "blue", "red")[volume])
  
  icons <- awesomeIcons(
    icon = 'plane',
    iconColor = 'black',
    library = 'fa',
    markerColor = trafic_aeroports$color
  )
  
  print("Icônes créées:")
  print(icons)
  
  # Utiliser leafletProxy pour mettre à jour la carte
  leafletProxy("carte", data = trafic_aeroports) %>%
    clearMarkers() %>%
    addAwesomeMarkers(
      lng = ~longitude,
      lat = ~latitude,
      icon = icons,
      label = ~paste0(Nom, " (", apt, ") : ", format(trafic, big.mark = " "), " voyageurs")
    )
})


# Création du graphique avec choix du plot_ly (!=plotly)
output$lineplot <- renderPlotly({
  # Appelle la fonction plot_airport_line avec les données de trafic aéroportuaire
  # et l'option sélectionnée par l'user
  plot_airport_line(pax_apt_all, input$select)
})







#   # Fonction pour filtrer les données selon la date sélectionnée
#   filtered_data <- reactive({
#     selected_year <- as.numeric(format(input$date, "%Y"))
#     selected_month <- as.numeric(format(input$date, "%m"))
# 
#     filtered_df <- pax_apt_all %>%
#       mutate(date = as.Date(paste(anmois, "01", sep=""), format="%Y%m%d")) %>%
#       filter(year(date) == selected_year, month(date) == selected_month)
# 
#     return(filtered_df)
#   })
# # 
#   # Value Box: Nombre Total de Passagers
#   output$total_passengers <- renderValueBox({
#     total_passengers <- filtered_data() %>%
#       summarize(total = sum(apt_pax_dep + apt_pax_arr + apt_pax_tr, na.rm = TRUE)) %>%
#       pull(total)
# 
#     month_year <- format(input$date, "%m/%y")
# 
#     valueBox(
#       format(total_passengers, big.mark = " "),
#       paste("Nombre Total de Passagers en France (", month_year, ")"),
#       icon = icon("users"),
#       color = "blue"
#     )
#   })
# 
#   # Value Box: Aéroport le Plus Fréquenté
#   output$busiest_airport <- renderValueBox({
#     busiest_airport <- filtered_data() %>%
#       group_by(apt_nom) %>%
#       summarize(total = sum(apt_pax_dep + apt_pax_arr + apt_pax_tr, na.rm = TRUE)) %>%
#       arrange(desc(total)) %>%
#       slice(1) %>%
#       pull(apt_nom)
#     
#     month_year <- format(input$date, "%m/%y")
#     
#     valueBox(
#       busiest_airport,
#       paste("Aéroport le Plus Fréquenté (", month_year, ")"),
#       icon = icon("plane"),
#       color = "green"
#     )
#   })
# 
#   # Value Box: Aéroport le Moins Fréquenté
#   output$least_busy_airport <- renderValueBox({
#     least_busy_airport <- filtered_data() %>%
#       group_by(apt_nom) %>%
#       summarize(total = sum(apt_pax_dep + apt_pax_arr + apt_pax_tr, na.rm = TRUE)) %>%
#       arrange(total) %>%
#       slice(1) %>%
#       pull(apt_nom)
# 
#     month_year <- format(input$date, "%m/%y")
#     
#     valueBox(
#       least_busy_airport,
#       paste("Aéroport le Moins Fréquenté (", month_year, ")"),
#       icon = icon("plane"),
#       color = "red"
#     )
#   })
# 
#   # Value Box: Variation Mois sur Mois
#   output$monthly_variation <- renderValueBox({
#     selected_year <- as.numeric(format(input$date, "%Y"))
#     selected_month <- as.numeric(format(input$date, "%m"))
# 
#     previous_month_df <- pax_apt_all %>%
#       mutate(date = as.Date(paste(anmois, "01", sep=""), format="%Y%m%d")) %>%
#       filter(year(date) == selected_year, month(date) == (selected_month - 1))
# 
#     current_month_passengers <- filtered_data() %>%
#       summarize(total = sum(apt_pax_dep + apt_pax_arr + apt_pax_tr, na.rm = TRUE)) %>%
#       pull(total)
# 
#     previous_month_passengers <- previous_month_df %>%
#       summarize(total = sum(apt_pax_dep + apt_pax_arr + apt_pax_tr, na.rm = TRUE)) %>%
#       pull(total)
# 
#     variation <- ((current_month_passengers - previous_month_passengers) / previous_month_passengers) * 100
#     
#     month_year <- format(input$date, "%m/%y")
#     
#     valueBox(
#       sprintf("%.2f%%", variation),
#       paste("Variation Mois sur Mois des Passagers  (", month_year, ")"),
#       icon = icon("line-chart"),
#       color = "purple"
#     )
#   })
# 
#   # Value Box: Variation Annuelle
#   output$annual_variation <- renderValueBox({
#     selected_year <- as.numeric(format(input$date, "%Y"))
#     selected_month <- as.numeric(format(input$date, "%m"))
# 
#     previous_year_df <- pax_apt_all %>%
#       mutate(date = as.Date(paste(anmois, "01", sep=""), format="%Y%m%d")) %>%
#       filter(year(date) == (selected_year - 1), month(date) == selected_month)
# 
#     current_year_passengers <- filtered_data() %>%
#       summarize(total = sum(apt_pax_dep + apt_pax_arr + apt_pax_tr, na.rm = TRUE)) %>%
#       pull(total)
# 
#     previous_year_passengers <- previous_year_df %>%
#       summarize(total = sum(apt_pax_dep + apt_pax_arr + apt_pax_tr, na.rm = TRUE)) %>%
#       pull(total)
# 
#     variation <- ((current_year_passengers - previous_year_passengers) / previous_year_passengers) * 100
#     
#     month_year <- format(input$date, "%m/%y")
#     
#     valueBox(
#       sprintf("%.2f%%", variation),
#       paste("Variation Annuelle des Passagers (", month_year, ")"),
#       icon = icon("line-chart"),
#       color = "orange"
#     )
#   })
# 
#   # Value Box: Nombre Total de Vols
#   output$total_flights <- renderValueBox({
#     total_flights <- filtered_data() %>%
#       summarize(total = sum(apt_nmvt_mxt + apt_nmvt_cgo, na.rm = TRUE)) %>%
#       pull(total)
# 
#     month_year <- format(input$date, "%m/%y")
# 
#     valueBox(
#       format(total_flights, big.mark = " "),
#       paste("Nombre Total de Vols en France (", month_year, ")"),
#       icon = icon("plane"),
#       color = "yellow"
#     )
#   })
# 
#   table_liaisons <- reactive({
#     summary_stat_airport(filtered_data())
#   })
# 
#   output$table <- renderDT({
#     df <- filtered_data() %>%
#       select(
#         Aéroport = apt,
#         `Nom de l'aéroport` = apt_nom,
#         `Passagers Départ` = apt_pax_dep,
#         `Passagers Arrivée` = apt_pax_arr,
#         `Passagers Transit` = apt_pax_tr
#       ) %>%
#       arrange(desc(`Passagers Départ`))
# 
#     datatable(df, options = list(pageLength = 10, searchHighlight = TRUE, autoWidth = TRUE,
#                                  rowCallback = JS(
#                                    "function(row, data, index) {",
#                                    "  if (index === 0) {",
#                                    "    $('td', row).css('background-color', '#FFB6C1');", # Rouge pastel pour le 1er
#                                    "  } else if (index === 1) {",
#                                    "    $('td', row).css('background-color', '#FFA07A');", # Orange pastel pour le 2e
#                                    "  } else if (index >= 2 && index <= 4) {",
#                                    "    $('td', row).css('background-color', '#FFFF99');", # Jaune pastel pour les 3-5e
#                                    "  }",
#                                    "}"
#                                  )), filter = 'top', rownames = FALSE) %>%
#       formatRound(columns = c('Passagers Départ', 'Passagers Arrivée', 'Passagers Transit'), digits = 0, mark = ' ')
#   })
#   output$table_annee <- renderDT({
#     selected_year <- input$select_year
#     
#     df_annee <- pax_apt_all %>%
#       mutate(Year = as.numeric(format(as.Date(paste(anmois, "01", sep=""), format="%Y%m%d"), "%Y"))) %>%
#       filter(Year == selected_year) %>%
#       group_by(Year, apt, apt_nom) %>%
#       summarize(
#         `Passagers Départ` = sum(apt_pax_dep, na.rm = TRUE),
#         `Passagers Arrivée` = sum(apt_pax_arr, na.rm = TRUE),
#         `Passagers Transit` = sum(apt_pax_tr, na.rm = TRUE)
#       ) %>%
#       ungroup() %>% 
#       arrange(desc(`Passagers Départ`))
#     
#     datatable(df_annee, options = list(pageLength = 10, searchHighlight = TRUE, autoWidth = TRUE,
#                                        rowCallback = JS(
#                                          "function(row, data, index) {",
#                                          "  if (index === 0) {",
#                                          "    $('td', row).css('background-color', '#FFB6C1');", # Rouge pastel pour le 1er
#                                          "  } else if (index === 1) {",
#                                          "    $('td', row).css('background-color', '#FFA07A');", # Orange pastel pour le 2e
#                                          "  } else if (index >= 2 && index <= 4) {",
#                                          "    $('td', row).css('background-color', '#FFFF99');", # Jaune pastel pour les 3-5e
#                                          "  }",
#                                          "}"
#                                        )), filter = 'top', rownames = FALSE) %>%
#     formatRound(columns = c('Passagers Départ', 'Passagers Arrivée', 'Passagers Transit'), digits = 0, mark = ' ')
#   })
#   
#   
#   # Création carte Leaflet initiale
#   output$carte <- renderLeaflet({
#     leaflet() %>%
#       # Ajoute une couche de tuiles par défaut à la carte (OpenStreetMap)
#       addTiles() %>%
#       # Définit le centre de la carte et le niveau de zoom initial - focus FR métropolitaine
#       setView(lng = 1.888334, lat = 46.603354, zoom = 5)
#   })
#   # Fonction pour observer les chgts dans les données filtrées et maj la carto en conséquence
#   observe({
#     # Récupère les données filtrées par la date sélectionnée par l'user
#     trafic_aeroports <- filtered_data() %>%
#       # Joint les données de trafic aéroportuaire avec les données de localisation des aéroports
#       inner_join(airports_location, by = c("apt" = "Code.OACI"))
# 
#     # Calcule le trafic total et assigne des couleurs en fonction du volume de trafic
#     trafic_aeroports <- trafic_aeroports %>%
#       # Remplace les valeurs manquantes dans les colonnes de départ, d'arrivée et de transit par les valeurs correspondantes
#       mutate(
#         apt_pax_dep = coalesce(apt_pax_dep.x, apt_pax_dep.y),
#         apt_pax_arr = coalesce(apt_pax_arr.x, apt_pax_arr.y),
#         apt_pax_tr = coalesce(apt_pax_tr.x, apt_pax_tr.y),
#         # Calcule le trafic total (départ + arrivée + transit)
#         trafic = apt_pax_dep + apt_pax_arr + apt_pax_tr
#       ) %>%
#       # Divise les données en trois groupes égaux basés sur le volume de trafic
#       mutate(volume = ntile(trafic, 3)) %>%
#       # Assigne une couleur à chaque groupe de trafic (vert, bleu, rouge)
#       mutate(color = c("green", "blue", "red")[volume])
#     # Récupère les coordonnées des aéroports
#     coords <- st_coordinates(trafic_aeroports$geometry)
#     trafic_aeroports$longitude <- coords[,1]
#     trafic_aeroports$latitude <- coords[,2]
#     # Crée des icônes personnalisées pour les marqueurs des aéroports
#     icons <- awesomeIcons(
#       icon = 'plane',
#       iconColor = 'black',
#       library = 'fa',
#       markerColor = trafic_aeroports$color
#     )
#     # Met à jour la carte Leaflet avec les nouveaux marqueurs
#     leafletProxy("carte", data = trafic_aeroports) %>%
#       # Efface les anciens marqueurs
#       clearMarkers() %>%
#       # Ajoute les nouveaux marqueurs avec des icônes et des étiquettes
#       addAwesomeMarkers(
#         lng = ~longitude,
#         lat = ~latitude,
#         icon = icons,
#         label = ~paste0(Nom, " (", apt, ") : ", trafic, " voyageurs")
#       )
#   })
#   
#   # Création du graphique avec choix du plot_ly (!=plotly)
#   output$lineplot <- renderPlotly({
#     # Appelle la fonction plot_airport_line avec les données de trafic aéroportuaire
#     # et l'option sélectionnée par l'user
#     plot_airport_line(pax_apt_all, input$select)
#   })