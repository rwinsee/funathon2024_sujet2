plot_airport_line <- function(df, selected_airport, display_type) {
  df <- df %>%
    mutate(trafic = apt_pax_dep + apt_pax_tr + apt_pax_arr,
           date = as.Date(paste(anmois, "01", sep = ""), format = "%Y%m%d"))
  
  if (selected_airport != "Tous") {
    df <- df %>% filter(apt_nom == selected_airport)
  } else {
    df <- df %>%
      group_by(date) %>%
      summarize(trafic = sum(trafic, na.rm = TRUE), .groups = 'drop')
  }
  
  if (display_type == "trimestriel") {
    df <- df %>%
      mutate(quarter = paste0("T", ceiling(month(date) / 3), "-", year(date)),
             quarter_end_date = as.Date(paste(year(date), ceiling(month(date) / 3) * 3, "01", sep = "-"), "%Y-%m-%d") + months(1) - days(1)) %>%
      group_by(quarter_end_date) %>%
      summarize(trafic = sum(trafic, na.rm = TRUE), .groups = 'drop') %>%
      arrange(quarter_end_date) %>%
      rename(date = quarter_end_date)
  } else if (display_type == "annuel") {
    df <- df %>%
      group_by(year = year(date)) %>%
      summarize(trafic = sum(trafic, na.rm = TRUE), .groups = 'drop') %>%
      mutate(date = as.Date(paste0(year, "-12-31"))) %>%
      arrange(date)
  }
  
  plot_title <- if (selected_airport == "Tous") {
    "Suivi temporel de la fréquentation des aéroports français"
  } else {
    paste("Suivi temporel de la fréquentation de l'aéroport de", selected_airport)
  }
  
  plot_ly(df, x = ~date, y = ~trafic, type = 'scatter', mode = 'lines+markers',
          text = ~ifelse(selected_airport == "Tous", "Tous les aéroports", selected_airport),
          hovertemplate = paste("<i>Aéroport:</i> %{text}<br>Trafic: %{y}")) %>%
    layout(title = plot_title,
           xaxis = list(title = 'Date', showgrid = TRUE, showline = TRUE, showticklabels = TRUE, ticks = 'outside', autotick = TRUE, tickwidth = 2, tickangle = -25, zeroline = FALSE),
           yaxis = list(title = 'Trafic', autotick = TRUE, showgrid = TRUE, showline = TRUE, showticklabels = TRUE, tickwidth = 2, ticks = 'outside', zeroline = FALSE))
}

# map_leaflet_airport <- function(df, airports_location, month, year, selected_airport = NULL) {
#   palette <- c("green", "blue", "red")
#   
#   # Convertir la date et filtrer les données selon le mois et l'année
#   trafic_date <- df %>%
#     mutate(date = as.Date(paste(anmois, "01", sep=""), format="%Y%m%d")) %>%
#     filter(month(date) == as.numeric(month), year(date) == as.numeric(year))
#   
#   # Renommer la colonne pour correspondre à la colonne Code.OACI
#   trafic_date <- trafic_date %>%
#     rename(Code.OACI = apt)
#   
#   # Fusionner avec les informations de localisation des aéroports
#   trafic_aeroports <- trafic_date %>%
#     inner_join(airports_location, by = "Code.OACI")
#   
#   # Vérifier si la colonne apt_nom existe après la fusion
#   if (!"apt_nom" %in% colnames(trafic_aeroports)) {
#     trafic_aeroports <- trafic_aeroports %>%
#       mutate(apt_nom = coalesce(apt_nom.x, apt_nom.y))
#   }
#   
#   # Sélectionner les colonnes pertinentes après la fusion
#   trafic_aeroports <- trafic_aeroports %>%
#     mutate(
#       apt_pax_dep = coalesce(apt_pax_dep.x, apt_pax_dep.y, 0),
#       apt_pax_arr = coalesce(apt_pax_arr.x, apt_pax_arr.y, 0),
#       apt_pax_tr = coalesce(apt_pax_tr.x, apt_pax_tr.y, 0),
#       trafic = apt_pax_dep + apt_pax_arr + apt_pax_tr
#     )
#   
#   # Extraire les coordonnées de la géométrie
#   coords <- st_coordinates(trafic_aeroports$geometry)
#   trafic_aeroports$longitude <- coords[,1]
#   trafic_aeroports$latitude <- coords[,2]
#   
#   # Créer des icônes personnalisées pour les marqueurs
#   trafic_aeroports <- trafic_aeroports %>%
#     mutate(volume = ntile(trafic, 3)) %>%
#     mutate(color = palette[volume])
#   
#   icons <- awesomeIcons(
#     icon = 'plane',
#     iconColor = 'black',
#     library = 'fa',
#     markerColor = trafic_aeroports$color
#   )
#   
#   # Créer la carte interactive avec les marqueurs
#   carte_interactive <- leaflet(trafic_aeroports) %>%
#     addTiles() %>%
#     setView(lng = 1.888334, lat = 46.603354, zoom = 5) %>%
#     addAwesomeMarkers(
#       lng = ~longitude,
#       lat = ~latitude,
#       icon = icons,
#       label = ~paste0(Nom, " (", Code.OACI, ") : ", format(trafic, big.mark = " "), " voyageurs")
#     )
#   
#   # Zoom sur l'aéroport sélectionné
#   if (!is.null(selected_airport) && selected_airport != "Tous") {
#     selected_coords <- trafic_aeroports %>%
#       filter(apt_nom == selected_airport) %>%
#       select(longitude, latitude)
#     
#     if (nrow(selected_coords) > 0) {
#       carte_interactive <- carte_interactive %>%
#         setView(lng = selected_coords$longitude, lat = selected_coords$latitude, zoom = 10)
#     }
#   }
#   
#   return(carte_interactive)
# }
# 
# 
# Définir la fonction map_leaflet_airport
map_leaflet_airport <- function(df, airports_location, display_type, period_value, selected_airport = NULL) {
  palette <- c("green", "blue", "red")
  
  # Convertir la date
  df <- df %>%
    mutate(date = as.Date(paste(anmois, "01", sep = ""), format = "%Y%m%d"))
  
  if (display_type == "mensuel") {
    df <- df %>%
      filter(format(date, "%Y-%m") == period_value)
  } else if (display_type == "trimestriel") {
    year <- as.numeric(substr(period_value, 1, 4))
    quarter <- as.numeric(substr(period_value, 6, 6))
    start_month <- (quarter - 1) * 3 + 1
    end_month <- start_month + 2
    df <- df %>%
      filter(year(date) == year, month(date) %in% start_month:end_month) %>%
      group_by(apt) %>%
      summarize(
        apt_pax_dep = sum(apt_pax_dep, na.rm = TRUE),
        apt_pax_arr = sum(apt_pax_arr, na.rm = TRUE),
        apt_pax_tr = sum(apt_pax_tr, na.rm = TRUE),
        .groups = 'drop'
      )
  } else if (display_type == "annuel") {
    df <- df %>%
      filter(year(date) == as.numeric(period_value)) %>%
      group_by(apt) %>%
      summarize(
        apt_pax_dep = sum(apt_pax_dep, na.rm = TRUE),
        apt_pax_arr = sum(apt_pax_arr, na.rm = TRUE),
        apt_pax_tr = sum(apt_pax_tr, na.rm = TRUE),
        .groups = 'drop'
      )
  } else {
    stop("Type d'affichage inconnu")
  }
  
  # Renommer la colonne pour correspondre à la colonne Code.OACI
  df <- df %>%
    rename(Code.OACI = apt)
  
  # Fusionner avec les informations de localisation des aéroports
  trafic_aeroports <- df %>%
    inner_join(airports_location, by = "Code.OACI")
  
  # Vérifier si la colonne apt_nom existe après la fusion
  if (!"apt_nom" %in% colnames(trafic_aeroports)) {
    trafic_aeroports <- trafic_aeroports %>%
      mutate(apt_nom = coalesce(apt_nom.x, apt_nom.y))
  }
  
  # Sélectionner les colonnes pertinentes après la fusion
  trafic_aeroports <- trafic_aeroports %>%
    mutate(
      apt_pax_dep = coalesce(apt_pax_dep.x, apt_pax_dep.y, 0),
      apt_pax_arr = coalesce(apt_pax_arr.x, apt_pax_arr.y, 0),
      apt_pax_tr = coalesce(apt_pax_tr.x, apt_pax_tr.y, 0),
      trafic = apt_pax_dep + apt_pax_arr + apt_pax_tr
    )
  
  # Extraire les coordonnées de la géométrie
  coords <- st_coordinates(trafic_aeroports$geometry)
  trafic_aeroports$longitude <- coords[,1]
  trafic_aeroports$latitude <- coords[,2]
  
  # Créer des icônes personnalisées pour les marqueurs
  trafic_aeroports <- trafic_aeroports %>%
    mutate(volume = ntile(trafic, 3)) %>%
    mutate(color = palette[volume])
  
  icons <- awesomeIcons(
    icon = 'plane',
    iconColor = 'black',
    library = 'fa',
    markerColor = trafic_aeroports$color
  )
  
  # Créer la carte interactive avec les marqueurs
  carte_interactive <- leaflet(trafic_aeroports) %>%
    addTiles() %>%
    setView(lng = 1.888334, lat = 46.603354, zoom = 5) %>%
    addAwesomeMarkers(
      lng = ~longitude,
      lat = ~latitude,
      icon = icons,
      label = ~paste0(Nom, " (", Code.OACI, ") : ", format(trafic, big.mark = " "), " voyageurs")
    )
  
  # Zoom sur l'aéroport sélectionné
  if (!is.null(selected_airport) && selected_airport != "Tous") {
    selected_coords <- trafic_aeroports %>%
      filter(apt_nom == selected_airport) %>%
      select(longitude, latitude)
    
    if (nrow(selected_coords) > 0) {
      carte_interactive <- carte_interactive %>%
        setView(lng = selected_coords$longitude, lat = selected_coords$latitude, zoom = 10)
    }
  }
  
  return(carte_interactive)
}

# Fonction pour générer le graphique des compagnies aériennes
plot_compagnies_vols_cie <- function(data) {
  data_passagers <- data %>%
    group_by(cie_nom) %>%
    summarize(total_passagers = sum(cie_pax, na.rm = TRUE)) %>%
    arrange(desc(total_passagers)) %>%
    head(10)
  
  data_vols <- data %>%
    group_by(cie_nom) %>%
    summarize(total_vols = sum(cie_vol, na.rm = TRUE)) %>%
    arrange(desc(total_vols)) %>%
    head(10)
  
  data <- full_join(data_passagers, data_vols, by = "cie_nom")
  
  data$cie_nom_factor_pax <- as.numeric(factor(data$cie_nom)) - 0.2
  data$cie_nom_factor_vols <- as.numeric(factor(data$cie_nom)) + 0.2
  
  plot_ly() %>%
    add_bars(data = data, x = ~cie_nom_factor_pax, y = ~total_passagers, name = 'Passagers', yaxis = 'y1', width = 0.4, marker = list(color = '#77DD77')) %>%
    add_bars(data = data, x = ~cie_nom_factor_vols, y = ~total_vols, name = 'Vols', yaxis = 'y2', width = 0.4, marker = list(color = '#89CFF0')) %>%
    layout(
      title = 'Nombre de passagers et de vols par compagnie',
      xaxis = list(title = 'Compagnie', 
                   tickvals = as.numeric(factor(data$cie_nom)), 
                   ticktext = data$cie_nom,
                   tickangle = -25
      ),
      yaxis = list(title = 'Nombre de passagers', side = 'left'),
      yaxis2 = list(title = 'Nombre de vols', overlaying = 'y', side = 'right'),
      barmode = 'group'
    )
}

# Fonction pour générer le graphique de l'évolution des passagers
plot_evolution_passagers_cie <- function(data, display_type) {
  data_all <- pax_cie_all %>%
    mutate(date = as.Date(paste(anmois, "01", sep=""), format="%Y%m%d")) %>%
    group_by(date, cie_nom) %>%
    summarize(total = sum(cie_pax, na.rm = TRUE)) %>%
    ungroup() %>%
    filter(cie_nom %in% (data %>%
                           group_by(cie_nom) %>%
                           summarize(total = sum(cie_pax, na.rm = TRUE)) %>%
                           arrange(desc(total)) %>%
                           head(10) %>%
                           pull(cie_nom)))
  
  plot_ly(data_all, x = ~date, y = ~total, color = ~cie_nom, type = 'scatter', mode = 'lines+markers', line = list(width = 1)) %>%
    layout(title = paste('Évolution', display_type, 'du nombre de passagers par compagnie'),
           xaxis = list(title = 'Période'),
           yaxis = list(title = 'Nombre de passagers'))
}

# Fonction pour générer le tableau des compagnies aériennes
table_detail_compagnies_cie <- function(data) {
  data <- data %>%
    select(
      Compagnie = cie_nom,
      `Nb de passagers` = cie_pax,
      `Nb de Vols` = cie_vol,
      `Nationalité Cie` = cie_nat,
      `Pays` = cie_pays
    ) %>%
    arrange(desc(`Nb de passagers`))
  
  datatable(data, options = list(pageLength = 5, searchHighlight = TRUE, autoWidth = TRUE,
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
                                 )
  ), filter = 'top', rownames = FALSE) %>%
    formatRound(columns = c('Nb de passagers', 'Nb de Vols'), digits = 0, mark = ' ')
}

# Fonction pour générer le graphique des nationalités des compagnies
plot_nationalite_compagnies_cie <- function(data) {
  data <- data %>%
    group_by(cie_nat) %>%
    summarize(total = sum(cie_pax, na.rm = TRUE)) %>%
    mutate(cie_nat = recode(cie_nat, "F" = "Français", "E" = "Étranger"))
  
  plot_ly(data, labels = ~cie_nat, values = ~total, type = 'pie', marker = list(colors = c("#77DD77", "#89CFF0"))) %>%
    layout(title = 'Répartition des passagers par nationalité de la compagnie')
}