# plot_airport_line <- function(df, selected_airport){
#   trafic_aeroports <- df %>%
#     mutate(trafic = apt_pax_dep + apt_pax_tr + apt_pax_arr) %>%
#     filter(apt %in% selected_airport) %>%
#     mutate(
#       date = as.Date(paste(anmois, "01", sep=""), format = "%Y%m%d")
#     )
#   
#   figure_plotly <- trafic_aeroports %>%
#     plot_ly(
#       x = ~date, y = ~trafic,
#       text = ~apt_nom,
#       hovertemplate = paste("<i>Aéroport:</i> %{text}<br>Trafic: %{y}"),
#       type = 'scatter', mode = 'lines+markers'
#     ) %>%
#     layout(
#       title = 'Fréquentation des aéroports',
#       xaxis = list(
#         title = 'Date',
#         showgrid = TRUE,
#         showline = TRUE,
#         showticklabels = TRUE,
#         ticks = 'outside',
#         autotick = TRUE,
#         tickwidth = 2,
#         tickangle = -25,
#         zeroline = FALSE
#       ),
#       yaxis = list(
#         title = 'En milliers',
#         autotick = TRUE,
#         showgrid = TRUE,
#         showline = TRUE,
#         showticklabels = TRUE,
#         tickwidth = 2,
#         ticks = 'outside',
#         tickvals = list(140000, 190000),
#         tickangle = 45,
#         zeroline = FALSE
#       )
#     )
#   
#   return(figure_plotly)
# }

# map_leaflet_airport <- function(df, airports_location, month, year) {
#   
#   palette <- c("green", "blue", "red")
#   
#   # Convertir la date et filtrer les données selon le mois et l'année
#   trafic_date <- df %>%
#     mutate(date = as.Date(paste(anmois, "01", sep=""), format="%Y%m%d")) %>%
#     filter(month(date) == month, year(date) == year)
#   
#   # Vérification des colonnes après filtrage
#   # print("Colonnes après filtrage:")
#   # print(colnames(trafic_date))
#   # 
#   # Vérification des colonnes avant la fusion
#   # print("Colonnes avant la fusion (airports_location):")
#   # print(colnames(airports_location))
#   
#   # Fusionner avec les informations de localisation des aéroports
#   trafic_aeroports <- trafic_date %>%
#     inner_join(airports_location, by = c("apt" = "Code.OACI"))
#   
#   # Vérification des colonnes après la fusion
#   # print("Colonnes après fusion:")
#   # print(colnames(trafic_aeroports))
#   
#   # Sélectionner les colonnes correctes après la fusion
#   trafic_aeroports <- trafic_aeroports %>%
#     mutate(
#       apt_pax_dep = coalesce(trafic_aeroports$apt_pax_dep.x, trafic_aeroports$apt_pax_dep.y),
#       apt_pax_arr = coalesce(trafic_aeroports$apt_pax_arr.x, trafic_aeroports$apt_pax_arr.y),
#       apt_pax_tr = coalesce(trafic_aeroports$apt_pax_tr.x, trafic_aeroports$apt_pax_tr.y),
#       trafic = apt_pax_dep + apt_pax_arr + apt_pax_tr
#     )
#   
#   # Extraire les coordonnées de la géométrie
#   coords <- st_coordinates(trafic_aeroports$geometry)
#   trafic_aeroports$longitude <- coords[,1]
#   trafic_aeroports$latitude <- coords[,2]
#   
#   trafic_aeroports <- trafic_aeroports %>%
#     mutate(volume = ntile(trafic, 3)) %>%
#     mutate(color = palette[volume])
#   
#   # Vérifier l'existence de la colonne Code.OACI après la fusion
#   if (!"Code.OACI" %in% colnames(trafic_aeroports)) {
#     trafic_aeroports$Code.OACI <- trafic_aeroports$apt
#   }
#   
#   # Créer des icônes personnalisées pour les marqueurs
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
#       label = ~paste0(Nom, " (", Code.OACI, ") : ", trafic, " voyageurs")
#     )
#   
#   return(carte_interactive)
# }
# map_leaflet_airport <- function(df, airports_location, month, year) {
# 
#   palette <- c("green", "blue", "red")
# 
#   # Convertir la date et filtrer les données selon le mois et l'année
#   trafic_date <- df %>%
#     mutate(date = as.Date(paste(anmois, "01", sep=""), format="%Y%m%d")) %>%
#     filter(month(date) == month, year(date) == year)
# 
#   # Afficher les données après le filtrage
#   print("Données après filtrage par date:")
#   print(head(trafic_date))
# 
#   # Fusionner avec les informations de localisation des aéroports
#   trafic_aeroports <- trafic_date %>%
#     inner_join(airports_location, by = c("apt" = "Code.OACI"))
# 
#   # Afficher les données après la fusion
#   print("Données après la fusion avec les informations de localisation:")
#   print(head(trafic_aeroports))
# 
#   # Sélectionner les colonnes correctes après la fusion
#   trafic_aeroports <- trafic_aeroports %>%
#     mutate(
#       apt_pax_dep = coalesce(apt_pax_dep.x, apt_pax_dep.y),
#       apt_pax_arr = coalesce(apt_pax_arr.x, apt_pax_arr.y),
#       apt_pax_tr = coalesce(apt_pax_tr.x, apt_pax_tr.y),
#       trafic = apt_pax_dep + apt_pax_arr + apt_pax_tr
#     )
# 
#   # Afficher les données après la sélection des colonnes
#   print("Données après la sélection des colonnes:")
#   print(head(trafic_aeroports))
# 
#   # Extraire les coordonnées de la géométrie
#   coords <- st_coordinates(trafic_aeroports$geometry)
#   trafic_aeroports$longitude <- coords[,1]
#   trafic_aeroports$latitude <- coords[,2]
# 
#   # Afficher les coordonnées
#   print("Coordonnées extraites:")
#   print(head(coords))
# 
#   trafic_aeroports <- trafic_aeroports %>%
#     mutate(volume = ntile(trafic, 3)) %>%
#     mutate(color = palette[volume])
# 
#   # Afficher les données après la création des colonnes volume et color
#   print("Données après la création des colonnes volume et color:")
#   print(head(trafic_aeroports))
# 
#   # Vérifier l'existence de la colonne Code.OACI après la fusion
#   if (!"Code.OACI" %in% colnames(trafic_aeroports)) {
#     trafic_aeroports$Code.OACI <- trafic_aeroports$apt
#   }
# 
#   # Créer des icônes personnalisées pour les marqueurs
#   icons <- awesomeIcons(
#     icon = 'plane',
#     iconColor = 'black',
#     library = 'fa',
#     markerColor = trafic_aeroports$color
#   )
#   print("Création des icônes:")
#   print(icons)
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
#   return(carte_interactive)
# }
# # 
# # # Définir le mois et l'année à tester
# # month <- 1
# # year <- 2022
# # 
# # # Appeler la fonction pour générer la carte
# # carte <- map_leaflet_airport(pax_apt_all, airports_location, month, year)
# # 
# # # Afficher la carte dans la console R
# # carte
# 
# 
# 
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

map_leaflet_airport <- function(df, airports_location, month, year) {
  
  palette <- c("green", "blue", "red")
  
  # Convertir la date et filtrer les données selon le mois et l'année
  trafic_date <- df %>%
    mutate(date = as.Date(paste(anmois, "01", sep=""), format="%Y%m%d")) %>%
    filter(month(date) == month, year(date) == year)
  
  # Fusionner avec les informations de localisation des aéroports
  trafic_aeroports <- trafic_date %>%
    inner_join(airports_location, by = c("apt" = "Code.OACI"))
  
  # Sélectionner les colonnes pertinentes après la fusion
  trafic_aeroports <- trafic_aeroports %>%
    mutate(
      apt_pax_dep = coalesce(apt_pax_dep.x, apt_pax_dep.y),
      apt_pax_arr = coalesce(apt_pax_arr.x, apt_pax_arr.y),
      apt_pax_tr = coalesce(apt_pax_tr.x, apt_pax_tr.y),
      trafic = apt_pax_dep + apt_pax_arr + apt_pax_tr
    )
  
  # Extraire les coordonnées de la géométrie
  coords <- st_coordinates(trafic_aeroports$geometry)
  trafic_aeroports$longitude <- coords[,1]
  trafic_aeroports$latitude <- coords[,2]
  
  # Créer des icônes personnalisées pour les marqueurs
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
  
  return(carte_interactive)
}
