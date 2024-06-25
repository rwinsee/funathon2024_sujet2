plot_airport_line <- function(df, selected_airport){
  trafic_aeroports <- df %>%
    mutate(trafic = apt_pax_dep + apt_pax_tr + apt_pax_arr) %>%
    filter(apt %in% selected_airport) %>%
    mutate(
      date = as.Date(paste(anmois, "01", sep=""), format = "%Y%m%d")
    )
  
  figure_plotly <- trafic_aeroports %>%
    plot_ly(
      x = ~date, y = ~trafic,
      text = ~apt_nom,
      hovertemplate = paste("<i>Aéroport:</i> %{text}<br>Trafic: %{y}"),
      type = 'scatter', mode = 'lines+markers'
    ) %>%
    layout(
      title = 'Fréquentation des aéroports',
      xaxis = list(
        title = 'Date',
        showgrid = TRUE,
        showline = TRUE,
        showticklabels = TRUE,
        ticks = 'outside',
        autotick = TRUE,
        tickwidth = 2,
        tickangle = -25,
        zeroline = FALSE
      ),
      yaxis = list(
        title = 'En milliers',
        autotick = TRUE,
        showgrid = TRUE,
        showline = TRUE,
        showticklabels = TRUE,
        tickwidth = 2,
        ticks = 'outside',
        tickvals = list(140000, 190000),
        tickangle = 45,
        zeroline = FALSE
      ),
      shapes = list(
        list(
          type = "line",
          x0 = covid_start,
          x1 = covid_start,
          y0 = 0,
          y1 = 1,
          xref = "x",
          yref = "paper",
          line = list(color = "red", dash = "dot", width = 1)
        ),
        list(
          type = "line",
          x0 = covid_end,
          x1 = covid_end,
          y0 = 0,
          y1 = 1,
          xref = "x",
          yref = "paper",
          line = list(color = "red", dash = "dot", width = 1)
        )
      ),
      annotations = list(
        list(
          x = covid_start,
          y = max(trafic_aeroports$trafic, na.rm = TRUE),
          xref = "x",
          yref = "y",
          text = "Début COVID FR",
          showarrow = TRUE,
          arrowhead = 2,
          ax = -50,
          ay = -40
        ),
        list(
          x = covid_end,
          y = max(trafic_aeroports$trafic, na.rm = TRUE),
          xref = "x",
          yref = "y",
          text = "Fin COVID FR",
          showarrow = TRUE,
          arrowhead = 2,
          ax = 40,
          ay = -40
        )
      )
    )
  
  return(figure_plotly)
}

map_leaflet_airport <- function(df, airports_location, month, year) {
  
  palette <- c("green", "blue", "red")
  
  # Convertir la date et filtrer les données selon le mois et l'année
  trafic_date <- df %>%
    mutate(date = as.Date(paste(anmois, "01", sep=""), format="%Y%m%d")) %>%
    filter(month(date) == month, year(date) == year)
  
  # Vérification des colonnes après filtrage
  print("Colonnes après filtrage:")
  print(colnames(trafic_date))
  
  # Vérification des colonnes avant la fusion
  print("Colonnes avant la fusion (airports_location):")
  print(colnames(airports_location))
  
  # Fusionner avec les informations de localisation des aéroports
  trafic_aeroports <- trafic_date %>%
    inner_join(airports_location, by = c("apt" = "Code.OACI"))
  
  # Vérification des colonnes après la fusion
  print("Colonnes après fusion:")
  print(colnames(trafic_aeroports))
  
  # Sélectionner les colonnes correctes après la fusion
  trafic_aeroports <- trafic_aeroports %>%
    mutate(
      apt_pax_dep = coalesce(trafic_aeroports$apt_pax_dep.x, trafic_aeroports$apt_pax_dep.y),
      apt_pax_arr = coalesce(trafic_aeroports$apt_pax_arr.x, trafic_aeroports$apt_pax_arr.y),
      apt_pax_tr = coalesce(trafic_aeroports$apt_pax_tr.x, trafic_aeroports$apt_pax_tr.y),
      trafic = apt_pax_dep + apt_pax_arr + apt_pax_tr
    )
  
  # Extraire les coordonnées de la géométrie
  coords <- st_coordinates(trafic_aeroports$geometry)
  trafic_aeroports$longitude <- coords[,1]
  trafic_aeroports$latitude <- coords[,2]
  
  trafic_aeroports <- trafic_aeroports %>%
    mutate(volume = ntile(trafic, 3)) %>%
    mutate(color = palette[volume])
  
  # Vérifier l'existence de la colonne Code.OACI après la fusion
  if (!"Code.OACI" %in% colnames(trafic_aeroports)) {
    trafic_aeroports$Code.OACI <- trafic_aeroports$apt
  }
  
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
      label = ~paste0(Nom, " (", Code.OACI, ") : ", trafic, " voyageurs")
    )
  
  return(carte_interactive)
}

# Fonction pour ajouter les annotations COVID
add_covid_annotations <- function(plot) {
  plot %>%
    layout(
      shapes = list(
        list(
          type = "line",
          x0 = covid_start,
          x1 = covid_start,
          y0 = 0,
          y1 = 1,
          xref = "x",
          yref = "paper",
          line = list(color = "red", dash = "dot", width = 1)
        ),
        list(
          type = "line",
          x0 = covid_end,
          x1 = covid_end,
          y0 = 0,
          y1 = 1,
          xref = "x",
          yref = "paper",
          line = list(color = "red", dash = "dot", width = 1)
        )
      ),
      annotations = list(
        list(
          x = covid_start,
          y = max(data$index_base_100, na.rm = TRUE),
          xref = "x",
          yref = "y",
          text = "Début COVID FR",
          showarrow = TRUE,
          arrowhead = 2,
          ax = -50,
          ay = -40
        ),
        list(
          x = covid_end,
          y = max(data$index_base_100, na.rm = TRUE),
          xref = "x",
          yref = "y",
          text = "Fin COVID FR",
          showarrow = TRUE,
          arrowhead = 2,
          ax = 40,
          ay = -40
        )
      )
    )
}