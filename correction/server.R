function(input, output) {
  
  # Afficher les noms des colonnes de airports_location pour vérification
  # print("Colonnes de airports_location :")
  # print(colnames(airports_location))
  
  # Fonction pour filtrer les données selon la date sélectionnée
  filtered_data <- reactive({
    selected_year <- as.numeric(format(input$date, "%Y"))
    selected_month <- as.numeric(format(input$date, "%m"))
    
    filtered_df <- pax_apt_all %>%
      mutate(date = as.Date(paste(anmois, "01", sep=""), format="%Y%m%d")) %>%
      filter(year(date) == selected_year, month(date) == selected_month)
    
    # print("Colonnes après filtrage des données :")
    # print(colnames(filtered_df))
    # 
    return(filtered_df)
  })
  
  table_liaisons <- reactive({
    summary_stat_airport(filtered_data())
  })
  
  output$table <- render_gt({
    create_table_airports <- function(data) {
      gt(data)
    }
    create_table_airports(table_liaisons())
  })
  
  output$carte <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = 1.888334, lat = 46.603354, zoom = 5)
  })
  
  observe({
    # Recréer la carte lorsque les données changent
    # print("Colonnes après filtrage des données :")
    # print(colnames(filtered_data()))
    
    trafic_aeroports <- filtered_data() %>%
      inner_join(airports_location, by = c("apt" = "Code.OACI"))
    
    # Vérification des colonnes après la fusion
    # print("Colonnes après fusion :")
    # print(colnames(trafic_aeroports))
    
    trafic_aeroports <- trafic_aeroports %>%
      mutate(
        apt_pax_dep = coalesce(apt_pax_dep.x, apt_pax_dep.y),
        apt_pax_arr = coalesce(apt_pax_arr.x, apt_pax_arr.y),
        apt_pax_tr = coalesce(apt_pax_tr.x, apt_pax_tr.y),
        trafic = apt_pax_dep + apt_pax_arr + apt_pax_tr
      ) %>%
      mutate(volume = ntile(trafic, 3)) %>%
      mutate(color = c("green", "blue", "red")[volume])
    
    # Extraire les coordonnées de la géométrie
    coords <- st_coordinates(trafic_aeroports$geometry)
    trafic_aeroports$longitude <- coords[,1]
    trafic_aeroports$latitude <- coords[,2]
    
    icons <- awesomeIcons(
      icon = 'plane',
      iconColor = 'black',
      library = 'fa',
      markerColor = trafic_aeroports$color
    )
    
    leafletProxy("carte", data = trafic_aeroports) %>%
      clearMarkers() %>%
      addAwesomeMarkers(
        lng = ~longitude,
        lat = ~latitude,
        icon = icons,
        label = ~paste0(Nom, " (", apt, ") : ", trafic, " voyageurs")
      )
  })
  
  output$lineplot <- renderPlotly({
    plot_airport_line(pax_apt_all, input$select)
  })
}