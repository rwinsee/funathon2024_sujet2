max_date <- max(as.Date(paste0(pax_apt_all$anmois, "01"), format = "%Y%m%d"))

output$period_selector <- renderUI({
  if (input$display_type == "mensuel") {
    selectInput("selected_month", "Choisir un mois:",
                choices = format(seq.Date(from = as.Date("2010-01-01"), to = max_date, by = "month"), "%Y-%m"),
                selected = format(max_date, "%Y-%m"))
  } else if (input$display_type == "trimestriel") {
    tagList(
      selectInput("selected_year_for_quarter", "Choisir une année:", 
                  choices = unique(year(seq.Date(from = as.Date("2010-01-01"), to = max_date, by = "year"))),
                  selected = year(max_date)),
      selectInput("selected_quarter", "Choisir un trimestre:",
                  choices = c("T1", "T2", "T3", "T4"),
                  selected = paste0("T", ceiling(month(max_date) / 3)))
    )
  } else {
    selectInput("selected_year", "Choisir une année:",
                choices = unique(year(seq.Date(from = as.Date("2010-01-01"), to = max_date, by = "year"))),
                selected = year(max_date))
  }
})

output$airport_selector <- renderUI({
  selectInput("selected_airport", "Choisir un aéroport:", 
              choices = c("Tous", unique(pax_apt_all$apt_nom)),
              selected = "Tous")
})

output$value_boxes <- renderUI({
  if (input$selected_airport == "Tous") {
    tagList(
      fluidRow(
        valueBoxOutput("total_passengers_freq"),
        valueBoxOutput("total_flights_freq"),
        valueBoxOutput("variation_freq"),
        # Espacement vide ou colonne vide pour le décalage
        div(style = "width: 100%;"),  # Espacement vide
        valueBoxOutput("busiest_airport_freq"),
        valueBoxOutput("least_busy_airport_freq")
      )
    )
  } else {
    tagList(
      fluidRow(
        valueBoxOutput("selected_airport_info_passengers"),
        valueBoxOutput("selected_airport_info_flights"),
        valueBoxOutput("variation_freq")
      )
    )
  }
})



filtered_data <- reactive({
  req(input$display_type)
  
  if (input$display_type == "mensuel") {
    req(input$selected_month)
    selected_date <- as.Date(paste0(input$selected_month, "-01"))
    selected_year <- year(selected_date)
    selected_month <- month(selected_date)
    
    filtered_df <- pax_apt_all %>%
      mutate(date = as.Date(paste(anmois, "01", sep=""), format="%Y%m%d")) %>%
      filter(year(date) == selected_year, month(date) == selected_month)
  } else if (input$display_type == "trimestriel") {
    req(input$selected_quarter, input$selected_year_for_quarter)
    quarter_start_month <- (as.numeric(substr(input$selected_quarter, 2, 2)) - 1) * 3 + 1
    selected_year <- as.numeric(input$selected_year_for_quarter)
    
    filtered_df <- pax_apt_all %>%
      mutate(date = as.Date(paste(anmois, "01", sep=""), format="%Y%m%d")) %>%
      filter(year(date) == selected_year, month(date) %in% quarter_start_month:(quarter_start_month + 2)) %>%
      group_by(apt, apt_nom) %>%
      summarize(
        apt_pax_dep = sum(apt_pax_dep, na.rm = TRUE),
        apt_pax_arr = sum(apt_pax_arr, na.rm = TRUE),
        apt_pax_tr = sum(apt_pax_tr, na.rm = TRUE),
        apt_nmvt_mxt = sum(apt_nmvt_mxt, na.rm = TRUE),
        apt_nmvt_cgo = sum(apt_nmvt_cgo, na.rm = TRUE),
        .groups = 'drop'
      )
  } else {
    req(input$selected_year)
    selected_year <- as.numeric(input$selected_year)
    
    filtered_df <- pax_apt_all %>%
      mutate(date = as.Date(paste(anmois, "01", sep=""), format="%Y%m%d")) %>%
      filter(year(date) == selected_year) %>%
      group_by(apt, apt_nom) %>%
      summarize(
        apt_pax_dep = sum(apt_pax_dep, na.rm = TRUE),
        apt_pax_arr = sum(apt_pax_arr, na.rm = TRUE),
        apt_pax_tr = sum(apt_pax_tr, na.rm = TRUE),
        apt_nmvt_mxt = sum(apt_nmvt_mxt, na.rm = TRUE),
        apt_nmvt_cgo = sum(apt_nmvt_cgo, na.rm = TRUE),
        .groups = 'drop'
      )
  }
  
  print("Données filtrées :")
  print(filtered_df)
  
  return(filtered_df)
})

previous_period_data <- reactive({
  req(input$display_type)
  
  if (input$display_type == "mensuel") {
    req(input$selected_month)
    selected_date <- as.Date(paste0(input$selected_month, "-01"))
    previous_date <- seq.Date(selected_date, length = 2, by = "-1 month")[2]
    
    filtered_df <- pax_apt_all %>%
      mutate(date = as.Date(paste(anmois, "01", sep=""), format="%Y%m%d")) %>%
      filter(year(date) == year(previous_date), month(date) == month(previous_date))
  } else if (input$display_type == "trimestriel") {
    req(input$selected_quarter, input$selected_year_for_quarter)
    selected_quarter <- as.numeric(substr(input$selected_quarter, 2, 2))
    previous_quarter <- ifelse(selected_quarter == 1, 4, selected_quarter - 1)
    previous_year <- ifelse(previous_quarter == 4, as.numeric(input$selected_year_for_quarter) - 1, as.numeric(input$selected_year_for_quarter))
    quarter_start_month <- (previous_quarter - 1) * 3 + 1
    
    filtered_df <- pax_apt_all %>%
      mutate(date = as.Date(paste(anmois, "01", sep=""), format="%Y%m%d")) %>%
      filter(year(date) == previous_year, month(date) %in% quarter_start_month:(quarter_start_month + 2)) %>%
      group_by(apt, apt_nom) %>%
      summarize(
        apt_pax_dep = sum(apt_pax_dep, na.rm = TRUE),
        apt_pax_arr = sum(apt_pax_arr, na.rm = TRUE),
        apt_pax_tr = sum(apt_pax_tr, na.rm = TRUE),
        apt_nmvt_mxt = sum(apt_nmvt_mxt, na.rm = TRUE),
        apt_nmvt_cgo = sum(apt_nmvt_cgo, na.rm = TRUE),
        .groups = 'drop'
      )
  } else {
    req(input$selected_year)
    selected_year <- as.numeric(input$selected_year)
    previous_year <- selected_year - 1
    
    filtered_df <- pax_apt_all %>%
      mutate(date = as.Date(paste(anmois, "01", sep=""), format="%Y%m%d")) %>%
      filter(year(date) == previous_year) %>%
      group_by(apt, apt_nom) %>%
      summarize(
        apt_pax_dep = sum(apt_pax_dep, na.rm = TRUE),
        apt_pax_arr = sum(apt_pax_arr, na.rm = TRUE),
        apt_pax_tr = sum(apt_pax_tr, na.rm = TRUE),
        apt_nmvt_mxt = sum(apt_nmvt_mxt, na.rm = TRUE),
        apt_nmvt_cgo = sum(apt_nmvt_cgo, na.rm = TRUE),
        .groups = 'drop'
      )
  }
  
  print("Données de la période précédente :")
  print(filtered_df)
  
  return(filtered_df)
})

# Value Box: Nombre Total de Passagers
output$total_passengers_freq <- renderValueBox({
  data <- filtered_data()
  
  total_passengers <- data %>%
    summarize(total = sum(apt_pax_dep + apt_pax_arr + apt_pax_tr, na.rm = TRUE)) %>%
    pull(total)
  
  if (is.na(total_passengers)) total_passengers <- 0
  
  valueBox(
    format(total_passengers, big.mark = " "),
    paste("Nombre total de passagers en France (", input$display_type, ")"),
    icon = icon("users"),
    color = "blue"
  )
})

# Value Box: Aéroport le Plus Fréquenté
output$busiest_airport_freq <- renderValueBox({
  data <- filtered_data()
  
  busiest_airport <- data %>%
    group_by(apt_nom) %>%
    summarize(total = sum(apt_pax_dep + apt_pax_arr + apt_pax_tr, na.rm = TRUE)) %>%
    arrange(desc(total)) %>%
    slice(1) %>%
    pull(apt_nom)
  
  if (is.na(busiest_airport)) busiest_airport <- "Aucun"
  
  valueBox(
    busiest_airport,
    paste("Aéroport le plus fréquenté (", input$display_type, ")"),
    icon = icon("plane"),
    color = "green"
  )
})

# Value Box: Aéroport le Moins Fréquenté
output$least_busy_airport_freq <- renderValueBox({
  data <- filtered_data()
  
  least_busy_airport <- data %>%
    group_by(apt_nom) %>%
    summarize(total = sum(apt_pax_dep + apt_pax_arr + apt_pax_tr, na.rm = TRUE)) %>%
    arrange(total) %>%
    slice(1) %>%
    pull(apt_nom)
  
  if (is.na(least_busy_airport)) least_busy_airport <- "Aucun"
  
  valueBox(
    least_busy_airport,
    paste("Aéroport le moins fréquenté (", input$display_type, ")"),
    icon = icon("plane"),
    color = "red"
  )
})

# Value Box: Variation
output$variation_freq <- renderValueBox({
  current_data <- filtered_data()
  previous_data <- previous_period_data()
  
  current_period_passengers <- current_data %>%
    summarize(total = sum(apt_pax_dep + apt_pax_arr + apt_pax_tr, na.rm = TRUE)) %>%
    pull(total)
  
  previous_period_passengers <- previous_data %>%
    summarize(total = sum(apt_pax_dep + apt_pax_arr + apt_pax_tr, na.rm = TRUE)) %>%
    pull(total)
  
  variation <- ifelse(previous_period_passengers == 0, NA, ((current_period_passengers - previous_period_passengers) / previous_period_passengers) * 100)
  
  period_label <- if (input$display_type == "mensuel") {
    "Evolution mensuelle du nombre de passagers"
  } else if (input$display_type == "trimestriel") {
    "Evolution trimestrielle du nombre de passagers"
  } else {
    "Evolution annuelle du nombre de passagers"
  }
  
  valueBox(
    if (is.na(variation)) "N/A" else sprintf("%.1f%%", variation),
    period_label,
    icon = icon("chart-line"),
    color = "purple"
  )
})

# Value Box: Nombre Total de Vols
output$total_flights_freq <- renderValueBox({
  data <- filtered_data()
  
  total_flights <- data %>%
    summarize(total = sum(apt_nmvt_mxt + apt_nmvt_cgo, na.rm = TRUE)) %>%
    pull(total)
  
  if (is.na(total_flights)) total_flights <- 0
  
  valueBox(
    format(total_flights, big.mark = " "),
    paste("Nombre total de vols en France (", input$display_type, ")"),
    icon = icon("plane"),
    color = "yellow"
  )
})

# Value Box: Aéroport consulté - Passagers
output$selected_airport_info_passengers <- renderValueBox({
  req(input$selected_airport != "Tous")
  data <- filtered_data() %>% filter(apt_nom == input$selected_airport)
  
  total_passengers <- data %>%
    summarize(total = sum(apt_pax_dep + apt_pax_arr + apt_pax_tr, na.rm = TRUE)) %>%
    pull(total)
  
  if (is.na(total_passengers)) total_passengers <- 0
  
  valueBox(
    format(total_passengers, big.mark = " "),
    paste("Nombre de passagers pour l'aéroport consulté (", input$display_type, ")"),
    icon = icon("plane"),
    color = "blue"
  )
})

# Value Box: Aéroport consulté - Vols
output$selected_airport_info_flights <- renderValueBox({
  req(input$selected_airport != "Tous")
  data <- filtered_data() %>% filter(apt_nom == input$selected_airport)
  
  total_flights <- data %>%
    summarize(total = sum(apt_nmvt_mxt + apt_nmvt_cgo, na.rm = TRUE)) %>%
    pull(total)
  
  if (is.na(total_flights)) total_flights <- 0
  
  valueBox(
    format(total_flights, big.mark = " "),
    paste("Nombre de vols (", input$display_type, ")"),
    icon = icon("plane"),
    color = "yellow"
  )
})

# Tableau des liaisons
output$table_freq <- renderDT({
  data <- filtered_data()
  
  if (input$selected_airport != "Tous") {
    data <- data %>% filter(apt_nom == input$selected_airport)
  }
  
  df <- data %>%
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

# Création du graphique Plotly
output$lineplot <- renderPlotly({
  selected_airport <- input$selected_airport
  plot_airport_line(pax_apt_all, selected_airport, input$display_type)
})

# Observer to update map based on filtered data
observe({
  req(input$period_selector, input$airport_selector)
  
  # Filtrer les données basées sur le mois et l'année sélectionnés
  month_selected <- switch(input$period_selector,
                           "Mensuel" = 1,
                           "Trimestriel" = 3,
                           "Annuel" = 12)
  
  year_selected <- as.numeric(input$airport_selector)
  
  # Appeler votre fonction map_leaflet_airport avec les données filtrées
  output$carte <- renderLeaflet({
    map_leaflet_airport(df = filtered_data(), airports_location, month_selected, year_selected)
  })
})