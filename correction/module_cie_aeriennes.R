
# Fonction pour filtrer les données selon la date sélectionnée
filtered_cie_data <- reactive({
  selected_year <- as.numeric(format(input$date, "%Y"))
  selected_month <- as.numeric(format(input$date, "%m"))
  
  filtered_df <- pax_cie_all %>%
    mutate(date = as.Date(paste(anmois, "01", sep=""), format="%Y%m%d")) %>%
    filter(year(date) == selected_year, month(date) == selected_month)
  
  return(filtered_df)
})

filtered_lsn_data <- reactive({
  selected_year <- as.numeric(format(input$date, "%Y"))
  selected_month <- as.numeric(format(input$date, "%m"))
  
  filtered_df <- pax_lsn_all %>%
    mutate(date = as.Date(paste(anmois, "01", sep=""), format="%Y%m%d")) %>%
    filter(year(date) == selected_year, month(date) == selected_month)
  
  return(filtered_df)
})

# Value Box: Compagnie la Plus Fréquentée
output$top_airline <- renderValueBox({
  top_airline <- filtered_cie_data() %>%
    group_by(cie_nom) %>%
    summarize(total = sum(cie_pax, na.rm = TRUE)) %>%
    arrange(desc(total)) %>%
    slice(1) %>%
    pull(cie_nom)

  valueBox(
    top_airline,
    "Compagnie la plus fréquentée",
    icon = icon("plane"),
    color = "blue"
  )
})

# Value Box: Compagnie avec le Plus de Vols
output$top_flights_airline <- renderValueBox({
  top_flights_airline <- filtered_cie_data() %>%
    group_by(cie_nom) %>%
    summarize(total = sum(cie_vol, na.rm = TRUE)) %>%
    arrange(desc(total)) %>%
    slice(1) %>%
    pull(cie_nom)
  
  valueBox(
    top_flights_airline,
    "Compagnie avec le plus de vols",
    icon = icon("plane-departure"),
    color = "purple"
  )
})

# Value Box: Nombre Total de Liaisons
output$total_routes <- renderValueBox({
  total_routes <- filtered_lsn_data() %>%
    summarize(total = n())
  
  month_year <- format(input$date, "%m/%y")
  
  valueBox(
    format(total_routes$total, big.mark = " "),
    paste("Nombre total de liaisons (", month_year, ")"),
    icon = icon("exchange"),
    color = "green"
  )
})

# Graphiques et Tableaux
output$bar_compagnies_vols <- renderPlotly({
  data_passagers <- filtered_cie_data() %>%
    group_by(cie_nom) %>%
    summarize(total_passagers = sum(cie_pax, na.rm = TRUE)) %>%
    arrange(desc(total_passagers)) %>%
    head(10)
  
  data_vols <- filtered_cie_data() %>%
    group_by(cie_nom) %>%
    summarize(total_vols = sum(cie_vol, na.rm = TRUE)) %>%
    arrange(desc(total_vols)) %>%
    head(10)
  
  data <- full_join(data_passagers, data_vols, by = "cie_nom")
  
  # Création d'un facteur pour le décalage des barres
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
})
output$line_evolution_passagers <- renderPlotly({
  data <- pax_cie_all %>%
    mutate(date = as.Date(paste(anmois, "01", sep=""), format="%Y%m%d")) %>%
    group_by(date, cie_nom) %>%
    summarize(total = sum(cie_pax, na.rm = TRUE)) %>%
    ungroup() %>%
    filter(cie_nom %in% (filtered_cie_data() %>%
                           group_by(cie_nom) %>%
                           summarize(total = sum(cie_pax, na.rm = TRUE)) %>%
                           arrange(desc(total)) %>%
                           head(10) %>%
                           pull(cie_nom)))
  
  plot_ly(data, x = ~date, y = ~total, color = ~cie_nom, type = 'scatter', mode = 'lines+markers', line = list(width = 1)) %>%
    layout(title = 'Évolution mensuelle du nombre de passagers par compagnie',
           xaxis = list(title = 'Période'),
           yaxis = list(title = 'Nombre de passagers'))
})



output$table_detail_compagnies <- renderDT({
  data <- filtered_cie_data() %>%
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
})

output$pie_nationalite_compagnies <- renderPlotly({
  data <- filtered_cie_data() %>%
    group_by(cie_nat) %>%
    summarize(total = sum(cie_pax, na.rm = TRUE)) %>%
    mutate(cie_nat = recode(cie_nat, "F" = "Français", "E" = "Étranger"))
  
  plot_ly(data, labels = ~cie_nat, values = ~total, type = 'pie', marker = list(colors = c("#77DD77", "#89CFF0"))) %>%
    layout(title = 'Répartition des passagers par nationalité de la compagnie')
})




