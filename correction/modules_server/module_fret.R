# Fonction pour filtrer les données selon la date sélectionnée
filtered_cie_data <- reactive({
  selected_year <- as.numeric(format(input$date, "%Y"))
  selected_month <- as.numeric(format(input$date, "%m"))
  
  filtered_df <- pax_cie_all %>%
    mutate(date = as.Date(paste(anmois, "01", sep=""), format="%Y%m%d")) %>%
    filter(year(date) == selected_year, month(date) == selected_month)
  
  return(filtered_df)
})

# Graphique: Compagnies de Fret les Plus Actives
output$bar_fret <- renderPlotly({
  data <- filtered_cie_data() %>%
    group_by(cie_nom) %>%
    summarize(total = sum(cie_frp, na.rm = TRUE)) %>%
    arrange(desc(total)) %>%
    head(10)
  data$cie_nom <- factor(data$cie_nom, levels = data$cie_nom) # Tri décroissant
  
  # Calcul de la moyenne et de la médiane
  mean_fret <- mean(data$total, na.rm = TRUE)
  median_fret <- median(data$total, na.rm = TRUE)
  
  plot_ly(data, x = ~cie_nom, y = ~total, type = 'bar', marker = list(color = '#77DD77'), name = 'Total Fret') %>%
    add_lines(x = ~cie_nom, y = rep(mean_fret, nrow(data)), name = 'Moyenne', line = list(color = 'red', dash = 'dash', width = 1), showlegend = TRUE, hoverinfo = 'none') %>%
    add_lines(x = ~cie_nom, y = rep(median_fret, nrow(data)), name = 'Médiane', line = list(color = 'green', dash = 'dash', width = 1), showlegend = TRUE, hoverinfo = 'none') %>%
    layout(
      title = 'Compagnies de Fret les plus actives',
      xaxis = list(title = 'Compagnie'),
      yaxis = list(title = 'Total Fret')
    )
})
# Value Box: Compagnie la Plus Axée Fret (Top 1)
output$top1_fret_airline <- renderValueBox({
  top1_fret_airline <- filtered_cie_data() %>%
    group_by(cie_nom) %>%
    summarize(total = sum(cie_frp, na.rm = TRUE)) %>%
    arrange(desc(total)) %>%
    slice(1) %>%
    pull(cie_nom)
  
  valueBox(
    top1_fret_airline,
    "Top 1 compagnie axée Fret",
    icon = icon("box"),
    color = "blue"
  )
})

# Value Box: Compagnie la Deuxième Plus Axée Fret (Top 2)
output$top2_fret_airline <- renderValueBox({
  top2_fret_airline <- filtered_cie_data() %>%
    group_by(cie_nom) %>%
    summarize(total = sum(cie_frp, na.rm = TRUE)) %>%
    arrange(desc(total)) %>%
    slice(2) %>%
    pull(cie_nom)
  
  valueBox(
    top2_fret_airline,
    "Top 2 compagnie axée Fret",
    icon = icon("box"),
    color = "blue"
  )
})

# Value Box: Compagnie la Troisième Plus Axée Fret (Top 3)
output$top3_fret_airline <- renderValueBox({
  top3_fret_airline <- filtered_cie_data() %>%
    group_by(cie_nom) %>%
    summarize(total = sum(cie_frp, na.rm = TRUE)) %>%
    arrange(desc(total)) %>%
    slice(3) %>%
    pull(cie_nom)
  
  valueBox(
    top3_fret_airline,
    "Top 3 compagnie axée Fret",
    icon = icon("box"),
    color = "blue"
  )
})

# Value Box: Compagnie la Moins Axée Fret
output$least_fret_airline <- renderValueBox({
  least_fret_airline <- filtered_cie_data() %>%
    group_by(cie_nom) %>%
    summarize(total = sum(cie_frp, na.rm = TRUE)) %>%
    arrange(total) %>%
    slice(1) %>%
    pull(cie_nom)
  
  valueBox(
    least_fret_airline,
    "Compagnie la moins axée Fret",
    icon = icon("box-open"),
    color = "red"
  )
})

# Tableau DT: Détail des Compagnies de Fret
output$table_fret_detail <- renderDT({
  data <- filtered_cie_data() %>%
    select(
      Compagnie = cie_nom,
      `Total Fret` = cie_frp,
      `Nombre de vols` = cie_vol
    ) %>%
    arrange(desc(`Total Fret`))
  
  datatable(data, options = list(
    pageLength = 5, 
    searchHighlight = TRUE, 
    autoWidth = TRUE, 
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
    formatRound(columns = c('Total Fret', 'Nombre de vols'), digits = 0, mark = ' ')
})
