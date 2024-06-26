# Données filtrées pour les liaisons
filtered_lsn_data <- reactive({
  selected_year <- as.numeric(format(input$date_liaisons, "%Y"))
  selected_month <- as.numeric(format(input$date_liaisons, "%m"))
  
  filtered_df <- pax_lsn_all %>%
    mutate(date = as.Date(paste(anmois, "01", sep=""), format="%Y%m%d")) %>%
    filter(year(date) == selected_year, month(date) == selected_month, lsn_sct != "AUTRES")
  
  return(filtered_df)
})

# Value Box: Nombre Total de Liaisons
output$total_routes <- renderValueBox({
  total_routes <- filtered_lsn_data() %>%
    summarize(total = n())
  
  month_year <- format(input$date_liaisons, "%m/%y")
  
  valueBox(
    format(total_routes$total, big.mark = " "),
    paste("Nombre Total de Liaisons (", month_year, ")"),
    icon = icon("exchange"),
    color = "green"
  )
})

# Value Box: Route la Plus Fréquentée
output$top_route <- renderValueBox({
  top_route <- filtered_lsn_data() %>%
    group_by(lsn_dep_nom, lsn_arr_nom) %>%
    summarize(total = sum(lsn_pax_loc, na.rm = TRUE)) %>%
    arrange(desc(total)) %>%
    slice(1) %>%
    ungroup() %>%
    mutate(route = paste(lsn_dep_nom, "-", lsn_arr_nom)) %>%
    pull(route)
  
  valueBox(
    top_route,
    "Route la Plus Fréquentée",
    icon = icon("route"),
    color = "blue"
  )
})

# Graphique: Nombre de Passagers par Liaison
output$bar_liaisons <- renderPlotly({
  data <- filtered_lsn_data() %>%
    filter(!str_detect(lsn_dep_nom, "AUTRES") & !str_detect(lsn_arr_nom, "AUTRES")) %>%
    group_by(lsn_dep_nom, lsn_arr_nom) %>%
    summarize(total = sum(lsn_pax_loc, na.rm = TRUE)) %>%
    arrange(desc(total)) %>%
    head(10)
  data$liaison <- factor(paste(data$lsn_dep_nom, "<br>", data$lsn_arr_nom), 
                         levels = paste(data$lsn_dep_nom, "<br>", data$lsn_arr_nom))  
  plot_ly(data, x = ~liaison, y = ~total, type = 'bar', marker = list(color = '#77DD77')) %>%
    layout(title = 'Nombre de passagers par liaison',
           xaxis = list(title = 'Départ<>Arrivée de la liaison'),
           yaxis = list(title = 'Nombre de passagers'))
           
})


# Tableau DT: Détail des Passagers par Liaison
output$table_liaisons_detail <- renderDT({
  data <- pax_lsn_all %>%
    filter(!str_detect(lsn_dep_nom, "AUTRES") & !str_detect(lsn_arr_nom, "AUTRES")) %>%
    select(
      Départ = lsn_dep_nom,
      Arrivée = lsn_arr_nom,
      `Passagers Locaux` = lsn_pax_loc,
      `Passagers en Transit` = lsn_pax_fs
    ) %>%
    arrange(desc(`Passagers Locaux`))
  
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
                                 )), 
            filter = 'top', rownames = FALSE) %>%
    formatRound(columns = c('Passagers Locaux', 'Passagers en Transit'), digits = 0, mark = ' ')
})