# Calcul des indicateurs pour la dernière période
# Calcul des indicateurs pour la dernière période
last_period <- max(as.Date(paste0(pax_apt_all$anmois, "01"), format = "%Y%m%d"))
previous_period <- last_period %m-% months(1)
last_quarter <- last_period %m-% months(3)
last_year <- last_period %m-% months(12)

last_period_label <- format(last_period, "%m/%y")
previous_period_label <- format(previous_period, "%m/%y")
monthly_growth_label <- paste(previous_period_label, "=>", last_period_label)
quarterly_growth_label <- paste(format(last_quarter, "%m/%y"), "=>", last_period_label)
annual_growth_label <- paste(format(last_year, "%m/%y"), "=>", last_period_label)

total_passengers_last_period <- sum(pax_apt_all %>% filter(anmois == format(last_period, "%Y%m")) %>% select(apt_pax_dep, apt_pax_arr, apt_pax_tr) %>% unlist(), na.rm = TRUE)
total_passengers_previous_period <- sum(pax_apt_all %>% filter(anmois == format(previous_period, "%Y%m")) %>% select(apt_pax_dep, apt_pax_arr, apt_pax_tr) %>% unlist(), na.rm = TRUE)
total_passengers_last_quarter <- sum(pax_apt_all %>% filter(anmois == format(last_quarter, "%Y%m")) %>% select(apt_pax_dep, apt_pax_arr, apt_pax_tr) %>% unlist(), na.rm = TRUE)
total_passengers_last_year <- sum(pax_apt_all %>% filter(anmois == format(last_year, "%Y%m")) %>% select(apt_pax_dep, apt_pax_arr, apt_pax_tr) %>% unlist(), na.rm = TRUE)

total_airports <- n_distinct(pax_apt_all$apt)
total_companies <- n_distinct(pax_cie_all$cie)


monthly_growth <- if (total_passengers_previous_period != 0) {
  (total_passengers_last_period - total_passengers_previous_period) / total_passengers_previous_period * 100
} else {
  NA
}

quarterly_growth <- if (total_passengers_last_quarter != 0) {
  (total_passengers_last_period - total_passengers_last_quarter) / total_passengers_last_quarter * 100
} else {
  NA
}

annual_growth <- if (total_passengers_last_year != 0) {
  (total_passengers_last_period - total_passengers_last_year) / total_passengers_last_year * 100
} else {
  NA
}


# Value Boxes
output$total_passengers_last_period <- renderValueBox({
  valueBox(
    format(total_passengers_last_period, big.mark = " "), 
    paste("Total Passagers (", last_period_label, ")", sep = ""), 
    icon = icon("users"), 
    color = "blue"
  )
})

output$monthly_growth <- renderValueBox({
  valueBox(
    sprintf("%.1f%%", monthly_growth), 
    paste("Évolution Mensuelle (", monthly_growth_label, ")", sep = ""), 
    icon = icon("line-chart"), 
    color = "green"
  )
})

output$quarterly_growth <- renderValueBox({
  valueBox(
    sprintf("%.1f%%", quarterly_growth), 
    paste("Évolution Trimestrielle (", quarterly_growth_label, ")", sep = ""), 
    icon = icon("bar-chart"), 
    color = "yellow"
  )
})

output$annual_growth <- renderValueBox({
  valueBox(
    sprintf("%.1f%%", annual_growth), 
    paste("Évolution Annuelle (", annual_growth_label, ")", sep = ""), 
    icon = icon("area-chart"), 
    color = "red"
  )
})

output$total_airports <- renderValueBox({
  valueBox(
    total_airports, 
    "Nombre d'Aéroports", 
    icon = icon("plane"), 
    color = "purple"
  )
})

output$total_companies <- renderValueBox({
  valueBox(
    total_companies, 
    "Nombre de Compagnies", 
    icon = icon("building"), 
    color = "orange"
  )
})

output$custom_indicator <- renderValueBox({
  valueBox(
    format(custom_indicator_value, big.mark = " "), 
    "Passagers Locaux", 
    icon = icon("street-view"), 
    color = "teal"
  )
})

output$stacked_bar_chart <- renderPlotly({
  df <- pax_apt_all %>%
    group_by(anmois) %>%
    summarise(
      dep = sum(apt_pax_dep, na.rm = TRUE),
      arr = sum(apt_pax_arr, na.rm = TRUE),
      tr = sum(apt_pax_tr, na.rm = TRUE)
    ) %>%
    mutate(total = dep + arr + tr) %>%
    arrange(anmois)
  
  plot_ly(df, x = ~anmois, y = ~dep, type = 'bar', name = 'Départs') %>%
    add_trace(y = ~arr, name = 'Arrivées') %>%
    add_trace(y = ~tr, name = 'Transits') %>%
    layout(barmode = 'stack', title = 'Nombre Total de Voyages par Mois',
           xaxis = list(title = 'Mois'), yaxis = list(title = 'Nombre de Passagers'))
})

output$market_share_chart <- renderPlotly({
  df <- pax_cie_all %>%
    group_by(cie_nom) %>%
    summarise(total_passengers = sum(cie_peq, na.rm = TRUE)) %>%
    arrange(desc(total_passengers)) %>%
    mutate(share = total_passengers / sum(total_passengers)) %>%
    mutate(cie_nom = if_else(share < 0.01, "Autres", cie_nom)) %>%
    group_by(cie_nom) %>%
    summarise(total_passengers = sum(total_passengers), share = sum(share)) %>%
    arrange(desc(total_passengers))
  
  plot_ly(df, labels = ~cie_nom, values = ~total_passengers, type = 'pie', textinfo = 'label+percent') %>%
    layout(title = 'Part de Marché des Compagnies Aériennes')
})

output$passenger_flight_line_chart <- renderPlotly({
  df <- pax_apt_all %>%
    group_by(anmois) %>%
    summarise(
      pax = sum(apt_pax_dep + apt_pax_arr + apt_pax_tr, na.rm = TRUE),
      vols = sum(apt_nmvt_mxt + apt_nmvt_cgo, na.rm = TRUE)
    ) %>%
    arrange(anmois)
  
  plot_ly(df, x = ~anmois) %>%
    add_lines(y = ~pax, name = 'Nombre de Passagers', yaxis = 'y1') %>%
    add_lines(y = ~vols, name = 'Nombre de Vols', yaxis = 'y2', line = list(color = 'red')) %>%
    layout(
      title = 'Nombre de Passagers et de Vols par Mois',
      xaxis = list(title = 'Mois'),
      yaxis = list(title = 'Nombre de Passagers', side = 'left', zeroline = FALSE),
      yaxis2 = list(title = 'Nombre de Vols', side = 'right', overlaying = 'y', zeroline = FALSE)
    )
})


output$indexed_passenger_chart <- renderPlotly({
  df <- pax_apt_all %>%
    group_by(anmois) %>%
    summarise(
      dep = sum(apt_pax_dep, na.rm = TRUE),
      arr = sum(apt_pax_arr, na.rm = TRUE),
      tr = sum(apt_pax_tr, na.rm = TRUE)
    ) %>%
    mutate(
      dep_index = dep / first(dep) * 100,
      arr_index = arr / first(arr) * 100,
      tr_index = tr / first(tr) * 100
    ) %>%
    arrange(anmois)
  
  plot_ly(df, x = ~anmois) %>%
    add_lines(y = ~dep_index, name = 'Index Départs') %>%
    add_lines(y = ~arr_index, name = 'Index Arrivées') %>%
    add_lines(y = ~tr_index, name = 'Index Transits') %>%
    layout(title = 'Indice des Types de Passagers par Mois',
           xaxis = list(title = 'Mois'), yaxis = list(title = 'Index (Base 100)'))
})