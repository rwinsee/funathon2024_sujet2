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
    paste("Total passagers (", last_period_label, ")", sep = ""), 
    icon = icon("users"), 
    color = "blue"
  )
})

output$monthly_growth <- renderValueBox({
  valueBox(
    sprintf("%.1f%%", monthly_growth), 
    paste("Évolution mensuelle (", monthly_growth_label, ")", sep = ""), 
    icon = icon("line-chart"), 
    color = "green"
  )
})

output$quarterly_growth <- renderValueBox({
  valueBox(
    sprintf("%.1f%%", quarterly_growth), 
    paste("Évolution trimestrielle (", quarterly_growth_label, ")", sep = ""), 
    icon = icon("bar-chart"), 
    color = "yellow"
  )
})

output$annual_growth <- renderValueBox({
  valueBox(
    sprintf("%.1f%%", annual_growth), 
    paste("Évolution annuelle (", annual_growth_label, ")", sep = ""), 
    icon = icon("area-chart"), 
    color = "red"
  )
})

output$total_airports <- renderValueBox({
  valueBox(
    total_airports, 
    "Nombre d'aéroports suivis", 
    icon = icon("plane"), 
    color = "purple"
  )
})

output$total_companies <- renderValueBox({
  valueBox(
    total_companies, 
    "Nombre de compagnies suivies", 
    icon = icon("building"), 
    color = "orange"
  )
})

output$custom_indicator <- renderValueBox({
  valueBox(
    format(custom_indicator_value, big.mark = " "), 
    "Passagers locaux", 
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
    mutate(formatted_date = format(as.Date(paste0(anmois, "01"), "%Y%m%d"), "%m-%Y")) %>%
    arrange(anmois)
  
  df$formatted_date <- factor(df$formatted_date, levels = unique(df$formatted_date))
  
  plot_ly(df, x = ~formatted_date, y = ~dep, type = 'bar', name = 'Départs') %>%
    add_trace(y = ~arr, name = 'Arrivées') %>%
    add_trace(y = ~tr, name = 'Transits') %>%
    layout(
      barmode = 'stack',
      title = 'Nombre de passagers par mois',
      xaxis = list(
        title = 'Période',
        showgrid = FALSE,
        showline = TRUE,
        tickwidth = 2, 
        autotick = TRUE,
        tickangle = -45,
        ticks = 'outside',
        zeroline = FALSE
      ),
      yaxis = list(
        title = 'Nombre de passagers',
        autotick = TRUE,
        showgrid = TRUE,
        showline = TRUE,
        showticklabels = TRUE,
        tickwidth = 2,
        ticks = 'outside',
        zeroline = FALSE
      ),
      hovermode = 'x unified',
      hoverlabel = list(
        namelength = -1
      )
    )
})


output$market_share_chart <- renderPlotly({
  df <- pax_cie_all %>%
    group_by(cie_nom) %>%
    summarise(total_passengers = sum(cie_peq, na.rm = TRUE)) %>%
    arrange(desc(total_passengers)) %>%
    mutate(share = total_passengers / sum(total_passengers)) %>%
    mutate(cie_nom = if_else(share < 0.01, "Autres Cie Inf. à 1%", cie_nom)) %>%
    group_by(cie_nom) %>%
    summarise(total_passengers = sum(total_passengers), share = sum(share)) %>%
    arrange(desc(total_passengers))
  
  plot_ly(df, labels = ~cie_nom, values = ~total_passengers, type = 'pie', textinfo = 'label+percent', textposition = 'inside',
          texttemplate = ~paste0(cie_nom, "<br>", format(total_passengers, big.mark = " ", scientific = FALSE), " passagers (", scales::percent(share, accuracy = 0.1), ")")) %>%
    layout(
      title = 'Part de marché des compagnies aériennes',
      showlegend = TRUE,
      legend = list(
        orientation = "v",
        x = 1,
        xanchor = "left",
        y = 0.5,
        yanchor = "center"
      )
    )
})


output$passenger_flight_line_chart <- renderPlotly({
  df <- pax_apt_all %>%
    group_by(anmois) %>%
    summarise(
      pax = sum(apt_pax_dep + apt_pax_arr + apt_pax_tr, na.rm = TRUE),
      vols = sum(apt_nmvt_mxt + apt_nmvt_cgo, na.rm = TRUE)
    ) %>%
    mutate(formatted_date = format(as.Date(paste0(anmois, "01"), "%Y%m%d"), "%m-%Y")) %>%
    arrange(anmois)
  
  df$formatted_date <- factor(df$formatted_date, levels = unique(df$formatted_date))
  
  plot_ly(df, x = ~formatted_date) %>%
    add_lines(y = ~pax, name = 'Nombre de passagers', yaxis = 'y1') %>%
    add_lines(y = ~vols, name = 'Nombre de vols', yaxis = 'y2', line = list(color = 'red')) %>%
    layout(
      title = 'Nombre de passagers et de vols par mois',
      xaxis = list(
        title = 'Période',
        showgrid = FALSE,
        showline = TRUE,
        tickwidth = 2, 
        autotick = TRUE,
        tickangle = -45,
        ticks = 'outside',
        zeroline = FALSE
      ),
      yaxis = list(
        title = 'Nb. de passagers',
        side = 'left',
        autotick = TRUE,
        showgrid = TRUE,
        showline = TRUE,
        showticklabels = TRUE,
        tickwidth = 2,
        ticks = 'outside',
        zeroline = FALSE
      ),
      yaxis2 = list(
        title = 'Nb. de vols',
        side = 'right',
        overlaying = 'y',
        autotick = TRUE,
        showgrid = FALSE,
        showline = TRUE,
        showticklabels = TRUE,
        tickwidth = 2,
        ticks = 'outside',
        zeroline = FALSE
      ),
      hovermode = 'x unified',
      hoverlabel = list(namelength = -1)
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
    mutate(formatted_date = format(as.Date(paste0(anmois, "01"), "%Y%m%d"), "%m-%Y")) %>%
    arrange(anmois)
  
  df$formatted_date <- factor(df$formatted_date, levels = unique(df$formatted_date))
  
  plot_ly(df, x = ~formatted_date) %>%
    add_lines(y = ~dep_index, name = 'Passagers au départ') %>%
    add_lines(y = ~arr_index, name = 'Pasagers à arrivée') %>%
    add_lines(y = ~tr_index, name = 'Passager en transit') %>%
    layout(
      title = 'Évolution en indice base100 (Janvier 2010) selon le type de voyageur',
      xaxis = list(
        title = 'Période',
        showgrid = FALSE,
        showline = TRUE,
        tickwidth = 2, 
        autotick = TRUE,
        tickangle = -45,
        ticks = 'outside',
        zeroline = FALSE
      ),
      yaxis = list(
        title = 'Index (Base 100)',
        autotick = TRUE,
        showgrid = TRUE,
        showline = TRUE,
        showticklabels = TRUE,
        tickwidth = 2,
        ticks = 'outside',
        zeroline = FALSE
      ),
      hovermode = 'x unified',
      hoverlabel = list(namelength = -1)
    )
})
