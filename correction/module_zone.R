output$bar_vols_zones <- renderPlotly({
  data <- filtered_data() %>%
    mutate(Zone = recode(apt_zon, "MT" = "Métropole", "OM" = "Outre-Mer")) %>%
    group_by(Zone) %>%
    summarize(total_vols = sum(apt_nmvt_mxt + apt_nmvt_cgo, na.rm = TRUE)) %>%
    arrange(desc(total_vols))
  
  plot_ly(data, x = ~Zone, y = ~total_vols, type = 'bar', marker = list(color = c("Métropole" = "#77DD77", "Outre-Mer" = "#89CFF0"))) %>%
    layout(title = 'Nombre de Vols par Zone',
           xaxis = list(title = 'Zone'),
           yaxis = list(title = 'Nombre de Vols'))
})

output$detail_passagers_zone <- renderDT({
  data <- filtered_data() %>%
    mutate(Zone = recode(apt_zon, "MT" = "Métropole", "OM" = "Outre-Mer")) %>%
    group_by(Zone) %>%
    summarize(
      `Passagers Départ` = sum(apt_pax_dep, na.rm = TRUE),
      `Passagers Arrivée` = sum(apt_pax_arr, na.rm = TRUE),
      `Passagers Transit` = sum(apt_pax_tr, na.rm = TRUE),
      `Nombre de vols` = sum(apt_nmvt_mxt + apt_nmvt_cgo, na.rm = TRUE)
    ) %>%
    arrange(desc(`Passagers Départ`))
  
  datatable(data, options = list(pageLength = 10, searchHighlight = TRUE, autoWidth = TRUE), filter = 'top', rownames = FALSE) %>%
    formatRound(columns = c('Passagers Départ', 'Passagers Arrivée', 'Passagers Transit', 'Nombre de vols'), digits = 0, mark = ' ')
})

output$bar_passengers_zones <- renderPlotly({
  data <- filtered_data() %>%
    mutate(Zone = recode(apt_zon, "MT" = "Métropole", "OM" = "Outre-Mer")) %>%
    group_by(Zone) %>%
    summarize(total_passagers = sum(apt_pax_dep + apt_pax_arr + apt_pax_tr, na.rm = TRUE)) %>%
    arrange(desc(total_passagers))
  
  plot_ly(data, x = ~Zone, y = ~total_passagers, type = 'bar', marker = list(color = c("Métropole" = "#77DD77", "Outre-Mer" = "#89CFF0"))) %>%
    layout(title = 'Nombre de Passagers par Zone',
           xaxis = list(title = 'Zone'),
           yaxis = list(title = 'Nombre de Passagers', separator = " "))
})
