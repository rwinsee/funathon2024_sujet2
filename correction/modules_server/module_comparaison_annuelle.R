output$line_comparaison <- renderPlotly({
  # Calcul des indices base 100 pour les passagers, les vols et le fret
  data_passagers <- pax_apt_all %>%
    group_by(an) %>%
    summarize(total_passagers = sum(apt_pax_dep + apt_pax_arr + apt_pax_tr, na.rm = TRUE)) %>%
    mutate(index_passagers = (total_passagers / first(total_passagers)) * 100)

  data_vols <- pax_apt_all %>%
    group_by(an) %>%
    summarize(total_vols = sum(apt_nmvt_mxt + apt_nmvt_cgo, na.rm = TRUE)) %>%
    mutate(index_vols = (total_vols / first(total_vols)) * 100)

  data_fret <- pax_cie_all %>%
    group_by(an) %>%
    summarize(total_fret = sum(cie_frp, na.rm = TRUE)) %>%
    mutate(index_fret = (total_fret / first(total_fret)) * 100)

  # Calcul des indices base 100 pour les passagers par zone
  data_zones <- pax_apt_all %>%
    mutate(Zone = recode(apt_zon, "mt" = "Métropole", "om" = "Outre-Mer")) %>%
    group_by(an, Zone) %>%
    summarize(total_passagers = sum(apt_pax_dep + apt_pax_arr + apt_pax_tr, na.rm = TRUE)) %>%
    mutate(index_passagers = (total_passagers / first(total_passagers)) * 100) %>%
    ungroup()

  data_zones_mt <- data_zones %>% filter(Zone == "Métropole") %>% select(an, index_passagers_mt = index_passagers)
  data_zones_om <- data_zones %>% filter(Zone == "Outre-Mer") %>% select(an, index_passagers_om = index_passagers)

  # Jointure des données
  data_combined <- data_passagers %>%
    left_join(data_vols, by = "an") %>%
    left_join(data_fret, by = "an") %>%
    left_join(data_zones_mt, by = "an") %>%
    left_join(data_zones_om, by = "an")

  # Création du graphique
  plot_ly(data_combined, x = ~an) %>%
    add_lines(y = ~index_passagers, name = 'Passagers (Total)', line = list(color = 'blue')) %>%
    add_lines(y = ~index_vols, name = 'Vols (Total)', line = list(color = 'green')) %>%
    add_lines(y = ~index_fret, name = 'Fret (Total)', line = list(color = 'orange')) %>%
    add_lines(y = ~index_passagers_mt, name = 'Passagers (Métropole)', line = list(color = 'purple')) %>%
    add_lines(y = ~index_passagers_om, name = 'Passagers (Outre-Mer)', line = list(color = 'red')) %>%
    layout(
      title = 'Comparaison Annuelle Indices Base 100 (2018)',
      xaxis = list(title = 'Année'),
      yaxis = list(title = 'Indice Base 100 (2018)', showgrid = TRUE, zeroline = FALSE))
})
