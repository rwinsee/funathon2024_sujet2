# Fonction de filtrage des données pour le fret
filtered_fret_data <- function(display_type, period_value) {
  print(paste("filtered_fret_data - display_type:", display_type))
  print(paste("filtered_fret_data - period_value:", period_value))
  
  if (display_type == "mens_uel") {
    selected_date <- as.Date(paste0(period_value, "-01"))
    selected_year <- as.numeric(format(selected_date, "%Y"))
    selected_month <- as.numeric(format(selected_date, "%m"))
    
    filtered_df_fret <- pax_cie_all %>%
      mutate(date = as.Date(paste(anmois, "01", sep = ""), format = "%Y%m%d")) %>%
      filter(year(date) == selected_year, month(date) == selected_month)
  } else if (display_type == "trimestr_iel") {
    if (!grepl("^\\d{4}-T[1-4]$", period_value)) {
      stop("Invalid period_value format for trimestr_iel. Expected format: YYYY-Tx (e.g., 2021-T2)")
    }
    print(paste("Period Value:", period_value))
    
    quarter_str <- substr(period_value, 7, 7)
    quarter <- as.numeric(quarter_str)
    quarter_start_month <- ifelse(is.na(quarter), NA, (quarter - 1) * 3 + 1)
    selected_year <- as.numeric(substr(period_value, 1, 4))
    
    print(paste("Quarter String:", quarter_str))
    print(paste("Quarter:", quarter))
    print(paste("Quarter Start Month:", quarter_start_month))
    print(paste("Selected Year:", selected_year))
    
    if (is.na(quarter) || is.na(quarter_start_month) || is.na(selected_year)) {
      stop("Quarter, quarter start month, or selected year is NA. Check the period_value format and content.")
    }
    
    filtered_df_fret <- pax_cie_all %>%
      mutate(date = as.Date(paste(anmois, "01", sep = ""), format = "%Y%m%d")) %>%
      filter(year(date) == selected_year, month(date) %in% quarter_start_month:(quarter_start_month + 2)) %>%
      group_by(cie, cie_nom, cie_nat, cie_pays) %>%
      summarize(
        cie_frp = sum(cie_frp, na.rm = TRUE),
        cie_vol = sum(cie_vol, na.rm = TRUE),
        .groups = 'drop'
      )
  } else if (display_type == "ann_uel") {
    selected_year <- as.numeric(period_value)
    
    filtered_df_fret <- pax_cie_all %>%
      mutate(date = as.Date(paste(anmois, "01", sep = ""), format = "%Y%m%d")) %>%
      filter(year(date) == selected_year) %>%
      group_by(cie, cie_nom, cie_nat, cie_pays) %>%
      summarize(
        cie_frp = sum(cie_frp, na.rm = TRUE),
        cie_vol = sum(cie_vol, na.rm = TRUE),
        .groups = 'drop'
      )
  } else {
    stop("Invalid display_type. Expected values: 'mens_uel', 'trimestr_iel', 'ann_uel'")
  }
  
  print("Filtered Data:")
  print(head(filtered_df_fret))
  
  return(filtered_df_fret)
}


output$period_selector_fret <- renderUI({
  print("Rendering period selector UI")
  max_date <- max(as.Date(paste0(pax_cie_all$anmois, "01"), format = "%Y%m%d"))
  
  if (input$display_type_fret == "mens_uel") {
    selectInput("selected_month_fret", "Choisir un mois:",
                choices = format(seq.Date(from = as.Date("2010-01-01"), to = max_date, by = "month"), "%Y-%m"),
                selected = format(max_date, "%Y-%m"))
  } else if (input$display_type_fret == "trimestr_iel") {
    tagList(
      selectInput("selected_year_for_quarter_fret", "Choisir une année:", 
                  choices = unique(year(seq.Date(from = as.Date("2010-01-01"), to = max_date, by = "year"))),
                  selected = year(max_date)),
      selectInput("selected_quarter_fret", "Choisir un trimestre:",
                  choices = c("T1", "T2", "T3", "T4"),
                  selected = paste0("T", ceiling(month(max_date) / 3)))
    )
  } else {
    selectInput("selected_year_fret", "Choisir une année:",
                choices = unique(year(seq.Date(from = as.Date("2010-01-01"), to = max_date, by = "year"))),
                selected = year(max_date))
  }
})

filtered_data_fret <- reactive({
  print("Reactive function: filtered_data_fret")
  req(input$display_type_fret)
  
  print(paste("Reactive function - input$display_type_fret:", input$display_type_fret))
  
  if (input$display_type_fret == "mens_uel") {
    req(input$selected_month_fret)
    period_value <- input$selected_month_fret
  } else if (input$display_type_fret == "trimestr_iel") {
    req(input$selected_year_for_quarter_fret, input$selected_quarter_fret)
    period_value <- paste0(input$selected_year_for_quarter_fret, "-", input$selected_quarter_fret)
  } else if (input$display_type_fret == "ann_uel") {
    req(input$selected_year_fret)
    period_value <- input$selected_year_fret
  } else {
    NULL
  }
  
  print(paste("Calling filtered_fret_data with display_type:", input$display_type_fret, "and period_value:", period_value))
  
  filtered_fret_data(input$display_type_fret, period_value)
})

output$bar_fret <- renderPlotly({
  print("Rendering bar_fret plot")
  data <- filtered_data_fret() %>%
    group_by(cie_nom) %>%
    summarize(total = sum(cie_frp, na.rm = TRUE)) %>%
    arrange(desc(total)) %>%
    head(10)
  data$cie_nom <- factor(data$cie_nom, levels = data$cie_nom) # Tri décroissant
  
  # Calcul de la moyenne et de la médiane
  mean_fret <- mean(data$total, na.rm = TRUE)
  median_fret <- median(data$total, na.rm = TRUE)
  
  plot_ly(data, x = ~cie_nom, y = ~total, type = 'bar', marker = list(color = '#77DD77'), name = 'Total Fret') %>%
    add_lines(x = ~cie_nom, y = rep(mean_fret, nrow(data)), name = 'Moyenne', line = list(color = 'red', dash = 'dash', width = 1), showlegend = TRUE, hoverinfo = 'text', text = ~paste("Moyenne:", mean_fret)) %>%
    add_lines(x = ~cie_nom, y = rep(median_fret, nrow(data)), name = 'Médiane', line = list(color = 'green', dash = 'dash', width = 1), showlegend = TRUE, hoverinfo = 'text', text = ~paste("Médiane:", median_fret)) %>%
    layout(
      title = 'Compagnies de Fret les plus actives',
      xaxis = list(title = 'Compagnie'),
      yaxis = list(title = 'Total Fret')
    )
})

output$top1_fret_airline <- renderValueBox({
  print("Rendering top1_fret_airline value box")
  top1_fret_airline <- filtered_data_fret() %>%
    group_by(cie_nom) %>%
    summarize(total = sum(cie_frp, na.rm = TRUE)) %>%
    arrange(desc(total)) %>%
    slice(1) %>%
    pull(cie_nom)
  
  valueBox(
    top1_fret_airline,
    "Top 1 compagnie axée Fret",
    icon = icon("box"),
    color = "green"
  )
})

output$top2_fret_airline <- renderValueBox({
  print("Rendering top2_fret_airline value box")
  top2_fret_airline <- filtered_data_fret() %>%
    group_by(cie_nom) %>%
    summarize(total = sum(cie_frp, na.rm = TRUE)) %>%
    arrange(desc(total)) %>%
    slice(2) %>%
    pull(cie_nom)
  
  valueBox(
    top2_fret_airline,
    "Top 2 compagnie axée Fret",
    icon = icon("box"),
    color = "orange"
  )
})

output$top3_fret_airline <- renderValueBox({
  print("Rendering top3_fret_airline value box")
  top3_fret_airline <- filtered_data_fret() %>%
    group_by(cie_nom) %>%
    summarize(total = sum(cie_frp, na.rm = TRUE)) %>%
    arrange(desc(total)) %>%
    slice(3) %>%
    pull(cie_nom)
  
  valueBox(
    top3_fret_airline,
    "Top 3 compagnie axée Fret",
    icon = icon("box"),
    color = "yellow"
  )
})

output$least_fret_airline <- renderValueBox({
  print("Rendering least_fret_airline value box")
  least_fret_airline <- filtered_data_fret() %>%
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

output$table_fret_detail <- renderDT({
  print("Rendering table_fret_detail table")
  data <- filtered_data_fret() %>%
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

output$value_boxes_fret <- renderUI({
  tagList(
    fluidRow(
      valueBoxOutput("top1_fret_airline"),
      valueBoxOutput("top2_fret_airline"),
      valueBoxOutput("top3_fret_airline"),
      valueBoxOutput("least_fret_airline")
    )
  )
})
