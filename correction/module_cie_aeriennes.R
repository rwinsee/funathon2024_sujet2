filtered_cie_data_v2 <- function(display_type, period_value) {
  print(paste("filtered_cie_data_v2 - display_type:", display_type))
  print(paste("filtered_cie_data_v2 - period_value:", period_value))
  
  if (display_type == "mens_uel") {
    selected_date <- as.Date(paste0(period_value, "-01"))
    selected_year <- as.numeric(format(selected_date, "%Y"))
    selected_month <- as.numeric(format(selected_date, "%m"))
    
    filtered_df_v2 <- pax_cie_all %>%
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
    
    filtered_df_v2 <- pax_cie_all %>%
      mutate(date = as.Date(paste(anmois, "01", sep = ""), format = "%Y%m%d")) %>%
      filter(year(date) == selected_year, month(date) %in% quarter_start_month:(quarter_start_month + 2)) %>%
      group_by(cie, cie_nom, cie_nat, cie_pays) %>%
      summarize(
        cie_pax = sum(cie_pax, na.rm = TRUE),
        cie_vol = sum(cie_vol, na.rm = TRUE),
        .groups = 'drop'
      )
  } else if (display_type == "ann_uel") {
    selected_year <- as.numeric(period_value)
    
    filtered_df_v2 <- pax_cie_all %>%
      mutate(date = as.Date(paste(anmois, "01", sep = ""), format = "%Y%m%d")) %>%
      filter(year(date) == selected_year) %>%
      group_by(cie, cie_nom, cie_nat, cie_pays) %>%
      summarize(
        cie_pax = sum(cie_pax, na.rm = TRUE),
        cie_vol = sum(cie_vol, na.rm = TRUE),
        .groups = 'drop'
      )
  } else {
    stop("Invalid display_type. Expected values: 'mens_uel', 'trimestr_iel', 'ann_uel'")
  }
  
  print("Filtered Data:")
  print(head(filtered_df_v2))
  
  return(filtered_df_v2)
}

output$period_selector_comp <- renderUI({
  max_date <- max(as.Date(paste0(pax_cie_all$anmois, "01"), format = "%Y%m%d"))
  
  if (input$display_type_comp == "mens_uel") {
    selectInput("selected_month_comp", "Choisir un mois:",
                choices = format(seq.Date(from = as.Date("2010-01-01"), to = max_date, by = "month"), "%Y-%m"),
                selected = format(max_date, "%Y-%m"))
  } else if (input$display_type_comp == "trimestr_iel") {
    tagList(
      selectInput("selected_year_for_quarter_comp", "Choisir une année:", 
                  choices = unique(year(seq.Date(from = as.Date("2010-01-01"), to = max_date, by = "year"))),
                  selected = year(max_date)),
      selectInput("selected_quarter_comp", "Choisir un trimestre:",
                  choices = c("T1", "T2", "T3", "T4"),
                  selected = paste0("T", ceiling(month(max_date) / 3)))
    )
  } else {
    selectInput("selected_year_comp", "Choisir une année:",
                choices = unique(year(seq.Date(from = as.Date("2010-01-01"), to = max_date, by = "year"))),
                selected = year(max_date))
  }
})

filtered_data_comp <- reactive({
  req(input$display_type_comp)
  
  if (input$display_type_comp == "mens_uel") {
    req(input$selected_month_comp)
    period_value <- input$selected_month_comp
  } else if (input$display_type_comp == "trimestr_iel") {
    req(input$selected_year_for_quarter_comp, input$selected_quarter_comp)
    period_value <- paste0(input$selected_year_for_quarter_comp, "-", input$selected_quarter_comp)
  } else if (input$display_type_comp == "ann_uel") {
    req(input$selected_year_comp)
    period_value <- input$selected_year_comp
  } else {
    NULL
  }
  
  print(paste("Calling filtered_cie_data_v2 with display_type:", input$display_type_comp, "and period_value:", period_value))
  
  result <- filtered_cie_data_v2(input$display_type_comp, period_value)
  
  print("Filtered Data in reactive function:")
  print(head(result))
  
  return(result)
})
output$period_selector_comp <- renderUI({
  print("Rendering period selector UI")
  max_date <- max(as.Date(paste0(pax_cie_all$anmois, "01"), format = "%Y%m%d"))
  
  if (input$display_type_comp == "mens_uel") {
    selectInput("selected_month_comp", "Choisir un mois:",
                choices = format(seq.Date(from = as.Date("2010-01-01"), to = max_date, by = "month"), "%Y-%m"),
                selected = format(max_date, "%Y-%m"))
  } else if (input$display_type_comp == "trimestr_iel") {
    tagList(
      selectInput("selected_year_for_quarter_comp", "Choisir une année:", 
                  choices = unique(year(seq.Date(from = as.Date("2010-01-01"), to = max_date, by = "year"))),
                  selected = year(max_date)),
      selectInput("selected_quarter_comp", "Choisir un trimestre:",
                  choices = c("T1", "T2", "T3", "T4"),
                  selected = paste0("T", ceiling(month(max_date) / 3)))
    )
  } else {
    selectInput("selected_year_comp", "Choisir une année:",
                choices = unique(year(seq.Date(from = as.Date("2010-01-01"), to = max_date, by = "year"))),
                selected = year(max_date))
  }
})

filtered_data_comp <- reactive({
  print("Reactive function: filtered_data_comp")
  req(input$display_type_comp)
  
  print(paste("Reactive function - input$display_type_comp:", input$display_type_comp))
  
  if (input$display_type_comp == "mens_uel") {
    req(input$selected_month_comp)
    period_value <- input$selected_month_comp
  } else if (input$display_type_comp == "trimestr_iel") {
    req(input$selected_year_for_quarter_comp, input$selected_quarter_comp)
    period_value <- paste0(input$selected_year_for_quarter_comp, "-", input$selected_quarter_comp)
  } else if (input$display_type_comp == "ann_uel") {
    req(input$selected_year_comp)
    period_value <- input$selected_year_comp
  } else {
    NULL
  }
  
  print(paste("Calling filtered_cie_data_v2 with display_type:", input$display_type_comp, "and period_value:", period_value))
  
  result <- filtered_cie_data_v2(input$display_type_comp, period_value)
  
  print("Filtered Data in reactive function:")
  print(head(result))
  
  return(result)
})

output$top_airline_comp <- renderValueBox({
  print("Rendering top_airline_comp value box")
  data <- filtered_data_comp()
  if (is.null(data)) {
    valueBox(
      "N/A",
      "Compagnie la plus fréquentée",
      icon = icon("plane"),
      color = "blue"
    )
  } else {
    top_airline <- data %>%
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
  }
})

output$top_flights_airline_comp <- renderValueBox({
  print("Rendering top_flights_airline_comp value box")
  data <- filtered_data_comp()
  if (is.null(data)) {
    valueBox(
      "N/A",
      "Compagnie avec le plus de vols",
      icon = icon("plane-departure"),
      color = "purple"
    )
  } else {
    top_flights_airline <- data %>%
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
  }
})

output$bar_compagnies_vols_comp <- renderPlotly({
  print("Rendering bar_compagnies_vols_comp plot")
  data <- filtered_data_comp()
  if (is.null(data)) {
    return(NULL)
  }
  plot_compagnies_vols_cie(data)
})

output$line_evolution_passagers_comp <- renderPlotly({
  print("Rendering line_evolution_passagers_comp plot")
  data <- filtered_data_comp()
  if (is.null(data)) {
    return(NULL)
  }
  plot_evolution_passagers_cie(data, input$display_type_comp, input$selected_period_comp)
})

output$table_detail_compagnies_comp <- renderDT({
  print("Rendering table_detail_compagnies_comp table")
  data <- filtered_data_comp()
  if (is.null(data)) {
    return(NULL)
  }
  table_detail_compagnies_cie(data)
})

output$pie_nationalite_compagnies_comp <- renderPlotly({
  print("Rendering pie_nationalite_compagnies_comp plot")
  data <- filtered_data_comp()
  if (is.null(data)) {
    return(NULL)
  }
  plot_nationalite_compagnies_cie(data)
})