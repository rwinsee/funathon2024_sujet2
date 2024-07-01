main_color <- "black"

dates_available <- pax_apt_all %>%
  mutate(date = as.Date(paste(anmois, "01", sep=""), format="%Y%m%d")) %>%
  pull(date)

min_date <- min(dates_available)
max_date <- max(dates_available)

years_available <- unique(year(dates_available))
months_available <- unique(format(dates_available, "%Y-%m-01"))

input_date <- shinyWidgets::airDatepickerInput(
  "date",
  label = "Mois choisi",
  value = max_date,
  view = "months",
  minView = "months",
  minDate = min_date,
  maxDate = max_date,
  dateFormat = "MMMM yyyy",
  language = "fr"
)

# Créer les choix pour le selectInput
choices_airports <- setNames(
  liste_aeroports$apt,
  paste(liste_aeroports$apt_nom, " (", liste_aeroports$apt, ")", sep = "")
)

input_annee <- selectInput(
  "select_year",
  "Année choisie",
  choices = years_available,
  selected = max(years_available)
)

input_airport <- selectInput(
  "select",
  "Aéroport choisi",
  choices = choices_airports,
  selected = "PARIS-CHARLES DE GAULLE" #default_airport
)




ui <- dashboardPage(
  dashboardHeader(title = "TdB trafic aérien"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Accueil", tabName = "accueil", icon = icon("home")),
      menuItem("Indicateurs nationaux", tabName = "indic_nat", icon = icon("plane")),
      menuItem("Suivi par aéroport", tabName = "frequentation", icon = icon("dashboard")),
      menuItem("Compagnies Aériennes", tabName = "compagnies", icon = icon("plane")),
      menuItem("Liaisons Aériennes", tabName = "liaisons", icon = icon("exchange")),
      menuItem("Zones Géographiques", tabName = "zones", icon = icon("globe")),
      menuItem("Fret Aérien", tabName = "fret", icon = icon("cubes")),
      menuItem("Comparaison Annuelle", tabName = "comparaison", icon = icon("chart-line"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "accueil",
              fluidRow(
                box(
                  title = "Bienvenue sur le tableau de bord du trafic aérien",
                  status = "success",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  width = 12,
                  uiOutput("accueil_content")
                )
              )
      ),
      tabItem(tabName = "indic_nat",
              fluidRow(
                valueBoxOutput("total_passengers_last_period", width = 4),
                valueBoxOutput("monthly_growth", width = 4),
                valueBoxOutput("quarterly_growth", width = 4)
              ),
              fluidRow(
                valueBoxOutput("annual_growth", width = 4),
                valueBoxOutput("total_airports", width = 4),
                valueBoxOutput("total_companies", width = 4)
              ),
              fluidRow(
                column(width = 6, plotlyOutput("stacked_bar_chart")),
                column(width = 6, plotlyOutput("market_share_chart"))
              ),
              br(),
              fluidRow(
                column(width = 6, plotlyOutput("passenger_flight_line_chart")),
                column(width = 6, plotlyOutput("indexed_passenger_chart"))
              )
      ),
      tabItem(tabName = "frequentation",
              fluidRow(
                valueBoxOutput("total_passengers"),
                valueBoxOutput("busiest_airport"),
                valueBoxOutput("least_busy_airport"),
                valueBoxOutput("monthly_variation"),
                valueBoxOutput("annual_variation"),
                valueBoxOutput("total_flights")
              ),
              fluidRow(
                box(
                  title = "Fréquentation par aéroport",
                  status = "success",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  input_airport,
                  plotlyOutput("lineplot")
                ),
                box(
                  title = "Carte des aéroports",
                  status = "success",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  leafletOutput("carte")
                )
              ),
              fluidRow(
                box(
                  title = "Données détaillées mensuelles",
                  status = "success",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  input_date,
                  DTOutput("table")
                ),
                box(
                  title = "Données détaillées annuelles",
                  status = "success",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  input_annee,
                  DTOutput("table_annee")
                )
              )
      ),
      tabItem(tabName = "compagnies",
              fluidRow(
                valueBoxOutput("top_airline"),
                valueBoxOutput("top_flights_airline")
                # valueBoxOutput("total_routes")
              ),
              fluidRow(
                box(
                  title = "Nombre de passagers par compagnie",
                  status = "success",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  plotlyOutput("bar_compagnies_vols")
                ),
                box(
                  title = "Évolution mensuelle du nombre de passagers",
                  status = "success",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  plotlyOutput("line_evolution_passagers")
                )),
              fluidRow(
                box(
                  title = "Répartition des passagers par nationalité",
                  status = "success",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  plotlyOutput("pie_nationalite_compagnies")
                ),
                box(
                  title = "Détail des passagers par compagnie",
                  status = "success",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  DTOutput("table_detail_compagnies")
                )
              )
      ),
      tabItem(tabName = "liaisons",
              fluidRow(
                box(
                  title = "Sélectionner une date",
                  status = "success",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  shinyWidgets::airDatepickerInput(
                    "date_liaisons",
                    label = "Mois choisi",
                    value = "2019-01-01",
                    view = "months",
                    minView = "months",
                    minDate = "2018-01-01",
                    maxDate = "2022-12-01",
                    dateFormat = "MMMM yyyy",
                    language = "fr"
                  )
                ),
                valueBoxOutput("total_routes")),
              fluidRow(
                box(
                  title = "Nombre de passagers par liaison (hors AUTRES)",
                  status = "success",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  plotlyOutput("bar_liaisons")
                ),
                box(
                  title = "Détail des passagers par liaison (hors AUTRES)",
                  status = "success",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  DTOutput("table_liaisons_detail")
                )
              )
      ),
      tabItem(tabName = "zones",
              fluidRow(
                # valueBoxOutput("zone_most_passengers"),
                box(
                  title = "Vols par Zone",
                  status = "success",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  plotlyOutput("bar_vols_zones")
                ),
                box(
                  title = "Nombre de Passagers par Zone",
                  status = "success",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  plotlyOutput("bar_passengers_zones")
                )
              ),
              fluidRow(
                box(
                  title = "Détails",
                  status = "success",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  DTOutput("detail_passagers_zone")
                )
              )
              
      ),
      tabItem(tabName = "fret",
              fluidRow(
                valueBoxOutput("top1_fret_airline"),
                # valueBoxOutput("top2_fret_airline"),
                # valueBoxOutput("top3_fret_airline"),
                valueBoxOutput("least_fret_airline")
              ),
              fluidRow(
                box(
                  title = "Compagnies de Fret les plus actives",
                  status = "success",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  plotlyOutput("bar_fret")
                ),
                box(
                  title = "Détail des Ccompagnies de Fret",
                  status = "success",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  DTOutput("table_fret_detail")
                )
              )
      ),
      tabItem(tabName = "comparaison",
              fluidRow(
                box(
                  title = "Comparaison annuelle (Base 100 en 2018)",
                  status = "success",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  plotlyOutput("line_comparaison")
                )
              )
      )
      
      
      
    )
  ),
  skin = "green" 
)
