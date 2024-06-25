main_color <- "black"

input_date <- shinyWidgets::airDatepickerInput(
  "date",
  label = "Mois choisi",
  value = "2019-01-01",
  view = "months",
  minView = "months",
  minDate = "2018-01-01",
  maxDate = "2022-12-01",
  dateFormat = "MMMM yyyy",
  language = "fr"
)

# input_airport <- selectInput(
#   "select",
#   "Aéroport choisi",
#   choices = liste_aeroports,
#   selected = default_airport
# )

# Créer les choix pour le selectInput
choices_airports <- setNames(
  liste_aeroports$apt,
  paste(liste_aeroports$apt_nom, " (", liste_aeroports$apt, ")", sep = "")
)

input_annee <- selectInput(
  "select_year",
  "Année choisie",
  choices = 2018:2022,
  selected = 2019
)

input_airport <- selectInput(
  "select",
  "Aéroport choisi",
  choices = choices_airports,
  selected = "PARIS-CHARLES DE GAULLE"#default_airport
)


ui <- dashboardPage(
  dashboardHeader(title = "DB aéroports FR"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Fréquentation FR", tabName = "frequentation", icon = icon("dashboard")),
      menuItem("Compagnies Aériennes", tabName = "compagnies", icon = icon("plane")),
      menuItem("Liaisons Aériennes", tabName = "liaisons", icon = icon("exchange")),
      menuItem("Zones Géographiques", tabName = "zones", icon = icon("globe")),
      menuItem("Fret Aérien", tabName = "fret", icon = icon("cubes")),
      menuItem("Comparaison Annuelle", tabName = "comparaison", icon = icon("chart-line"))
    )
  ),
  dashboardBody(
    tabItems(
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
                  title = "Fréquentation d'un aéroport",
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
                  title = "Nombre de Passagers par Compagnie",
                  status = "success",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  plotlyOutput("bar_compagnies_vols")
                ),
                box(
                  title = "Évolution Mensuelle du Nombre de Passagers",
                  status = "success",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  plotlyOutput("line_evolution_passagers")
                )),
                fluidRow(
                box(
                  title = "Répartition des Passagers par Nationalité",
                  status = "success",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  plotlyOutput("pie_nationalite_compagnies")
                ),
                box(
                  title = "Détail des Passagers par Compagnie",
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
                  title = "Nombre de Passagers par Liaison (hors AUTRES)",
                  status = "success",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  plotlyOutput("bar_liaisons")
                ),
                  box(
                  title = "Détail des Passagers par Liaison (hors AUTRES)",
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
                  title = "Compagnies de Fret les Plus Actives",
                  status = "success",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  plotlyOutput("bar_fret")
                ),
                box(
                  title = "Détail des Compagnies de Fret",
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
                  title = "Comparaison Annuelle (Base 100 en 2018)",
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
