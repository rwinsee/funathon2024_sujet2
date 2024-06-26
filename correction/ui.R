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
  selected = "PARIS-CHARLES DE GAULLE" #default_airport
)


ui <- dashboardPage(
  dashboardHeader(title = "DB aéroports FR",
                  tags$li(class = "dropdown",
                          tags$head(
                            tags$link(rel = "shortcut icon", href = "www/favicon.ico")
                          ))),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Accueil", tabName = "accueil", icon = icon("home")),
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
      tabItem(tabName = "accueil",
              fluidRow(
                box(
                  title = "Bienvenue sur le tableau de bord du trafic aérien",
                  status = "success",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  width = 12,
                  # img(src = "www/accueil_image.png", height = "300px"), # Ajoutez votre image ici
                  div(
                    style = "text-align: justify; padding: 10px; text-align: center;",
                    br(),
                    img(src = "https://github.com/rwinsee/funathon2024_sujet2/raw/main/img/cockpit.png", height = "300px"),
                    br(),
                    br(),
                    br(),
                    p("Bienvenue sur le tableau de bord interactif du trafic aérien. Ce tableau de bord vous permet de visualiser et d'analyser les données de fréquentation, de compagnies aériennes, de liaisons aériennes, de zones géographiques, de fret aérien."),
                    p("Pour commencer votre navigation, sélectionnez l'onglet correspondant à la catégorie de données que vous souhaitez explorer. Chaque onglet contient des visualisations interactives et des tableaux détaillés."),
                    p(HTML("<strong>Ce tableau de bord a été développé dans le cadre du FUNATHON 2024 organisé par les équipes innovation de l’Insee & DGAC sur la thématique des statistiques sur l’aviation civile.</strong>")),
                    p("N'hésitez pas à explorer les différentes sections et à utiliser les filtres pour affiner votre analyse."),
                    br(),
                    h5("Liens utiles :"),
                    tags$ul(
                      tags$li(tags$a(href = "https://inseefrlab.github.io/funathon2024/about.html", "Qu'est-ce que le FUNATHON ?", target = "_blank")),
                      tags$li(tags$a(href = "https://inseefrlab.github.io/funathon2024/", "Lien vers le site dédié au FUNATHON 2024", target = "_blank")),
                      tags$li(tags$a(href = "https://inseefrlab.github.io/funathon2024_sujet2/", "Lien vers le sujet dédié du FUNATHON 2024", target = "_blank")),
                      tags$li(tags$a(href = "https://github.com/rwinsee/funathon2024_sujet2", "Lien vers le dépôt de code source de l'application", target = "_blank")),
                      tags$li(tags$a(href = "https://github.com/rwinsee/funathon2024_sujet2_cd", "Lien vers le dépôt de code source du déploiement de l'application", target = "_blank")),
                      tags$li(tags$a(href = "https://github.com/InseeFrLab/funathon2024", "Lien vers la ressource de cet événement ", target = "_blank")),
                      tags$li(tags$a(href = "https://droits-passagers-aeriens.aviation-civile.gouv.fr/", "Site officiel de la DGAC", target = "_blank")),
                      tags$li(tags$a(href = "https://www.insee.fr", "Site officiel de l'Insee", target = "_blank"))
                    ),
                    br(),
                    h5("Contact :"),
                    p("Ce projet étant développé dans le cadre du FUNATHON, aucune évolution ne sera déployée. Toutefois, vous pouvez nous contacter à l'adresse email suivante : ",
                      tags$a(href = "mailto:romuald.weidmann@insee.fr", "romuald.weidmann@insee.fr"))
                  )
                )
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
