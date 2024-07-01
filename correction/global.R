# Environment ----------------------------------

library(readr)
library(dplyr)
library(lubridate)
library(stringr)
library(sf)
library(ggplot2)
library(plotly)
library(gt)
library(leaflet)
library(bslib)
library(shinydashboard)
library(DT)

# library(httr)
# library(utils)
# 
# # Fonction pour télécharger et décompresser un fichier ZIP avec gestion des erreurs
# download_and_unzip <- function(url, dest_dir) {
#   temp <- tempfile()
#   response <- GET(url)
#   
#   if (response$status_code == 200) {
#     writeBin(content(response, "raw"), temp)
#     unzip(temp, exdir = dest_dir)
#     unlink(temp)
#     message(paste("Téléchargement et décompression réussis pour:", url))
#   } else {
#     stop(paste("Erreur lors du téléchargement de l'URL:", url, "- Statut:", response$status_code))
#   }
# }
# 
# # Chemins de destination pour les fichiers décompressés
# dest_dir_airports <- "data/airports"
# dest_dir_liaisons <- "data/liaisons"
# dest_dir_compagnies <- "data/compagnies"
# 
# # Créer les dossiers de destination s'ils n'existent pas
# dir.create(dest_dir_airports, showWarnings = FALSE, recursive = TRUE)
# dir.create(dest_dir_liaisons, showWarnings = FALSE, recursive = TRUE)
# dir.create(dest_dir_compagnies, showWarnings = FALSE, recursive = TRUE)
# 
# # URLs des fichiers ZIP
# urls <- list(
#   airports = "https://www.data.gouv.fr/fr/datasets/r/75aa06d3-21ed-4a1f-8cbe-cda84fcfd140",
#   liaisons = "https://www.data.gouv.fr/fr/datasets/r/5d2e4a84-ece5-4cdf-934f-281c178e4dc5",
#   compagnies = "https://www.data.gouv.fr/fr/datasets/r/27889c6b-7132-4b6c-9b13-71d0367fedd9"
# )
# 
# # Télécharger et décompresser les fichiers
# download_and_unzip(urls$airports, dest_dir_airports)
# download_and_unzip(urls$liaisons, dest_dir_liaisons)
# download_and_unzip(urls$compagnies, dest_dir_compagnies)
# 
# # Vérifier les fichiers décompressés
# list.files(dest_dir_airports)
# list.files(dest_dir_liaisons)
# list.files(dest_dir_compagnies)

# setwd("correction/")#indicates the right WD, otherwise it doesn't run correctly
source("R/import_data.R")
source("R/create_data_list.R")
source("R/clean_dataframe.R")
source("R/divers_functions.R")
source("R/tables.R")
source("R/figures.R")


# Global variables ---------------------------

YEARS_LIST <- 2018:2022
MONTHS_LIST = 1:12

# # Load data ----------------------------------
# Lire le fichier YAML pour obtenir les URLs de données
urls <- create_data_list("sources.yml")
# Vérifiez les URLs extraites
# print(urls)

# Télécharger et lire les fichiers pour chaque type
pax_apt_all <- download_and_read_zip(urls$airports$zip) %>% clean_airport_data()
pax_lsn_all <- download_and_read_zip(urls$liaisons$zip) %>% clean_liaison_data()
pax_cie_all <- download_and_read_zip(urls$compagnies$zip) %>% clean_compagnie_data()


airports_location <- st_read(urls$geojson$airport)

# liste_aeroports <- unique(pax_apt_all$apt)
liste_aeroports <- pax_apt_all %>%
  select(apt, apt_nom) %>%
  distinct() %>%
  arrange(apt_nom)

default_airport <- liste_aeroports$apt[1]
# default_airport <- "PARIS-CHARLES DE GAULLE"


# OBJETS NECESSAIRES A L'APPLICATION ------------------------

trafic_aeroports <- pax_apt_all %>%
  mutate(trafic = apt_pax_dep + apt_pax_tr + apt_pax_arr) %>%
  filter(apt %in% default_airport) %>%
  mutate(
    date = as.Date(paste(anmois, "01", sep=""), format = "%Y%m%d")
  )

# Définir les dates de la période COVID ----------------------
covid_start <- as.Date("2020-02-17")
covid_end <- as.Date("2022-01-01")