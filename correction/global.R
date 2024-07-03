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
library(httr)
library(utils)

# setwd("correction/")#indicates the right WD, otherwise it doesn't run correctly
source("R/import_data_parquet.R")
# source("R/import_data_csv.R")
source("R/create_data_list.R")
source("R/clean_dataframe.R")
source("R/divers_functions.R")
source("R/tables.R")
source("R/figures.R")

# Global variables ---------------------------
YEARS_LIST <- 2013:2024
MONTHS_LIST <- 1:12

# # Load data ----------------------------------
# Lire le fichier YAML pour obtenir les URLs de données
urls <- create_data_list("sources.yml")
# Vérifiez les URLs extraites
# print(urls)

# Télécharger et lire les fichiers pour chaque type en csv
# pax_apt_all <- download_and_read_zip(urls$airports$zip) %>% clean_airport_data()
# pax_lsn_all <- download_and_read_zip(urls$liaisons$zip) %>% clean_liaison_data()
# pax_cie_all <- download_and_read_zip(urls$compagnies$zip) %>% clean_compagnie_data()

# Télécharger et lire les fichiers pour chaque type en Parquet
pax_apt_all <- import_airport_data_parquet(urls$airports$parquet) %>% clean_airport_data()
pax_lsn_all <- import_liaisons_data_parquet(urls$liaisons$parquet) %>% clean_liaison_data()
pax_cie_all <- import_compagnies_data_parquet(urls$compagnies$parquet) %>% clean_compagnie_data()

airports_location <- st_read(urls$geojson$airport)

# liste_aeroports <- unique(pax_apt_all$apt)
liste_aeroports <- pax_apt_all %>%
  select(apt, apt_nom) %>%
  distinct() %>%
  arrange(apt_nom)

default_airport <- liste_aeroports$apt[1]

# OBJETS NECESSAIRES A L'APPLICATION ------------------------

trafic_aeroports <- pax_apt_all %>%
  mutate(trafic = apt_pax_dep + apt_pax_tr + apt_pax_arr) %>%
  filter(apt %in% default_airport) %>%
  mutate(
    date = as.Date(paste(anmois, "01", sep=""), format = "%Y%m%d")
  )

