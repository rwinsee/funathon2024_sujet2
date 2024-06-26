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

# Load data ----------------------------------
urls <- create_data_list("./sources.yml")


pax_apt_all <- import_airport_data(unlist(urls$airports))
pax_cie_all <- import_compagnies_data(unlist(urls$compagnies))
pax_lsn_all <- import_liaisons_data(unlist(urls$liaisons))

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