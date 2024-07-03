# Fonction pour lire un fichier Parquet à partir d'une URL
read_parquet_from_url <- function(url) {
  temp <- tempfile()
  on.exit(unlink(temp))
  response <- GET(url)
  
  if (response$status_code == 200) {
    writeBin(content(response, "raw"), temp)
    data <- arrow::read_parquet(temp)
    return(data)
  } else {
    stop(paste("Erreur lors du téléchargement de l'URL:", url, "- Statut:", response$status_code))
  }
}

# Fonction pour lire les fichiers de données des aéroports en Parquet
import_airport_data_parquet <- function(url) {
  pax_apt_all <- read_parquet_from_url(url) %>% 
    clean_airport_data()
  
  return(pax_apt_all)
}

# Fonction pour lire les fichiers de données des compagnies en Parquet
import_compagnies_data_parquet <- function(url) {
  pax_cie_all <- read_parquet_from_url(url) %>% 
    clean_compagnie_data()
  
  return(pax_cie_all)
}

# Fonction pour lire les fichiers de données des liaisons en Parquet
import_liaisons_data_parquet <- function(url) {
  pax_lsn_all <- read_parquet_from_url(url) %>% 
    clean_liaison_data()
  
  return(pax_lsn_all)
}
