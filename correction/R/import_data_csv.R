# R/import_data.R

# Fonction pour télécharger et décompresser un fichier ZIP et lire les CSV
download_and_read_zip <- function(url) {
  if (is.null(url) || url == "") {
    stop("URL non valide.")
  }
  temp <- tempfile()
  response <- GET(url)
  
  if (response$status_code == 200) {
    writeBin(content(response, "raw"), temp)
    files <- unzip(temp, list = TRUE)$Name
    data <- lapply(files, function(file) {
      temp_dir <- tempdir()
      unzip(temp, files = file, exdir = temp_dir)
      read_csv2(file.path(temp_dir, file), col_types = cols(.default = col_character()))
    })
    data <- bind_rows(data)
    unlink(temp)
    return(data)
  } else {
    stop(paste("Erreur lors du téléchargement de l'URL:", url, "- Statut:", response$status_code))
  }
}



# Fonction pour lire les fichiers de données des aéroports
import_airport_data <- function(list_files){
  pax_apt_all <- readr::read_csv2(
    list_files, 
    col_types = cols(
      ANMOIS = col_character(),
      APT = col_character(),
      APT_NOM = col_character(),
      APT_ZON = col_character(),
      .default = col_double()
    )
  ) %>% 
    clean_dataframe()
  
  return(pax_apt_all)
}

# Fonction pour lire les fichiers de données des compagnies
import_compagnies_data <- function(list_files){
  pax_cie_all <- readr::read_csv2(
    file = list_files,
    col_types = cols(
      ANMOIS = col_character(),
      CIE = col_character(),
      CIE_NOM = col_character(),
      CIE_NAT = col_character(),
      CIE_PAYS = col_character(),
      .default = col_double()
    )
  ) %>% 
    clean_dataframe()
  
  return(pax_cie_all)
}

# Fonction pour lire les fichiers de données des liaisons
import_liaisons_data <- function(list_files){
  pax_lsn_all <- readr::read_csv2(
    file = list_files,
    col_types = cols(
      ANMOIS = col_character(),
      LSN = col_character(),
      LSN_DEP_NOM = col_character(),
      LSN_ARR_NOM = col_character(),
      LSN_SCT = col_character(),
      LSN_FSC = col_character(),
      .default = col_double()
    ) 
  ) %>% 
    clean_dataframe()
  
  return(pax_lsn_all)
}

