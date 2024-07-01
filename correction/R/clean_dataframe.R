clean_airport_data <- function(df) {
  # Mettre toutes les colonnes en minuscules
  colnames(df) <- tolower(colnames(df))
  
  # Appliquer les mutations
  df <- df %>%
    mutate(
      an = str_sub(anmois, 1, 4),
      mois = str_sub(anmois, 5, 6)
    ) %>%
    mutate(
      mois = str_remove(mois, "^0+")
    ) %>%
    mutate(
      apt_peq = as.numeric(str_replace_all(apt_peq, ",", ".")),
      apt_pax_dep = as.numeric(str_replace_all(apt_pax_dep, ",", ".")),
      apt_pax_arr = as.numeric(str_replace_all(apt_pax_arr, ",", ".")),
      apt_pax_tr = as.numeric(str_replace_all(apt_pax_tr, ",", ".")),
      apt_frp_dep = as.numeric(str_replace_all(apt_frp_dep, ",", ".")),
      apt_frp_arr = as.numeric(str_replace_all(apt_frp_arr, ",", ".")),
      apt_nmvt_mxt = as.numeric(str_replace_all(apt_nmvt_mxt, ",", ".")),
      apt_nmvt_cgo = as.numeric(str_replace_all(apt_nmvt_cgo, ",", "."))
    )
  
  return(df)
}

clean_compagnie_data <- function(df) {
  # Mettre toutes les colonnes en minuscules
  colnames(df) <- tolower(colnames(df))
  
  # Appliquer les mutations
  df <- df %>%
    mutate(
      an = str_sub(anmois, 1, 4),
      mois = str_sub(anmois, 5, 6)
    ) %>%
    mutate(
      mois = str_remove(mois, "^0+")
    ) %>%
    mutate(
      cie_peq = as.numeric(str_replace_all(cie_peq, ",", ".")),
      cie_pax = as.numeric(str_replace_all(cie_pax, ",", ".")),
      cie_frp = as.numeric(str_replace_all(cie_frp, ",", ".")),
      cie_pkt = as.numeric(str_replace_all(cie_pkt, ",", ".")),
      cie_tkt = as.numeric(str_replace_all(cie_tkt, ",", ".")),
      cie_peqkt = as.numeric(str_replace_all(cie_peqkt, ",", ".")),
      cie_vol = as.numeric(str_replace_all(cie_vol, ",", "."))
    )
  
  return(df)
}

clean_liaison_data <- function(df) {
  # Mettre toutes les colonnes en minuscules
  colnames(df) <- tolower(colnames(df))
  
  # Appliquer les mutations
  df <- df %>%
    mutate(
      an = str_sub(anmois, 1, 4),
      mois = str_sub(anmois, 5, 6)
    ) %>%
    mutate(
      mois = str_remove(mois, "^0+")
    ) %>%
    mutate(
      lsn_dist = as.numeric(str_replace_all(lsn_dist, ",", ".")),
      lsn_peq_loc = as.numeric(str_replace_all(lsn_peq_loc, ",", ".")),
      lsn_pax_loc = as.numeric(str_replace_all(lsn_pax_loc, ",", ".")),
      lsn_frp_loc = as.numeric(str_replace_all(lsn_frp_loc, ",", ".")),
      lsn_vol = as.numeric(str_replace_all(lsn_vol, ",", ".")),
      lsn_pax_fs = as.numeric(str_replace_all(lsn_pax_fs, ",", ".")),
      lsn_frp_fs = as.numeric(str_replace_all(lsn_frp_fs, ",", ".")),
      lsn_peq_fs = as.numeric(str_replace_all(lsn_peq_fs, ",", ".")),
      lsn_drt = as.numeric(str_replace_all(lsn_drt, ",", "."))
    )
  
  return(df)
}



