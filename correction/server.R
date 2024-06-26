server <- function(input, output, session) {
  source("module_freq_fr.R", encoding = "UTF-8", local = TRUE)
  source("module_cie_aeriennes.R", encoding = "UTF-8", local = TRUE)
  source("module_liaisons.R", encoding = "UTF-8", local = TRUE)
  source("module_zone.R", encoding = "UTF-8", local = TRUE)
  source("module_fret.R", encoding = "UTF-8", local = TRUE)
  source("module_comparaison_annuelle.R", encoding = "UTF-8", local = TRUE)
}
