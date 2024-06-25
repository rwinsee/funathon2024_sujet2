server <- function(input, output, session) {
  source("modules_server/module_freq_fr.R", encoding = "UTF-8", local = TRUE)
  source("modules_server/module_cie_aeriennes.R", encoding = "UTF-8", local = TRUE)
  source("modules_server/module_liaisons.R", encoding = "UTF-8", local = TRUE)
  source("modules_server/module_zone.R", encoding = "UTF-8", local = TRUE)
  source("modules_server/module_fret.R", encoding = "UTF-8", local = TRUE)
  }
