# module_accueil.R

output$source_code <- renderPrint({
  cat("source_code.R")
})

output$accueil_content <- renderUI({
  tagList(
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
})
