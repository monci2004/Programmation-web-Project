# ==================================================================
# Application : exploration & classification
# Les packages et les modules sont chargés dans global.R
# (la démo d'Achraf est conservée dans docs/demo_achraf.R)
# ==================================================================

# Avec app.R, Shiny ne lit pas global.R tout seul : on le charge ici
source("global.R")

# Contenu de l'interface
ui <- navbarPage(
  # Titre de l'application
  title = "Exploration & Classification",

  tabPanel("1. Données",       mod_donnees_ui("donnees")),
  tabPanel("2. Préparation",   h3("Prétraitement")),
  tabPanel("3. Exploration",   h3("Analyse exploratoire")),
  tabPanel("4. Modélisation",  h3("Entraînement des modèles")),
  tabPanel("5. Étude de cas",  h3("Étude de cas"))
)

# Commandes à exécuter
server <- function(input, output, session){

  # Valeurs partagées entre tous les onglets
  # ----
  rv <- reactiveValues(raw = NULL,       # données brutes chargées
                       prepared = NULL,  # données après prétraitement
                       target = NULL)    # nom de la variable cible

  # Module 1 : chargement des données
  # ----
  # (le module renvoie une liste : data = les données, cible = le nom de la cible)
  donnees <- mod_donnees_server("donnees")
  observe({
    rv$raw    <- donnees$data()
    rv$target <- donnees$cible()
  })
}

# Association interface & commandes
shinyApp(ui = ui, server = server)
