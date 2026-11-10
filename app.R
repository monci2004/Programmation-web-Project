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
  id    = "onglets",                 # identifiant : permet de changer d'onglet depuis le serveur

  tabPanel("1. Données",       mod_donnees_ui("donnees")),
  tabPanel("2. Préparation",   mod_preparation_ui("preparation")),
  tabPanel("3. Exploration",   h3("Analyse exploratoire")),
  tabPanel("4. Modélisation",  h3("Entraînement des modèles")),
  tabPanel("5. Étude de cas",  h3("Étude de cas"))
)

# Commandes à exécuter
server <- function(input, output, session){

  # Valeurs partagées entre tous les onglets
  # ----
  rv <- reactiveValues(raw = NULL,             # données brutes chargées
                       prepared = NULL,        # données après nettoyage (onglet 2)
                       transformations = NULL, # transformations apprises configurées (pour onglet 4)
                       target = NULL)          # nom de la variable cible

  # Module 1 : chargement des données
  # ----
  # (le module renvoie une liste : data et cible validées, valide = compteur de validations)
  donnees <- mod_donnees_server("donnees")
  observe({
    rv$raw    <- donnees$data()
    rv$target <- donnees$cible()
  })

  # Après chaque validation dans l'onglet 1 : on passe à l'onglet 2
  observeEvent(donnees$valide(), {
    updateNavbarPage(session, "onglets", selected = "2. Préparation")
  }, ignoreInit = TRUE)

  # Module 2 : préparation (reçoit les données du module 1)
  # ----
  preparation <- mod_preparation_server("preparation", donnees = donnees)
  observe({
    rv$prepared        <- preparation$data()
    rv$transformations <- preparation$transformations()
  })
}

# Association interface & commandes
shinyApp(ui = ui, server = server)
