# ==================================================================
# Module 2 : préparation des données
# - opérations de nettoyage, chacune enregistrée dans un historique
# - annuler / revenir à une étape / réinitialiser
# - reprend les suggestions de l'onglet 1 (colonnes et lignes à supprimer)
# - reçoit la liste renvoyée par mod_donnees (data, cible, alertes, seuil_na_ligne)
# - renvoie une liste : data (données préparées), cible, historique
# ==================================================================

# Interface du module
mod_preparation_ui <- function(id) {
  ns <- NS(id)

  sidebarLayout(
    sidebarPanel(
      h4("Opérations"),
      # Supprimer des colonnes (la cible n'est pas proposée)
      selectInput(ns("cols_suppr"), "Supprimer des colonnes",
                  choices = NULL, multiple = TRUE),
      helpText("Pré-rempli avec les colonnes signalées dans l'onglet 1."),
      actionButton(ns("btn_suppr_cols"), "Appliquer"),
      br(), br(),

      # Supprimer des lignes (le nombre est recalculé sur les données actuelles)
      strong("Supprimer des lignes"),
      sliderInput(ns("seuil_na_ligne"), "Ligne : % de NA max",
                  min = 10, max = 100, value = 50, step = 5, post = " %"),
      actionButton(ns("btn_suppr_na"), "Supprimer les lignes avec trop de NA"),
      br(), br(),
      actionButton(ns("btn_suppr_doublons"), "Supprimer les lignes en double"),
      br(), br(),
      helpText("Ou cliquez sur des lignes du tableau de droite pour les sélectionner :"),
      actionButton(ns("btn_suppr_selection"), "Supprimer les lignes sélectionnées"),
      hr(),

      h4("Historique"),
      helpText("Cliquez sur une étape du tableau pour pouvoir y revenir."),
      DTOutput(ns("historique")),
      br(),
      actionButton(ns("btn_annuler"), "Annuler la dernière étape", icon = icon("rotate-left")),
      actionButton(ns("btn_revenir"), "Revenir à l'étape choisie"),
      actionButton(ns("btn_reset"),   "Réinitialiser", class = "btn-danger"),
      width = 5
    ),
    mainPanel(
      h4("Suggestions de l'onglet 1"),
      DTOutput(ns("suggestions")),
      h4("Données préparées"),
      textOutput(ns("dimensions")),
      DTOutput(ns("apercu")),
      width = 7
    )
  )
}

# Logique du module
# - donnees : liste renvoyée par mod_donnees_server (data, cible, alertes, seuil_na_ligne)
mod_preparation_server <- function(id, donnees) {
  moduleServer(id, function(input, output, session) {

    # L'historique est stocké dans une valeur réactive
    rv <- reactiveValues(h = NULL)

    # Nouvelles données (nouveau fichier, types modifiés...) : on repart de zéro
    # ----
    observeEvent(donnees$data(), {
      if (!is.null(rv$h) && length(rv$h) > 1)
        showNotification("Les données ont changé : historique de préparation réinitialisé.",
                         type = "warning")
      rv$h <- hist_nouveau(donnees$data())
    })

    # Données actuelles = dernière étape de l'historique
    data_courante <- reactive({
      validate(need(rv$h, "Chargez puis validez d'abord les données dans l'onglet 1."))
      hist_courant(rv$h)
    })

    # Liste des colonnes supprimables (toutes sauf la cible),
    # pré-remplie avec les colonnes signalées dans l'onglet 1
    observeEvent(data_courante(), {
      noms <- names(data_courante())
      updateSelectInput(session, "cols_suppr",
                        choices  = setdiff(noms, donnees$cible()),
                        selected = colonnes_suggerees(donnees$alertes(), noms, donnees$cible()))
    })

    # Le seuil de NA par ligne reprend celui choisi dans l'onglet 1
    observeEvent(donnees$seuil_na_ligne(), {
      updateSliderInput(session, "seuil_na_ligne", value = 100 * donnees$seuil_na_ligne())
    })

    # Lignes concernées (recalculées sur les données actuelles)
    # ----
    # Identifiants signalés dans l'onglet 1 : ignorés pour repérer les doublons
    ids <- reactive({
      a <- donnees$alertes()
      intersect(a$Cible[a$Probleme == "Identifiant probable"], names(data_courante()))
    })
    lignes_na  <- reactive(lignes_trop_na(data_courante(), input$seuil_na_ligne / 100))
    lignes_dup <- reactive(lignes_doublons(data_courante(), ignorer = ids()))

    # Les boutons affichent le nombre de lignes concernées
    observe({
      updateActionButton(session, "btn_suppr_na",
                         label = sprintf("Supprimer les %d ligne(s) avec plus de %d %% de NA",
                                         sum(lignes_na()), input$seuil_na_ligne))
      updateActionButton(session, "btn_suppr_doublons",
                         label = sprintf("Supprimer les %d ligne(s) en double", sum(lignes_dup())))
      updateActionButton(session, "btn_suppr_selection",
                         label = sprintf("Supprimer les %d ligne(s) sélectionnée(s)",
                                         length(input$apercu_rows_selected)))
    })

    # Opération : supprimer des colonnes
    # ----
    observeEvent(input$btn_suppr_cols, {
      req(input$cols_suppr)
      cols <- input$cols_suppr
      rv$h <- hist_ajouter(rv$h, "Supprimer des colonnes",
                           detail = paste(cols, collapse = ", "),
                           data   = supprimer_colonnes(data_courante(), cols),
                           params = list(colonnes = cols))
    })

    # Opération : supprimer les lignes avec trop de NA
    # ----
    observeEvent(input$btn_suppr_na, {
      n <- sum(lignes_na())
      if (n == 0) { showNotification("Aucune ligne concernée.", type = "message"); return() }
      rv$h <- hist_ajouter(rv$h, "Supprimer les lignes avec trop de NA",
                           detail = sprintf("%d ligne(s), seuil %d %%", n, input$seuil_na_ligne),
                           data   = supprimer_lignes(data_courante(), lignes_na()),
                           params = list(seuil = input$seuil_na_ligne / 100))
    })

    # Opération : supprimer les doublons
    # ----
    observeEvent(input$btn_suppr_doublons, {
      n <- sum(lignes_dup())
      if (n == 0) { showNotification("Aucune ligne en double.", type = "message"); return() }
      rv$h <- hist_ajouter(rv$h, "Supprimer les lignes en double",
                           detail = paste0(n, " ligne(s)",
                                           if (length(ids())) paste0(" (sans tenir compte de ",
                                                                     paste(ids(), collapse = ", "), ")")),
                           data   = supprimer_lignes(data_courante(), lignes_dup()),
                           params = list(ignorer = ids()))
    })

    # Opération : supprimer les lignes sélectionnées dans le tableau
    # ----
    observeEvent(input$btn_suppr_selection, {
      idx <- input$apercu_rows_selected          # positions des lignes cliquées
      if (length(idx) == 0) {
        showNotification("Cliquez d'abord sur des lignes du tableau.", type = "warning")
        return()
      }
      d <- data_courante()
      noms <- rownames(d)[idx]                   # numéros d'origine des lignes
      rv$h <- hist_ajouter(rv$h, "Supprimer des lignes (sélection)",
                           detail = paste0(length(idx), " ligne(s) : ",
                                           paste(head(noms, 10), collapse = ", "),
                                           if (length(noms) > 10) "..."),
                           data   = supprimer_lignes(d, seq_len(nrow(d)) %in% idx),
                           params = list(lignes = noms))
    })

    # Annuler / revenir / réinitialiser
    # ----
    observeEvent(input$btn_annuler, { rv$h <- hist_annuler(rv$h) })

    observeEvent(input$btn_revenir, {
      k <- input$historique_rows_selected
      if (is.null(k)) {
        showNotification("Choisissez d'abord une étape dans le tableau.", type = "warning")
        return()
      }
      rv$h <- hist_revenir(rv$h, k)
    })

    observeEvent(input$btn_reset, { rv$h <- hist_reinitialiser(rv$h) })

    # Affichage
    # ----
    output$historique <- renderDT({
      req(rv$h)
      datatable(hist_tableau(rv$h), rownames = FALSE, selection = "single",
                options = list(dom = "t", ordering = FALSE, pageLength = 100))
    })

    # Rappel des alertes de l'onglet 1 (seulement les colonnes encore présentes)
    output$suggestions <- renderDT({
      a <- donnees$alertes()
      a <- a[a$Niveau == "lignes" | a$Cible %in% names(data_courante()),
             c("Cible", "Probleme", "Detail", "Suggestion")]
      datatable(a, rownames = FALSE,
                options = list(dom = "t", ordering = FALSE, pageLength = 50, scrollX = TRUE))
    })

    output$dimensions <- renderText({
      paste(nrow(data_courante()), "lignes,", ncol(data_courante()), "colonnes")
    })

    output$apercu <- renderDT({
      # selection = "multiple" : on peut cliquer sur plusieurs lignes
      # (les numéros de ligne affichés sont ceux du fichier d'origine)
      datatable(data_courante(), selection = "multiple",
                options = list(pageLength = 10, scrollX = TRUE))
    })

    # On renvoie aux autres modules
    return(list(
      data       = data_courante,
      cible      = donnees$cible,
      historique = reactive(rv$h)
    ))
  })
}
