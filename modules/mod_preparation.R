# ==================================================================
# Module 2 : préparation des données
# A. Nettoyage (appliqué dans l'onglet 2, enregistré dans l'historique) :
#    - suppression de colonnes / lignes (suggestions onglet 1 + sélection)
#    - outliers par la règle IQR (garder / plafonner / supprimer)
#    - regroupement des modalités rares en "Autre"
#    - fusion de colonnes (somme / moyenne / différence / ratio / concaténation)
#    - annuler / revenir à une étape / réinitialiser
# B. Transformations apprises (configurées ici, ajustées sur train dans
#    l'onglet 4 pour éviter toute fuite de données / data leakage) :
#    - imputation des NA (moyenne / médiane + mode / supprimer)
#    - normalisation (aucune / z-score / min-max)
#    - dummification (encodage one-hot)
#    - gestion du déséquilibre des classes (oversampling / undersampling / poids)
# ==================================================================

# Interface du module
mod_preparation_ui <- function(id) {
  ns <- NS(id)

  sidebarLayout(
    sidebarPanel(
      tabsetPanel(
        id = ns("onglets_prep"),

        # ---- Sous-onglet 1 : Nettoyage (appliqué immédiatement) ----
        tabPanel("1. Nettoyage",
          br(),
          h5(strong("A. Supprimer des colonnes")),
          selectInput(ns("cols_suppr"), "Colonnes à retirer (hors cible)",
                      choices = NULL, multiple = TRUE),
          helpText("Pré-rempli avec les colonnes signalées dans l'onglet 1."),
          actionButton(ns("btn_suppr_cols"), "Supprimer les colonnes"),
          hr(),

          h5(strong("B. Supprimer des lignes")),
          sliderInput(ns("seuil_na_ligne"), "Ligne : % de NA max",
                      min = 10, max = 100, value = 50, step = 5, post = " %"),
          actionButton(ns("btn_suppr_na"), "Supprimer les lignes avec trop de NA"),
          br(), br(),
          actionButton(ns("btn_suppr_tous_na"), "Supprimer toute ligne ayant au moins 1 NA"),
          br(), br(),
          actionButton(ns("btn_suppr_doublons"), "Supprimer les lignes en double"),
          br(), br(),
          helpText("Ou cliquez sur des lignes du tableau à droite :"),
          actionButton(ns("btn_suppr_selection"), "Supprimer les lignes sélectionnées"),
          hr(),

          h5(strong("C. Outliers (règle IQR : 1.5 × IQR)")),
          selectInput(ns("cols_outliers"), "Variables quantitatives à traiter",
                      choices = NULL, multiple = TRUE),
          radioButtons(ns("methode_outliers"), "Traitement des outliers",
                       choices = c("Garder (ne rien faire)"          = "garder",
                                   "Plafonner aux moustaches (cap)"  = "plafonner",
                                   "Supprimer les lignes concernées" = "supprimer"),
                       selected = "plafonner"),
          actionButton(ns("btn_outliers"), "Appliquer aux outliers"),
          hr(),

          h5(strong("D. Modalités rares (variables qualitatives)")),
          selectInput(ns("cols_rares"), "Variables qualitatives à regrouper",
                      choices = NULL, multiple = TRUE),
          sliderInput(ns("seuil_rare"), "Seuil de fréquence pour 'Autre'",
                      min = 1, max = 25, value = 5, step = 1, post = " %"),
          actionButton(ns("btn_rares"), "Regrouper les modalités rares en 'Autre'"),
          hr(),

          h5(strong("E. Fusionner deux colonnes")),
          radioButtons(ns("type_fusion"), "Type de colonnes à combiner",
                       choices = c("Quantitatives (calcul)"        = "quanti",
                                   "Qualitatives (concaténation)"  = "quali"),
                       inline = TRUE),
          fluidRow(
            column(6, selectInput(ns("fusion_col1"), "Colonne 1", choices = NULL)),
            column(6, selectInput(ns("fusion_col2"), "Colonne 2", choices = NULL))
          ),
          conditionalPanel(
            condition = sprintf("input['%s'] == 'quanti'", ns("type_fusion")),
            selectInput(ns("fusion_op"), "Opération",
                        choices = c("Somme (col1 + col2)"           = "somme",
                                    "Moyenne ((col1 + col2) / 2)"   = "moyenne",
                                    "Différence (col1 - col2)"      = "difference",
                                    "Ratio (col1 / col2)"           = "ratio"))
          ),
          textInput(ns("fusion_nom"), "Nom de la nouvelle colonne", value = ""),
          checkboxInput(ns("fusion_suppr"), "Supprimer les colonnes d'origine", FALSE),
          actionButton(ns("btn_fusionner"), "Créer la colonne fusionnée")
        ),

        # ---- Sous-onglet 2 : Transformations apprises (sans fuite de données) ----
        tabPanel("2. Transformations",
          br(),
          p(style = "color: #1e40af; background: #eff6ff; padding: 8px; border-radius: 4px;",
            icon("shield-halved"),
            " Pour éviter toute fuite de données (data leakage), ces transformations sont ",
            strong("configurées ici"), " mais leurs paramètres (moyenne, médiane, mode, ",
            "écart-type, min/max, modalités, rééquilibrage) seront ",
            strong("appris uniquement sur l'échantillon d'entraînement (train)"),
            " dans l'onglet 4."),

          h5(strong("1. Valeurs manquantes restantes (Imputation)")),
          radioButtons(ns("trans_imputation"), NULL,
                       choices = c("Médiane (quanti) + Mode (quali)"  = "mediane",
                                   "Moyenne (quanti) + Mode (quali)"  = "moyenne",
                                   "Supprimer les lignes avec NA"     = "supprimer",
                                   "Aucune (laisser les NA)"          = "aucune"),
                       selected = "mediane"),
          hr(),

          h5(strong("2. Normalisation (variables quantitatives)")),
          radioButtons(ns("trans_normalisation"), NULL,
                       choices = c("Aucune"                                    = "aucune",
                                   "Standardisation Z-score ((x - moy) / sd)" = "zscore",
                                   "Min-Max sur [0, 1]"                        = "minmax"),
                       selected = "aucune"),
          hr(),

          h5(strong("3. Dummification (variables qualitatives)")),
          checkboxInput(ns("trans_dummification"),
                        "Encodage One-Hot (transformer chaque modalité en colonne 0/1)",
                        value = FALSE),
          hr(),

          h5(strong("4. Déséquilibre des classes (variable cible)")),
          helpText("Répartition actuelle de la cible :"),
          tableOutput(ns("dist_cible_prep")),
          radioButtons(ns("trans_desequilibre"), "Méthode (appliquée sur train uniquement)",
                       choices = c("Aucun"                                      = "aucun",
                                   "Sur-échantillonnage (oversampling)"         = "oversampling",
                                   "Sous-échantillonnage (undersampling)"       = "undersampling",
                                   "Pondération des classes (class weights)"    = "poids"),
                       selected = "aucun")
        ),

        # ---- Sous-onglet 3 : Historique (annuler / revenir / reset) ----
        tabPanel("3. Historique",
          br(),
          helpText("Cliquez sur une étape du tableau pour pouvoir y revenir."),
          DTOutput(ns("historique")),
          br(),
          actionButton(ns("btn_annuler"), "Annuler la dernière étape", icon = icon("rotate-left")),
          actionButton(ns("btn_revenir"), "Revenir à l'étape choisie"),
          actionButton(ns("btn_reset"),   "Réinitialiser", class = "btn-danger")
        )
      ),
      width = 5
    ),

    mainPanel(
      tabsetPanel(
        tabPanel("Données préparées",
          br(),
          uiOutput(ns("bilan_prep")),
          h4("Suggestions restantes de l'onglet 1"),
          DTOutput(ns("suggestions")),
          h4("Aperçu des données préparées"),
          textOutput(ns("dimensions")),
          DTOutput(ns("apercu"))
        ),
        tabPanel("Résumé & Pipeline",
          br(),
          h4("Transformations apprises configurées (pour l'onglet 4)"),
          tableOutput(ns("tab_transformations")),
          h4("Résumé des variables après nettoyage"),
          DTOutput(ns("resume_prep"))
        )
      ),
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

    # Mise à jour dynamique des listes de colonnes selon `data_courante()`
    # ----
    observeEvent(data_courante(), {
      d     <- data_courante()
      cible <- donnees$cible()
      noms  <- names(d)
      pred  <- setdiff(noms, cible)

      cols_num  <- pred[sapply(d[pred], is.numeric)]
      cols_qual <- pred[sapply(d[pred], is.factor)]

      # A. Colonnes à supprimer (pré-remplies avec les alertes de l'onglet 1)
      updateSelectInput(session, "cols_suppr",
                        choices  = pred,
                        selected = colonnes_suggerees(donnees$alertes(), noms, cible))

      # C. Colonnes numériques avec outliers (pré-remplies si outliers > 0)
      avec_out <- colonnes_avec_outliers(d, cible)
      updateSelectInput(session, "cols_outliers",
                        choices  = cols_num,
                        selected = avec_out)

      # D. Colonnes qualitatives ayant des modalités rares
      avec_rares <- cols_qual[sapply(d[cols_qual], function(x)
        length(modalites_rares(x, input$seuil_rare / 100)) > 0)]
      updateSelectInput(session, "cols_rares",
                        choices  = cols_qual,
                        selected = avec_rares)
    })

    # Quand le seuil de modalités rares change, met à jour la présélection
    observeEvent(input$seuil_rare, {
      req(rv$h)
      d         <- data_courante()
      pred      <- setdiff(names(d), donnees$cible())
      cols_qual <- pred[sapply(d[pred], is.factor)]
      avec_rares <- cols_qual[sapply(d[cols_qual], function(x)
        length(modalites_rares(x, input$seuil_rare / 100)) > 0)]
      updateSelectInput(session, "cols_rares",
                        choices  = cols_qual,
                        selected = avec_rares)
    }, ignoreInit = TRUE)

    # E. Mise à jour des colonnes proposées pour la fusion
    observe({
      req(rv$h)
      d    <- data_courante()
      pred <- setdiff(names(d), donnees$cible())
      cols <- if (input$type_fusion == "quanti") {
        pred[sapply(d[pred], is.numeric)]
      } else {
        pred[sapply(d[pred], is.factor)]
      }
      updateSelectInput(session, "fusion_col1", choices = cols,
                        selected = if (length(cols) >= 1) cols[1] else character(0))
      updateSelectInput(session, "fusion_col2", choices = cols,
                        selected = if (length(cols) >= 2) cols[2] else character(0))
    })

    # Nom par défaut de la colonne fusionnée
    observe({
      req(input$fusion_col1, input$fusion_col2)
      op <- if (input$type_fusion == "quali") "concat" else input$fusion_op
      nom_defaut <- make.names(paste(input$fusion_col1, op, input$fusion_col2, sep = "_"))
      updateTextInput(session, "fusion_nom", value = nom_defaut)
    })

    # Le seuil de NA par ligne reprend celui choisi dans l'onglet 1
    observeEvent(donnees$seuil_na_ligne(), {
      updateSliderInput(session, "seuil_na_ligne", value = 100 * donnees$seuil_na_ligne())
    })

    # Lignes concernées (recalculées sur les données actuelles)
    # ----
    ids <- reactive({
      a <- donnees$alertes()
      intersect(a$Cible[a$Probleme == "Identifiant probable"], names(data_courante()))
    })
    lignes_na      <- reactive(lignes_trop_na(data_courante(), input$seuil_na_ligne / 100))
    lignes_tous_na <- reactive(!complete.cases(data_courante()))
    lignes_dup     <- reactive(lignes_doublons(data_courante(), ignorer = ids()))

    # Les boutons affichent le nombre de lignes concernées
    observe({
      req(rv$h)
      updateActionButton(session, "btn_suppr_na",
                         label = sprintf("Supprimer les %d ligne(s) avec plus de %d %% de NA",
                                         sum(lignes_na()), input$seuil_na_ligne))
      updateActionButton(session, "btn_suppr_tous_na",
                         label = sprintf("Supprimer les %d ligne(s) ayant au moins 1 NA",
                                         sum(lignes_tous_na())))
      updateActionButton(session, "btn_suppr_doublons",
                         label = sprintf("Supprimer les %d ligne(s) en double", sum(lignes_dup())))
      updateActionButton(session, "btn_suppr_selection",
                         label = sprintf("Supprimer les %d ligne(s) sélectionnée(s)",
                                         length(input$apercu_rows_selected)))
    })

    # ==================================================================
    # OPÉRATIONS DE NETTOYAGE (ajoutées à l'historique rv$h)
    # ==================================================================

    # A. Supprimer des colonnes
    observeEvent(input$btn_suppr_cols, {
      req(input$cols_suppr)
      cols <- input$cols_suppr
      rv$h <- hist_ajouter(rv$h, "Supprimer des colonnes",
                           detail = paste(cols, collapse = ", "),
                           data   = supprimer_colonnes(data_courante(), cols),
                           params = list(colonnes = cols))
    })

    # B1. Supprimer les lignes avec trop de NA
    observeEvent(input$btn_suppr_na, {
      n <- sum(lignes_na())
      if (n == 0) { showNotification("Aucune ligne concernée.", type = "message"); return() }
      rv$h <- hist_ajouter(rv$h, "Supprimer les lignes avec trop de NA",
                           detail = sprintf("%d ligne(s), seuil %d %%", n, input$seuil_na_ligne),
                           data   = supprimer_lignes(data_courante(), lignes_na()),
                           params = list(seuil = input$seuil_na_ligne / 100))
    })

    # B2. Supprimer toutes les lignes ayant au moins 1 NA
    observeEvent(input$btn_suppr_tous_na, {
      n <- sum(lignes_tous_na())
      if (n == 0) { showNotification("Aucune ligne ne contient de NA.", type = "message"); return() }
      rv$h <- hist_ajouter(rv$h, "Supprimer les lignes incomplètes (NA)",
                           detail = sprintf("%d ligne(s) contenant au moins un NA", n),
                           data   = supprimer_lignes(data_courante(), lignes_tous_na()),
                           params = list())
    })

    # B3. Supprimer les doublons
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

    # B4. Supprimer les lignes sélectionnées dans le tableau
    observeEvent(input$btn_suppr_selection, {
      idx <- input$apercu_rows_selected
      if (length(idx) == 0) {
        showNotification("Cliquez d'abord sur des lignes du tableau.", type = "warning")
        return()
      }
      d <- data_courante()
      noms <- rownames(d)[idx]
      rv$h <- hist_ajouter(rv$h, "Supprimer des lignes (sélection)",
                           detail = paste0(length(idx), " ligne(s) : ",
                                           paste(head(noms, 10), collapse = ", "),
                                           if (length(noms) > 10) "..."),
                           data   = supprimer_lignes(d, seq_len(nrow(d)) %in% idx),
                           params = list(lignes = noms))
    })

    # C. Traitement des outliers (IQR)
    observeEvent(input$btn_outliers, {
      if (input$methode_outliers == "garder") {
        showNotification("Option 'Garder' sélectionnée : aucune modification.", type = "message")
        return()
      }
      cols <- input$cols_outliers
      if (length(cols) == 0) {
        showNotification("Sélectionnez au moins une variable quantitative.", type = "warning")
        return()
      }
      d <- data_courante()
      nb_out <- sum(sapply(d[cols], function(x) sum(est_outlier_iqr(x))))
      if (nb_out == 0) {
        showNotification("Aucun outlier (1.5 × IQR) dans les colonnes choisies.", type = "message")
        return()
      }
      d_apres <- traiter_outliers(d, cols, input$methode_outliers)
      action_txt <- if (input$methode_outliers == "plafonner") {
        sprintf("Plafonnement (cap) de %d valeur(s) sur : %s", nb_out, paste(cols, collapse = ", "))
      } else {
        sprintf("%d ligne(s) supprimée(s) sur : %s", nrow(d) - nrow(d_apres), paste(cols, collapse = ", "))
      }
      rv$h <- hist_ajouter(rv$h, paste0("Outliers IQR (", input$methode_outliers, ")"),
                           detail = action_txt,
                           data   = d_apres,
                           params = list(colonnes = cols, methode = input$methode_outliers))
    })

    # D. Regroupement des modalités rares en "Autre"
    observeEvent(input$btn_rares, {
      cols <- input$cols_rares
      if (length(cols) == 0) {
        showNotification("Sélectionnez au moins une variable qualitative.", type = "warning")
        return()
      }
      d     <- data_courante()
      seuil <- input$seuil_rare / 100
      nb_rares <- sapply(d[cols], function(x) length(modalites_rares(x, seuil)))
      cols_eff <- cols[nb_rares > 0]
      if (length(cols_eff) == 0) {
        showNotification(sprintf("Aucune modalité sous %d %% dans les colonnes choisies.",
                                 input$seuil_rare), type = "message")
        return()
      }
      d_apres <- regrouper_modalites_rares(d, cols_eff, seuil = seuil, label = "Autre")
       resume_txt <- paste(sprintf("%s (%d -> 'Autre')", cols_eff, nb_rares[cols_eff]),
                          collapse = ", ")
      rv$h <- hist_ajouter(rv$h, "Regrouper les modalités rares",
                           detail = sprintf("Seuil < %d %% : %s", input$seuil_rare, resume_txt),
                           data   = d_apres,
                           params = list(colonnes = cols_eff, seuil = seuil))
    })

    # E. Fusion de deux colonnes
    observeEvent(input$btn_fusionner, {
      col1 <- input$fusion_col1
      col2 <- input$fusion_col2
      nom  <- trimws(input$fusion_nom)
      if (!nzchar(col1) || !nzchar(col2)) {
        showNotification("Choisissez deux colonnes à fusionner.", type = "warning")
        return()
      }
      if (!nzchar(nom)) {
        showNotification("Indiquez un nom pour la nouvelle colonne.", type = "warning")
        return()
      }
      nom <- make.names(nom)
      op  <- if (input$type_fusion == "quali") "concatenation" else input$fusion_op
      d   <- data_courante()
      d_apres <- fusionner_colonnes(d, col1, col2, op, nom,
                                    supprimer_origine = isTRUE(input$fusion_suppr),
                                    cible             = donnees$cible())
      sym_op <- switch(op, somme = "+", moyenne = "moy", difference = "-",
                       ratio = "/", concatenation = "&")
      detail_txt <- sprintf("%s = %s %s %s%s", nom, col1, sym_op, col2,
                            if (isTRUE(input$fusion_suppr)) " (origines supprimées)" else "")
      rv$h <- hist_ajouter(rv$h, "Fusionner des colonnes",
                           detail = detail_txt,
                           data   = d_apres,
                           params = list(col1 = col1, col2 = col2, operation = op,
                                         nouveau_nom = nom,
                                         supprimer_origine = isTRUE(input$fusion_suppr)))
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

    # ==================================================================
    # CONFIGURATION DES TRANSFORMATIONS APPRISES (pour l'onglet 4)
    # ==================================================================
    config_transformations <- reactive({
      list(
        imputation    = if (is.null(input$trans_imputation))    "mediane" else input$trans_imputation,
        normalisation = if (is.null(input$trans_normalisation)) "aucune"  else input$trans_normalisation,
        dummification = isTRUE(input$trans_dummification),
        desequilibre  = if (is.null(input$trans_desequilibre))  "aucun"   else input$trans_desequilibre
      )
    })

    output$dist_cible_prep <- renderTable({
      d <- data_courante()
      req(donnees$cible() %in% names(d))
      tableau_cible(d[[donnees$cible()]])
    })

    output$tab_transformations <- renderTable({
      resumer_transformations(config_transformations())
    }, striped = TRUE, bordered = TRUE, width = "100%")

    # ==================================================================
    # AFFICHAGE (historique, bilan, suggestions, aperçu, résumé)
    # ==================================================================
    output$historique <- renderDT({
      req(rv$h)
      datatable(hist_tableau(rv$h), rownames = FALSE, selection = "single",
                options = list(dom = "t", ordering = FALSE, pageLength = 100))
    })

    # Bilan comparatif avant / après nettoyage
    output$bilan_prep <- renderUI({
      req(rv$h)
      d0 <- rv$h[[1]]$data
      d1 <- data_courante()
      out0 <- sum(sapply(d0, nb_outliers), na.rm = TRUE)
      out1 <- sum(sapply(d1, nb_outliers), na.rm = TRUE)
      div(style = "background: #f8fafc; border-left: 4px solid #0284c7; padding: 10px; margin-bottom: 12px;",
          strong(sprintf("Étapes appliquées : %d", length(rv$h) - 1)), br(),
          sprintf("Lignes : %d → %d  |  Colonnes : %d → %d  |  NA : %d → %d  |  Outliers (IQR) : %d → %d",
                  nrow(d0), nrow(d1), ncol(d0), ncol(d1),
                  sum(is.na(d0)), sum(is.na(d1)), out0, out1))
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
      d <- data_courante()
      paste(nrow(d), "lignes,", ncol(d), "colonnes,", sum(is.na(d)), "NA")
    })

    output$apercu <- renderDT({
      datatable(data_courante(), selection = "multiple",
                options = list(pageLength = 10, scrollX = TRUE))
    })

    output$resume_prep <- renderDT({
      d <- data_courante()
      cible <- donnees$cible()
      a <- verifier_donnees(d, d)
      r <- resume_variables(d, a, cible)
      r$N_alertes <- NULL
      names(r)[names(r) == "NA_pct"] <- "NA (%)"
      datatable(r, rownames = FALSE,
                options = list(pageLength = 25, scrollX = TRUE, dom = "ftp", ordering = FALSE)) |>
        formatStyle("Role", target = "row", fontWeight = styleEqual("cible", "bold"),
                    backgroundColor = styleEqual("cible", "#e3f2fd"))
    })

    # On renvoie aux autres modules : données nettoyées, cible, historique,
    # et configuration des transformations apprises (pour l'onglet 4)
    return(list(
      data            = data_courante,
      cible           = donnees$cible,
      historique      = reactive(rv$h),
      transformations = config_transformations
    ))
  })
}
