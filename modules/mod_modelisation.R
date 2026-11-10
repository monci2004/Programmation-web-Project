# ==================================================================
# Module 4 : modélisation et évaluation des classifieurs
# - reçoit `preparation` (liste réactive de mod_preparation_server :
#   data, cible, historique, transformations)
# - découpage train / test (stratifié ou aléatoire)
# - transformations apprises (imputation, normalisation, dummification,
#   rééquilibrage) ajustées UNIQUEMENT sur train (zéro data leakage)
# - modèles : Régression logistique (glm) + Forêt aléatoire (randomForest)
# - métriques : Exactitude, Précision, Rappel, Spécificité, F-score, AUC,
#   matrices de confusion, courbes ROC, importance des variables
# - historique complet des runs (comparaison ROC multi-runs, suppression, export CSV)
# ==================================================================

# Interface du module
mod_modelisation_ui <- function(id) {
  ns <- NS(id)

  sidebarLayout(
    sidebarPanel(
      h4("1. Découpage & Cible"),
      selectInput(ns("classe_positive"), "Classe positive (à détecter)", choices = NULL),
      sliderInput(ns("part_train"), "Part d'entraînement (Train)",
                  min = 50, max = 90, value = 70, step = 5, post = " %"),
      checkboxInput(ns("stratifie"), "Découpage stratifié par classe", value = TRUE),
      fluidRow(
        column(6, numericInput(ns("seed"), "Graine (seed)", value = 42, min = 1, step = 1)),
        column(6, sliderInput(ns("seuil"), "Seuil de décision",
                              min = 0.1, max = 0.9, value = 0.5, step = 0.05))
      ),
      selectInput(ns("variables"), "Variables explicatives incluses",
                  choices = NULL, multiple = TRUE),
      hr(),

      h4("2. Modèles & Paramètres"),
      # --- Modèle 1 : Régression logistique ---
      checkboxInput(ns("activer_glm"), strong("Régression logistique (glm)"), value = TRUE),
      conditionalPanel(
        condition = sprintf("input['%s']", ns("activer_glm")),
        div(style = "padding-left: 12px; border-left: 3px solid #cbd5e1; margin-bottom: 10px;",
            checkboxInput(ns("glm_interactions"),
                          "Ajouter les interactions d'ordre 2", value = FALSE),
            helpText("Ajuste un modèle binomial (logit) sur l'échantillon train."))
      ),

      # --- Modèle 2 : Forêt aléatoire ---
      checkboxInput(ns("activer_rf"), strong("Forêt aléatoire (Random Forest)"), value = TRUE),
      conditionalPanel(
        condition = sprintf("input['%s']", ns("activer_rf")),
        div(style = "padding-left: 12px; border-left: 3px solid #cbd5e1; margin-bottom: 10px;",
            fluidRow(
              column(6, numericInput(ns("rf_ntree"), "Arbres (ntree)",
                                     value = 300, min = 50, max = 2000, step = 50)),
              column(6, numericInput(ns("rf_mtry"), "Variables/split (mtry)",
                                     value = 2, min = 1, step = 1))
            ),
            fluidRow(
              column(6, numericInput(ns("rf_nodesize"), "Taille min feuille",
                                     value = 1, min = 1, max = 50, step = 1)),
              column(6, numericInput(ns("rf_maxnodes"), "Max feuilles (0 = libre)",
                                     value = 0, min = 0, step = 5))
            ),
            checkboxInput(ns("rf_autotune"),
                          "Auto-tune mtry (minimise l'erreur OOB sur train)", value = FALSE))
      ),
      hr(),

      actionButton(ns("btn_entrainer"), "Entraîner les modèles",
                   icon = icon("play"), class = "btn-primary", width = "100%"),
      width = 4
    ),

    mainPanel(
      tabsetPanel(
        id = ns("onglets_mod"),

        # ---- Onglet 1 : Résultats du dernier entraînement ----
        tabPanel("Résultats actuels",
          br(),
          uiOutput(ns("bilan_entrainement")),
          h4("Comparaison des performances (sur l'échantillon Test)"),
          tableOutput(ns("tab_metriques")),
          fluidRow(
            column(7,
              h4("Courbes ROC (sur Test)"),
              plotOutput(ns("plot_roc_courant"), height = 340)
            ),
            column(5,
              h4("Matrices de confusion"),
              uiOutput(ns("bloc_confusions"))
            )
          )
        ),

        # ---- Onglet 2 : Importance des variables ----
        tabPanel("Importance des variables",
          br(),
          fluidRow(
            column(6,
              h4("Forêt aléatoire (MeanDecreaseGini)"),
              plotOutput(ns("plot_imp_rf"), height = 300),
              DTOutput(ns("tab_imp_rf"))
            ),
            column(6,
              h4("Régression logistique (|z|)"),
              plotOutput(ns("plot_imp_glm"), height = 300),
              DTOutput(ns("tab_imp_glm"))
            )
          )
        ),

        # ---- Onglet 3 : Historique de tous les runs ----
        tabPanel("Historique des runs",
          br(),
          helpText("Chaque entraînement est archivé ci-dessous. Cliquez sur plusieurs lignes ",
                   "du tableau pour comparer leurs courbes ROC sur le même graphique."),
          div(style = "margin-bottom: 10px;",
              actionButton(ns("btn_suppr_runs"), "Supprimer les runs sélectionnés",
                           icon = icon("trash")),
              actionButton(ns("btn_vider_hist"), "Vider l'historique",
                           icon = icon("broom"), class = "btn-danger"),
              downloadButton(ns("btn_export_csv"), "Exporter en CSV")
          ),
          DTOutput(ns("tab_historique")),
          br(),
          h4("Superposition des courbes ROC (runs sélectionnés ou tous les runs)"),
          plotOutput(ns("plot_roc_hist"), height = 380)
        )
      ),
      width = 8
    )
  )
}

# Logique du module
# - preparation : liste renvoyée par mod_preparation_server
#   (data, cible, historique, transformations)
mod_modelisation_server <- function(id, preparation) {
  moduleServer(id, function(input, output, session) {

    # Stockage réactif : dernier run entraîné + historique de tous les runs
    rv <- reactiveValues(
      dernier = NULL,
      hist    = hist_modele_nouveau()
    )

    # Mise à jour des choix (classe positive, variables explicatives, mtry par défaut)
    # quand les données préparées ou la cible changent
    observeEvent(list(preparation$data(), preparation$cible()), {
      d     <- preparation$data()
      cible <- preparation$cible()
      req(d, cible %in% names(d))

      # Modalités de la cible : par défaut on propose la classe minoritaire (souvent la classe d'intérêt)
      y   <- droplevels(factor(d[[cible]]))
      eff <- table(y)
      cl_defaut <- if (length(eff) >= 2) names(which.min(eff)) else levels(y)[1]
      updateSelectInput(session, "classe_positive",
                        choices  = levels(y),
                        selected = cl_defaut)

      # Variables explicatives disponibles
      pred <- setdiff(names(d), cible)
      updateSelectInput(session, "variables",
                        choices  = pred,
                        selected = pred)

      # Valeur par défaut de mtry = floor(sqrt(p))
      p <- max(1L, length(pred))
      updateNumericInput(session, "rf_mtry",
                         value = max(1L, floor(sqrt(p))),
                         max   = p)
    })

    # Entraînement au clic sur le bouton
    # ----
    observeEvent(input$btn_entrainer, {
      d     <- preparation$data()
      cible <- preparation$cible()
      cfg   <- preparation$transformations()
      hist_prep <- preparation$historique()

      if (is.null(d) || !(cible %in% names(d))) {
        showNotification("Validez d'abord les données dans l'onglet 1.", type = "error")
        return()
      }
      if (!isTRUE(input$activer_glm) && !isTRUE(input$activer_rf)) {
        showNotification("Cochez au moins un modèle à entraîner.", type = "warning")
        return()
      }
      if (length(input$variables) == 0) {
        showNotification("Sélectionnez au moins une variable explicative.", type = "warning")
        return()
      }

      y_propre <- droplevels(factor(d[[cible]][!is.na(d[[cible]])]))
      if (nlevels(y_propre) != 2) {
        showNotification(sprintf(
          "La cible '%s' a %d classes : la modélisation binaire (ROC / logistique) nécessite exactement 2 classes.",
          cible, nlevels(y_propre)), type = "error", duration = 8)
        return()
      }

      # 1) Découpage train/test + transformations apprises sur train uniquement
      prep <- tryCatch(
        preparer_train_test(
          d          = d,
          cible      = cible,
          variables  = input$variables,
          prop_train = input$part_train / 100,
          stratifie  = isTRUE(input$stratifie),
          seed       = input$seed,
          cfg        = cfg
        ),
        error = function(e) {
          showNotification(paste("Erreur lors de la préparation train/test :", conditionMessage(e)),
                           type = "error", duration = 8)
          NULL
        }
      )
      req(prep)

      if (nrow(prep$train) < 10 || nrow(prep$test) < 5) {
        showNotification("Trop peu de lignes complètes dans train ou test après filtrage des NA.",
                         type = "error", duration = 8)
        return()
      }
      if (length(unique(prep$test[[cible]])) < 2) {
        showNotification("L'échantillon test ne contient qu'une seule classe : activez le découpage stratifié.",
                         type = "error", duration = 8)
        return()
      }

      cl_pos       <- input$classe_positive
      seuil        <- input$seuil
      split_txt    <- sprintf("%d/%d %s (seed=%d)",
                              input$part_train, 100 - input$part_train,
                              if (isTRUE(input$stratifie)) "strat." else "aléat.",
                              input$seed)
      pipeline_txt <- resumer_pipeline_texte(hist_prep, cfg)

      res_glm <- NULL; eval_glm <- NULL; imp_glm <- NULL
      res_rf  <- NULL; eval_rf  <- NULL; imp_rf  <- NULL

      # 2) Entraînement de la Régression logistique
      if (isTRUE(input$activer_glm)) {
        res_glm <- tryCatch(
          entrainer_logistique(
            train           = prep$train,
            test            = prep$test,
            cible           = cible,
            classe_positive = cl_pos,
            interactions    = isTRUE(input$glm_interactions),
            poids_lignes    = if (!is.null(prep$poids)) prep$poids$poids_lignes else NULL
          ),
          error = function(e) {
            showNotification(paste("Erreur Régression logistique :", conditionMessage(e)),
                             type = "error", duration = 8)
            NULL
          }
        )
        if (!is.null(res_glm)) {
          eval_glm <- evaluer_modele(res_glm$proba, prep$test[[cible]], cl_pos, seuil = seuil)
          imp_glm  <- importance_variables(res_glm$modele)
          param_glm <- sprintf("interactions=%s, vars=%d",
                               if (isTRUE(input$glm_interactions)) "oui" else "non",
                               length(input$variables))
          rv$hist <- hist_modele_ajouter(rv$hist, "Régression logistique",
                                         param_glm, split_txt, pipeline_txt,
                                         eval_glm, imp_glm)
        }
      }

      # 3) Entraînement de la Forêt aléatoire
      if (isTRUE(input$activer_rf)) {
        max_n <- if (!is.null(input$rf_maxnodes) && input$rf_maxnodes > 1) input$rf_maxnodes else NULL
        res_rf <- tryCatch(
          entrainer_rf(
            train           = prep$train,
            test            = prep$test,
            cible           = cible,
            classe_positive = cl_pos,
            ntree           = input$rf_ntree,
            mtry            = input$rf_mtry,
            nodesize        = input$rf_nodesize,
            maxnodes        = max_n,
            auto_tune       = isTRUE(input$rf_autotune),
            poids_classes   = if (!is.null(prep$poids)) prep$poids$poids_classes else NULL,
            seed            = input$seed
          ),
          error = function(e) {
            showNotification(paste("Erreur Forêt aléatoire :", conditionMessage(e)),
                             type = "error", duration = 8)
            NULL
          }
        )
        if (!is.null(res_rf)) {
          eval_rf <- evaluer_modele(res_rf$proba, prep$test[[cible]], cl_pos, seuil = seuil)
          imp_rf  <- importance_variables(res_rf$modele)
          param_rf <- sprintf("ntree=%d, mtry=%d%s, nodesize=%d%s",
                              input$rf_ntree, res_rf$mtry_utilise,
                              if (isTRUE(input$rf_autotune)) " (auto)" else "",
                              input$rf_nodesize,
                              if (!is.null(max_n)) paste0(", maxnodes=", max_n) else "")
          rv$hist <- hist_modele_ajouter(rv$hist, "Forêt aléatoire",
                                         param_rf, split_txt, pipeline_txt,
                                         eval_rf, imp_rf)
        }
      }

      rv$dernier <- list(
        prep     = prep,
        cfg      = cfg,
        eval_glm = eval_glm,
        imp_glm  = imp_glm,
        eval_rf  = eval_rf,
        imp_rf   = imp_rf,
        mtry_rf  = if (!is.null(res_rf)) res_rf$mtry_utilise else NULL
      )

      showNotification("Entraînement terminé et ajouté à l'historique.", type = "message")
    })

    # ==================================================================
    # AFFICHAGE : ONGLET 1 (Résultats actuels)
    # ==================================================================

    output$bilan_entrainement <- renderUI({
      validate(need(rv$dernier, "Configurez les paramètres à gauche puis cliquez sur 'Entraîner les modèles'."))
      d_res <- rv$dernier
      p     <- d_res$prep
      cfg   <- d_res$cfg
      na_tot <- p$n_na_cible + p$n_na_train + p$n_na_test

      tagList(
        div(style = "background: #f8fafc; border-left: 4px solid #2563eb; padding: 10px; margin-bottom: 12px;",
            strong(sprintf("Échantillons : Train = %d lignes%s  |  Test = %d lignes  |  Classe positive = '%s' (seuil = %.2f)",
                           nrow(p$train),
                           if (nrow(p$train) != p$n_train_avant_reeq)
                             sprintf(" (%d avant %s)", p$n_train_avant_reeq, cfg$desequilibre) else "",
                           nrow(p$test),
                           input$classe_positive,
                           input$seuil)),
            br(),
            span(style = "color: #475569;",
                 sprintf("Transformations apprises sur Train uniquement : Imputation = %s | Normalisation = %s | Dummification = %s | Déséquilibre = %s",
                         cfg$imputation, cfg$normalisation,
                         if (isTRUE(cfg$dummification)) "oui" else "non",
                         cfg$desequilibre)),
            if (na_tot > 0) {
              div(style = "color: #b45309; margin-top: 4px;",
                  icon("triangle-exclamation"),
                  sprintf(" %d ligne(s) incomplète(s) (NA) retirée(s) avant modélisation (%d cible, %d train, %d test).",
                          na_tot, p$n_na_cible, p$n_na_train, p$n_na_test))
            }
        )
      )
    })

    output$tab_metriques <- renderTable({
      req(rv$dernier)
      lignes <- list()
      if (!is.null(rv$dernier$eval_glm)) {
        e <- rv$dernier$eval_glm
        lignes[[length(lignes) + 1]] <- data.frame(
          Modele      = "Régression logistique",
          Exactitude  = e$exactitude,
          Precision   = e$precision,
          Rappel      = e$rappel,
          Specificite = e$specificite,
          `F-score`   = e$fscore,
          AUC         = e$auc,
          check.names = FALSE
        )
      }
      if (!is.null(rv$dernier$eval_rf)) {
        e <- rv$dernier$eval_rf
        lignes[[length(lignes) + 1]] <- data.frame(
          Modele      = sprintf("Forêt aléatoire (mtry=%d)", rv$dernier$mtry_rf),
          Exactitude  = e$exactitude,
          Precision   = e$precision,
          Rappel      = e$rappel,
          Specificite = e$specificite,
          `F-score`   = e$fscore,
          AUC         = e$auc,
          check.names = FALSE
        )
      }
      do.call(rbind, lignes)
    }, digits = 3, striped = TRUE, bordered = TRUE, width = "100%")

    output$plot_roc_courant <- renderPlot({
      req(rv$dernier)
      entrees <- list()
      if (!is.null(rv$dernier$eval_glm)) {
        entrees[[length(entrees) + 1]] <- list(
          id = 1, modele = "Régression logistique",
          auc = rv$dernier$eval_glm$auc,
          roc = rv$dernier$eval_glm$roc,
          courbe_roc = rv$dernier$eval_glm$courbe_roc
        )
      }
      if (!is.null(rv$dernier$eval_rf)) {
        entrees[[length(entrees) + 1]] <- list(
          id = 2, modele = "Forêt aléatoire",
          auc = rv$dernier$eval_rf$auc,
          roc = rv$dernier$eval_rf$roc,
          courbe_roc = rv$dernier$eval_rf$courbe_roc
        )
      }
      tracer_courbes_roc(entrees)
    })

    output$bloc_confusions <- renderUI({
      req(rv$dernier)
      blocs <- list()
      if (!is.null(rv$dernier$eval_glm)) {
        blocs[[length(blocs) + 1]] <- tagList(
          h5(strong("Régression logistique")),
          verbatimTextOutput(session$ns("conf_glm"))
        )
      }
      if (!is.null(rv$dernier$eval_rf)) {
        blocs[[length(blocs) + 1]] <- tagList(
          h5(strong("Forêt aléatoire")),
          verbatimTextOutput(session$ns("conf_rf"))
        )
      }
      do.call(tagList, blocs)
    })

    output$conf_glm <- renderPrint({ req(rv$dernier$eval_glm); rv$dernier$eval_glm$confusion })
    output$conf_rf  <- renderPrint({ req(rv$dernier$eval_rf);  rv$dernier$eval_rf$confusion })

    # ==================================================================
    # AFFICHAGE : ONGLET 2 (Importance des variables)
    # ==================================================================

    output$plot_imp_rf <- renderPlot({
      validate(need(rv$dernier$imp_rf, "Entraînez la Forêt aléatoire pour voir l'importance Gini."))
      tracer_importance(rv$dernier$imp_rf, lab_y = "MeanDecreaseGini")
    })

    output$tab_imp_rf <- renderDT({
      req(rv$dernier$imp_rf)
      datatable(rv$dernier$imp_rf, rownames = FALSE,
                options = list(pageLength = 10, dom = "tp", ordering = FALSE))
    })

    output$plot_imp_glm <- renderPlot({
      validate(need(rv$dernier$imp_glm, "Entraînez la Régression logistique pour voir |z|."))
      tracer_importance(rv$dernier$imp_glm, lab_y = "Statistique |z|")
    })

    output$tab_imp_glm <- renderDT({
      req(rv$dernier$imp_glm)
      datatable(rv$dernier$imp_glm, rownames = FALSE,
                options = list(pageLength = 10, dom = "tp", ordering = FALSE))
    })

    # ==================================================================
    # AFFICHAGE : ONGLET 3 (Historique des runs & comparaison ROC)
    # ==================================================================

    observeEvent(input$btn_suppr_runs, {
      idx <- input$tab_historique_rows_selected
      if (length(idx) == 0) {
        showNotification("Sélectionnez d'abord une ou plusieurs lignes dans l'historique.",
                         type = "warning")
        return()
      }
      tab <- hist_modele_tableau(rv$hist)
      ids <- tab$Run[idx]
      rv$hist <- hist_modele_supprimer(rv$hist, ids)
    })

    observeEvent(input$btn_vider_hist, {
      rv$hist <- hist_modele_vider()
    })

    output$btn_export_csv <- downloadHandler(
      filename = function() {
        paste0("historique_modeles_", format(Sys.Date(), "%Y%m%d"), ".csv")
      },
      content = function(file) {
        write.csv(hist_modele_tableau(rv$hist), file, row.names = FALSE)
      }
    )

    output$tab_historique <- renderDT({
      tab <- hist_modele_tableau(rv$hist)
      dt  <- datatable(tab, rownames = FALSE, selection = "multiple",
                       options = list(pageLength = 15, scrollX = TRUE, dom = "ftp"))
      if (nrow(tab) > 0) {
        max_f1  <- max(tab$F_score, na.rm = TRUE)
        max_auc <- max(tab$AUC,     na.rm = TRUE)
        if (is.finite(max_f1)) {
          dt <- dt |> formatStyle("F_score",
                                  backgroundColor = styleEqual(max_f1, "#dcfce7"),
                                  fontWeight      = styleEqual(max_f1, "bold"))
        }
        if (is.finite(max_auc)) {
          dt <- dt |> formatStyle("AUC",
                                  backgroundColor = styleEqual(max_auc, "#dcfce7"),
                                  fontWeight      = styleEqual(max_auc, "bold"))
        }
      }
      dt
    })

    output$plot_roc_hist <- renderPlot({
      validate(need(length(rv$hist) > 0, "Aucun run enregistré dans l'historique."))
      idx <- input$tab_historique_rows_selected
      entrees <- if (length(idx) > 0) rv$hist[idx] else rv$hist
      tracer_courbes_roc(entrees)
    })

    return(list(
      historique = reactive(rv$hist)
    ))
  })
}

