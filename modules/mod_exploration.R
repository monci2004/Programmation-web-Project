# ==================================================================
# Module 3 : analyse exploratoire (Cours 1 & Cours 2)
# - reçoit `preparation` (liste réactive de mod_preparation_server : data, cible)
# - Sous-onglet 1 : Analyse unidimensionnelle (tableau statistique complet,
#   histogramme Sturges/Scott/FD + densité + quantiles, boxplot + rug, ECDF,
#   diagramme en barres, camembert)
# - Sous-onglet 2 : Analyse bidimensionnelle qui s'adapte automatiquement aux types :
#   * Quant × Quant : nuage + lm, histogrammes dos à dos (Hmisc::histbackback), cov, Pearson r
#   * Quant × Qual  : boxplots côte à côte (base R / ggplot + jitter), rapport de corrélation η
#   * Qual  × Qual  : table de contingence, barres (empilées / côte à côte / profils), χ², Φ², V de Cramér
# - Sous-onglet 3 : Matrice de corrélation (heatmap) de toutes les variables quantitatives
# ==================================================================

# Interface du module
mod_exploration_ui <- function(id) {
  ns <- NS(id)

  tabsetPanel(
    id = ns("onglets_explo"),

    # ==================================================================
    # 1. ANALYSE UNIDIMENSIONNELLE (COURS 1)
    # ==================================================================
    tabPanel("1. Une variable (Unidimensionnel)",
      br(),
      sidebarLayout(
        sidebarPanel(
          h4("Variable à explorer"),
          selectInput(ns("var_uni"), "Choisir une variable", choices = NULL),
          uiOutput(ns("badge_type_uni")),
          hr(),
          h4("Options d'affichage"),
          uiOutput(ns("options_uni")),
          width = 3
        ),
        mainPanel(
          uiOutput(ns("resultats_uni")),
          width = 9
        )
      )
    ),

    # ==================================================================
    # 2. ANALYSE BIDIMENSIONNELLE (COURS 2)
    # ==================================================================
    tabPanel("2. Deux variables (Bidimensionnel)",
      br(),
      sidebarLayout(
        sidebarPanel(
          h4("Couple de variables"),
          selectInput(ns("var_bi1"), "Variable 1 (X)", choices = NULL),
          selectInput(ns("var_bi2"), "Variable 2 (Y)", choices = NULL),
          actionButton(ns("btn_cible_y"), "Placer la cible en Variable 2",
                       icon = icon("bullseye"), width = "100%"),
          br(), br(),
          uiOutput(ns("badge_type_bi")),
          hr(),
          h4("Options du graphique"),
          uiOutput(ns("options_bi")),
          width = 3
        ),
        mainPanel(
          uiOutput(ns("resultats_bi")),
          width = 9
        )
      )
    ),

    # ==================================================================
    # 3. MATRICE DE CORRÉLATION (TOUTES LES VARIABLES QUANTITATIVES)
    # ==================================================================
    tabPanel("3. Matrice de corrélation",
      br(),
      sidebarLayout(
        sidebarPanel(
          h4("Paramètres"),
          radioButtons(ns("methode_corr"), "Coefficient de corrélation",
                       choices = c("Linéaire de Pearson (r)" = "pearson",
                                   "Rangs de Spearman (ρ)"   = "spearman"),
                       selected = "pearson"),
          helpText("Calculé sur toutes les variables quantitatives des données préparées."),
          width = 3
        ),
        mainPanel(
          fluidRow(
            column(7, plotOutput(ns("plot_heatmap_corr"), height = 460)),
            column(5,
              h4("Tableau des corrélations"),
              DTOutput(ns("tab_matrice_corr"))
            )
          ),
          width = 9
        )
      )
    )
  )
}

# Logique du module
# - preparation : liste renvoyée par mod_preparation_server (data, cible, ...)
mod_exploration_server <- function(id, preparation) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Données préparées courantes
    donnees_prep <- reactive({
      preparation$data()
    })

    # Mise à jour des listes de variables quand les données changent
    observeEvent(donnees_prep(), {
      d <- donnees_prep()
      req(d)
      noms  <- names(d)
      cible <- preparation$cible()
      v2_defaut <- if (!is.null(cible) && cible %in% noms) cible else noms[min(2, length(noms))]
      v1_defaut <- setdiff(noms, v2_defaut)[1]
      if (is.na(v1_defaut)) v1_defaut <- noms[1]

      updateSelectInput(session, "var_uni", choices = noms, selected = noms[1])
      updateSelectInput(session, "var_bi1", choices = noms, selected = v1_defaut)
      updateSelectInput(session, "var_bi2", choices = noms, selected = v2_defaut)
    })

    observeEvent(input$btn_cible_y, {
      cible <- preparation$cible()
      req(cible %in% names(donnees_prep()))
      updateSelectInput(session, "var_bi2", selected = cible)
    })

    # ==================================================================
    # 1. LOGIQUE UNIDIMENSIONNELLE
    # ==================================================================

    est_quanti_uni <- reactive({
      d <- donnees_prep()
      req(input$var_uni %in% names(d))
      is.numeric(d[[input$var_uni]])
    })

    output$badge_type_uni <- renderUI({
      type_txt <- if (est_quanti_uni()) "Variable quantitative (numérique)" else "Variable qualitative (catégorielle)"
      couleur  <- if (est_quanti_uni()) "#0284c7" else "#7c3aed"
      div(style = sprintf("background: %s15; color: %s; padding: 6px 10px; border-radius: 4px; font-weight: bold;",
                          couleur, couleur),
          type_txt)
    })

    output$options_uni <- renderUI({
      if (est_quanti_uni()) {
        tagList(
          selectInput(ns("regle_bins"), "Règle des classes (Histogramme)",
                      choices = c("Sturges"                = "Sturges",
                                  "Scott"                  = "Scott",
                                  "Freedman-Diaconis (FD)" = "FD"),
                      selected = "Sturges"),
          radioButtons(ns("echelle_hist"), "Axe vertical de l'histogramme",
                       choices = c("Densité"   = "densite",
                                   "Effectifs" = "effectifs"),
                       selected = "densite", inline = TRUE),
          checkboxInput(ns("afficher_densite"),   "Courbe de densité empirique", value = TRUE),
          checkboxInput(ns("afficher_quantiles"), "Lignes Q1 / Médiane / Q3 / Moyenne", value = TRUE),
          checkboxInput(ns("afficher_rug"),       "Peigne (rug) sous le boxplot", value = TRUE)
        )
      } else {
        radioButtons(ns("echelle_barres"), "Axe vertical des barres",
                     choices = c("Effectifs (ni)" = "effectifs",
                                 "Fréquences (%)" = "frequences"),
                     selected = "effectifs")
      }
    })

    output$resultats_uni <- renderUI({
      if (est_quanti_uni()) {
        tagList(
          fluidRow(
            column(6, plotOutput(ns("plot_uni_hist"), height = 310)),
            column(6, plotOutput(ns("plot_uni_box"),  height = 310))
          ),
          fluidRow(
            column(6, plotOutput(ns("plot_uni_ecdf"), height = 310)),
            column(6,
              h4("Indicateurs de centre et de dispersion"),
              tableOutput(ns("tab_uni_indic"))
            )
          ),
          hr(),
          h4("Tableau statistique (effectifs, cumulés et fréquences)"),
          DTOutput(ns("tab_uni_stat"))
        )
      } else {
        tagList(
          fluidRow(
            column(6, plotOutput(ns("plot_uni_bar"), height = 330)),
            column(6, plotOutput(ns("plot_uni_pie"), height = 330))
          ),
          hr(),
          h4("Tableau statistique (effectifs, effectifs cumulés, fréquences et fréquences cumulées)"),
          DTOutput(ns("tab_uni_stat"))
        )
      }
    })

    output$plot_uni_hist <- renderPlot({
      d <- donnees_prep(); v <- input$var_uni
      req(v %in% names(d), is.numeric(d[[v]]), input$regle_bins, input$echelle_hist)
      tracer_uni_histogramme(
        x                  = d[[v]],
        nom                = v,
        regle_bins         = input$regle_bins,
        en_densite         = (input$echelle_hist == "densite"),
        afficher_densite   = isTRUE(input$afficher_densite),
        afficher_quantiles = isTRUE(input$afficher_quantiles)
      )
    })

    output$plot_uni_box <- renderPlot({
      d <- donnees_prep(); v <- input$var_uni
      req(v %in% names(d), is.numeric(d[[v]]))
      tracer_uni_boxplot(d[[v]], v, afficher_rug = isTRUE(input$afficher_rug))
    })

    output$plot_uni_ecdf <- renderPlot({
      d <- donnees_prep(); v <- input$var_uni
      req(v %in% names(d), is.numeric(d[[v]]))
      tracer_uni_ecdf(d[[v]], v)
    })

    output$tab_uni_indic <- renderTable({
      d <- donnees_prep(); v <- input$var_uni
      req(v %in% names(d), is.numeric(d[[v]]))
      indicateurs_quanti(d[[v]])
    }, striped = TRUE, bordered = TRUE, width = "100%")

    output$plot_uni_bar <- renderPlot({
      d <- donnees_prep(); v <- input$var_uni
      req(v %in% names(d), !is.numeric(d[[v]]), input$echelle_barres)
      tracer_uni_barres(d[[v]], v, en_frequence = (input$echelle_barres == "frequences"))
    })

    output$plot_uni_pie <- renderPlot({
      d <- donnees_prep(); v <- input$var_uni
      req(v %in% names(d), !is.numeric(d[[v]]))
      tracer_uni_camembert(d[[v]], v)
    })

    output$tab_uni_stat <- renderDT({
      d <- donnees_prep(); v <- input$var_uni
      req(v %in% names(d))
      regle <- if (is.null(input$regle_bins)) "Sturges" else input$regle_bins
      tab <- tableau_statistique_uni(d[[v]], regle_bins = regle)
      datatable(tab, rownames = FALSE,
                options = list(pageLength = 15, dom = "tp", ordering = FALSE))
    })

    # ==================================================================
    # 2. LOGIQUE BIDIMENSIONNELLE
    # ==================================================================

    croisement_bi <- reactive({
      d  <- donnees_prep()
      v1 <- input$var_bi1; v2 <- input$var_bi2
      req(v1 %in% names(d), v2 %in% names(d))
      type_croisement(d[[v1]], d[[v2]])
    })

    output$badge_type_bi <- renderUI({
      lib <- switch(croisement_bi(),
        "quanti_quanti" = "Quantitatif × Quantitatif (Corrélation & Régression)",
        "quanti_quali"  = "Quantitatif × Qualitatif (ANOVA & Rapport η²)",
        "quali_quali"   = "Qualitatif × Qualitatif (Contingence, χ² & V de Cramér)"
      )
      div(style = "background: #f0fdf4; color: #166534; padding: 6px 10px; border-radius: 4px; font-weight: bold;",
          lib)
    })

    output$options_bi <- renderUI({
      switch(croisement_bi(),
        "quanti_quanti" = checkboxInput(ns("bi_lm"),
                                        "Afficher la droite de régression linéaire (lm)",
                                        value = TRUE),
        "quanti_quali"  = radioButtons(ns("bi_style_box"), "Style des boîtes à moustaches",
                                       choices = c("ggplot2 + points (jitter) + moyennes" = "ggplot_jitter",
                                                   "Classique (base R)"                   = "base"),
                                       selected = "ggplot_jitter"),
        "quali_quali"   = tagList(
          radioButtons(ns("bi_pos_bar"), "Représentation en barres",
                       choices = c("Profils conditionnels (position = 'fill')" = "fill",
                                   "Barres côte à côte (position = 'dodge')"   = "dodge",
                                   "Barres empilées (position = 'stack')"      = "stack"),
                       selected = "fill"),
          selectInput(ns("bi_mode_table"), "Table de contingence",
                      choices = c("Effectifs (avec marges)" = "effectifs",
                                  "Profils lignes (%)"      = "lignes",
                                  "Profils colonnes (%)"    = "colonnes",
                                  "Fréquences totales (%)"  = "total"))
        )
      )
    })

    output$resultats_bi <- renderUI({
      d  <- donnees_prep()
      v1 <- input$var_bi1; v2 <- input$var_bi2
      req(v1 %in% names(d), v2 %in% names(d))
      validate(need(v1 != v2, "Choisissez deux variables différentes (Variable 1 ≠ Variable 2)."))

      switch(croisement_bi(),
        "quanti_quanti" = tagList(
          fluidRow(
            column(6, plotOutput(ns("plot_bi_nuage"),    height = 340)),
            column(6, plotOutput(ns("plot_bi_histback"), height = 340))
          ),
          hr(),
          h4("Indicateurs de liaison (Covariance, Pearson r, Spearman ρ, droite de régression)"),
          tableOutput(ns("tab_bi_qq"))
        ),
        "quanti_quali" = tagList(
          plotOutput(ns("plot_bi_box"), height = 350),
          hr(),
          fluidRow(
            column(6,
              h4("Décomposition de la variance & Rapport de corrélation η"),
              tableOutput(ns("tab_bi_eta"))
            ),
            column(6,
              h4("Résumé statistique par modalité"),
              tableOutput(ns("tab_bi_groupes"))
            )
          )
        ),
        "quali_quali" = tagList(
          plotOutput(ns("plot_bi_bar_quali"), height = 340),
          hr(),
          fluidRow(
            column(6,
              h4("Table de contingence"),
              tableOutput(ns("tab_bi_contingence"))
            ),
            column(6,
              h4("Test du χ², Φ² et V de Cramér"),
              tableOutput(ns("tab_bi_chi2"))
            )
          )
        )
      )
    })

    # --- Sorties Quant × Quant ---
    output$plot_bi_nuage <- renderPlot({
      d <- donnees_prep(); v1 <- input$var_bi1; v2 <- input$var_bi2
      req(croisement_bi() == "quanti_quanti", v1 != v2)
      tracer_bi_nuage(d, v1, v2, afficher_lm = isTRUE(input$bi_lm))
    })

    output$plot_bi_histback <- renderPlot({
      d <- donnees_prep(); v1 <- input$var_bi1; v2 <- input$var_bi2
      req(croisement_bi() == "quanti_quanti", v1 != v2)
      tracer_bi_histbackback(d[[v1]], d[[v2]], v1, v2)
    })

    output$tab_bi_qq <- renderTable({
      d <- donnees_prep(); v1 <- input$var_bi1; v2 <- input$var_bi2
      req(croisement_bi() == "quanti_quanti", v1 != v2)
      indicateurs_quanti_quanti(d[[v1]], d[[v2]], v1, v2)
    }, striped = TRUE, bordered = TRUE, width = "100%")

    # --- Sorties Quant × Qual ---
    vars_quanti_quali <- reactive({
      d <- donnees_prep(); v1 <- input$var_bi1; v2 <- input$var_bi2
      req(croisement_bi() == "quanti_quali", v1 != v2)
      if (is.numeric(d[[v1]])) list(num = v1, fac = v2) else list(num = v2, fac = v1)
    })

    output$plot_bi_box <- renderPlot({
      d  <- donnees_prep(); vq <- vars_quanti_quali()
      style <- if (is.null(input$bi_style_box)) "ggplot_jitter" else input$bi_style_box
      tracer_bi_boxplots(d, vq$num, vq$fac, style = style)
    })

    res_quanti_quali <- reactive({
      d  <- donnees_prep(); vq <- vars_quanti_quali()
      indicateurs_quanti_quali(d[[vq$num]], d[[vq$fac]], vq$num, vq$fac)
    })

    output$tab_bi_eta <- renderTable({
      res_quanti_quali()$liaison
    }, striped = TRUE, bordered = TRUE, width = "100%")

    output$tab_bi_groupes <- renderTable({
      res_quanti_quali()$resume_groupes
    }, striped = TRUE, bordered = TRUE, width = "100%")

    # --- Sorties Qual × Qual ---
    output$plot_bi_bar_quali <- renderPlot({
      d <- donnees_prep(); v1 <- input$var_bi1; v2 <- input$var_bi2
      req(croisement_bi() == "quali_quali", v1 != v2)
      pos <- if (is.null(input$bi_pos_bar)) "fill" else input$bi_pos_bar
      tracer_bi_barres_quali(d, v1, v2, position = pos)
    })

    output$tab_bi_contingence <- renderTable({
      d <- donnees_prep(); v1 <- input$var_bi1; v2 <- input$var_bi2
      req(croisement_bi() == "quali_quali", v1 != v2)
      mode_t <- if (is.null(input$bi_mode_table)) "effectifs" else input$bi_mode_table
      tableaux_quali_quali(d[[v1]], d[[v2]], mode_table = mode_t)
    }, striped = TRUE, bordered = TRUE, width = "100%")

    output$tab_bi_chi2 <- renderTable({
      d <- donnees_prep(); v1 <- input$var_bi1; v2 <- input$var_bi2
      req(croisement_bi() == "quali_quali", v1 != v2)
      indicateurs_quali_quali(d[[v1]], d[[v2]])
    }, striped = TRUE, bordered = TRUE, width = "100%")

    # ==================================================================
    # 3. MATRICE DE CORRÉLATION (HEATMAP)
    # ==================================================================

    output$plot_heatmap_corr <- renderPlot({
      d <- donnees_prep()
      validate(need(sum(sapply(d, is.numeric)) >= 2,
                    "Il faut au moins 2 variables quantitatives pour calculer une matrice de corrélation."))
      tracer_heatmap_correlation(d, methode = input$methode_corr)
    })

    output$tab_matrice_corr <- renderDT({
      d <- donnees_prep()
      mat <- calculer_matrice_correlation(d, methode = input$methode_corr)
      req(mat)
      datatable(as.data.frame(mat), options = list(dom = "t", scrollX = TRUE, ordering = FALSE))
    })
  })
}

