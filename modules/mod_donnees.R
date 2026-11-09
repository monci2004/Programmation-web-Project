# ==================================================================
# Module 1 : chargement des données
# - choix du fichier CSV et des options de lecture
# - aperçu des données et résumé des variables
# - détection (et correction) du type de chaque variable
# - vérifications du contenu (constantes, identifiants, doublons, NA...)
# - choix de la variable cible (dernière colonne par défaut)
# - bouton de validation : seules les données validées passent aux autres onglets
# - renvoie une liste : data, cible, alertes, seuil_na_ligne (validés) et valide (compteur)
# ==================================================================

# Interface du module
mod_donnees_ui <- function(id) {
  ns <- NS(id)

  sidebarLayout(
    sidebarPanel(
      # Bouton de recherche du fichier à charger
      fileInput(ns("file1"), "Choisir un fichier CSV", accept = c("text/csv", ".csv")),

      # Options de lecture
      radioButtons(ns("sep"), "Séparateur",
                   choices = c("Virgule ," = ",", "Point-virgule ;" = ";", "Tabulation" = "\t"),
                   inline = TRUE),
      radioButtons(ns("dec"), "Décimale",
                   choices = c("Point ." = ".", "Virgule ," = ","), inline = TRUE),
      checkboxInput(ns("header"), "La première ligne contient les noms des colonnes", TRUE),
      textInput(ns("na_codes"), "Codes des valeurs manquantes (séparés par des virgules)",
                value = paste(setdiff(NA_CODES_DEFAUT, ""), collapse = ", ")),

      # Bouton de chargement 'en retard'
      actionButton(ns("go"), "Load", class = "btn-primary"),
      hr(),

      # Variable à prédire (par défaut : la dernière colonne)
      selectInput(ns("cible"), "Variable cible (à prédire)", choices = NULL),

      # Validation : les données ne passent à l'onglet 2 qu'après ce clic
      actionButton(ns("btn_valider"), "Valider et passer à la préparation",
                   icon = icon("check"), class = "btn-success"),
      uiOutput(ns("statut"))
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Aperçu",
                 h4("Aperçu des données"),
                 textOutput(ns("dimensions")),
                 DTOutput(ns("apercu"))
        ),
        tabPanel("Résumé",
                 h4("Résumé des variables"),
                 helpText("La cible en premier, puis les variables avec des alertes (détails dans 'Vérifications')."),
                 DTOutput(ns("resume"))
        ),
        tabPanel("Types des variables",
                 h4("Types détectés automatiquement"),
                 helpText("Le type est une suggestion : corrigez-le si besoin avec les listes ci-dessous."),
                 fluidRow(
                   column(4, numericInput(ns("seuil_modalites"),
                                          "Entiers avec au plus N valeurs = codes qualitatifs",
                                          value = 10, min = 2, max = 50)),
                   column(4, selectInput(ns("forcer_quali"), "Forcer en qualitative",
                                         choices = NULL, multiple = TRUE)),
                   column(4, selectInput(ns("forcer_quanti"), "Forcer en quantitative",
                                         choices = NULL, multiple = TRUE))
                 ),
                 DTOutput(ns("types"))
        ),
        tabPanel("Vérifications",
                 h4("Problèmes détectés dans le contenu"),
                 fluidRow(
                   column(4, sliderInput(ns("seuil_na_col"), "Colonne : % de NA max",
                                         min = 10, max = 100, value = 50, step = 5, post = " %")),
                   column(4, sliderInput(ns("seuil_na_ligne"), "Ligne : % de NA max",
                                         min = 10, max = 100, value = 50, step = 5, post = " %")),
                   column(4, sliderInput(ns("seuil_quasi"), "Quasi constante à partir de",
                                         min = 80, max = 100, value = 95, step = 1, post = " %"))
                 ),
                 htmlOutput(ns("bilan")),
                 DTOutput(ns("alertes")),
                 h4("Nombre de valeurs manquantes par ligne"),
                 plotOutput(ns("hist_na_lignes"), height = 280)
        ),
        tabPanel("Variable cible",
                 h4(textOutput(ns("titre_cible"))),
                 uiOutput(ns("alertes_cible")),
                 fluidRow(
                   column(5, tableOutput(ns("tab_cible"))),
                   column(7, plotOutput(ns("plot_cible"), height = 280))
                 )
        )
      )
    )
  )
}

# Logique du module
mod_donnees_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    # Recherche et chargement du fichier de données
    # ----
    data <- eventReactive(input$go, {
      validate(need(input$file1, "Choisissez d'abord un fichier CSV."))
      tryCatch(
        lire_csv(input$file1$datapath,
                 sep      = input$sep,
                 dec      = input$dec,
                 header   = input$header,
                 na_codes = parser_codes_na(input$na_codes)),
        error = function(e) {
          showNotification(paste("Erreur de lecture :", conditionMessage(e)),
                           type = "error", duration = 8)
          NULL
        }
      )
    })

    # Dimensions
    # ----
    output$dimensions <- renderText({
      req(data())
      paste(nrow(data()), "lignes,", ncol(data()), "colonnes,",
            sum(is.na(data())), "valeurs manquantes")
    })

    # Aperçu (table interactive)
    # ----
    output$apercu <- renderDT({
      req(data())
      datatable(data(), options = list(pageLength = 10, scrollX = TRUE))
    })

    # Détection des types
    # ----
    types_detectes <- reactive({
      req(data())
      detecter_types(data(), seuil_modalites = input$seuil_modalites)
    })

    # Nouveau fichier : on remet à jour les listes de correction
    observeEvent(data(), {
      updateSelectInput(session, "forcer_quali",  choices = names(data()), selected = character(0))
      updateSelectInput(session, "forcer_quanti", choices = names(data()), selected = character(0))
    })

    # Types finaux = détection + corrections de l'utilisateur
    # (si une variable est dans les deux listes, "quantitative" l'emporte)
    types_finaux <- reactive({
      t <- types_detectes()
      types <- setNames(t$Type, t$Variable)
      types[input$forcer_quali]  <- "qualitative"
      types[input$forcer_quanti] <- "quantitative"
      types
    })

    # Tableau des types
    # ----
    output$types <- renderDT({
      t <- types_detectes()
      t$Type_final <- types_finaux()[t$Variable]
      t$Corrige    <- ifelse(t$Type_final != t$Type, "oui", "")
      t$A_verifier <- ifelse(t$A_verifier, "à vérifier", "")
      # Les variables à vérifier en premier (puis l'ordre du fichier)
      t <- t[order(t$A_verifier == ""), ]
      datatable(t, rownames = FALSE,
                options = list(pageLength = 25, scrollX = TRUE, dom = "ftp",
                               ordering = FALSE)) |>
        formatStyle("Type_final", fontWeight = "bold") |>
        formatStyle("A_verifier", target = "row",
                    backgroundColor = styleEqual("à vérifier", "#ffe0b2")) |>
        formatStyle("Corrige", target = "row",
                    backgroundColor = styleEqual("oui", "#fff3cd"))
    })

    # Données avec les bons types (numeric / factor)
    # ----
    data_typees <- reactive({
      req(data())
      appliquer_types(data(), types_finaux())
    })

    # Vérifications du contenu
    # ----
    alertes <- reactive({
      verifier_donnees(data(), data_typees(),
                       seuil_na_col         = input$seuil_na_col / 100,
                       seuil_na_ligne       = input$seuil_na_ligne / 100,
                       seuil_quasi_constant = input$seuil_quasi / 100)
    })

    output$bilan <- renderUI({
      a <- alertes()
      if (nrow(a) == 0)
        return(p(style = "color: green;", "Aucun problème détecté."))
      p(style = "color: #b45309;",
        sprintf("%d problème(s) détecté(s) : %d sur des colonnes, %d sur des lignes.",
                nrow(a), sum(a$Niveau == "colonne"), sum(a$Niveau == "lignes")))
    })

    output$alertes <- renderDT({
      datatable(alertes(), rownames = FALSE,
                options = list(pageLength = 15, scrollX = TRUE, dom = "ftp"))
    })

    # Histogramme du nombre de NA par ligne (seuil en rouge)
    output$hist_na_lignes <- renderPlot({
      d <- data_typees()
      d <- d[, colSums(!is.na(d)) > 0, drop = FALSE]   # sans les colonnes vides
      n_na <- rowSums(is.na(d))
      hist(n_na, breaks = seq(-0.5, ncol(d) + 0.5, by = 1), col = "grey",
           main = "", xlab = "Nombre de NA dans la ligne", ylab = "Nombre de lignes", las = 1)
      abline(v = input$seuil_na_ligne / 100 * ncol(d), col = "red", lwd = 2, lty = 2)
    })

    # Variable cible
    # ----
    # Nouveau fichier : la cible par défaut est la dernière colonne
    observeEvent(data(), {
      noms <- names(data())
      updateSelectInput(session, "cible", choices = noms, selected = noms[length(noms)])
    })

    # Données finales : types appliqués + cible en facteur
    data_finales <- reactive({
      req(input$cible %in% names(data_typees()))
      preparer_cible(data_typees(), input$cible)
    })

    output$titre_cible <- renderText({
      req(input$cible)
      paste("Répartition des classes de", input$cible)
    })

    output$alertes_cible <- renderUI({
      v <- verifier_cible(data_finales()[[input$cible]], input$cible, alertes())
      couleurs <- c(erreur = "#c62828", attention = "#b45309", info = "#2e7d32")
      tagList(lapply(seq_len(nrow(v)), function(i)
        p(style = paste0("color:", couleurs[[v$Gravite[i]]], ";"),
          strong(paste0(toupper(v$Gravite[i]), " : ")), v$Message[i])))
    })

    # Tableau statistique et diagramme en barres (comme au cours 1)
    output$tab_cible <- renderTable({
      tableau_cible(data_finales()[[input$cible]])
    })

    output$plot_cible <- renderPlot({
      y <- data_finales()[[input$cible]]
      req(length(unique(y[!is.na(y)])) <= MAX_CLASSES)
      barplot(table(y), col = "steelblue", las = 2,
              main = "", ylab = "Effectifs", xlab = input$cible)
    })

    # Tableau récapitulatif des variables
    # ----
    output$resume <- renderDT({
      r <- resume_variables(data_finales(), alertes(), input$cible)
      r$N_alertes <- NULL
      names(r)[names(r) == "NA_pct"] <- "NA (%)"
      datatable(r, rownames = FALSE,
                options = list(pageLength = 25, scrollX = TRUE, dom = "ftp", ordering = FALSE)) |>
        formatStyle("Role", target = "row", fontWeight = styleEqual("cible", "bold"),
                    backgroundColor = styleEqual("cible", "#e3f2fd")) |>
        formatStyle("Alertes", color = "#b45309")
    })

    # Validation du choix
    # ----
    # Seules les données validées sont transmises aux autres onglets
    valide <- reactiveValues(data = NULL, cible = NULL, alertes = NULL,
                             seuil_na_ligne = 0.5, n = 0)

    observeEvent(input$btn_valider, {
      if (is.null(data())) {
        showNotification("Chargez d'abord un fichier.", type = "error")
        return()
      }
      v <- verifier_cible(data_finales()[[input$cible]], input$cible, alertes())
      if (any(v$Gravite == "erreur")) {
        showNotification(paste("Validation impossible :",
                               paste(v$Message[v$Gravite == "erreur"], collapse = " ")),
                         type = "error", duration = 8)
        return()
      }
      valide$data  <- data_finales()
      valide$cible <- input$cible
      valide$alertes        <- alertes()               # suggestions pour l'onglet 2
      valide$seuil_na_ligne <- input$seuil_na_ligne / 100
      valide$n     <- valide$n + 1          # compteur : sert à changer d'onglet
      showNotification(sprintf("Choix validé : %d lignes, %d colonnes, cible '%s'.",
                               nrow(valide$data), ncol(valide$data), valide$cible),
                       type = "message")
    })

    # Statut affiché sous le bouton
    output$statut <- renderUI({
      if (is.null(valide$data))
        return(p(style = "color: grey; margin-top: 8px;", "Non validé"))
      a_jour <- isTRUE(input$cible == valide$cible) &&
                isTRUE(identical(data_finales(), valide$data))
      if (a_jour)
        p(style = "color: #2e7d32; margin-top: 8px;",
          icon("check"), sprintf(" Validé (cible : %s)", valide$cible))
      else
        p(style = "color: #b45309; margin-top: 8px;",
          icon("triangle-exclamation"), " Modifications non validées")
    })

    # On renvoie aux autres modules : les données validées, la cible validée,
    # les alertes et le seuil de NA (suggestions pour l'onglet 2),
    # et un compteur de validations (pour passer à l'onglet suivant)
    return(list(
      data           = reactive({ req(valide$data);  valide$data }),
      cible          = reactive({ req(valide$cible); valide$cible }),
      alertes        = reactive({ req(valide$alertes); valide$alertes }),
      seuil_na_ligne = reactive(valide$seuil_na_ligne),
      valide         = reactive(valide$n)
    ))
  })
}
