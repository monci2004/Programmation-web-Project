# ============================================================
# Mini-appli Shiny : exploration + classification
# Packages à installer une seule fois :
# install.packages(c("shiny","DT","ggplot2","randomForest","pROC"))
# Lancer : ouvrir ce fichier dans RStudio puis cliquer "Run App"
# (enregistrer le fichier en encodage UTF-8 pour les accents)
# ============================================================

library(shiny)
library(DT)
library(ggplot2)
library(randomForest)
library(pROC)

# ------------------------------------------------------------
# 1. FONCTIONS DE CALCUL (séparées de l'interface)
# ------------------------------------------------------------

# Jeu de démonstration : contient volontairement des valeurs manquantes,
# des outliers, des variables qualitatives et une cible déséquilibrée
jeu_demo <- function() {
  set.seed(1)
  n <- 300
  age     <- round(rnorm(n, 40, 12))
  revenu  <- round(rlnorm(n, 10, 0.4))
  ville   <- factor(sample(c("Paris", "Lyon", "Lille"), n, replace = TRUE))
  sport   <- factor(sample(c("oui", "non"), n, replace = TRUE))
  score   <- 0.04 * (age - 40) + 0.00003 * (revenu - 24000) +
    0.8 * (ville == "Paris") - 2
  achat   <- factor(ifelse(runif(n) < plogis(score), "oui", "non"))
  d <- data.frame(age, revenu, ville, sport, achat)
  d$age[sample(n, 15)]    <- NA        # valeurs manquantes
  d$revenu[sample(n, 10)] <- NA
  d$revenu[sample(n, 3)]  <- 500000    # outliers
  d
}

colonnes_num <- function(d, cible) {
  setdiff(names(d)[sapply(d, is.numeric)], cible)
}

mode_valeur <- function(x) names(which.max(table(x)))

traiter_na <- function(d, methode) {
  if (methode == "supprimer") return(na.omit(d))
  if (methode == "imputer") {
    for (col in names(d)) {
      if (is.numeric(d[[col]])) {
        d[[col]][is.na(d[[col]])] <- median(d[[col]], na.rm = TRUE)
      } else {
        d[[col]][is.na(d[[col]])] <- mode_valeur(d[[col]])
      }
    }
  }
  d
}

supprimer_outliers <- function(d, cols) {
  for (col in cols) {
    lim <- boxplot.stats(d[[col]])$stats[c(1, 5)]   # règle du boxplot (IQR)
    d <- d[is.na(d[[col]]) | (d[[col]] >= lim[1] & d[[col]] <= lim[2]), ]
  }
  d
}

normaliser <- function(d, cols) {
  if (length(cols) > 0) d[cols] <- as.data.frame(scale(d[cols]))
  d
}

dummifier <- function(d, cible) {
  d <- droplevels(d)
  X <- d[setdiff(names(d), cible)]
  est_facteur <- sapply(X, is.factor)
  if (!any(est_facteur)) return(d)
  mf <- model.frame(~ ., data = X[est_facteur], na.action = na.pass)
  M  <- model.matrix(~ ., data = mf)[, -1, drop = FALSE]   # enlève l'intercept
  colnames(M) <- make.names(colnames(M))
  sortie <- cbind(X[!est_facteur], as.data.frame(M))
  sortie[[cible]] <- d[[cible]]
  sortie
}

sur_echantillonner <- function(train, cible) {
  effectifs <- table(train[[cible]])
  maxi <- max(effectifs)
  morceaux <- lapply(names(effectifs), function(cl) {
    lignes <- which(train[[cible]] == cl)
    train[lignes[sample.int(length(lignes), maxi, replace = TRUE)], ]
  })
  do.call(rbind, morceaux)
}

# Calcule precision, recall, F-score, AUC, ROC et matrice de confusion
evaluer <- function(proba, vrai, lev) {
  pred <- factor(ifelse(proba > 0.5, lev[2], lev[1]), levels = lev)
  tp <- sum(pred == lev[2] & vrai == lev[2])
  fp <- sum(pred == lev[2] & vrai == lev[1])
  fn <- sum(pred == lev[1] & vrai == lev[2])
  precision <- if (tp + fp == 0) NA else tp / (tp + fp)
  recall    <- if (tp + fn == 0) NA else tp / (tp + fn)
  fscore    <- if (is.na(precision) || is.na(recall) || precision + recall == 0) NA else
    2 * precision * recall / (precision + recall)
  r <- roc(vrai, proba, levels = lev, direction = "<", quiet = TRUE)
  list(precision = precision, recall = recall, fscore = fscore,
       auc = as.numeric(auc(r)), roc = r,
       confusion = table(Observé = vrai, Prédit = pred))
}

# ------------------------------------------------------------
# 2. INTERFACE (ce que l'utilisateur voit)
# ------------------------------------------------------------

ui <- fluidPage(
  titlePanel("Mini-appli : exploration et classification"),
  tabsetPanel(
    
    # ---------- Onglet 1 : Données ----------
    tabPanel("1. Données",
             sidebarLayout(
               sidebarPanel(
                 radioButtons("source", "Source des données",
                              c("Exemple intégré" = "demo", "Mon fichier CSV" = "csv")),
                 conditionalPanel("input.source == 'csv'",
                                  fileInput("fichier", "Charger un CSV", accept = ".csv")),
                 selectInput("cible", "Variable à prédire (cible)", choices = NULL),
                 hr(),
                 h4("Traitements"),
                 selectInput("choix_na", "Valeurs manquantes",
                             c("Ne rien faire" = "rien",
                               "Supprimer les lignes" = "supprimer",
                               "Remplacer (médiane / valeur la plus fréquente)" = "imputer")),
                 radioButtons("choix_outliers", "Outliers (variables numériques)",
                              c("Garder" = "garder", "Supprimer" = "supprimer")),
                 checkboxInput("normaliser", "Normaliser les variables numériques", FALSE),
                 checkboxInput("dummifier", "Dummifier les variables qualitatives", FALSE)
               ),
               mainPanel(
                 radioButtons("vue", "Afficher :",
                              c("Données brutes" = "brutes", "Données traitées" = "traitees"),
                              inline = TRUE),
                 textOutput("resume"),
                 textOutput("alerte"),
                 h4("Résumé des variables"),
                 DTOutput("types"),
                 h4("Répartition de la cible"),
                 textOutput("prop_cible"),
                 plotOutput("plot_cible", height = 250),
                 h4("Tableau"),
                 DTOutput("tableau")
               )
             )
    ),
    
    # ---------- Onglet 2 : Exploration ----------
    tabPanel("2. Exploration",
             h3("Analyse d'une variable"),
             selectInput("var1", "Variable", choices = NULL),
             fluidRow(
               column(6, plotOutput("uni_plot", height = 280)),
               column(6, plotOutput("uni_box", height = 280))
             ),
             verbatimTextOutput("uni_resume"),
             hr(),
             h3("Analyse de deux variables"),
             fluidRow(
               column(3, selectInput("var1b", "Variable 1", choices = NULL)),
               column(3, selectInput("var2b", "Variable 2", choices = NULL))
             ),
             plotOutput("bi_plot", height = 350),
             verbatimTextOutput("bi_texte")
    ),
    
    # ---------- Onglet 3 : Modèles ----------
    tabPanel("3. Modèles",
             sidebarLayout(
               sidebarPanel(
                 sliderInput("part_train", "Part des données pour l'entraînement (%)",
                             min = 50, max = 90, value = 70, step = 5),
                 checkboxInput("reequilibrer",
                               "Rééquilibrer les classes (sur-échantillonnage de l'entraînement)", FALSE),
                 actionButton("entrainer", "Entraîner les modèles", class = "btn-primary")
               ),
               mainPanel(
                 h4("Comparaison des modèles"),
                 tableOutput("metriques"),
                 h4("Courbes ROC"),
                 plotOutput("roc", height = 350),
                 h4("Matrices de confusion"),
                 fluidRow(
                   column(6, h5("Régression logistique"), verbatimTextOutput("conf1")),
                   column(6, h5("Forêt aléatoire"), verbatimTextOutput("conf2"))
                 ),
                 h4("Variables les plus importantes"),
                 fluidRow(
                   column(6, h5("Forêt aléatoire"), plotOutput("imp_rf", height = 300)),
                   column(6, h5("Régression logistique (|z|)"), tableOutput("imp_glm"))
                 )
               )
             )
    )
  )
)

# ------------------------------------------------------------
# 3. SERVEUR (ce qui calcule)
# ------------------------------------------------------------

server <- function(input, output, session) {
  
  # --- Chaîne de transformations : chaque étape dépend de la précédente ---
  
  brutes <- reactive({
    if (input$source == "demo") {
      d <- jeu_demo()
    } else {
      req(input$fichier)
      d <- read.csv(input$fichier$datapath, stringsAsFactors = TRUE)
    }
    names(d) <- make.names(names(d), unique = TRUE)
    d
  })
  
  # La cible est toujours transformée en variable qualitative (facteur)
  typees <- reactive({
    d <- brutes()
    req(input$cible %in% names(d))
    d[[input$cible]] <- factor(d[[input$cible]])
    d
  })
  
  sans_na <- reactive(traiter_na(typees(), input$choix_na))
  
  sans_outliers <- reactive({
    d <- sans_na()
    if (input$choix_outliers == "supprimer")
      d <- supprimer_outliers(d, colonnes_num(d, input$cible))
    d
  })
  
  # Utilisé pour l'exploration (avant dummification)
  donnees_explo <- reactive({
    d <- sans_outliers()
    if (input$normaliser) d <- normaliser(d, colonnes_num(d, input$cible))
    d
  })
  
  # Utilisé pour les modèles
  donnees_pretes <- reactive({
    d <- donnees_explo()
    if (input$dummifier) d <- dummifier(d, input$cible)
    d
  })
  
  # --- Menus qui s'adaptent aux colonnes du fichier chargé ---
  observeEvent(brutes(), {
    noms <- names(brutes())
    updateSelectInput(session, "cible", choices = noms, selected = noms[length(noms)])
    updateSelectInput(session, "var1",  choices = noms)
    updateSelectInput(session, "var1b", choices = noms)
    updateSelectInput(session, "var2b", choices = noms, selected = noms[min(2, length(noms))])
  })
  
  # ---------------- Onglet 1 ----------------
  output$resume <- renderText({
    paste(nrow(brutes()), "lignes au départ ;", nrow(donnees_pretes()),
          "lignes et", ncol(donnees_pretes()), "colonnes après traitement.")
  })
  
  output$alerte <- renderText({
    d <- brutes()
    trop <- names(d)[sapply(d, function(x) is.factor(x) && nlevels(x) > 10)]
    if (length(trop) > 0)
      paste("Attention : beaucoup de catégories pour", paste(trop, collapse = ", "),
            "(la dummification créera beaucoup de colonnes).")
    else ""
  })
  
  output$types <- renderDT({
    d <- brutes()
    data.frame(
      Variable   = names(d),
      Type       = ifelse(sapply(d, is.numeric), "quantitative", "qualitative"),
      Categories = sapply(d, function(x) if (is.factor(x)) nlevels(x) else NA),
      Manquantes = sapply(d, function(x) sum(is.na(x))),
      Outliers   = sapply(d, function(x) if (is.numeric(x)) length(boxplot.stats(x)$out) else NA),
      row.names  = NULL
    )
  }, options = list(dom = "t", pageLength = 20))
  
  output$prop_cible <- renderText({
    t <- prop.table(table(typees()[[input$cible]]))
    paste(paste0(names(t), " : ", round(100 * t, 1), " %"), collapse = "  |  ")
  })
  
  output$plot_cible <- renderPlot({
    ggplot(typees(), aes(x = .data[[input$cible]])) +
      geom_bar(fill = "steelblue") +
      labs(x = input$cible, y = "effectif")
  })
  
  output$tableau <- renderDT({
    if (input$vue == "brutes") brutes() else donnees_pretes()
  }, options = list(pageLength = 8, scrollX = TRUE))
  
  # ---------------- Onglet 2 : une variable ----------------
  output$uni_plot <- renderPlot({
    d <- donnees_explo(); req(input$var1 %in% names(d))
    p <- ggplot(d, aes(x = .data[[input$var1]]))
    if (is.numeric(d[[input$var1]])) {
      p + geom_histogram(bins = 30, fill = "steelblue", colour = "white")
    } else {
      p + geom_bar(fill = "steelblue")
    }
  })
  
  output$uni_box <- renderPlot({
    d <- donnees_explo(); req(input$var1 %in% names(d))
    req(is.numeric(d[[input$var1]]))
    ggplot(d, aes(x = "", y = .data[[input$var1]])) +
      geom_boxplot(fill = "lightblue") + coord_flip() + labs(x = "")
  })
  
  output$uni_resume <- renderPrint({
    d <- donnees_explo(); req(input$var1 %in% names(d))
    summary(d[[input$var1]])
  })
  
  # ---------------- Onglet 2 : deux variables ----------------
  analyse_bi <- reactive({
    d <- donnees_explo()
    v1 <- input$var1b; v2 <- input$var2b
    req(v1 %in% names(d), v2 %in% names(d), v1 != v2)
    x <- d[[v1]]; y <- d[[v2]]
    
    if (is.numeric(x) && is.numeric(y)) {
      # quantitative / quantitative
      p <- ggplot(d, aes(x = .data[[v1]], y = .data[[v2]])) +
        geom_point(alpha = 0.6) +
        geom_smooth(method = "lm", formula = y ~ x, se = FALSE)
      txt <- paste0(
        "Corrélation de Pearson  : ", round(cor(x, y, use = "complete.obs"), 3), "\n",
        "Corrélation de Spearman : ", round(cor(x, y, use = "complete.obs", method = "spearman"), 3), "\n",
        "(0 = aucune relation linéaire, +/-1 = relation parfaite)")
      
    } else if (is.numeric(x) || is.numeric(y)) {
      # quantitative / qualitative
      num <- if (is.numeric(x)) v1 else v2
      fac <- if (is.numeric(x)) v2 else v1
      p <- ggplot(d, aes(x = .data[[fac]], y = .data[[num]])) +
        geom_boxplot(fill = "lightblue")
      a <- summary(aov(d[[num]] ~ d[[fac]]))[[1]]
      eta2 <- a[["Sum Sq"]][1] / sum(a[["Sum Sq"]])
      txt <- paste0(
        "Rapport de corrélation eta² : ", round(eta2, 3), "\n",
        "ANOVA, p-value : ", signif(a[["Pr(>F)"]][1], 3), "\n",
        "(eta² : 0 = aucune relation, 1 = la catégorie explique tout)")
      
    } else {
      # qualitative / qualitative
      tab <- table(x, y)
      test <- suppressWarnings(chisq.test(tab))
      v <- sqrt(as.numeric(test$statistic) / (sum(tab) * (min(dim(tab)) - 1)))
      p <- ggplot(d, aes(x = .data[[v1]], fill = .data[[v2]])) +
        geom_bar(position = "fill") + labs(y = "proportion")
      txt <- paste0(
        "Test du khi-deux, p-value : ", signif(test$p.value, 3), "\n",
        "V de Cramér : ", round(v, 3), "\n",
        "(0 = aucune relation, 1 = relation parfaite)")
    }
    list(plot = p, texte = txt)
  })
  
  output$bi_plot  <- renderPlot(analyse_bi()$plot)
  output$bi_texte <- renderText(analyse_bi()$texte)
  
  # ---------------- Onglet 3 : modèles ----------------
  # eventReactive : le calcul ne démarre qu'au clic sur le bouton
  resultats <- eventReactive(input$entrainer, {
    d <- donnees_pretes()
    cible <- input$cible
    d <- d[complete.cases(d), ]            # les modèles n'acceptent pas les NA
    d[[cible]] <- droplevels(d[[cible]])
    lev <- levels(d[[cible]])
    validate(
      need(length(lev) == 2, "La cible doit avoir exactement 2 classes."),
      need(nrow(d) > 20, "Pas assez de lignes pour entraîner les modèles.")
    )
    
    set.seed(42)
    idx   <- sample(nrow(d), round(input$part_train / 100 * nrow(d)))
    train <- d[idx, ]
    test  <- d[-idx, ]
    if (input$reequilibrer) train <- sur_echantillonner(train, cible)
    
    formule <- as.formula(paste0("`", cible, "` ~ ."))
    m_glm <- glm(formule, data = train, family = binomial)
    m_rf  <- randomForest(formule, data = train, importance = TRUE)
    
    p_glm <- predict(m_glm, newdata = test, type = "response")
    p_rf  <- predict(m_rf,  newdata = test, type = "prob")[, lev[2]]
    
    list(lev = lev,
         glm = evaluer(p_glm, test[[cible]], lev),
         rf  = evaluer(p_rf,  test[[cible]], lev),
         m_glm = m_glm, m_rf = m_rf)
  })
  
  output$metriques <- renderTable({
    r <- resultats()
    data.frame(
      Modele    = c("Régression logistique", "Forêt aléatoire"),
      Precision = c(r$glm$precision, r$rf$precision),
      Recall    = c(r$glm$recall,    r$rf$recall),
      Fscore    = c(r$glm$fscore,    r$rf$fscore),
      AUC       = c(r$glm$auc,       r$rf$auc)
    )
  }, digits = 3)
  
  output$roc <- renderPlot({
    r <- resultats()
    ggroc(list("Régression logistique" = r$glm$roc, "Forêt aléatoire" = r$rf$roc),
          legacy.axes = TRUE) +
      geom_abline(linetype = "dashed") +
      labs(x = "Taux de faux positifs", y = "Taux de vrais positifs", colour = "Modèle")
  })
  
  output$conf1 <- renderPrint(resultats()$glm$confusion)
  output$conf2 <- renderPrint(resultats()$rf$confusion)
  
  output$imp_rf <- renderPlot({
    imp <- importance(resultats()$m_rf)[, "MeanDecreaseGini"]
    df <- data.frame(variable = names(imp), importance = as.numeric(imp))
    ggplot(df, aes(x = reorder(variable, importance), y = importance)) +
      geom_col(fill = "steelblue") + coord_flip() + labs(x = "")
  })
  
  output$imp_glm <- renderTable({
    co <- summary(resultats()$m_glm)$coefficients
    df <- data.frame(Variable = rownames(co)[-1],
                     z = round(abs(co[-1, "z value"]), 2))
    df[order(-df$z), ]
  })
}

shinyApp(ui, server)