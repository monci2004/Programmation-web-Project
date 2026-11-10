# ==================================================================
# Modélisation (Phase 4) : fonctions pures de calcul
# 1. Découpage train / test (aléatoire ou stratifié par classe)
# 2. Pipeline sans fuite de données (fit sur train uniquement -> apply sur train & test)
# 3. Entraînement des modèles (Régression logistique, Forêt aléatoire + auto-tune mtry)
# 4. Évaluation (Précision, Rappel, F-score, Exactitude, Spécificité, matrice de confusion, ROC & AUC)
# 5. Importance des variables (|z| pour logistique, MeanDecreaseGini pour Random Forest)
# 6. Historique des expériences de modélisation (ajout, tableau comparatif, suppression, export)
# ==================================================================

# ------------------------------------------------------------------
# 1. Découpage Train / Test (avec option de stratification)
# ------------------------------------------------------------------

# Découpe un jeu de données en train et test
# - prop_train : part d'entraînement entre 0.5 et 0.9 (ex. 0.7)
# - stratifie  : si TRUE, conserve la proportion de chaque classe dans train et test
# - seed       : graine aléatoire pour la reproductibilité
decouper_train_test <- function(d, cible, prop_train = 0.7, stratifie = TRUE, seed = 42) {
  set.seed(seed)
  y <- droplevels(factor(d[[cible]]))

  if (isTRUE(stratifie)) {
    # Tirage séparé à l'intérieur de chaque classe
    idx_train <- unlist(lapply(levels(y), function(cl) {
      lignes_cl <- which(y == cl)
      n_cl      <- length(lignes_cl)
      # Garde au moins 1 observation en train et 1 en test si la classe a >= 2 lignes
      n_tr      <- max(1L, min(n_cl - 1L, round(prop_train * n_cl)))
      if (n_cl <= 1L) return(lignes_cl)
      sample(lignes_cl, size = n_tr)
    }))
    idx_train <- sort(idx_train)
  } else {
    n_tr      <- max(1L, min(nrow(d) - 1L, round(prop_train * nrow(d))))
    idx_train <- sort(sample.int(nrow(d), size = n_tr))
  }

  train <- d[idx_train, , drop = FALSE]
  test  <- d[-idx_train, , drop = FALSE]
  list(train = train, test = test, idx_train = idx_train)
}

# ------------------------------------------------------------------
# 2. Préparation Train / Test sans fuite de données (Data Leakage)
# ------------------------------------------------------------------

# Enchaîne :
#   a) retrait des lignes où la cible est NA
#   b) sélection éventuelle d'un sous-ensemble de variables explicatives
#   c) découpage train / test (stratifié ou non)
#   d) apprentissage des transformations (imputation, normalisation, dummification) SUR TRAIN SEUL
#   e) application de ces transformations à train et à test
#   f) suppression des éventuels NA restants (avec comptage précis pour informer l'utilisateur)
#   g) rééquilibrage (oversampling / undersampling) ou calcul des poids SUR TRAIN SEUL
preparer_train_test <- function(d, cible, variables = NULL,
                                prop_train = 0.7, stratifie = TRUE, seed = 42,
                                cfg = config_transformations_defaut()) {
  # Retirer les lignes où la cible est manquante
  na_cible <- is.na(d[[cible]])
  n_na_cible <- sum(na_cible)
  if (n_na_cible > 0) d <- d[!na_cible, , drop = FALSE]
  d[[cible]] <- droplevels(factor(d[[cible]]))

  # Restreindre aux variables explicatives choisies (+ la cible)
  if (!is.null(variables) && length(variables) > 0) {
    cols_gardees <- unique(c(intersect(variables, names(d)), cible))
    d <- d[, cols_gardees, drop = FALSE]
  }

  # Découpage train / test avant toute transformation apprise
  split <- decouper_train_test(d, cible, prop_train = prop_train,
                               stratifie = stratifie, seed = seed)
  train_brut <- split$train
  test_brut  <- split$test

  # Apprentissage des paramètres (moyenne/médiane/mode, z-score/min-max, modalités) sur TRAIN uniquement
  params_trans <- apprendre_transformations(train_brut, cible, cfg)

  # Application aux deux échantillons
  train_tr <- appliquer_transformations(train_brut, params_trans)
  test_tr  <- appliquer_transformations(test_brut,  params_trans)

  # Les modèles (glm, randomForest) n'acceptent pas les NA : on retire les lignes encore incomplètes
  inc_train <- !complete.cases(train_tr)
  inc_test  <- !complete.cases(test_tr)
  n_na_train <- (nrow(train_brut) - nrow(train_tr)) + sum(inc_train)
  n_na_test  <- (nrow(test_brut)  - nrow(test_tr))  + sum(inc_test)

  if (any(inc_train)) train_tr <- train_tr[!inc_train, , drop = FALSE]
  if (any(inc_test))  test_tr  <- test_tr[!inc_test, , drop = FALSE]

  # Alignement des niveaux de la cible
  niveaux_cible    <- levels(droplevels(factor(d[[cible]])))
  train_tr[[cible]] <- factor(train_tr[[cible]], levels = niveaux_cible)
  test_tr[[cible]]  <- factor(test_tr[[cible]],  levels = niveaux_cible)

  # Gestion du déséquilibre des classes (SUR TRAIN UNIQUEMENT)
  set.seed(seed)
  n_train_avant_reeq <- nrow(train_tr)
  if (cfg$desequilibre %in% c("oversampling", "undersampling")) {
    train_tr <- reequilibrer_train(train_tr, cible, cfg$desequilibre)
  }
  poids <- if (cfg$desequilibre == "poids") {
    calculer_poids_classes(train_tr, cible)
  } else {
    NULL
  }

  list(
    train              = train_tr,
    test               = test_tr,
    cible              = cible,
    niveaux_cible      = niveaux_cible,
    params_trans       = params_trans,
    poids              = poids,
    n_na_cible         = n_na_cible,
    n_na_train         = n_na_train,
    n_na_test          = n_na_test,
    n_train_avant_reeq = n_train_avant_reeq
  )
}

# ------------------------------------------------------------------
# 3. Entraînement des modèles & prédiction des probabilités
# ------------------------------------------------------------------

# Construit la formule du modèle (avec ou sans interactions d'ordre 2)
construire_formule <- function(cible, donnees, interactions = FALSE) {
  rhs <- if (isTRUE(interactions) && (ncol(donnees) - 1) <= 15) "(. )^2" else "."
  as.formula(paste0("`", cible, "` ~ ", rhs))
}

# A. Régression logistique (glm binomial)
# Renvoie le modèle ajusté et les probabilités prédites de `classe_positive` sur `test`
entrainer_logistique <- function(train, test, cible, classe_positive,
                                 interactions = FALSE, poids_lignes = NULL) {
  # Cible binaire 0 / 1 où 1 représente la classe positive choisie par l'utilisateur
  train_glm <- train
  train_glm[[cible]] <- as.integer(train[[cible]] == classe_positive)
  formule <- construire_formule(cible, train_glm, interactions = interactions)
  environment(formule) <- environment()

  args_glm <- list(
    formula = formule,
    data    = train_glm,
    family  = binomial(link = "logit")
  )
  if (!is.null(poids_lignes)) {
    args_glm$weights <- as.numeric(poids_lignes)
  }

  modele <- suppressWarnings(do.call(glm, args_glm))

  proba_test <- as.numeric(suppressWarnings(
    predict(modele, newdata = test, type = "response")
  ))

  list(modele = modele, proba = proba_test, type = "glm")
}

# Recherche automatique du meilleur `mtry` pour la Forêt aléatoire (erreur OOB minimale sur train)
tuner_rf_mtry <- function(train, cible, ntree = 300, nodesize = 1, maxnodes = NULL,
                          poids_classes = NULL, seed = 42) {
  p <- ncol(train) - 1L
  if (p <= 1L) return(1L)
  grille <- unique(pmax(1L, pmin(p, c(
    floor(sqrt(p) / 2),
    floor(sqrt(p)),
    ceiling(sqrt(p) * 1.5),
    floor(p / 3),
    floor(p / 2)
  ))))
  formule <- as.formula(paste0("`", cible, "` ~ ."))

  erreurs_oob <- sapply(grille, function(m) {
    set.seed(seed)
    args_rf <- list(formula = formule, data = train, ntree = ntree,
                    mtry = m, nodesize = nodesize)
    if (!is.null(maxnodes) && !is.na(maxnodes) && maxnodes > 1) args_rf$maxnodes <- maxnodes
    if (!is.null(poids_classes)) args_rf$classwt <- poids_classes
    rf <- do.call(randomForest::randomForest, args_rf)
    rf$err.rate[nrow(rf$err.rate), "OOB"]
  })

  grille[which.min(erreurs_oob)]
}

# B. Forêt aléatoire (randomForest)
# Renvoie le modèle ajusté et les probabilités prédites de `classe_positive` sur `test`
entrainer_rf <- function(train, test, cible, classe_positive,
                         ntree = 500, mtry = NULL, nodesize = 1, maxnodes = NULL,
                         auto_tune = FALSE, poids_classes = NULL, seed = 42) {
  p <- ncol(train) - 1L
  if (isTRUE(auto_tune)) {
    mtry <- tuner_rf_mtry(train, cible, ntree = min(ntree, 300),
                          nodesize = nodesize, maxnodes = maxnodes,
                          poids_classes = poids_classes, seed = seed)
  } else if (is.null(mtry) || is.na(mtry) || mtry < 1) {
    mtry <- max(1L, floor(sqrt(p)))
  } else {
    mtry <- min(as.integer(mtry), p)
  }

  set.seed(seed)
  formule <- as.formula(paste0("`", cible, "` ~ ."))
  args_rf <- list(
    formula    = formule,
    data       = train,
    ntree      = as.integer(ntree),
    mtry       = as.integer(mtry),
    nodesize   = as.integer(nodesize),
    importance = TRUE
  )
  if (!is.null(maxnodes) && !is.na(maxnodes) && maxnodes > 1) {
    args_rf$maxnodes <- as.integer(maxnodes)
  }
  if (!is.null(poids_classes)) {
    args_rf$classwt <- poids_classes
  }

  modele <- do.call(randomForest::randomForest, args_rf)
  mat_proba  <- predict(modele, newdata = test, type = "prob")
  proba_test <- as.numeric(mat_proba[, classe_positive])

  list(modele = modele, proba = proba_test, mtry_utilise = mtry, type = "rf")
}

# ------------------------------------------------------------------
# 4. Évaluation des performances & Courbe ROC
# ------------------------------------------------------------------

# Calcule les points (FPR, TPR) de la courbe ROC pour tout seuil de décision
calculer_courbe_roc <- function(proba, vrai, classe_positive, classe_negative) {
  ok    <- !is.na(proba) & !is.na(vrai)
  proba <- proba[ok]
  vrai  <- vrai[ok]
  n_pos <- sum(vrai == classe_positive)
  n_neg <- sum(vrai == classe_negative)
  if (n_pos == 0 || n_neg == 0) {
    return(data.frame(FPR = c(0, 1), TPR = c(0, 1)))
  }
  seuils <- c(Inf, sort(unique(proba), decreasing = TRUE), -Inf)
  pts <- lapply(seuils, function(s) {
    c(FPR = sum(proba >= s & vrai == classe_negative) / n_neg,
      TPR = sum(proba >= s & vrai == classe_positive) / n_pos)
  })
   as.data.frame(do.call(rbind, pts))
}

# Calcule les métriques de classification binaire par rapport à `classe_positive`
# et au seuil de décision `seuil` (par défaut 0.5)
evaluer_modele <- function(proba, vrai, classe_positive, seuil = 0.5) {
  vrai <- droplevels(factor(vrai))
  niveaux <- levels(vrai)
  classe_negative <- setdiff(niveaux, classe_positive)[1]
  # Ordre de référence : négative d'abord, positive ensuite
  lev <- c(classe_negative, classe_positive)
  vrai_ord <- factor(vrai, levels = lev)

  pred <- factor(ifelse(proba >= seuil, classe_positive, classe_negative), levels = lev)

  tp <- sum(pred == classe_positive & vrai_ord == classe_positive, na.rm = TRUE)
  fp <- sum(pred == classe_positive & vrai_ord == classe_negative, na.rm = TRUE)
  fn <- sum(pred == classe_negative & vrai_ord == classe_positive, na.rm = TRUE)
  tn <- sum(pred == classe_negative & vrai_ord == classe_negative, na.rm = TRUE)

  exactitude  <- (tp + tn) / (tp + tn + fp + fn)
  precision   <- if ((tp + fp) == 0) NA_real_ else tp / (tp + fp)
  rappel      <- if ((tp + fn) == 0) NA_real_ else tp / (tp + fn)
  specificite <- if ((tn + fp) == 0) NA_real_ else tn / (tn + fp)
  fscore      <- if (is.na(precision) || is.na(rappel) || (precision + rappel) == 0) {
    NA_real_
  } else {
    2 * precision * rappel / (precision + rappel)
  }

  # Courbe ROC et AUC (avec pROC, et repli Mann-Whitney si pROC indisponible)
  objet_roc <- tryCatch(
    pROC::roc(response = vrai_ord, predictor = proba,
              levels = lev, direction = "<", quiet = TRUE),
    error = function(e) NULL
  )
  courbe_roc <- calculer_courbe_roc(proba, vrai_ord, classe_positive, classe_negative)
  val_auc <- if (!is.null(objet_roc)) {
    as.numeric(pROC::auc(objet_roc))
  } else {
    # Calcul exact de l'AUC par la statistique de Mann-Whitney-Wilcoxon
    pos <- proba[vrai_ord == classe_positive & !is.na(proba)]
    neg <- proba[vrai_ord == classe_negative & !is.na(proba)]
    if (length(pos) == 0 || length(neg) == 0) {
      NA_real_
    } else {
      rangs <- rank(c(pos, neg))
      (sum(rangs[seq_along(pos)]) - length(pos) * (length(pos) + 1) / 2) /
        (length(pos) * length(neg))
    }
  }

  list(
    exactitude      = exactitude,
    precision       = precision,
    rappel          = rappel,
    specificite     = specificite,
    fscore          = fscore,
    auc             = val_auc,
    roc             = objet_roc,
    courbe_roc      = courbe_roc,
    confusion       = table(Observe = vrai_ord, Predit = pred),
    seuil           = seuil,
    classe_positive = classe_positive,
    classe_negative = classe_negative
  )
}

# ------------------------------------------------------------------
# 5. Importance des variables
# ------------------------------------------------------------------

# Extrait l'importance des variables sous forme d'un data.frame trié par importance décroissante
# - Pour la Régression logistique : Coefficient, Erreur-type, |z| (importance) et p-value
# - Pour la Forêt aléatoire       : MeanDecreaseGini (et MeanDecreaseAccuracy si présent)
importance_variables <- function(modele) {
  if (inherits(modele, "glm")) {
    co <- summary(modele)$coefficients
    if (nrow(co) <= 1) {
      return(data.frame(Variable = character(0), Coefficient = numeric(0),
                        Importance = numeric(0), P_value = numeric(0),
                        stringsAsFactors = FALSE))
    }
    co <- co[-1, , drop = FALSE]   # retire l'intercept
    df <- data.frame(
      Variable    = gsub("`", "", rownames(co), fixed = TRUE),
      Coefficient = round(co[, "Estimate"], 4),
      Importance  = round(abs(co[, "z value"]), 3),
      P_value     = signif(co[, "Pr(>|z|)"], 3),
      row.names   = NULL,
      stringsAsFactors = FALSE
    )
    return(df[order(-df$Importance), , drop = FALSE])
  }

  if (inherits(modele, "randomForest")) {
    imp <- randomForest::importance(modele)
    col_gini <- if ("MeanDecreaseGini" %in% colnames(imp)) "MeanDecreaseGini" else colnames(imp)[ncol(imp)]
    df <- data.frame(
      Variable   = gsub("`", "", rownames(imp), fixed = TRUE),
      Importance = round(as.numeric(imp[, col_gini]), 3),
      row.names  = NULL,
      stringsAsFactors = FALSE
    )
    if ("MeanDecreaseAccuracy" %in% colnames(imp)) {
      df$MeanDecreaseAccuracy <- round(as.numeric(imp[, "MeanDecreaseAccuracy"]), 3)
    }
    return(df[order(-df$Importance), , drop = FALSE])
  }

  data.frame(Variable = character(0), Importance = numeric(0), stringsAsFactors = FALSE)
}

# ------------------------------------------------------------------
# 6. Historique des résultats de modélisation
# ------------------------------------------------------------------

# Résume en une chaîne courte le pipeline de préparation (historique onglet 2 + transformations)
resumer_pipeline_texte <- function(hist_prep = NULL, cfg = config_transformations_defaut()) {
  etapes_net <- if (!is.null(hist_prep) && length(hist_prep) > 1) {
    paste(sapply(hist_prep[-1], `[[`, "etape"), collapse = " -> ")
  } else {
    "Aucun nettoyage"
  }
  trans <- sprintf("NA:%s | Norm:%s | Dummy:%s | Rééq:%s",
                   cfg$imputation, cfg$normalisation,
                   if (isTRUE(cfg$dummification)) "oui" else "non",
                   cfg$desequilibre)
  paste0(etapes_net, " || ", trans)
}

# Crée un historique de modèles vide
hist_modele_nouveau <- function() {
  list()
}

# Ajoute un modèle entraîné et évalué dans l'historique
hist_modele_ajouter <- function(h, modele_nom, parametres_txt, split_txt,
                                pipeline_txt, eval_res, importance_df) {
  prochain_id <- if (length(h) == 0) 1L else max(sapply(h, `[[`, "id")) + 1L
  entree <- list(
    id              = prochain_id,
    heure           = format(Sys.time(), "%H:%M:%S"),
    modele          = modele_nom,
    classe_positive = eval_res$classe_positive,
    seuil           = eval_res$seuil,
    split           = split_txt,
    parametres      = parametres_txt,
    pretraitement   = pipeline_txt,
    exactitude      = eval_res$exactitude,
    precision       = eval_res$precision,
    rappel          = eval_res$rappel,
    specificite     = eval_res$specificite,
    fscore          = eval_res$fscore,
    auc             = eval_res$auc,
    confusion       = eval_res$confusion,
    roc             = eval_res$roc,
    courbe_roc      = eval_res$courbe_roc,
    importance      = importance_df
  )
  c(h, list(entree))
}

# Supprime une ou plusieurs entrées de l'historique par leur identifiant `id`
hist_modele_supprimer <- function(h, ids_a_supprimer) {
  Filter(function(e) !(e$id %in% ids_a_supprimer), h)
}

# Vide complètement l'historique des modèles
hist_modele_vider <- function() {
  list()
}

# Convertit l'historique des modèles en un data.frame comparatif (pour DT et export CSV)
hist_modele_tableau <- function(h) {
  if (length(h) == 0) {
    return(data.frame(
      Run = integer(0), Heure = character(0), Modele = character(0),
      Cible_Pos = character(0), Seuil = numeric(0), Split = character(0),
      Parametres = character(0), Pretraitement = character(0),
      Exactitude = numeric(0), Precision = numeric(0), Rappel = numeric(0),
      F_score = numeric(0), AUC = numeric(0),
      stringsAsFactors = FALSE
    ))
  }

  data.frame(
    Run           = sapply(h, `[[`, "id"),
    Heure         = sapply(h, `[[`, "heure"),
    Modele        = sapply(h, `[[`, "modele"),
    Cible_Pos     = sapply(h, `[[`, "classe_positive"),
    Seuil         = sapply(h, `[[`, "seuil"),
    Split         = sapply(h, `[[`, "split"),
    Parametres    = sapply(h, `[[`, "parametres"),
    Pretraitement = sapply(h, `[[`, "pretraitement"),
    Exactitude    = round(sapply(h, `[[`, "exactitude"), 3),
    Precision     = round(sapply(h, `[[`, "precision"),  3),
    Rappel        = round(sapply(h, `[[`, "rappel"),     3),
    F_score       = round(sapply(h, `[[`, "fscore"),     3),
    AUC           = round(sapply(h, `[[`, "auc"),        3),
    stringsAsFactors = FALSE
  )
}

# ------------------------------------------------------------------
# 7. Graphiques (Courbes ROC superposées & Importance des variables)
# ------------------------------------------------------------------

# Trace sur un même graphique les courbes ROC d'une liste d'entrées de l'historique
tracer_courbes_roc <- function(entrees) {
  entrees <- Filter(function(e) !is.null(e$roc) || !is.null(e$courbe_roc), entrees)
  if (length(entrees) == 0) return(NULL)

  # Si pROC et ggplot2 sont disponibles et que tous les objets `roc` existent : ggroc
  tous_proc <- all(sapply(entrees, function(e) !is.null(e$roc)))
  if (tous_proc && requireNamespace("pROC", quietly = TRUE) &&
      requireNamespace("ggplot2", quietly = TRUE)) {
    noms <- sapply(entrees, function(e) {
      sprintf("Run #%d - %s (AUC = %.3f)", e$id, e$modele, e$auc)
    })
    liste_roc <- setNames(lapply(entrees, `[[`, "roc"), noms)
    return(
      pROC::ggroc(liste_roc, legacy.axes = TRUE, linewidth = 1) +
        ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey50") +
        ggplot2::labs(
          x      = "Taux de faux positifs (1 - Spécificité)",
          y      = "Taux de vrais positifs (Sensibilité / Rappel)",
          colour = "Modèle (Run)"
        ) +
        ggplot2::theme_minimal(base_size = 12) +
        ggplot2::theme(legend.position = "bottom", legend.direction = "vertical")
    )
  }

  # Sinon : tracé ggplot2 à partir des points (FPR, TPR) calculés dans `courbe_roc`
  df_plot <- do.call(rbind, lapply(entrees, function(e) {
    pts <- e$courbe_roc
    pts$Modele <- sprintf("Run #%d - %s (AUC = %.3f)", e$id, e$modele, e$auc)
    pts
  }))
  ggplot2::ggplot(df_plot, ggplot2::aes(x = FPR, y = TPR, colour = Modele)) +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey50") +
    ggplot2::labs(
      x      = "Taux de faux positifs (1 - Spécificité)",
      y      = "Taux de vrais positifs (Sensibilité / Rappel)",
      colour = "Modèle (Run)"
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(legend.position = "bottom", legend.direction = "vertical")
}

# Trace le diagramme en barres horizontales de l'importance des variables (top N)
tracer_importance <- function(df_imp, lab_y = "Importance", top_n = 15) {
  if (is.null(df_imp) || nrow(df_imp) == 0) return(NULL)
  df <- head(df_imp[order(-df_imp$Importance), , drop = FALSE], top_n)

  ggplot2::ggplot(df, ggplot2::aes(x = stats::reorder(Variable, Importance), y = Importance)) +
    ggplot2::geom_col(fill = "steelblue") +
    ggplot2::coord_flip() +
    ggplot2::labs(x = "", y = lab_y) +
    ggplot2::theme_minimal(base_size = 12)
}
