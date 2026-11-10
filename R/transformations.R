# ==================================================================
# Transformations apprises (configurées dans l'onglet 2, ajustées sur
# l'échantillon d'entraînement uniquement dans l'onglet 4 pour éviter
# toute fuite de données / data leakage) :
# - imputation des valeurs manquantes (moyenne / médiane / mode)
# - normalisation (aucune / z-score / min-max)
# - dummification (encodage one-hot des variables qualitatives)
# - gestion du déséquilibre des classes (oversampling / undersampling / poids)
# ==================================================================

# Configuration par défaut des transformations apprises
config_transformations_defaut <- function() {
  list(
    imputation    = "mediane",      # "aucune", "supprimer", "moyenne", "mediane"
    normalisation = "aucune",       # "aucune", "zscore", "minmax"
    dummification = FALSE,          # TRUE / FALSE
    desequilibre  = "aucun"         # "aucun", "oversampling", "undersampling", "poids"
  )
}

# Tableau récapitulatif lisible de la configuration choisie
resumer_transformations <- function(cfg) {
  lib_imp <- c(
    aucune    = "Aucune (garder les NA)",
    supprimer = "Supprimer les lignes avec NA",
    moyenne   = "Moyenne (quantitatives) + Mode (qualitatives)",
    mediane   = "Médiane (quantitatives) + Mode (qualitatives)"
  )
  lib_norm <- c(
    aucune = "Aucune",
    zscore = "Standardisation Z-score ((x - moy) / sd)",
    minmax = "Min-Max sur [0, 1] ((x - min) / (max - min))"
  )
  lib_deseq <- c(
    aucun         = "Aucun",
    oversampling  = "Sur-échantillonnage (oversampling sur train)",
    undersampling = "Sous-échantillonnage (undersampling sur train)",
    poids         = "Pondération des classes (class weights sur train)"
  )

  data.frame(
    Transformation = c("Imputation des NA", "Normalisation",
                       "Dummification (One-Hot)", "Déséquilibre des classes"),
    Choix = c(
      unname(lib_imp[cfg$imputation]),
      unname(lib_norm[cfg$normalisation]),
      if (isTRUE(cfg$dummification)) "Activée (variables qualitatives -> 0/1)" else "Désactivée",
      unname(lib_deseq[cfg$desequilibre])
    ),
    Application = c(
      if (cfg$imputation %in% c("moyenne", "mediane"))
        "Calculée sur train, appliquée à train & test"
      else "Appliquée avant modélisation",
      if (cfg$normalisation != "aucune")
        "Calculée sur train, appliquée à train & test"
      else "—",
      if (isTRUE(cfg$dummification))
        "Modalités apprises sur train, appliquées à train & test"
      else "—",
      if (cfg$desequilibre != "aucun")
        "Appliqué uniquement sur train"
      else "—"
    ),
    stringsAsFactors = FALSE
  )
}

# ------------------------------------------------------------------
# Fonctions d'apprentissage (fit sur train) et d'application (transform)
# ------------------------------------------------------------------

# Valeur la plus fréquente (mode) d'un vecteur (hors NA)
mode_valeur <- function(x) {
  valeurs <- x[!is.na(x)]
  if (length(valeurs) == 0) return(NA)
  names(which.max(table(valeurs)))
}

# 1) Apprend les paramètres de transformation sur le jeu d'entraînement (train)
apprendre_transformations <- function(train, cible, cfg = config_transformations_defaut()) {
  predicteurs <- setdiff(names(train), cible)
  cols_num    <- predicteurs[sapply(train[predicteurs], is.numeric)]
  cols_qual   <- predicteurs[sapply(train[predicteurs], function(x) is.factor(x) || is.character(x))]

  # A. Paramètres d'imputation appris sur train
  valeurs_imp <- list()
  if (cfg$imputation %in% c("moyenne", "mediane")) {
    for (col in cols_num) {
      x <- train[[col]]
      valeurs_imp[[col]] <- if (cfg$imputation == "moyenne") {
        mean(x, na.rm = TRUE)
      } else {
        median(x, na.rm = TRUE)
      }
    }
    for (col in cols_qual) {
      valeurs_imp[[col]] <- mode_valeur(train[[col]])
    }
  }

  # B. Paramètres de normalisation appris sur train
  bornes_norm <- list()
  if (cfg$normalisation %in% c("zscore", "minmax")) {
    for (col in cols_num) {
      x <- train[[col]]
      if (cfg$normalisation == "zscore") {
        centre  <- mean(x, na.rm = TRUE)
        echelle <- sd(x, na.rm = TRUE)
      } else {
        centre  <- min(x, na.rm = TRUE)
        echelle <- max(x, na.rm = TRUE) - min(x, na.rm = TRUE)
      }
      if (is.na(echelle) || echelle == 0) echelle <- 1
      bornes_norm[[col]] <- list(centre = centre, echelle = echelle)
    }
  }

  # C. Niveaux des facteurs appris sur train (pour la dummification)
  niveaux_qual <- lapply(train[cols_qual], function(x) levels(droplevels(factor(x))))

  list(
    cfg          = cfg,
    cible        = cible,
    cols_num     = cols_num,
    cols_qual    = cols_qual,
    valeurs_imp  = valeurs_imp,
    bornes_norm  = bornes_norm,
    niveaux_qual = niveaux_qual
  )
}

# 2) Applique les paramètres appris (sur train) à un jeu de données (train ou test)
appliquer_transformations <- function(d, params) {
  cfg   <- params$cfg
  cible <- params$cible

  # Aligne d'abord les niveaux des variables qualitatives sur ceux appris dans train
  # (une modalité absente de train devient NA et pourra être imputée par le mode de train)
  for (col in params$cols_qual) {
    if (col %in% names(d)) {
      niv <- params$niveaux_qual[[col]]
      d[[col]] <- factor(as.character(d[[col]]), levels = niv)
    }
  }

  # A. Imputation ou suppression des NA
  if (cfg$imputation == "supprimer") {
    d <- d[complete.cases(d), , drop = FALSE]
  } else if (cfg$imputation %in% c("moyenne", "mediane")) {
    for (col in names(params$valeurs_imp)) {
      if (col %in% names(d) && any(is.na(d[[col]]))) {
        val <- params$valeurs_imp[[col]]
        if (!is.na(val)) d[[col]][is.na(d[[col]])] <- val
      }
    }
  }

  # B. Normalisation des variables quantitatives avec les paramètres de train
  if (cfg$normalisation %in% c("zscore", "minmax")) {
    for (col in names(params$bornes_norm)) {
      if (col %in% names(d)) {
        b <- params$bornes_norm[[col]]
        d[[col]] <- (d[[col]] - b$centre) / b$echelle
      }
    }
  }

  # C. Dummification (encodage One-Hot) des variables qualitatives
  if (isTRUE(cfg$dummification) && length(params$cols_qual) > 0) {
    cols_q <- intersect(params$cols_qual, names(d))
    # On ne dummifie que les facteurs ayant au moins 2 modalités sur train
    cols_q <- cols_q[sapply(params$niveaux_qual[cols_q], length) >= 2]
    if (length(cols_q) > 0) {
      mf <- model.frame(~ ., data = d[cols_q], na.action = na.pass)
      M  <- model.matrix(~ ., data = mf)[, -1, drop = FALSE]
      colnames(M) <- make.names(colnames(M), unique = TRUE)
      autres_cols <- setdiff(names(d), c(cols_q, cible))
      sortie <- cbind(d[autres_cols], as.data.frame(M))
      if (cible %in% names(d)) sortie[[cible]] <- d[[cible]]
      rownames(sortie) <- rownames(d)
      d <- sortie
    }
  }

  d
}

# ------------------------------------------------------------------
# 3) Gestion du déséquilibre des classes (sur train uniquement)
# ------------------------------------------------------------------

# Sur-échantillonnage (oversampling) ou sous-échantillonnage (undersampling) de train
reequilibrer_train <- function(train, cible, methode = "aucun") {
  if (methode %in% c("aucun", "poids")) return(train)

  y <- droplevels(factor(train[[cible]]))
  effectifs <- table(y)
  if (length(effectifs) < 2) return(train)

  taille_cible <- if (methode == "oversampling") max(effectifs) else min(effectifs)

  morceaux <- lapply(names(effectifs), function(cl) {
    lignes <- which(y == cl)
    remplacer <- (methode == "oversampling") && (length(lignes) < taille_cible)
    choix <- if (length(lignes) == taille_cible) {
      lignes
    } else {
      sample(lignes, size = taille_cible, replace = remplacer)
    }
    train[choix, , drop = FALSE]
  })

  res <- do.call(rbind, morceaux)
  # Mélange aléatoire des lignes pour ne pas grouper les classes
  res[sample.int(nrow(res)), , drop = FALSE]
}

# Calcule les poids des classes inversement proportionnels à leur fréquence dans train
# Renvoie list(poids_classes = vecteur nommé par classe, poids_lignes = vecteur par observation)
calculer_poids_classes <- function(train, cible) {
  y <- droplevels(factor(train[[cible]]))
  eff <- table(y)
  k   <- length(eff)
  n   <- sum(eff)
  # Poids équilibrés : n / (k * n_c) -> moyenne des poids = 1
  poids_cl <- setNames(as.numeric(n / (k * eff)), names(eff))
  poids_obs <- poids_cl[as.character(y)]
  list(poids_classes = poids_cl, poids_lignes = as.numeric(poids_obs))
}
