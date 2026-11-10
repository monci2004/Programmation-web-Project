# ==================================================================
# Nettoyage : opérations appliquées dans l'onglet 2 (avec historique)
# - suppression de colonnes / lignes (suggestions de l'onglet 1)
# - traitement des outliers par la règle de l'IQR (garder / supprimer / plafonner)
# - regroupement des modalités rares en "Autre"
# - fusion de colonnes (quantitatives ou qualitatives)
# ==================================================================

# Problèmes de colonnes pour lesquels on propose la suppression
PROBLEMES_COLONNE_A_SUPPRIMER <- c("Colonne vide", "Colonne constante", "Identifiant probable",
                                   "Trop de valeurs manquantes", "Colonne en double")

# Colonnes suggérées pour la suppression
# (seulement celles encore présentes, jamais la cible)
colonnes_suggerees <- function(alertes, noms, cible) {
  a <- alertes[alertes$Niveau == "colonne" & alertes$Probleme %in% PROBLEMES_COLONNE_A_SUPPRIMER, ]
  setdiff(intersect(unique(a$Cible), noms), cible)
}

# Lignes en double (TRUE = à supprimer), en ignorant certaines colonnes (identifiants)
lignes_doublons <- function(d, ignorer = character(0)) {
  duplicated(d[, setdiff(names(d), ignorer), drop = FALSE])
}

# Lignes avec trop de NA (TRUE = à supprimer)
lignes_trop_na <- function(d, seuil = 0.5) {
  na_par_ligne(d) > seuil
}

# Supprime les lignes marquées TRUE
supprimer_lignes <- function(d, a_supprimer) {
  d[!a_supprimer, , drop = FALSE]
}

# ------------------------------------------------------------------
# 1. Outliers (règle de la boîte à moustaches : 1.5 x IQR)
# ------------------------------------------------------------------

# Bornes [moustaches basse, moustache haute] d'une variable numérique
bornes_iqr <- function(x) {
  if (!is.numeric(x) || all(is.na(x))) return(c(NA_real_, NA_real_))
  boxplot.stats(x)$stats[c(1, 5)]
}

# Indique quelles valeurs sont des outliers (FALSE pour les NA)
est_outlier_iqr <- function(x) {
  lim <- bornes_iqr(x)
  if (any(is.na(lim))) return(rep(FALSE, length(x)))
  !is.na(x) & (x < lim[1] | x > lim[2])
}

# Colonnes numériques (hors cible) qui contiennent au moins un outlier
colonnes_avec_outliers <- function(d, cible = NULL) {
  cols_num <- setdiff(names(d)[sapply(d, is.numeric)], cible)
  cols_num[sapply(d[cols_num], function(x) any(est_outlier_iqr(x)))]
}

# Traite les outliers sur les colonnes choisies :
# - "garder"    : ne modifie rien
# - "supprimer" : retire les lignes ayant un outlier dans au moins une des colonnes
# - "plafonner" : ramène les valeurs hors bornes à la borne la plus proche (winsorisation)
traiter_outliers <- function(d, colonnes, methode = "garder") {
  colonnes <- intersect(colonnes, names(d)[sapply(d, is.numeric)])
  if (methode == "garder" || length(colonnes) == 0) return(d)

  if (methode == "supprimer") {
    a_suppr <- Reduce(`|`, lapply(d[colonnes], est_outlier_iqr))
    return(supprimer_lignes(d, a_suppr))
  }

  if (methode == "plafonner") {
    for (col in colonnes) {
      lim <- bornes_iqr(d[[col]])
      if (!any(is.na(lim)))
        d[[col]] <- pmin(pmax(d[[col]], lim[1]), lim[2])
    }
  }
  d
}

# ------------------------------------------------------------------
# 2. Regroupement des modalités rares en "Autre"
# ------------------------------------------------------------------

# Noms des modalités dont la fréquence est strictement inférieure à `seuil`
modalites_rares <- function(x, seuil = 0.05) {
  valeurs <- x[!is.na(x)]
  if (length(valeurs) == 0) return(character(0))
  freq <- prop.table(table(valeurs))
  names(freq[freq < seuil])
}

# Remplace les modalités rares par `label` ("Autre") dans les colonnes choisies
regrouper_modalites_rares <- function(d, colonnes, seuil = 0.05, label = "Autre") {
  colonnes <- intersect(colonnes, names(d)[sapply(d, is.factor)])
  for (col in colonnes) {
    rares <- modalites_rares(d[[col]], seuil)
    if (length(rares) > 0) {
      x_chr <- as.character(d[[col]])
      x_chr[!is.na(x_chr) & x_chr %in% rares] <- label
      # Conserve l'ordre des modalités restantes et place "Autre" à la fin
      niveaux_gardes <- setdiff(levels(d[[col]]), rares)
      d[[col]] <- factor(x_chr, levels = unique(c(niveaux_gardes, label)))
    }
  }
  d
}

# ------------------------------------------------------------------
# 3. Fusion de colonnes
# ------------------------------------------------------------------

# Combine deux colonnes dans une nouvelle colonne :
# - quantitatives : "somme" (+), "moyenne", "difference" (-), "ratio" (/)
# - qualitatives  : "concatenation" (valeurs collées avec "_")
fusionner_colonnes <- function(d, col1, col2, operation, nouveau_nom,
                               supprimer_origine = FALSE, cible = NULL) {
  x1 <- d[[col1]]
  x2 <- d[[col2]]

  nouvelle <- switch(operation,
    "somme"         = x1 + x2,
    "moyenne"       = (x1 + x2) / 2,
    "difference"    = x1 - x2,
    "ratio"         = ifelse(!is.na(x2) & x2 == 0, NA_real_, x1 / x2),
    "concatenation" = {
      res <- ifelse(is.na(x1) | is.na(x2), NA_character_,
                    paste(as.character(x1), as.character(x2), sep = "_"))
      factor(res)
    },
    stop("Opération de fusion inconnue : ", operation)
  )

  d[[nouveau_nom]] <- nouvelle

  if (supprimer_origine) {
    a_retirer <- setdiff(c(col1, col2), c(nouveau_nom, cible))
    d <- supprimer_colonnes(d, a_retirer)
  }
  d
}
