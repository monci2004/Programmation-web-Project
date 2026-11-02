# ==================================================================
# Vérifications du contenu des données
# Chaque problème trouvé = une ligne dans un tableau d'alertes :
#   Niveau (colonne / lignes), Cible, Probleme, Detail, Suggestion
# ==================================================================

# Petite fonction pour créer une alerte
alerte <- function(niveau, cible, probleme, detail, suggestion) {
  data.frame(Niveau = niveau, Cible = cible, Probleme = probleme,
             Detail = detail, Suggestion = suggestion, stringsAsFactors = FALSE)
}

# Valeurs numériques souvent utilisées pour coder un manquant
CODES_SUSPECTS <- c(-999, -99, -9, 999, 9999)

# Pourcentage de NA de chaque ligne
# (les colonnes entièrement vides sont ignorées : sinon toutes les lignes
#  paraîtraient incomplètes à cause d'une seule colonne)
na_par_ligne <- function(d) {
  d <- d[, colSums(!is.na(d)) > 0, drop = FALSE]
  rowSums(is.na(d)) / ncol(d)
}

# ------------------------------------------------------------------
# Vérifications sur UNE colonne
# - x_brut : la colonne telle que lue dans le fichier
# - x      : la colonne après application des types
# ------------------------------------------------------------------
verifier_colonne <- function(nom, x_brut, x, seuil_na_col = 0.5,
                             seuil_quasi_constant = 0.95, max_modalites = 20) {
  n       <- length(x)
  valeurs <- x[!is.na(x)]
  n_val   <- length(valeurs)
  n_dist  <- length(unique(valeurs))
  res     <- list()

  # Colonne vide ou trop de NA
  part_na <- 1 - n_val / n
  if (n_val == 0)
    return(alerte("colonne", nom, "Colonne vide", "aucune valeur", "supprimer la colonne"))
  if (part_na > seuil_na_col)
    res[[length(res) + 1]] <- alerte("colonne", nom, "Trop de valeurs manquantes",
                                     sprintf("%.0f %% de NA", 100 * part_na),
                                     "supprimer la colonne")

  # Constante / quasi constante
  if (n_dist == 1) {
    res[[length(res) + 1]] <- alerte("colonne", nom, "Colonne constante",
                                     paste("une seule valeur :", valeurs[1]),
                                     "supprimer la colonne (n'apporte aucune information)")
  } else {
    part_max <- max(table(valeurs)) / n_val
    if (part_max >= seuil_quasi_constant)
      res[[length(res) + 1]] <- alerte("colonne", nom, "Colonne quasi constante",
                                       sprintf("%.1f %% de la même valeur", 100 * part_max),
                                       "vérifier son utilité")
  }

  # Identifiant : (presque) pas de NA, toutes les valeurs différentes, et
  # du texte, des entiers qui se suivent (1, 2, 3...) ou un nom du type "id"
  if (n_dist == n_val && n_val > 1 && part_na <= 0.05) {
    nom_id   <- grepl("^id$|^id_|_id$|^num|^no$|^code", tolower(nom))
    texte    <- is.character(x_brut) || is.factor(x)
    suite    <- is.numeric(x) && all(valeurs == round(valeurs)) &&
                (max(valeurs) - min(valeurs) + 1) == n_val
    if (texte || suite || nom_id)
      res[[length(res) + 1]] <- alerte("colonne", nom, "Identifiant probable",
                                       "toutes les valeurs sont différentes",
                                       "supprimer la colonne (ne sert pas à prédire)")
  }

  # Colonne mixte : du texte, mais surtout des nombres
  if (is.character(x_brut)) {
    convertis <- texte_vers_nombre(x_brut[!is.na(x_brut)])
    part_num  <- mean(!is.na(convertis))
    if (part_num >= 0.5 && part_num < 1) {
      exemples <- head(unique(x_brut[!is.na(x_brut)][is.na(convertis)]), 3)
      res[[length(res) + 1]] <- alerte("colonne", nom, "Colonne mixte",
                                       sprintf("%.0f %% de nombres ; texte : %s",
                                               100 * part_num, paste(exemples, collapse = ", ")),
                                       "ajouter ces valeurs aux codes NA, ou corriger le fichier")
    }
  }

  # Codes numériques suspects (ex. -999 pour "manquant")
  if (is.numeric(x)) {
    suspects <- intersect(CODES_SUSPECTS, unique(valeurs))
    if (length(suspects) > 0)
      res[[length(res) + 1]] <- alerte("colonne", nom, "Code de manquant possible",
                                       paste("valeur(s) :", paste(suspects, collapse = ", ")),
                                       "ajouter ces valeurs aux codes NA si ce sont des manquants")
  }

  # Trop de modalités pour une variable qualitative
  if (is.factor(x) && n_dist > max_modalites)
    res[[length(res) + 1]] <- alerte("colonne", nom, "Trop de modalités",
                                     paste(n_dist, "modalités"),
                                     "regrouper les modalités rares (dummification très lourde)")

  do.call(rbind, res)
}

# ------------------------------------------------------------------
# Vérifications sur TOUT le jeu de données
# - d_brut : données lues ; d : données typées
# ------------------------------------------------------------------
verifier_donnees <- function(d_brut, d, seuil_na_col = 0.5, seuil_na_ligne = 0.5,
                             seuil_quasi_constant = 0.95, max_modalites = 20) {
  # 1) Colonne par colonne
  res <- lapply(names(d), function(nom)
    verifier_colonne(nom, d_brut[[nom]], d[[nom]], seuil_na_col,
                     seuil_quasi_constant, max_modalites))

  # 2) Colonnes identiques
  noms <- names(d)
  if (length(noms) > 1) {
    for (i in 1:(length(noms) - 1)) for (j in (i + 1):length(noms)) {
      if (identical(d[[i]], d[[j]]))
        res[[length(res) + 1]] <- alerte("colonne", noms[j], "Colonne en double",
                                         paste("identique à", noms[i]),
                                         "supprimer la colonne")
    }
  }

  # 3) Lignes en double (en ignorant les identifiants, toujours différents)
  res <- do.call(rbind, res)
  ids <- if (is.null(res)) character(0) else res$Cible[res$Probleme == "Identifiant probable"]
  res <- list(res)
  n_dup <- sum(duplicated(d[, setdiff(names(d), ids), drop = FALSE]))
  if (n_dup > 0)
    res[[length(res) + 1]] <- alerte("lignes", paste(n_dup, "ligne(s)"), "Lignes en double",
                                     paste0("identiques à une ligne précédente",
                                            if (length(ids)) paste0(" (sans tenir compte de ",
                                                                   paste(ids, collapse = ", "), ")")),
                                     "supprimer les doublons")

  # 4) Lignes avec beaucoup de NA
  p <- na_par_ligne(d)
  n_vides <- sum(p > seuil_na_ligne)
  if (n_vides > 0)
    res[[length(res) + 1]] <- alerte("lignes", paste(n_vides, "ligne(s)"),
                                     "Lignes avec beaucoup de NA",
                                     sprintf("plus de %.0f %% de colonnes manquantes", 100 * seuil_na_ligne),
                                     "supprimer ces lignes plutôt que les imputer")

  res <- do.call(rbind, res)
  if (is.null(res)) res <- alerte(character(0), character(0), character(0),
                                  character(0), character(0))
  res
}
