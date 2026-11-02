# ==================================================================
# Détection automatique du type des variables
# - "quantitative" : nombres (mesures, comptages)
# - "qualitative"  : catégories (texte, ou nombres qui sont des codes)
# - "vide"         : colonne sans aucune valeur
# Chaque détection est accompagnée d'une raison lisible
# ==================================================================

# Essaie de convertir du texte en nombres ("3,5" -> 3.5)
# Renvoie NA pour les valeurs non convertibles
texte_vers_nombre <- function(x) {
  x <- gsub(",", ".", trimws(x), fixed = TRUE)
  suppressWarnings(as.numeric(x))
}

# Détecte le type d'UNE colonne
# - seuil_modalites : en dessous, des entiers sont considérés comme des codes
# - seuil_conversion : part minimale de texte convertible pour dire "nombres"
detecter_type <- function(x, seuil_modalites = 10, seuil_conversion = 0.95) {
  valeurs <- x[!is.na(x)]
  n_dist  <- length(unique(valeurs))

  # Colonne vide
  if (length(valeurs) == 0)
    return(list(type = "vide", raison = "aucune valeur"))

  # Vrai / faux
  if (is.logical(x))
    return(list(type = "qualitative", raison = "valeurs TRUE / FALSE"))

  # Texte : est-ce en réalité des nombres ?
  if (is.character(x) || is.factor(x)) {
    convertis <- texte_vers_nombre(as.character(valeurs))
    part_ok   <- mean(!is.na(convertis))
    if (part_ok >= seuil_conversion)
      return(list(type = "quantitative",
                  raison = "texte qui contient des nombres (ex. décimale ',')"))
    if (part_ok >= 0.5)
      return(list(type = "qualitative", a_verifier = TRUE,
                  raison = sprintf("colonne mixte : %.0f %% de nombres, le reste du texte",
                                   100 * part_ok)))
    return(list(type = "qualitative", raison = sprintf("texte, %d modalités", n_dist)))
  }

  # Nombres : sont-ce des codes (0/1, 1-5...) ?
  if (is.numeric(x)) {
    entiers  <- all(valeurs == round(valeurs))
    # Des codes se répètent : en moyenne au moins 2 lignes par valeur
    repetes  <- n_dist <= length(valeurs) / 2
    if (n_dist == 2)
      return(list(type = "qualitative", raison = "nombres avec 2 valeurs seulement (binaire)"))
    if (entiers && repetes && n_dist <= seuil_modalites)
      return(list(type = "qualitative", a_verifier = TRUE,
                  raison = sprintf("entiers avec seulement %d valeurs : probablement des codes (à vérifier)",
                                   n_dist)))
    return(list(type = "quantitative",
                raison = sprintf("nombres, %d valeurs distinctes", n_dist)))
  }

  list(type = "qualitative", raison = paste("classe R :", class(x)[1]))
}

# Détecte le type de TOUTES les colonnes : un tableau récapitulatif
detecter_types <- function(d, seuil_modalites = 10) {
  res <- lapply(d, detecter_type, seuil_modalites = seuil_modalites)
  data.frame(
    Variable = names(d),
    Classe_R = sapply(d, function(x) class(x)[1]),
    Distinct = sapply(d, function(x) length(unique(x[!is.na(x)]))),
    Type     = sapply(res, `[[`, "type"),
    A_verifier = sapply(res, function(r) isTRUE(r$a_verifier)),
    Raison   = sapply(res, `[[`, "raison"),
    row.names = NULL, stringsAsFactors = FALSE
  )
}

# Applique les types choisis : quantitative -> numeric, qualitative -> factor
# - types : vecteur nommé, ex. c(age = "quantitative", sexe = "qualitative")
appliquer_types <- function(d, types) {
  for (col in names(types)) {
    x <- d[[col]]
    if (types[[col]] == "quantitative") {
      d[[col]] <- if (is.numeric(x)) x else texte_vers_nombre(as.character(x))
    } else if (types[[col]] == "qualitative") {
      d[[col]] <- factor(x)
    }
  }
  d
}
