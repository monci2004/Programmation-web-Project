# ==================================================================
# Variable cible (la classe à prédire)
# ==================================================================

# Au-delà de ce nombre de valeurs, une cible numérique est vue comme
# une mesure continue : c'est de la régression, pas de la classification
MAX_CLASSES <- 20

# Tableau statistique de la cible (comme au cours 1)
tableau_cible <- function(y) {
  eff <- table(y, useNA = "no")
  data.frame(Classe          = names(eff),
             Effectifs       = as.integer(eff),
             `Fréquences (%)` = round(100 * as.numeric(eff) / sum(eff), 1),
             check.names = FALSE)
}

# Vérifie que la variable choisie est une cible de classification valable
# - alertes : tableau des vérifications du contenu (pour repérer un identifiant)
# Renvoie un tableau : Gravite ("erreur" / "attention" / "info"), Message
verifier_cible <- function(y, nom, alertes = NULL) {
  msg <- function(gravite, texte) data.frame(Gravite = gravite, Message = texte)
  res <- list()

  valeurs <- y[!is.na(y)]
  n_dist  <- length(unique(valeurs))

  if (n_dist < 2)
    res[[length(res) + 1]] <- msg("erreur", "Une seule classe : impossible de faire de la classification.")

  if (is.numeric(y) && n_dist > MAX_CLASSES)
    res[[length(res) + 1]] <- msg("erreur", sprintf(
      "Variable quantitative avec %d valeurs : c'est une mesure (régression), pas une classe.", n_dist))
  else if (n_dist > 10)
    res[[length(res) + 1]] <- msg("attention", sprintf(
      "%d classes : beaucoup pour de la classification, vérifiez que c'est la bonne variable.", n_dist))

  if (!is.null(alertes) && any(alertes$Cible == nom & alertes$Probleme == "Identifiant probable"))
    res[[length(res) + 1]] <- msg("erreur", "Cette variable ressemble à un identifiant.")

  n_na <- sum(is.na(y))
  if (n_na > 0)
    res[[length(res) + 1]] <- msg("attention", sprintf(
      "%d ligne(s) sans classe (NA) : elles ne pourront pas servir à entraîner les modèles.", n_na))

  # Déséquilibre : part de la classe la plus rare
  if (n_dist >= 2 && n_dist <= MAX_CLASSES) {
    freq <- table(valeurs) / length(valeurs)
    if (min(freq) < 0.2)
      res[[length(res) + 1]] <- msg("attention", sprintf(
        "Classes déséquilibrées : la classe '%s' ne représente que %.1f %% des lignes (rééquilibrage possible en Phase 2).",
        names(which.min(freq)), 100 * min(freq)))
    if (n_dist == 2)
      res[[length(res) + 1]] <- msg("info", "Cible binaire (2 classes).")
  }

  if (length(res) == 0) return(msg(character(0), character(0)))
  do.call(rbind, res)
}

# Prépare la cible pour la classification : toujours un facteur
# (sauf si c'est clairement une mesure continue : on la laisse telle quelle)
preparer_cible <- function(d, nom) {
  y <- d[[nom]]
  if (!is.factor(y) && length(unique(y[!is.na(y)])) <= MAX_CLASSES)
    d[[nom]] <- factor(y)
  d
}
