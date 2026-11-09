# ==================================================================
# Historique des traitements
# Une étape = list(etape, detail, params, data)
# - l'étape 1 contient les données de départ
# - les données actuelles sont celles de la dernière étape
# ==================================================================

# Crée un historique à partir des données de départ
hist_nouveau <- function(d) {
  list(list(etape = "Données de départ", detail = "", params = list(), data = d))
}

# Ajoute une étape (avec les données APRÈS l'opération)
hist_ajouter <- function(h, etape, detail, data, params = list()) {
  c(h, list(list(etape = etape, detail = detail, params = params, data = data)))
}

# Données actuelles
hist_courant <- function(h) h[[length(h)]]$data

# Annuler : retire la dernière étape (jamais l'étape de départ)
hist_annuler <- function(h) {
  if (length(h) > 1) h[-length(h)] else h
}

# Revenir à l'étape k : garde les étapes 1 à k, supprime les suivantes
hist_revenir <- function(h, k) {
  k <- max(1, min(k, length(h)))
  h[seq_len(k)]
}

# Réinitialiser : ne garde que les données de départ
hist_reinitialiser <- function(h) h[1]

# Tableau lisible de l'historique (pour l'affichage)
hist_tableau <- function(h) {
  data.frame(
    Etape    = seq_along(h) - 1,
    Operation = sapply(h, `[[`, "etape"),
    Detail   = sapply(h, `[[`, "detail"),
    Lignes   = sapply(h, function(e) nrow(e$data)),
    Colonnes = sapply(h, function(e) ncol(e$data)),
    stringsAsFactors = FALSE
  )
}

# ------------------------------------------------------------------
# Opérations (chacune prend des données et renvoie des données)
# ------------------------------------------------------------------

# Supprime des colonnes
supprimer_colonnes <- function(d, colonnes) {
  d[, setdiff(names(d), colonnes), drop = FALSE]
}
