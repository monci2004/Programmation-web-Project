# ==================================================================
# Nettoyage : reprend les suggestions des vérifications (onglet 1)
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
