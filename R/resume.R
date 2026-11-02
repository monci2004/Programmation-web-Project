# ==================================================================
# Tableau récapitulatif : une ligne par variable
# Rassemble types, valeurs manquantes, outliers et alertes
# ==================================================================

# Nombre d'outliers selon la règle de la boîte à moustaches (1.5 x IQR)
nb_outliers <- function(x) {
  if (!is.numeric(x) || all(is.na(x))) return(NA_integer_)
  length(boxplot.stats(x)$out)
}

# Petit résumé lisible d'une variable
resumer_valeurs <- function(x) {
  valeurs <- x[!is.na(x)]
  if (length(valeurs) == 0) return("")
  if (is.numeric(x))
    return(sprintf("min %s | médiane %s | max %s",
                   signif(min(valeurs), 4), signif(median(valeurs), 4), signif(max(valeurs), 4)))
  eff <- sort(table(valeurs), decreasing = TRUE)
  sprintf("plus fréquente : %s (%.0f %%)", names(eff)[1], 100 * eff[[1]] / length(valeurs))
}

# Tableau récapitulatif
# - d       : données typées
# - alertes : tableau renvoyé par verifier_donnees()
# - cible   : nom de la variable cible
resume_variables <- function(d, alertes, cible = NULL) {
  r <- data.frame(
    Variable = names(d),
    Role     = ifelse(names(d) %in% cible, "cible", ""),
    Type     = sapply(d, function(x)
                 if (all(is.na(x))) "vide" else if (is.numeric(x)) "quantitative" else "qualitative"),
    Distinct = sapply(d, function(x) length(unique(x[!is.na(x)]))),
    NA_pct   = sapply(d, function(x) round(100 * mean(is.na(x)), 1)),
    Outliers = sapply(d, nb_outliers),
    Resume   = sapply(d, resumer_valeurs),
    row.names = NULL, stringsAsFactors = FALSE
  )

  # Alertes de chaque colonne, regroupées dans une seule cellule
  a <- alertes[alertes$Niveau == "colonne", ]
  r$Alertes  <- sapply(r$Variable, function(v) paste(a$Probleme[a$Cible == v], collapse = " ; "))
  r$N_alertes <- sapply(r$Variable, function(v) sum(a$Cible == v))

  # La cible d'abord, puis les variables avec le plus d'alertes, puis l'ordre du fichier
  r[order(r$Role != "cible", -r$N_alertes, seq_len(nrow(r))), ]
}
