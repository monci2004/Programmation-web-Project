# ==================================================================
# Lecture d'un fichier CSV
# ==================================================================

# Codes proposés par défaut pour les valeurs manquantes
# (les jeux UCI utilisent souvent "?")
NA_CODES_DEFAUT <- c("?", "", "NA", "N/A", "-")

# Transforme "?, NA, -" (saisie utilisateur) en c("?", "NA", "-")
# La chaîne vide "" est toujours ajoutée (cellule vide = manquante)
parser_codes_na <- function(texte) {
  codes <- trimws(unlist(strsplit(texte, ",")))
  unique(c(codes[codes != ""], ""))
}

# Lit un CSV avec les options choisies par l'utilisateur
# - les colonnes texte restent en texte (character) : la détection
#   des types se fera dans une étape séparée
lire_csv <- function(chemin, sep = ",", dec = ".", header = TRUE,
                     na_codes = NA_CODES_DEFAUT) {
  d <- read.csv(chemin, sep = sep, dec = dec, header = header,
                na.strings = na_codes, strip.white = TRUE,
                stringsAsFactors = FALSE, check.names = TRUE,
                fileEncoding = "UTF-8")
  if (ncol(d) < 2)
    stop("Une seule colonne détectée : le séparateur est probablement mauvais.")
  d
}
