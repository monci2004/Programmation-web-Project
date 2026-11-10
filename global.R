# ==================================================================
# global.R : exécuté UNE fois au démarrage, avant ui et server
# - chargement des packages (rsconnect les détecte pour le déploiement)
# - chargement des fonctions (R/) puis des modules (modules/)
# On charge R/ nous-mêmes : sinon Shiny le met dans un environnement
# que les modules (chargés ici) ne voient pas
# ==================================================================

# Installation automatique des packages s'ils manquent sur la machine locale
pkgs_requis <- c("shiny", "DT", "ggplot2", "randomForest", "pROC", "Hmisc")
pkgs_manquants <- pkgs_requis[!sapply(pkgs_requis, requireNamespace, quietly = TRUE)]
if (length(pkgs_manquants) > 0) {
  message("Installation automatique des packages manquants : ",
          paste(pkgs_manquants, collapse = ", "))
  install.packages(pkgs_manquants, repos = "https://cloud.r-project.org")
}

library(shiny)
library(DT)
library(ggplot2)
library(randomForest)
library(pROC)

# Fonctions de calcul, puis modules (un fichier par onglet)
for (f in list.files("R", pattern = "\\.R$", full.names = TRUE)) source(f, encoding = "UTF-8")
for (f in list.files("modules", pattern = "\\.R$", full.names = TRUE)) source(f, encoding = "UTF-8")
