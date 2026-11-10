# ==================================================================
# global.R : exécuté UNE fois au démarrage, avant ui et server
# - chargement des packages (rsconnect les détecte pour le déploiement)
# - chargement des fonctions (R/) puis des modules (modules/)
# On charge R/ nous-mêmes : sinon Shiny le met dans un environnement
# que les modules (chargés ici) ne voient pas
# ==================================================================

library(shiny)
library(DT)
library(ggplot2)
library(randomForest)
library(pROC)

# Fonctions de calcul, puis modules (un fichier par onglet)
for (f in list.files("R", pattern = "\\.R$", full.names = TRUE)) source(f)
for (f in list.files("modules", pattern = "\\.R$", full.names = TRUE)) source(f)
