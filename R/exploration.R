# ==================================================================
# Analyse exploratoire (Phase 3) : fonctions de calcul et graphiques
# Reprend les méthodes vues en cours 1 (unidimensionnel) et cours 2 (bidimensionnel)
# 1. Analyse unidimensionnelle :
#    - Tableau statistique : effectifs, effectifs cumulés, fréquences, fréquences cumulées
#    - Quantitative : indicateurs (centre & dispersion), histogramme (Sturges / Scott / FD,
#      effectifs ou densité, courbe de densité, lignes de quantiles), boxplot + rug, ECDF
#    - Qualitative  : diagramme en barres, diagramme circulaire (pie chart), tableau de fréquences
# 2. Analyse bidimensionnelle :
#    - Quant × Quant : nuage de points + droite de régression lm, histogrammes dos à dos
#                      (Hmisc::histbackback), covariance, corrélation de Pearson r et Spearman
#    - Quant × Qual  : boxplots côte à côte (base R et ggplot + jitter), décomposition de la
#                      variance (inter / intra) et rapport de corrélation η² et η
#    - Qual  × Qual  : table de contingence (avec marges, profils lignes/colonnes), diagrammes
#                      en barres (empilés, côte à côte, profils position="fill"), χ², Φ², V de Cramér
# 3. Matrice de corrélation (heatmap) de toutes les variables quantitatives
# ==================================================================

# ==================================================================
# PARTIE 1 : ANALYSE UNIDIMENSIONNELLE (COURS 1)
# ==================================================================

# Tableau statistique complet : Effectifs (ni), Effectifs cumulés (Ni),
# Fréquences % (fi) et Fréquences cumulées % (Fi)
# - Pour une variable qualitative (ou numérique discrète <= 12 valeurs) : par modalité
# - Pour une variable quantitative continue : découpage en classes (règle choisie)
tableau_statistique_uni <- function(x, regle_bins = "Sturges") {
  valeurs <- x[!is.na(x)]
  if (length(valeurs) == 0) {
    return(data.frame(Modalite = character(0), Effectif = integer(0),
                      Effectif_cumule = integer(0), Frequence_pct = numeric(0),
                      Frequence_cumulee_pct = numeric(0), stringsAsFactors = FALSE))
  }

  if (is.numeric(valeurs) && length(unique(valeurs)) > 12) {
    # Découpage en intervalles selon la règle d'histogramme choisie
    h <- graphics::hist(valeurs, breaks = regle_bins, plot = FALSE)
    classes <- cut(valeurs, breaks = h$breaks, include.lowest = TRUE, dig.lab = 4)
    eff <- table(classes)
  } else {
    eff <- table(valeurs)
  }

  ni <- as.integer(eff)
  Ni <- cumsum(ni)
  fi <- round(100 * ni / sum(ni), 2)
  Fi <- round(100 * Ni / sum(ni), 2)

  data.frame(
    `Modalité / Classe`      = names(eff),
    `Effectif (ni)`          = ni,
    `Effectif cumulé (Ni)`   = Ni,
    `Fréquence (%)`          = fi,
    `Fréquence cumulée (%)`  = Fi,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

# Indicateurs de position et de dispersion d'une variable quantitative
indicateurs_quanti <- function(x) {
  val <- x[!is.na(x)]
  n_na <- sum(is.na(x))
  if (length(val) == 0) {
    return(data.frame(Indicateur = "Aucune valeur", Valeur = "—", stringsAsFactors = FALSE))
  }
  q   <- stats::quantile(val, probs = c(0.25, 0.5, 0.75), names = FALSE)
  moy <- mean(val)
  v   <- if (length(val) > 1) stats::var(val) else 0
  ec  <- sqrt(v)
  cv  <- if (moy != 0) round(100 * ec / abs(moy), 2) else NA_real_

  data.frame(
    Indicateur = c(
      "Effectif valide (n)", "Valeurs manquantes (NA)",
      "Minimum", "1er quartile (Q1 - 25%)", "Médiane (Q2 - 50%)",
      "Moyenne", "3e quartile (Q3 - 75%)", "Maximum",
      "Étendue (Max - Min)", "Écart interquartile (IQR = Q3 - Q1)",
      "Variance (s²)", "Écart-type (s)", "Coefficient de variation (%)"
    ),
    Valeur = c(
      as.character(length(val)), as.character(n_na),
      signif(min(val), 5), signif(q[1], 5), signif(q[2], 5),
      signif(moy, 5), signif(q[3], 5), signif(max(val), 5),
      signif(max(val) - min(val), 5), signif(q[3] - q[1], 5),
      signif(v, 5), signif(ec, 5),
      if (is.na(cv)) "—" else paste0(cv, " %")
    ),
    stringsAsFactors = FALSE
  )
}

# 1.1 Histogramme (règle Sturges / Scott / FD, effectifs ou densité, courbe de densité, quantiles)
tracer_uni_histogramme <- function(x, nom, regle_bins = "Sturges",
                                   en_densite = TRUE, afficher_densite = TRUE,
                                   afficher_quantiles = TRUE) {
  val <- x[!is.na(x)]
  if (length(val) < 2 || length(unique(val)) < 2) {
    graphics::plot.new()
    graphics::title(main = paste("Trop peu de valeurs distinctes pour", nom))
    return(invisible(NULL))
  }

  graphics::hist(
    val,
    breaks = regle_bins,
    freq   = !en_densite,
    col    = "lightblue",
    border = "white",
    main   = sprintf("Histogramme de %s (règle : %s)", nom, regle_bins),
    xlab   = nom,
    ylab   = if (en_densite) "Densité" else "Effectifs",
    las    = 1
  )

  # Courbe de densité empirique (uniquement si l'axe Y est en densité)
  if (en_densite && afficher_densite && length(val) >= 3) {
    graphics::lines(stats::density(val), col = "#dc2626", lwd = 2)
  }

  # Lignes verticales des quantiles (Q1, Médiane, Q3) et de la moyenne
  if (afficher_quantiles) {
    q <- stats::quantile(val, probs = c(0.25, 0.5, 0.75))
    m <- mean(val)
    graphics::abline(v = q, col = "#1d4ed8", lwd = 2, lty = 2)
    graphics::abline(v = m, col = "#15803d", lwd = 2, lty = 1)
    graphics::legend(
      "topright",
      legend = c("Q1 / Médiane / Q3", "Moyenne", if (en_densite && afficher_densite) "Densité"),
      col    = c("#1d4ed8", "#15803d", if (en_densite && afficher_densite) "#dc2626"),
      lty    = c(2, 1, if (en_densite && afficher_densite) 1),
      lwd    = 2,
      bty    = "n",
      cex    = 0.85
    )
  }
}

# 1.2 Boîte à moustaches horizontale + rug (comme au cours 1)
tracer_uni_boxplot <- function(x, nom, afficher_rug = TRUE) {
  val <- x[!is.na(x)]
  if (length(val) == 0) return(invisible(NULL))
  graphics::boxplot(
    val,
    horizontal = TRUE,
    col        = "lightblue",
    main       = paste("Boîte à moustaches de", nom),
    xlab       = nom,
    las        = 1
  )
  if (afficher_rug) {
    graphics::rug(val, col = "#1e3a8a", ticksize = 0.04)
  }
}

# 1.3 Fonction de répartition empirique (ECDF)
tracer_uni_ecdf <- function(x, nom) {
  val <- x[!is.na(x)]
  if (length(val) == 0) return(invisible(NULL))
  graphics::plot(
    stats::ecdf(val),
    main = paste("Fonction de répartition empirique (ECDF) —", nom),
    xlab = nom,
    ylab = "Fréquence cumulée F(x)",
    col  = "#2563eb",
    lwd  = 2,
    las  = 1
  )
  graphics::abline(h = c(0.25, 0.5, 0.75), col = "grey60", lty = 3)
}

# 1.4 Diagramme en barres pour une variable qualitative
tracer_uni_barres <- function(x, nom, en_frequence = FALSE) {
  val <- x[!is.na(x)]
  if (length(val) == 0) return(invisible(NULL))
  eff <- table(val)
  hauteurs <- if (en_frequence) round(100 * prop.table(eff), 1) else as.numeric(eff)
  bp <- graphics::barplot(
    hauteurs,
    names.arg = names(eff),
    col       = "steelblue",
    border    = "white",
    main      = paste("Diagramme en barres —", nom),
    ylab      = if (en_frequence) "Fréquence (%)" else "Effectifs",
    ylim      = c(0, max(hauteurs) * 1.15),
    las       = 2
  )
  graphics::text(
    x      = bp,
    y      = hauteurs,
    labels = if (en_frequence) paste0(hauteurs, "%") else hauteurs,
    pos    = 3,
    cex    = 0.85
  )
}

# 1.5 Diagramme circulaire (camembert / pie chart) pour une variable qualitative
tracer_uni_camembert <- function(x, nom) {
  val <- x[!is.na(x)]
  if (length(val) == 0) return(invisible(NULL))
  eff <- sort(table(val), decreasing = TRUE)
  # Si plus de 8 modalités, regroupe les petites dans l'affichage du camembert pour rester lisible
  if (length(eff) > 8) {
    gardes <- eff[1:7]
    reste  <- sum(eff[8:length(eff)])
    eff    <- c(gardes, `Autres modalités` = reste)
  }
  pct <- round(100 * eff / sum(eff), 1)
  etiquettes <- sprintf("%s (%s %%)", names(eff), pct)
  couleurs <- grDevices::hcl.colors(length(eff), palette = "Pastel 1")
  graphics::pie(eff, labels = etiquettes, col = couleurs,
                main = paste("Diagramme circulaire —", nom))
}

# ==================================================================
# PARTIE 2 : ANALYSE BIDIMENSIONNELLE (COURS 2)
# ==================================================================

# Détecte le type de croisement entre deux variables
type_croisement <- function(x, y) {
  if (is.numeric(x) && is.numeric(y)) return("quanti_quanti")
  if (is.numeric(x) || is.numeric(y)) return("quanti_quali")
  "quali_quali"
}

# ------------------------------------------------------------------
# 2.A Quantitatif × Quantitatif
# ------------------------------------------------------------------

# Indicateurs de liaison entre deux variables quantitatives
indicateurs_quanti_quanti <- function(x, y, nom_x, nom_y) {
  ok <- !is.na(x) & !is.na(y)
  x  <- x[ok]; y <- y[ok]
  if (length(x) < 3) {
    return(data.frame(Indicateur = "Observations complètes", Valeur = as.character(length(x)),
                      stringsAsFactors = FALSE))
  }
  cov_xy <- stats::cov(x, y)
  r_pear <- stats::cor(x, y, method = "pearson")
  r_spea <- stats::cor(x, y, method = "spearman")
  mod_lm <- stats::lm(y ~ x)
  coefs  <- stats::coef(mod_lm)
  p_val  <- summary(mod_lm)$coefficients[2, "Pr(>|t|)"]

  force_r <- if (is.na(r_pear)) "—" else if (abs(r_pear) >= 0.7) {
    "Forte liaison linéaire"
  } else if (abs(r_pear) >= 0.3) {
    "Liaison linéaire modérée"
  } else {
    "Liaison linéaire faible ou nulle"
  }

  data.frame(
    Indicateur = c(
      "Paires complètes (n)",
      "Covariance cov(X, Y)",
      "Corrélation linéaire de Pearson (r)",
      "Coefficient de détermination (R² = r²)",
      "Corrélation des rangs de Spearman (ρ)",
      paste0("Droite de régression (", nom_y, " ~ ", nom_x, ")"),
      "p-value de la pente (test t)",
      "Interprétation"
    ),
    Valeur = c(
      as.character(length(x)),
      signif(cov_xy, 4),
      round(r_pear, 4),
      paste0(round(100 * r_pear^2, 2), " %"),
      round(r_spea, 4),
      sprintf("%s = %.4g × %s %s %.4g", nom_y, coefs[2], nom_x,
              if (coefs[1] >= 0) "+" else "-", abs(coefs[1])),
      signif(p_val, 3),
      force_r
    ),
    stringsAsFactors = FALSE
  )
}

# Nuage de points + droite de régression linéaire `lm`
tracer_bi_nuage <- function(d, nom_x, nom_y, afficher_lm = TRUE) {
  p <- ggplot2::ggplot(d, ggplot2::aes(x = .data[[nom_x]], y = .data[[nom_y]])) +
    ggplot2::geom_point(alpha = 0.65, color = "#1e40af", size = 2) +
    ggplot2::labs(
      title = sprintf("Nuage de points : %s en fonction de %s", nom_y, nom_x),
      x     = nom_x,
      y     = nom_y
    ) +
    ggplot2::theme_minimal(base_size = 12)

  if (afficher_lm) {
    p <- p + ggplot2::geom_smooth(method = "lm", formula = y ~ x,
                                  color = "#dc2626", fill = "#fecaca", se = TRUE)
  }
  p
}

# Histogrammes dos à dos (Hmisc::histbackback comme au cours 2, avec repli en base R)
tracer_bi_histbackback <- function(x, y, nom_x, nom_y) {
  x <- x[!is.na(x)]
  y <- y[!is.na(y)]
  if (length(x) < 2 || length(y) < 2) return(invisible(NULL))

  if (requireNamespace("Hmisc", quietly = TRUE)) {
    Hmisc::histbackback(
      x, y,
      probability = TRUE,
      xlab        = c(nom_x, nom_y),
      main        = sprintf("Histogrammes dos à dos : %s vs %s", nom_x, nom_y)
    )
  } else {
    old_par <- graphics::par(mfrow = c(1, 2))
    on.exit(graphics::par(old_par))
    graphics::hist(x, probability = TRUE, col = "lightblue", main = nom_x, xlab = nom_x)
    graphics::hist(y, probability = TRUE, col = "lightgreen", main = nom_y, xlab = nom_y)
  }
}

# ------------------------------------------------------------------
# 2.B Quantitatif × Qualitatif
# ------------------------------------------------------------------

# Décomposition de la variance (intra / inter) et rapport de corrélation η² et η (cours 2)
indicateurs_quanti_quali <- function(num, fac, nom_num, nom_fac) {
  ok  <- !is.na(num) & !is.na(fac)
  num <- num[ok]
  fac <- droplevels(factor(fac[ok]))
  n   <- length(num)

  if (n < 3 || nlevels(fac) < 2) {
    return(list(
      resume_groupes = data.frame(),
      liaison = data.frame(Indicateur = "Données insuffisantes", Valeur = "—")
    ))
  }

  # Tableau par groupe (effectif, moyenne, écart-type, médiane, IQR)
  groupes <- split(num, fac)
  tab_grp <- do.call(rbind, lapply(names(groupes), function(g) {
    v <- groupes[[g]]
    data.frame(
      Modalite   = g,
      Effectif   = length(v),
      Moyenne    = round(mean(v), 3),
      Ecart_type = if (length(v) > 1) round(stats::sd(v), 3) else NA_real_,
      Mediane    = round(stats::median(v), 3),
      IQR        = round(stats::IQR(v), 3),
      stringsAsFactors = FALSE
    )
  }))
  names(tab_grp)[1] <- nom_fac

  # Variance totale, variance intra-classe et variance inter-classe (formules du cours 2)
  moy_tot <- mean(num)
  n_k     <- sapply(groupes, length)
  moy_k   <- sapply(groupes, mean)
  # Variances empiriques non corrigées (1/n) pour l'égalité exacte V_tot = V_intra + V_inter
  var_k      <- sapply(groupes, function(v) mean((v - mean(v))^2))
  v_intra    <- sum(n_k * var_k) / n
  v_inter    <- sum(n_k * (moy_k - moy_tot)^2) / n
  v_totale   <- v_intra + v_inter
  eta2       <- if (v_totale > 0) v_inter / v_totale else 0
  eta        <- sqrt(eta2)

  p_aov <- tryCatch(
    summary(stats::aov(num ~ fac))[[1]][["Pr(>F)"]][1],
    error = function(e) NA_real_
  )

  interp <- if (eta2 >= 0.14) {
    "Effet fort de la catégorie sur la variable numérique"
  } else if (eta2 >= 0.06) {
    "Effet modéré"
  } else {
    "Effet faible ou nul"
  }

  liaison <- data.frame(
    Indicateur = c(
      "Observations complètes (n)",
      "Variance intra-groupes (V_intra)",
      "Variance inter-groupes (V_inter)",
      "Variance totale (V_totale = V_intra + V_inter)",
      "Rapport de corrélation η² (V_inter / V_totale)",
      "Rapport de corrélation η = √(η²)",
      "ANOVA (test F, p-value)",
      "Interprétation"
    ),
    Valeur = c(
      as.character(n),
      signif(v_intra, 4),
      signif(v_inter, 4),
      signif(v_totale, 4),
      sprintf("%.4f (%.1f %% de variance expliquée)", eta2, 100 * eta2),
      round(eta, 4),
      if (is.na(p_aov)) "—" else signif(p_aov, 3),
      interp
    ),
    stringsAsFactors = FALSE
  )

  list(resume_groupes = tab_grp, liaison = liaison)
}

# Boxplots côte à côte (ggplot2 avec jitter + moyenne, ou base R)
tracer_bi_boxplots <- function(d, nom_num, nom_fac, style = "ggplot_jitter") {
  d_propre <- d[!is.na(d[[nom_num]]) & !is.na(d[[nom_fac]]), , drop = FALSE]
  d_propre[[nom_fac]] <- droplevels(factor(d_propre[[nom_fac]]))

  if (style == "base") {
    couleurs <- grDevices::hcl.colors(nlevels(d_propre[[nom_fac]]), palette = "Pastel 1")
    graphics::boxplot(
      stats::as.formula(paste0("`", nom_num, "` ~ `", nom_fac, "`")),
      data = d_propre,
      col  = couleurs,
      main = sprintf("Boîtes à moustaches de %s selon %s", nom_num, nom_fac),
      xlab = nom_fac,
      ylab = nom_num,
      las  = 1
    )
    return(invisible(NULL))
  }

  ggplot2::ggplot(d_propre, ggplot2::aes(x = .data[[nom_fac]], y = .data[[nom_num]],
                                         fill = .data[[nom_fac]])) +
    ggplot2::geom_boxplot(alpha = 0.7, outlier.shape = 16) +
    ggplot2::geom_jitter(width = 0.15, alpha = 0.45, size = 1.6, color = "#1e293b") +
    ggplot2::stat_summary(fun = mean, geom = "point", shape = 18, size = 3.5, color = "#dc2626") +
    ggplot2::labs(
      title    = sprintf("Distribution de %s par modalité de %s", nom_num, nom_fac),
      subtitle = "Losange rouge = moyenne du groupe | Points = observations (jitter)",
      x        = nom_fac,
      y        = nom_num,
      fill     = nom_fac
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(legend.position = "none")
}

# ------------------------------------------------------------------
# 2.C Qualitatif × Qualitatif
# ------------------------------------------------------------------

# Table de contingence (effectifs, profils lignes %, profils colonnes %)
tableaux_quali_quali <- function(x, y, mode_table = "effectifs") {
  tab <- table(x, y, useNA = "no")
  if (nrow(tab) == 0 || ncol(tab) == 0) return(data.frame())

  mat <- switch(mode_table,
    "effectifs" = stats::addmargins(tab),
    "lignes"    = round(100 * stats::addmargins(prop.table(tab, margin = 1), margin = 2), 1),
    "colonnes"  = round(100 * stats::addmargins(prop.table(tab, margin = 2), margin = 1), 1),
    "total"     = round(100 * stats::addmargins(prop.table(tab)), 1)
  )
  df <- as.data.frame.matrix(mat)
  df <- cbind(`Modalité` = rownames(df), df, stringsAsFactors = FALSE)
  rownames(df) <- NULL
  df
}

# Indicateurs d'association entre deux variables qualitatives : χ², ddl, p-value, Φ², V de Cramér
indicateurs_quali_quali <- function(x, y) {
  tab <- table(x, y, useNA = "no")
  # Retire les niveaux vides
  tab <- tab[rowSums(tab) > 0, colSums(tab) > 0, drop = FALSE]
  n   <- sum(tab)
  if (n < 3 || nrow(tab) < 2 || ncol(tab) < 2) {
    return(data.frame(Indicateur = "Données insuffisantes", Valeur = "—",
                      stringsAsFactors = FALSE))
  }

  test_chi <- suppressWarnings(stats::chisq.test(tab, correct = FALSE))
  chi2     <- as.numeric(test_chi$statistic)
  ddl      <- as.numeric(test_chi$parameter)
  p_val    <- as.numeric(test_chi$p.value)
  phi2     <- chi2 / n
  k        <- min(nrow(tab), ncol(tab)) - 1
  cramer_v <- if (k > 0) sqrt(phi2 / k) else NA_real_

  interp <- if (is.na(cramer_v)) "—" else if (cramer_v >= 0.35) {
    "Association forte entre les deux variables"
  } else if (cramer_v >= 0.15) {
    "Association modérée"
  } else {
    "Association faible ou indépendance"
  }

  data.frame(
    Indicateur = c(
      "Effectif total (n)",
      "Statistique du χ² (Khi-deux)",
      "Degrés de liberté (ddl)",
      "p-value du test d'indépendance χ²",
      "Phi-deux (Φ² = χ² / n)",
      "V de Cramér (entre 0 et 1)",
      "Interprétation"
    ),
    Valeur = c(
      as.character(n),
      round(chi2, 3),
      as.character(ddl),
      signif(p_val, 3),
      round(phi2, 4),
      round(cramer_v, 4),
      interp
    ),
    stringsAsFactors = FALSE
  )
}

# Diagrammes en barres croisés (empilés "stack", côte à côte "dodge", ou profils "fill")
tracer_bi_barres_quali <- function(d, nom_x, nom_y, position = "fill") {
  d_propre <- d[!is.na(d[[nom_x]]) & !is.na(d[[nom_y]]), , drop = FALSE]
  lab_y <- if (position == "fill") "Proportion conditionnelle" else "Effectifs"

  ggplot2::ggplot(d_propre, ggplot2::aes(x = .data[[nom_x]], fill = .data[[nom_y]])) +
    ggplot2::geom_bar(position = position) +
    ggplot2::labs(
      title = sprintf("Répartition croisée : %s × %s", nom_x, nom_y),
      x     = nom_x,
      y     = lab_y,
      fill  = nom_y
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 25, hjust = 1))
}

# ==================================================================
# PARTIE 3 : MATRICE DE CORRÉLATION (HEATMAP)
# ==================================================================

# Calcule la matrice de corrélation de toutes les variables quantitatives
calculer_matrice_correlation <- function(d, methode = "pearson") {
  cols_num <- names(d)[sapply(d, is.numeric)]
  if (length(cols_num) < 2) return(NULL)
  round(stats::cor(d[cols_num], use = "pairwise.complete.obs", method = methode), 3)
}

# Trace la carte de chaleur (heatmap) des corrélations avec valeurs affichées
tracer_heatmap_correlation <- function(d, methode = "pearson") {
  mat <- calculer_matrice_correlation(d, methode = methode)
  if (is.null(mat)) return(NULL)

  # Mise au format long sans dépendance externe obligatoire
  noms <- colnames(mat)
  grille <- expand.grid(Var1 = noms, Var2 = rev(noms), stringsAsFactors = FALSE)
  grille$Correlation <- as.numeric(mat[cbind(grille$Var1, grille$Var2)])

  ggplot2::ggplot(grille, ggplot2::aes(x = Var1, y = Var2, fill = Correlation)) +
    ggplot2::geom_tile(color = "white", linewidth = 0.8) +
    ggplot2::geom_text(ggplot2::aes(label = sprintf("%.2f", Correlation)),
                       size = 3.8, color = "#0f172a") +
    ggplot2::scale_fill_gradient2(low = "#2563eb", mid = "white", high = "#dc2626",
                                  midpoint = 0, limits = c(-1, 1)) +
    ggplot2::labs(
      title = sprintf("Matrice de corrélation (%s)",
                      if (methode == "pearson") "Pearson" else "Spearman"),
      x = "", y = "", fill = "Corrélation"
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 30, hjust = 1))
}

