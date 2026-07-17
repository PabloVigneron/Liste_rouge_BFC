

## ============================================================
## ggplot_trend() / ggplot_trend_multi() : version ggplot2 de
## poptrend:::plot.trend(), avec faceting pour comparer plusieurs
## objets `trend` (ex: plusieurs especes) sur une meme figure.
## ============================================================
## Reproduit la logique exacte de plot.trend (baseline, IC bootstrap,
## segments colores pour la 1ere derivee significative, boites colorees
## pour la 2eme derivee significative, points/IC pour effets aleatoires
## temporels ou modeles index).
##
## Depend des fonctions internes du package poptrend : getGradient() et
## getRuns() (non exportees), appelees ici via poptrend:::.
## ============================================================

library(ggplot2)

## ------------------------------------------------------------
## extract_trend_data() : coeur du calcul, factorise pour etre
## reutilise a la fois par ggplot_trend() (1 objet) et
## ggplot_trend_multi() (plusieurs objets, un par facette).
## Renvoie une liste de data.frames, chacun avec une colonne
## `species` = label, prete a etre empilee avec rbind() et facettee.
## ------------------------------------------------------------
extract_trend_data <- function(x, label = "1",
                               baseline = NULL,
                               alpha = 0.05,
                               incCol = "#009E73",
                               decCol = "#D55E00",
                               ranef = "pointCI",
                               secDeriv = TRUE) {
  
  stopifnot(inherits(x, "trend"))
  ranef <- match.arg(ranef, c("pointCI", "point", "CI", "no"))
  
  timeVar  <- x$timeVar
  isGridP  <- x$trendFrame$isGridP
  tGrid    <- x$trendFrame[[timeVar]]
  trendEst <- x$trendFrame$trend
  if (x$trendType == "index") trendEst <- x$trendFrame$trendResid
  
  ## ---- 1. Baseline ----
  bDiv <- NULL
  if (is.null(baseline) || is.numeric(baseline)) {
    bInt <- if (is.null(baseline)) which.min(isGridP) else which.min(abs(tGrid - baseline))
    tDiv <- as.numeric(trendEst[bInt])
    if (!is.null(x$bootTrend)) bDiv <- x$bootTrend[bInt, ]
    if (x$trendType == "index" && !is.null(x$bootResid)) bDiv <- x$bootResid[bInt, ]
  } else if (is.function(baseline)) {
    tDiv <- baseline(trendEst)
    if (!is.null(x$bootTrend)) bDiv <- apply(x$bootTrend, 2, baseline)
    if (x$trendType == "index" && !is.null(x$bootResid)) bDiv <- apply(x$bootResid, 2, baseline)
  } else {
    tDiv <- 1
    if (!is.null(x$bootTrend)) bDiv <- rep(1, ncol(x$bootTrend))
  }
  
  ## ---- 2. IC + segments significatifs (1ere derivee) + boites (2eme derivee) ----
  ci <- NULL
  pGradInd <- nGradInd <- integer(0)
  pGrad2Ind <- nGrad2Ind <- NULL
  
  if (!is.null(x$bootTrend)) {
    grad   <- poptrend:::getGradient(x$bootTrend[isGridP, ], order = 1)
    ciGrad <- apply(grad, 1, quantile, probs = c(alpha / 2, 1 - alpha / 2))
    pGradInd <- which(ciGrad[1, ] > 0)
    nGradInd <- which(ciGrad[2, ] < 0)
    
    if (x$trendType == "smooth" && secDeriv) {
      grad2   <- poptrend:::getGradient(x$bootTrend[isGridP, ], order = 2)
      ciGrad2 <- apply(grad2, 1, quantile, probs = c(alpha / 2, 1 - alpha / 2))
      pGrad2Ind <- poptrend:::getRuns(which(ciGrad2[1, ] > 0))
      nGrad2Ind <- poptrend:::getRuns(which(ciGrad2[2, ] < 0))
    }
    
    ci <- data.frame(t(apply(x$bootTrend, 1, function(row)
      quantile(row / bDiv, probs = c(alpha / 2, 1 - alpha / 2), type = 1))))
    colnames(ci) <- c("low", "upp")
    ci$low <- lowess(tGrid, ci$low, f = .03)$y
    ci$upp <- lowess(tGrid, ci$upp, f = .03)$y
  }
  
  ## ---- 3. IC effets aleatoires temporels / modele index ----
  cip <- NULL
  resGrid <- NULL
  indRE <- NULL
  if (x$timeRE || x$trendType == "index") {
    timeVarFac <- x$timeVarFac
    indRE <- match(unique(x$trendFrame[[timeVarFac]]), x$trendFrame[[timeVarFac]])
    resGrid <- as.numeric(levels(x$trendFrame[[timeVarFac]][indRE]))
    if (!is.null(x$bootTrend)) {
      cip <- apply(x$bootTrend * x$bootResid, 1, function(row)
        quantile(row / bDiv, probs = c(alpha / 2, 1 - alpha / 2), type = 1))
    } else if (!is.null(x$bootResid) && (grepl("CI", ranef) || !x$timeRE)) {
      cip <- apply(x$bootResid, 1, function(row)
        quantile(row / bDiv, probs = c(alpha / 2, 1 - alpha / 2), type = 1))
    }
  }
  
  ## ---- 4. Data frame principal (ligne + ruban) ----
  df <- data.frame(species = label, t = tGrid, trend = trendEst / tDiv)
  if (!is.null(ci)) df <- cbind(df, ci)
  
  ## ---- 5. Segments colores (1ere derivee) ----
  segAll <- NULL
  if (!is.null(x$bootTrend)) {
    tGrid2 <- tGrid[isGridP]
    trendGridVal <- trendEst[isGridP] / tDiv
    mkSeg <- function(idx, col) {
      if (length(idx) == 0) return(NULL)
      idx0 <- replace(idx - 1, idx == 1, 1)
      rbind(
        data.frame(species = label, x = tGrid2[idx0], xend = tGrid2[idx],
                   y = trendGridVal[idx0], yend = trendGridVal[idx], col = col),
        data.frame(species = label, x = tGrid2[idx],  xend = tGrid2[idx + 1],
                   y = trendGridVal[idx], yend = trendGridVal[idx + 1], col = col)
      )
    }
    segAll <- rbind(mkSeg(pGradInd, incCol), mkSeg(nGradInd, decCol))
  }
  
  ## ---- 6. Boites colorees (2eme derivee) ----
  boxes <- NULL
  if (!is.null(pGrad2Ind) || !is.null(nGrad2Ind)) {
    tGrid2 <- tGrid[isGridP]
    if (!is.null(pGrad2Ind) && nrow(pGrad2Ind) > 0) {
      boxes <- rbind(boxes, data.frame(species = label,
                                       xmin = tGrid2[pGrad2Ind[, 1]], xmax = tGrid2[pGrad2Ind[, 2]], col = incCol))
    }
    if (!is.null(nGrad2Ind) && nrow(nGrad2Ind) > 0) {
      boxes <- rbind(boxes, data.frame(species = label,
                                       xmin = tGrid2[nGrad2Ind[, 1]], xmax = tGrid2[nGrad2Ind[, 2]], col = decCol))
    }
  }
  
  ## ---- 7. Effets aleatoires / index : points + IC ----
  reSeg <- NULL
  rePts <- NULL
  if ((x$timeRE || x$trendType == "index") && !is.null(cip)) {
    if (grepl("CI", ranef) || x$trendType == "index") {
      reSeg <- data.frame(species = label, t = resGrid, low = cip[1, indRE], upp = cip[2, indRE])
    }
    if (x$timeRE && grepl("point", ranef)) {
      rePts <- data.frame(species = label, t = resGrid,
                          y = (trendEst[indRE] * x$trendFrame$trendResid[indRE]) / tDiv)
    } else if (x$trendType == "index") {
      rePts <- data.frame(species = label, t = resGrid, y = x$trendFrame$trendResid[indRE] / tDiv)
    }
  }
  
  list(main = df, seg = segAll, boxes = boxes, reSeg = reSeg, rePts = rePts,
       trendType = x$trendType, timeVar = timeVar)
}

## ------------------------------------------------------------
## ggplot_trend() : un seul objet trend (identique a l'usage precedent,
## reimplemente comme un simple appel a ggplot_trend_multi()).
## ------------------------------------------------------------
ggplot_trend <- function(x,
                         baseline = NULL,
                         alpha = 0.05,
                         ylab = "abundance index",
                         xlab = NULL,
                         trendCol = "black",
                         shadeCol = "#0072B2",
                         incCol = "#009E73",
                         decCol = "#D55E00",
                         ranef = "pointCI",
                         secDeriv = TRUE) {
  ggplot_trend_multi(list(" " = x), baseline = baseline, alpha = alpha, ylab = ylab, xlab = xlab,
                     trendCol = trendCol, shadeCol = shadeCol, incCol = incCol, decCol = decCol,
                     ranef = ranef, secDeriv = secDeriv, facet = FALSE)
}

## ------------------------------------------------------------
## ggplot_trend_multi() : plusieurs objets trend, une facette par objet.
##
## trend_list : liste NOMMEE d'objets de classe `trend`
##              ex: list(TRF = trFit_TRF, XYZ = trFit_XYZ, ...)
##              Les noms de la liste servent de titres de facette.
## facet_scales : "free_y" (defaut, comme dans ton script d'origine),
##              "fixed", "free_x" ou "free"
## ncol / nrow : mise en page de facet_wrap (NULL = auto)
## ------------------------------------------------------------
ggplot_trend_multi <- function(trend_list,
                               baseline = NULL,
                               alpha = 0.05,
                               ylab = "abundance index",
                               xlab = NULL,
                               trendCol = "black",
                               shadeCol = "#0072B2",
                               incCol = "#009E73",
                               decCol = "#D55E00",
                               ranef = "pointCI",
                               secDeriv = TRUE,
                               facet = TRUE,
                               facet_scales = "free_y",
                               ncol = NULL,
                               nrow = NULL) {
  
  stopifnot(is.list(trend_list), all(sapply(trend_list, inherits, "trend")))
  if (is.null(names(trend_list)) || any(names(trend_list) == "")) {
    names(trend_list) <- paste0("trend_", seq_along(trend_list))
  }
  
  ## baseline peut differer d'un objet a l'autre si c'est une fonction ;
  ## si c'est une valeur numerique/NULL commune, elle s'applique a tous.
  extracted <- Map(function(x, lab) {
    extract_trend_data(x, label = lab, baseline = baseline, alpha = alpha,
                       incCol = incCol, decCol = decCol, ranef = ranef, secDeriv = secDeriv)
  }, trend_list, names(trend_list))
  
  mainDf  <- do.call(rbind, lapply(extracted, `[[`, "main"))
  segDf   <- do.call(rbind, lapply(extracted, `[[`, "seg"))
  boxDf   <- do.call(rbind, lapply(extracted, `[[`, "boxes"))
  reSegDf <- do.call(rbind, lapply(extracted, `[[`, "reSeg"))
  rePtsDf <- do.call(rbind, lapply(extracted, `[[`, "rePts"))
  
  mainDf$species <- factor(mainDf$species, levels = names(trend_list))
  if (!is.null(segDf))   segDf$species   <- factor(segDf$species,   levels = names(trend_list))
  if (!is.null(boxDf))   boxDf$species   <- factor(boxDf$species,   levels = names(trend_list))
  if (!is.null(reSegDf)) reSegDf$species <- factor(reSegDf$species, levels = names(trend_list))
  if (!is.null(rePtsDf)) rePtsDf$species <- factor(rePtsDf$species, levels = names(trend_list))
  
  ## positionnement des boites de significativite (2eme derivee) : bas
  ## de CHAQUE facette -> calcule par groupe (species) car les echelles
  ## y peuvent differer (facet_scales = "free_y").
  if (!is.null(boxDf)) {
    yr <- do.call(rbind, lapply(split(mainDf, mainDf$species), function(d) {
      rng <- range(c(d$trend, d$low, d$upp), na.rm = TRUE)
      data.frame(species = d$species[1],
                 ymin_box = rng[1] - 0.06 * diff(rng),
                 ymax_box = rng[1] - 0.01 * diff(rng))
    }))
    boxDf <- merge(boxDf, yr, by = "species")
  }
  
  p <- ggplot(mainDf, aes(x = t))
  
  if ("low" %in% names(mainDf)) {
    p <- p + geom_ribbon(aes(ymin = low, ymax = upp), fill = shadeCol, alpha = 0.4)
  }
  
  if (!is.null(boxDf)) {
    p <- p + geom_rect(data = boxDf,
                       aes(xmin = xmin, xmax = xmax, ymin = ymin_box, ymax = ymax_box),
                       fill = boxDf$col, alpha = 0.5, inherit.aes = FALSE)
  }
  
  p <- p + geom_line(aes(y = trend), color = trendCol, linewidth = 1)
  
  if (!is.null(segDf)) {
    p <- p + geom_segment(data = segDf,
                          aes(x = x, xend = xend, y = y, yend = yend),
                          color = segDf$col, linewidth = 1.2, inherit.aes = FALSE)
  }
  
  if (!is.null(reSegDf)) {
    p <- p + geom_segment(data = reSegDf, aes(x = t, xend = t, y = low, yend = upp),
                          color = trendCol, linewidth = 0.4, inherit.aes = FALSE)
  }
  if (!is.null(rePtsDf)) {
    p <- p + geom_point(data = rePtsDf, aes(x = t, y = y), color = trendCol, size = 1.3, inherit.aes = FALSE)
  }
  
  if (facet) {
    p <- p + facet_wrap(~ species, scales = facet_scales, ncol = ncol, nrow = nrow)
  }
  
  p <- p +
    labs(x = if (is.null(xlab)) unique(sapply(trend_list, function(x) x$timeVar))[1] else xlab,
         y = ylab) +
    theme_minimal(base_size = 13) +
    theme(panel.grid.minor = element_blank(),
          strip.text = element_text(face = "bold"))
  
  p
}

## ============================================================
## Exemple d'utilisation
## ============================================================
## library(poptrend)
##
## ## un seul objet, comme avant
## ggplot_trend(trFit_TRF) + ggtitle("TRF")
##
## ## comparaison de plusieurs especes sur une meme figure, une facette chacune
## trend_list <- list(
##   TRF = trFit_TRF,
##   ESP2 = trFit_ESP2,
##   ESP3 = trFit_ESP3
## )
##
## ggplot_trend_multi(trend_list, facet_scales = "free_y", ncol = 2)
##
## ## avec une echelle y commune pour comparer les amplitudes entre especes
## ggplot_trend_multi(trend_list, facet_scales = "fixed")

## ============================================================
## fit_trends_by_species() : ajuste un ptrend() par espece a partir
## d'un jeu de donnees long (une ligne par ope_id x espece), et
## renvoie une liste NOMMEE d'objets `trend`, prete pour
## ggplot_trend_multi().
## ============================================================
## Jeu de donnees attendu (format long) :
##   ope_id | espece | indicateur | valeur | stade | pop_id | annee | x | y | bassin_versant
##
## Le formulaire GAM construit pour chaque espece est de la forme :
##   valeur ~ trend(annee, tempRE = tempRE, type = type) + s(pop_id, bs = "re")
## avec ajout facultatif de s(x, y) si include_xy = TRUE.
## ------------------------------------------------------------
fit_trends_by_species <- function(data,
                                  species_col     = "espece",
                                  value_col       = "valeur",
                                  time_col        = "annee",
                                  site_col        = "pop_id",
                                  indicateur      = NULL,   # ex: "effectif_total" -> filtre data[[indicateur_col]] == indicateur
                                  indicateur_col  = "indicateur",
                                  species         = NULL,   # NULL = toutes les especes presentes dans data
                                  type            = "smooth",   # "smooth", "loglinear" ou "index"
                                  tempRE          = FALSE,      # effet aleatoire temporel en plus de la tendance
                                  siteRE          = TRUE,       # effet aleatoire site : s(pop_id, bs = "re")
                                  include_xy      = FALSE,      # ajoute s(x, y) comme covariable spatiale
                                  family          = quasipoisson(),
                                  nGrid           = 500,
                                  nBoot           = 500,
                                  k               = -1,
                                  fx              = FALSE,
                                  min_obs         = 20,         # nb minimum d'observations pour tenter l'ajustement
                                  verbose         = TRUE,
                                  ...) {
  
  stopifnot(all(c(species_col, value_col, time_col, site_col) %in% names(data)))
  
  if (!is.null(indicateur)) {
    stopifnot(indicateur_col %in% names(data))
    data <- data[data[[indicateur_col]] == indicateur, , drop = FALSE]
  }
  
  if (is.null(species)) species <- sort(unique(data[[species_col]]))
  
  ## construction du membre droit de la formule
  rhs <- paste0("trend(", time_col, ", tempRE = ", tempRE, ", type = \"", type,
                "\", k = ", k, ", fx = ", fx, ")")
  if (siteRE)     rhs <- paste0(rhs, " + s(", site_col, ", bs = \"re\")")
  if (include_xy) rhs <- paste0(rhs, " + s(x, y)")
  
  frm <- as.formula(paste0(value_col, " ~ ", rhs))
  
  results <- list()
  for (sp in species) {
    
    df_sp <- data[data[[species_col]] == sp, , drop = FALSE]
    df_sp[[site_col]] <- factor(df_sp[[site_col]])
    
    if (nrow(df_sp) < min_obs) {
      if (verbose) message(sprintf("[%s] ignoree : seulement %d observations (< min_obs = %d)",
                                   sp, nrow(df_sp), min_obs))
      next
    }
    
    fit <- tryCatch(
      ptrend(frm, data = df_sp, family = family, nGrid = nGrid, nBoot = nBoot, ...),
      error = function(e) {
        if (verbose) message(sprintf("[%s] echec de l'ajustement : %s", sp, conditionMessage(e)))
        NULL
      }
    )
    
    if (!is.null(fit)) {
      results[[sp]] <- fit
      if (verbose) message(sprintf("[%s] OK (n = %d, sites = %d)",
                                   sp, nrow(df_sp), nlevels(df_sp[[site_col]])))
    }
  }
  
  results
}

## ------------------------------------------------------------
## plot_trends_by_species() : pipeline complet -> ajuste + facette.
## Renvoie une liste avec $trends (les objets trend, un par espece,
## utile pour checkFit()/summary()/change()) et $plot (le ggplot).
## ------------------------------------------------------------
plot_trends_by_species <- function(data,
                                   species_col    = "espece",
                                   value_col      = "valeur",
                                   time_col       = "annee",
                                   site_col       = "pop_id",
                                   indicateur     = NULL,
                                   indicateur_col = "indicateur",
                                   species        = NULL,
                                   type           = "smooth",
                                   tempRE         = FALSE,
                                   siteRE         = TRUE,
                                   include_xy     = FALSE,
                                   family         = quasipoisson(),
                                   nGrid          = 500,
                                   nBoot          = 500,
                                   min_obs        = 20,
                                   baseline       = NULL,
                                   alpha          = 0.05,
                                   ylab           = "abundance index",
                                   facet_scales   = "free_y",
                                   ncol           = NULL,
                                   nrow           = NULL,
                                   verbose        = TRUE,
                                   ...) {
  
  trends <- fit_trends_by_species(
    data, species_col = species_col, value_col = value_col, time_col = time_col,
    site_col = site_col, indicateur = indicateur, indicateur_col = indicateur_col,
    species = species, type = type, tempRE = tempRE, siteRE = siteRE,
    include_xy = include_xy, family = family, nGrid = nGrid, nBoot = nBoot,
    min_obs = min_obs, verbose = verbose, ...
  )
  
  if (length(trends) == 0) stop("Aucun modele n'a pu etre ajuste : verifie min_obs / les donnees.")
  
  p <- ggplot_trend_multi(trends, baseline = baseline, alpha = alpha, ylab = ylab,
                          facet_scales = facet_scales, ncol = ncol, nrow = nrow)
  
  list(trends = trends, plot = p)
}

## ============================================================
## Exemple d'utilisation avec ton jeu de donnees
## ============================================================
## library(poptrend)
##
## res <- plot_trends_by_species(
##   data           = data_esp,             # ton jeu de donnees long
##   indicateur     = "effectif_total",      # filtre sur indicateur
##   species        = c("TRF", "CHA", "CHE"),# NULL = toutes les especes
##   type           = "smooth",
##   siteRE         = TRUE,                  # s(pop_id, bs = "re")
##   tempRE         = FALSE,
##   family         = quasipoisson(),
##   nBoot          = 500,
##   min_obs        = 20,
##   facet_scales   = "free_y",
##   ncol           = 3
## )
##
## res$plot                     # la figure facettee
## checkFit(res$trends$TRF)     # verifier l'ajustement d'une espece
## summary(res$trends$TRF)
## change(res$trends$TRF, 2015, 2019)

































# ## ============================================================
# ## ggplot_trend() / ggplot_trend_multi() : version ggplot2 de
# ## poptrend:::plot.trend(), avec faceting pour comparer plusieurs
# ## objets `trend` (ex: plusieurs especes) sur une meme figure.
# ## ============================================================
# ## Reproduit la logique exacte de plot.trend (baseline, IC bootstrap,
# ## segments colores pour la 1ere derivee significative, boites colorees
# ## pour la 2eme derivee significative, points/IC pour effets aleatoires
# ## temporels ou modeles index).
# ##
# ## Depend des fonctions internes du package poptrend : getGradient() et
# ## getRuns() (non exportees), appelees ici via poptrend:::.
# ## ============================================================
# 
# library(ggplot2)
# 
# ## ------------------------------------------------------------
# ## extract_trend_data() : coeur du calcul, factorise pour etre
# ## reutilise a la fois par ggplot_trend() (1 objet) et
# ## ggplot_trend_multi() (plusieurs objets, un par facette).
# ## Renvoie une liste de data.frames, chacun avec une colonne
# ## `species` = label, prete a etre empilee avec rbind() et facettee.
# ## ------------------------------------------------------------
# extract_trend_data <- function(x, label = "1",
#                                baseline = NULL,
#                                alpha = 0.05,
#                                incCol = "#009E73",
#                                decCol = "#D55E00",
#                                ranef = "pointCI",
#                                secDeriv = TRUE) {
#   
#   stopifnot(inherits(x, "trend"))
#   ranef <- match.arg(ranef, c("pointCI", "point", "CI", "no"))
#   
#   timeVar  <- x$timeVar
#   isGridP  <- x$trendFrame$isGridP
#   tGrid    <- x$trendFrame[[timeVar]]
#   trendEst <- x$trendFrame$trend
#   if (x$trendType == "index") trendEst <- x$trendFrame$trendResid
#   
#   ## ---- 1. Baseline ----
#   bDiv <- NULL
#   if (is.null(baseline) || is.numeric(baseline)) {
#     bInt <- if (is.null(baseline)) which.min(isGridP) else which.min(abs(tGrid - baseline))
#     tDiv <- as.numeric(trendEst[bInt])
#     if (!is.null(x$bootTrend)) bDiv <- x$bootTrend[bInt, ]
#     if (x$trendType == "index" && !is.null(x$bootResid)) bDiv <- x$bootResid[bInt, ]
#   } else if (is.function(baseline)) {
#     tDiv <- baseline(trendEst)
#     if (!is.null(x$bootTrend)) bDiv <- apply(x$bootTrend, 2, baseline)
#     if (x$trendType == "index" && !is.null(x$bootResid)) bDiv <- apply(x$bootResid, 2, baseline)
#   } else {
#     tDiv <- 1
#     if (!is.null(x$bootTrend)) bDiv <- rep(1, ncol(x$bootTrend))
#   }
#   
#   ## ---- 2. IC + segments significatifs (1ere derivee) + boites (2eme derivee) ----
#   ci <- NULL
#   pGradInd <- nGradInd <- integer(0)
#   pGrad2Ind <- nGrad2Ind <- NULL
#   
#   if (!is.null(x$bootTrend)) {
#     grad   <- poptrend:::getGradient(x$bootTrend[isGridP, ], order = 1)
#     ciGrad <- apply(grad, 1, quantile, probs = c(alpha / 2, 1 - alpha / 2))
#     pGradInd <- which(ciGrad[1, ] > 0)
#     nGradInd <- which(ciGrad[2, ] < 0)
#     
#     if (x$trendType == "smooth" && secDeriv) {
#       grad2   <- poptrend:::getGradient(x$bootTrend[isGridP, ], order = 2)
#       ciGrad2 <- apply(grad2, 1, quantile, probs = c(alpha / 2, 1 - alpha / 2))
#       pGrad2Ind <- poptrend:::getRuns(which(ciGrad2[1, ] > 0))
#       nGrad2Ind <- poptrend:::getRuns(which(ciGrad2[2, ] < 0))
#     }
#     
#     ci <- data.frame(t(apply(x$bootTrend, 1, function(row)
#       quantile(row / bDiv, probs = c(alpha / 2, 1 - alpha / 2), type = 1))))
#     colnames(ci) <- c("low", "upp")
#     ci$low <- lowess(tGrid, ci$low, f = .03)$y
#     ci$upp <- lowess(tGrid, ci$upp, f = .03)$y
#   }
#   
#   ## ---- 3. IC effets aleatoires temporels / modele index ----
#   cip <- NULL
#   resGrid <- NULL
#   indRE <- NULL
#   if (x$timeRE || x$trendType == "index") {
#     timeVarFac <- x$timeVarFac
#     indRE <- match(unique(x$trendFrame[[timeVarFac]]), x$trendFrame[[timeVarFac]])
#     resGrid <- as.numeric(levels(x$trendFrame[[timeVarFac]][indRE]))
#     if (!is.null(x$bootTrend)) {
#       cip <- apply(x$bootTrend * x$bootResid, 1, function(row)
#         quantile(row / bDiv, probs = c(alpha / 2, 1 - alpha / 2), type = 1))
#     } else if (!is.null(x$bootResid) && (grepl("CI", ranef) || !x$timeRE)) {
#       cip <- apply(x$bootResid, 1, function(row)
#         quantile(row / bDiv, probs = c(alpha / 2, 1 - alpha / 2), type = 1))
#     }
#   }
#   
#   ## ---- 4. Data frame principal (ligne + ruban) ----
#   df <- data.frame(species = label, t = tGrid, trend = trendEst / tDiv)
#   if (!is.null(ci)) df <- cbind(df, ci)
#   
#   ## ---- 5. Segments colores (1ere derivee) ----
#   segAll <- NULL
#   if (!is.null(x$bootTrend)) {
#     tGrid2 <- tGrid[isGridP]
#     trendGridVal <- trendEst[isGridP] / tDiv
#     mkSeg <- function(idx, col) {
#       if (length(idx) == 0) return(NULL)
#       idx0 <- replace(idx - 1, idx == 1, 1)
#       rbind(
#         data.frame(species = label, x = tGrid2[idx0], xend = tGrid2[idx],
#                    y = trendGridVal[idx0], yend = trendGridVal[idx], col = col),
#         data.frame(species = label, x = tGrid2[idx],  xend = tGrid2[idx + 1],
#                    y = trendGridVal[idx], yend = trendGridVal[idx + 1], col = col)
#       )
#     }
#     segAll <- rbind(mkSeg(pGradInd, incCol), mkSeg(nGradInd, decCol))
#   }
#   
#   ## ---- 6. Boites colorees (2eme derivee) ----
#   boxes <- NULL
#   if (!is.null(pGrad2Ind) || !is.null(nGrad2Ind)) {
#     tGrid2 <- tGrid[isGridP]
#     if (!is.null(pGrad2Ind) && nrow(pGrad2Ind) > 0) {
#       boxes <- rbind(boxes, data.frame(species = label,
#                                        xmin = tGrid2[pGrad2Ind[, 1]], xmax = tGrid2[pGrad2Ind[, 2]], col = incCol))
#     }
#     if (!is.null(nGrad2Ind) && nrow(nGrad2Ind) > 0) {
#       boxes <- rbind(boxes, data.frame(species = label,
#                                        xmin = tGrid2[nGrad2Ind[, 1]], xmax = tGrid2[nGrad2Ind[, 2]], col = decCol))
#     }
#   }
#   
#   ## ---- 7. Effets aleatoires / index : points + IC ----
#   reSeg <- NULL
#   rePts <- NULL
#   if ((x$timeRE || x$trendType == "index") && !is.null(cip)) {
#     if (grepl("CI", ranef) || x$trendType == "index") {
#       reSeg <- data.frame(species = label, t = resGrid, low = cip[1, indRE], upp = cip[2, indRE])
#     }
#     if (x$timeRE && grepl("point", ranef)) {
#       rePts <- data.frame(species = label, t = resGrid,
#                           y = (trendEst[indRE] * x$trendFrame$trendResid[indRE]) / tDiv)
#     } else if (x$trendType == "index") {
#       rePts <- data.frame(species = label, t = resGrid, y = x$trendFrame$trendResid[indRE] / tDiv)
#     }
#   }
#   
#   list(main = df, seg = segAll, boxes = boxes, reSeg = reSeg, rePts = rePts,
#        trendType = x$trendType, timeVar = timeVar)
# }
# 
# ## ------------------------------------------------------------
# ## ggplot_trend() : un seul objet trend (identique a l'usage precedent,
# ## reimplemente comme un simple appel a ggplot_trend_multi()).
# ## ------------------------------------------------------------
# ggplot_trend <- function(x,
#                          baseline = NULL,
#                          alpha = 0.05,
#                          ylab = "abundance index",
#                          xlab = NULL,
#                          trendCol = "black",
#                          shadeCol = "#0072B2",
#                          incCol = "#009E73",
#                          decCol = "#D55E00",
#                          ranef = "pointCI",
#                          secDeriv = TRUE) {
#   ggplot_trend_multi(list(" " = x), baseline = baseline, alpha = alpha, ylab = ylab, xlab = xlab,
#                      trendCol = trendCol, shadeCol = shadeCol, incCol = incCol, decCol = decCol,
#                      ranef = ranef, secDeriv = secDeriv, facet = FALSE)
# }
# 
# ## ------------------------------------------------------------
# ## ggplot_trend_multi() : plusieurs objets trend, une facette par objet.
# ##
# ## trend_list : liste NOMMEE d'objets de classe `trend`
# ##              ex: list(TRF = trFit_TRF, XYZ = trFit_XYZ, ...)
# ##              Les noms de la liste servent de titres de facette.
# ## facet_scales : "free_y" (defaut, comme dans ton script d'origine),
# ##              "fixed", "free_x" ou "free"
# ## ncol / nrow : mise en page de facet_wrap (NULL = auto)
# ## ------------------------------------------------------------
# ggplot_trend_multi <- function(trend_list,
#                                baseline = NULL,
#                                alpha = 0.05,
#                                ylab = "abundance index",
#                                xlab = NULL,
#                                trendCol = "black",
#                                shadeCol = "#0072B2",
#                                incCol = "#009E73",
#                                decCol = "#D55E00",
#                                ranef = "pointCI",
#                                secDeriv = TRUE,
#                                facet = TRUE,
#                                facet_scales = "free_y",
#                                ncol = NULL,
#                                nrow = NULL) {
#   
#   stopifnot(is.list(trend_list), all(sapply(trend_list, inherits, "trend")))
#   if (is.null(names(trend_list)) || any(names(trend_list) == "")) {
#     names(trend_list) <- paste0("trend_", seq_along(trend_list))
#   }
#   
#   ## baseline peut differer d'un objet a l'autre si c'est une fonction ;
#   ## si c'est une valeur numerique/NULL commune, elle s'applique a tous.
#   extracted <- Map(function(x, lab) {
#     extract_trend_data(x, label = lab, baseline = baseline, alpha = alpha,
#                        incCol = incCol, decCol = decCol, ranef = ranef, secDeriv = secDeriv)
#   }, trend_list, names(trend_list))
#   
#   mainDf  <- do.call(rbind, lapply(extracted, `[[`, "main"))
#   segDf   <- do.call(rbind, lapply(extracted, `[[`, "seg"))
#   boxDf   <- do.call(rbind, lapply(extracted, `[[`, "boxes"))
#   reSegDf <- do.call(rbind, lapply(extracted, `[[`, "reSeg"))
#   rePtsDf <- do.call(rbind, lapply(extracted, `[[`, "rePts"))
#   
#   mainDf$species <- factor(mainDf$species, levels = names(trend_list))
#   if (!is.null(segDf))   segDf$species   <- factor(segDf$species,   levels = names(trend_list))
#   if (!is.null(boxDf))   boxDf$species   <- factor(boxDf$species,   levels = names(trend_list))
#   if (!is.null(reSegDf)) reSegDf$species <- factor(reSegDf$species, levels = names(trend_list))
#   if (!is.null(rePtsDf)) rePtsDf$species <- factor(rePtsDf$species, levels = names(trend_list))
#   
#   ## positionnement des boites de significativite (2eme derivee) : bas
#   ## de CHAQUE facette -> calcule par groupe (species) car les echelles
#   ## y peuvent differer (facet_scales = "free_y").
#   if (!is.null(boxDf)) {
#     yr <- do.call(rbind, lapply(split(mainDf, mainDf$species), function(d) {
#       rng <- range(c(d$trend, d$low, d$upp), na.rm = TRUE)
#       data.frame(species = d$species[1],
#                  ymin_box = rng[1] - 0.06 * diff(rng),
#                  ymax_box = rng[1] - 0.01 * diff(rng))
#     }))
#     boxDf <- merge(boxDf, yr, by = "species")
#   }
#   
#   p <- ggplot(mainDf, aes(x = t))
#   
#   if ("low" %in% names(mainDf)) {
#     p <- p + geom_ribbon(aes(ymin = low, ymax = upp), fill = shadeCol, alpha = 0.4)
#   }
#   
#   if (!is.null(boxDf)) {
#     p <- p + geom_rect(data = boxDf,
#                        aes(xmin = xmin, xmax = xmax, ymin = ymin_box, ymax = ymax_box),
#                        fill = boxDf$col, alpha = 0.5, inherit.aes = FALSE)
#   }
#   
#   p <- p + geom_line(aes(y = trend), color = trendCol, linewidth = 1)
#   
#   if (!is.null(segDf)) {
#     p <- p + geom_segment(data = segDf,
#                           aes(x = x, xend = xend, y = y, yend = yend),
#                           color = segDf$col, linewidth = 1.2, inherit.aes = FALSE)
#   }
#   
#   if (!is.null(reSegDf)) {
#     p <- p + geom_segment(data = reSegDf, aes(x = t, xend = t, y = low, yend = upp),
#                           color = trendCol, linewidth = 0.4, inherit.aes = FALSE)
#   }
#   if (!is.null(rePtsDf)) {
#     p <- p + geom_point(data = rePtsDf, aes(x = t, y = y), color = trendCol, size = 1.3, inherit.aes = FALSE)
#   }
#   
#   if (facet) {
#     p <- p + facet_wrap(~ species, scales = facet_scales, ncol = ncol, nrow = nrow)
#   }
#   
#   p <- p +
#     labs(x = if (is.null(xlab)) unique(sapply(trend_list, function(x) x$timeVar))[1] else xlab,
#          y = ylab) +
#     theme_minimal(base_size = 13) +
#     theme(panel.grid.minor = element_blank(),
#           strip.text = element_text(face = "bold"))
#   
#   p
# }
# 
# ## ============================================================
# ## Exemple d'utilisation
# ## ============================================================
# ## library(poptrend)
# ##
# ## ## un seul objet, comme avant
# ## ggplot_trend(trFit_TRF) + ggtitle("TRF")
# ##
# ## ## comparaison de plusieurs especes sur une meme figure, une facette chacune
# ## trend_list <- list(
# ##   TRF = trFit_TRF,
# ##   ESP2 = trFit_ESP2,
# ##   ESP3 = trFit_ESP3
# ## )
# ##
# ## ggplot_trend_multi(trend_list, facet_scales = "free_y", ncol = 2)
# ##
# ## ## avec une echelle y commune pour comparer les amplitudes entre especes
# ## ggplot_trend_multi(trend_list, facet_scales = "fixed")