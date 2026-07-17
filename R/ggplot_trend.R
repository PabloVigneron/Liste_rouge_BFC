#' Version ggplot2 de poptrend:::plot.trend ()
#' Reproduit la logique exacte de plot.trend (baseline, IC bootstrap,
#' segments colorés pour la 1ere derivee significative, boites colorees
#' pour la 2eme derivee significative, points/IC pour effets aleatoires
#' temporels ou modeles index).
#'
#'Depend des fonctions internes du package poptrend : getGradient() et
#' getRuns() (non exportees), appelees ici via poptrend:::.
#' 
#' @param x trend issu de poptrend::ptrend()
#' @return ggplot object
#' @export 
#' 
#' @importClassesFrom ggplot2
#' @importClassesFrom poptrend
#'
#' @examples
#' \dontrun{
#' data <- simTrend(15, 25)
#' trFit <- ptrend(count ~ trend(year, tempRE = TRUE, type = "smooth") + site, data = data)
#'
#' ggplot_trend(trFit)
#'
#' options equivalentes a plot(trFit, ...)
#' ggplot_trend(trFit, alpha = 0.10, ranef = "point", secDeriv = TRUE)
#'
#' comparaison avec la version base R d'origine
#' plot(trFit)
#' }

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
                         secDeriv = TRUE,
                         plotLines = FALSE,
                         lineCol = "grey70",
                         lineAlpha = 0.05) {
  
  stopifnot(inherits(x, "trend"))
  ranef <- match.arg(ranef, c("pointCI", "point", "CI", "no"))
  
  timeVar  <- x$timeVar
  isGridP  <- x$trendFrame$isGridP
  tGrid    <- x$trendFrame[[timeVar]]
  trendEst <- x$trendFrame$trend
  if (x$trendType == "index") trendEst <- x$trendFrame$trendResid
  
  ## ---------------------------------------------------------
  ## 1. Baseline (reference a laquelle l'indice vaut 1)
  ## ---------------------------------------------------------
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
  
  ## ---------------------------------------------------------
  ## 2. Bande de confiance + segments significatifs (1ere derivee)
  ##    + boites significatives (2eme derivee)
  ## ---------------------------------------------------------
  ci <- NULL
  pGradInd <- nGradInd <- integer(0)
  pGrad2Ind <- nGrad2Ind <- NULL
  
  if (!is.null(x$bootTrend)) {
    
    grad   <- poptrend:::getGradient(x$bootTrend[isGridP, ], order = 1)
    ciGrad <- apply(grad, 1, quantile, probs = c(alpha / 2, 1 - alpha / 2))
    pGradInd <- which(ciGrad[1, ] > 0)   # pente significativement positive
    nGradInd <- which(ciGrad[2, ] < 0)   # pente significativement negative
    
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
  
  ## ---------------------------------------------------------
  ## 3. IC pour effets aleatoires temporels / modele index
  ## ---------------------------------------------------------
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
  
  ## ---------------------------------------------------------
  ## 4. Data frame principal
  ## ---------------------------------------------------------
  df <- data.frame(t = tGrid, trend = trendEst / tDiv)
  if (!is.null(ci)) df <- cbind(df, ci)
  
  yrange <- range(c(df$trend, df$low, df$upp), na.rm = TRUE)
  
  p <- ggplot(df, aes(x = t))
  
  ## bootstrap : lignes individuelles (optionnel)
  if (!is.null(x$bootTrend) && plotLines) {
    bootDf <- data.frame(
      t = rep(tGrid, ncol(x$bootTrend)),
      y = as.vector(sweep(x$bootTrend, 2, bDiv, "/")),
      sim = rep(seq_len(ncol(x$bootTrend)), each = length(tGrid))
    )
    p <- p + geom_line(data = bootDf, aes(x = t, y = y, group = sim),
                       color = lineCol, alpha = lineAlpha, inherit.aes = FALSE)
  }
  
  ## bande de confiance
  if (!is.null(ci)) {
    p <- p + geom_ribbon(aes(ymin = low, ymax = upp), fill = shadeCol, alpha = 0.4)
  }
  
  ## boites de significativite de la 2eme derivee (bas du graphique)
  if (!is.null(pGrad2Ind) || !is.null(nGrad2Ind)) {
    ymin_box <- yrange[1] - 0.06 * diff(yrange)
    ymax_box <- yrange[1] - 0.01 * diff(yrange)
    tGrid2 <- tGrid[isGridP]
    
    boxes <- data.frame()
    if (!is.null(pGrad2Ind) && nrow(pGrad2Ind) > 0) {
      boxes <- rbind(boxes, data.frame(
        xmin = tGrid2[pGrad2Ind[, 1]], xmax = tGrid2[pGrad2Ind[, 2]], col = incCol))
    }
    if (!is.null(nGrad2Ind) && nrow(nGrad2Ind) > 0) {
      boxes <- rbind(boxes, data.frame(
        xmin = tGrid2[nGrad2Ind[, 1]], xmax = tGrid2[nGrad2Ind[, 2]], col = decCol))
    }
    if (nrow(boxes) > 0) {
      p <- p + geom_rect(data = boxes,
                         aes(xmin = xmin, xmax = xmax, ymin = ymin_box, ymax = ymax_box),
                         fill = boxes$col, alpha = 0.5, inherit.aes = FALSE)
    }
  }
  
  ## ligne centrale (noire, sert de base ; les segments colores sont ajoutes par-dessus)
  if (x$trendType != "index") {
    p <- p + geom_line(aes(y = trend), color = trendCol, linewidth = 1)
  }
  
  ## segments colores : pente significativement positive / negative
  if (!is.null(x$bootTrend)) {
    tGrid2 <- tGrid[isGridP]
    trendGridVal <- trendEst[isGridP] / tDiv
    
    mkSeg <- function(idx, col) {
      if (length(idx) == 0) return(NULL)
      idx0 <- replace(idx - 1, idx == 1, 1)
      rbind(
        data.frame(x = tGrid2[idx0], xend = tGrid2[idx],
                   y = trendGridVal[idx0], yend = trendGridVal[idx], col = col),
        data.frame(x = tGrid2[idx],  xend = tGrid2[idx + 1],
                   y = trendGridVal[idx], yend = trendGridVal[idx + 1], col = col)
      )
    }
    segAll <- rbind(mkSeg(pGradInd, incCol), mkSeg(nGradInd, decCol))
    if (!is.null(segAll)) {
      p <- p + geom_segment(data = segAll,
                            aes(x = x, xend = xend, y = y, yend = yend),
                            color = segAll$col, linewidth = 1.2, inherit.aes = FALSE)
    }
  }
  
  ## points / IC pour effets aleatoires temporels ou modele index
  if ((x$timeRE || x$trendType == "index") && !is.null(cip)) {
    reDf <- data.frame(t = resGrid, low = cip[1, indRE], upp = cip[2, indRE])
    if (grepl("CI", ranef) || x$trendType == "index") {
      p <- p + geom_segment(data = reDf, aes(x = t, xend = t, y = low, yend = upp),
                            color = trendCol, linewidth = 0.4, inherit.aes = FALSE)
    }
    if (x$timeRE && grepl("point", ranef)) {
      ptDf <- data.frame(t = resGrid,
                         y = (trendEst[indRE] * x$trendFrame$trendResid[indRE]) / tDiv)
      p <- p + geom_point(data = ptDf, aes(x = t, y = y), color = trendCol, size = 1.5, inherit.aes = FALSE)
    } else if (x$trendType == "index") {
      ptDf <- data.frame(t = resGrid, y = x$trendFrame$trendResid[indRE] / tDiv)
      p <- p + geom_point(data = ptDf, aes(x = t, y = y), color = trendCol, size = 1.5, inherit.aes = FALSE)
    }
  }
  
  p <- p +
    labs(x = if (is.null(xlab)) timeVar else xlab, y = ylab) +
    theme_minimal(base_size = 13) +
    theme(panel.grid.minor = element_blank())
  
  p
}
