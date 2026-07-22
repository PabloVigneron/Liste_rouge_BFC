#' Calculer les résultats des modèles GAMM avec poptrend pour un df et une espèce choisit
#'
#' @param df Dataframe contenant les données
#' @param mon_espece esp_code_alternatif : le code espèce de l'espèce ciblé par l'analyse
#' @return Dataframe avec resultats 
#' @export 
#' 
#' @importClassesFrom dplyr; mutate filter rename ;lme4, glmer; base summary  nrow rename length unique ; glmmTMB
#'
#' @examples
#' \dontrun{
#' mod <- gamm_poptrend (data = esp_ope_selection, mon_espece == "BOU")
#' }



gamm_poptrend <- function(data, mon_espece) {
  # --- Filtrage et vérifications préalables ---
  filtered_data <- data %>%
    filter(espece == mon_espece)
  
  if (nrow(filtered_data) < 2 ||
      length(unique(filtered_data$pop_id))      < 2 ||
      length(unique(filtered_data$annee))       < 2 ||
      length(unique(filtered_data$pro_libelle)) < 2) {
    return(NULL)
  }
  
  model <- try(poptrend::ptrend(
    valeur ~ trend(annee, tempRE = F, type = "smooth") + offset(log(ope_surface_calculee)) + pro_libelle + s(x, y) + s(sta_id, bs = "re"),
    data = filtered_data,
    family = nb(link = "log"),
    bootType = "hessian",
    gamModel = TRUE,
    engine = "gam"
  ),
  silent = TRUE)
  
  if (inherits(model, "try-error"))
    return(NULL)
  graph <- ggplot_trend(
    model,
    alpha = 0.05,
    ylab = "Abundance index",
    xlab = "Annee",
    trendCol = "black",
    shadeCol = "grey60",
    secDeriv = TRUE,
    plotLines = T,
    lineCol = "grey30",
    lineAlpha = 0.05
  ) +
    theme_gray() +
    ggtitle(mon_espece)
  # graph <- plot(model, main = mon_espece) 
  # check <- appraise(model$gam) +
  #   labs(caption = mon_espece)

    return(graph)
  
}

