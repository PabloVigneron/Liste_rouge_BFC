#' Calculer les résultats de pourcentage de changement des espèces modèles GAMM avec poptrend pour un df et une espèce choisit
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
#' mod <- gamm_poptrend_change(ope_effectif_glmm_abs, liste_periodes = list(c(2007, 2025)))
#' }

gamm_poptrend_change <- function(data,
                                    liste_periodes) {
  
  results_list <- map(liste_periodes, function(period) {
    mon_annee_depart <- period[1]
    mon_annee_fin    <- period[2]
    
    # Filtrer les donnees pour la periode courante
    period_data <- data %>%
      filter(annee >= mon_annee_depart & annee <= mon_annee_fin)
    
    period_label <- paste0(mon_annee_depart, "-", mon_annee_fin)
    
    # Liste des especes presentes sur la periode
    especes <- period_data %>%
      pull(espece) %>%
      unique()
    
    results <- map(especes, function(esp) {
      gamm_poptrend_change_esp (data = period_data, mon_espece = esp, start = mon_annee_depart, end = mon_annee_fin, alpha = 0.05)  
      
    }) %>%
      keep(~ !is.null(.)) %>%
      map_dfr(~ mutate(.x, periode = period_label))
    
    return(results)
  }) %>%
    bind_rows()
  
  return(results_list)
}


gamm_poptrend_change_esp <- function(data, mon_espece, start, end, alpha) {
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
  
  # Calcul du pourcentage de changement 
  change <- poptrend::change(model, start = start, end = end, alpha = alpha)
  
  # Df de sortie 
  result <- data.frame(
    espece = mon_espece,
    rate_change_gamm = change$percentChange,
    IC_inf = paste0(round(change$CI[1], 2)), 
    IC_sup = paste0(round(change$CI[2], 2)),
    # IC = paste0("[", round(change$CI[1], 2), "; ", round(change$CI[2], 2), "]"),
    # periode = paste0(change$start, "-", change$end),
    stringsAsFactors = F
  )
  return(result)
  
}
