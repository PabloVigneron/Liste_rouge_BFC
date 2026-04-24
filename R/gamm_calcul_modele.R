#' Calculer les résultats des modèles GAMMs pour un df et une espèce choisit
#'
#' @param df Dataframe contenant les données
#' @param mon_espece esp_code_alternatif : le code espèce de l'espèce ciblé par l'analyse
#' @return Dataframe avec resultats 
#' @export 
#' 
#' @importClassesFrom dplyr; mutate filter rename ; mgcv, ; base summary  nrow rename length unique 
#'
#' @examples
#' \dontrun{
#' mod <- gamm_calcul_modele (df = esp_ope_selection, mon_espece == "BOU")
#' }


gamm_calcul_modele <- function(data, mon_espece) {
  filtered_data <- data %>%
    filter(espece == mon_espece)
  
  if (nrow(filtered_data) < 2 ||
      length(unique(filtered_data$pop_id)) < 2 ||
      length(unique(filtered_data$annee)) < 2 ||
      length(unique(filtered_data$pro_libelle)) < 2) {
    return(NULL)
  }
  
  
  model <- try(gamm(
    valeur ~ s(annee) + s(ope_surface_calculee)  + pro_libelle,
    random = list(sta_id =  ~ 1),
    data = filtered_data,
    family = poisson,
    method = "REML"
  ),
  silent = TRUE
  )
  
  if (inherits(model, "try-error")) {
    return(NULL)
  }
  
  res <- summary(model$gam)$coefficients %>%
    as.data.frame() %>%
    rename(p_value = `Pr(>|z|)`) %>%
    mutate(sig = case_when(
      p_value < 0.001 ~ "***",
      p_value < 0.01 ~ "**",
      p_value < 0.05 ~ "*",
      TRUE ~ ""
    )) %>%
    mutate(esp_code_alternatif = mon_espece)
  
  return(res)
}

