#' Calculer les résultats des modèles GLMM pour un df et une espèce choisit
#'
#' @param df Dataframe contenant les données
#' @param mon_espece esp_code_alternatif : le code espèce de l'espèce ciblé par l'analyse
#' @return Dataframe avec resultats 
#' @export 
#' 
#' @importClassesFrom dplyr; mutate filter rename ;lme4, glmer; base summary  nrow rename length unique 
#'
#' @examples
#' \dontrun{
#' mod <- glmm_calcul_modele (df = esp_ope_selection, mon_espece == "BOU")
#' }

glmm_calcul_modele <- function(data, mon_espece) {
  filtered_data <- data %>%
    filter(espece == mon_espece)
  
  if (nrow(filtered_data) < 2 ||
      length(unique(filtered_data$pop_id)) < 2 ||
      length(unique(filtered_data$annee)) < 2 ||
      length(unique(filtered_data$pro_libelle)) < 2) {
    return(NULL)
  }
  
  
  model <- try(glmer(
    valeur ~ annee +
      ope_surface_calculee +
      pro_libelle +
      (1 | pop_id), 
      data = filtered_data,
    family = poisson(link = "log"),
  ),
  silent = TRUE)
  
  if (inherits(model, "try-error")) {
    return(NULL)
  }
  
  res <- summary(model)$coefficients %>%
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

