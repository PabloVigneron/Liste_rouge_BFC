#' Calculer les résultats des modèles ZIP_GLMM pour un df et une espèce choisit
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
#' mod <- zip_glmm_calcul_modele (data = esp_ope_selection, mon_espece == "BOU")
#' }


zip_glmm_calcul_modele <- function(data, mon_espece) {
  filtered_data <- data %>%
    filter(espece == mon_espece)
  
  if (nrow(filtered_data) < 2 ||
      length(unique(filtered_data$pop_id)) < 2 ||
      length(unique(filtered_data$annee)) < 2 ||
      length(unique(filtered_data$pro_libelle)) < 2) {
    return(NULL)
  }
  
  # Fonction pour vérifier la validité des coef
  coef_valide <- function(coef) {
    !is.null(coef) && nrow(coef) > 0 &&
      "annee" %in% rownames(coef) &&
      !is.na(coef["annee", "Estimate"]) &&
      !is.na(coef["annee", "Pr(>|z|)"])
  }
  
  # Modèle 1 : truncated_nbinom2
  model  <- try(glmmTMB(
    valeur ~ annee + offset(log(ope_surface_calculee)) + pro_libelle + (1 |
                                                                          sta_id),
    family = truncated_nbinom2(link = "log"),
    data  = filtered_data,
    ziformula = ~ 1
  ),
  silent = TRUE)
  
  family <- "truncated_nbinom2"
  coef   <- if (!inherits(model, "try-error"))
    summary(model)$coefficients$cond
  else
    NULL
  
  # Fallback modèle 2 : nbinom2 si try-error ou coef invalides
  if (inherits(model, "try-error") || !coef_valide(coef)) {
    model <- try(glmmTMB(
      valeur ~ annee + offset(log(ope_surface_calculee)) + pro_libelle + (1 |
                                                                            sta_id),
      family = nbinom2(link = "log"),
      data = filtered_data,
      ziformula = ~ 1
    ),
    silent = TRUE)
    
    family <- "nbinom2"
    coef <- if (!inherits(model, "try-error"))
      summary(model)$coefficients$cond
    else
      NULL
    
    if (inherits(model, "try-error") ||
        !coef_valide(coef))
      return(NULL)
  }
  
  res <- coef %>%
    as.data.frame() %>%
    rename(p_value = 'Pr(>|z|)') %>%
    mutate(
      sig = case_when(
        p_value < 0.001 ~ "***",
        p_value < 0.01  ~ "**",
        p_value < 0.05  ~ "*",
        TRUE            ~ "NS"
      ),
      esp_code_alternatif = mon_espece,
      family = family
    )
  
  return(res)
}