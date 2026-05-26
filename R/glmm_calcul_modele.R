#' Calculer les résultats des modèles ZIP_GLMM pour un df et une espèce choisit
#'
#' @param df Dataframe contenant les données
#' @param mon_espece esp_code_alternatif : le code espèce de l'espèce ciblé par l'analyse
#' @return Dataframe avec resultats 
#' @export 
#' 
#' @importClassesFrom lmerTest dplyr; mutate filter rename ;lme4, glmer; base summary  nrow rename length unique ; glmmTMB
#'
#' @examples
#' \dontrun{
#' mod <- glmm_calcul_modele (data = esp_ope_selection, mon_espece == "BOU")
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
  
  # Fonction pour vérifier la validité des coef
  # Accepte Pr(>|t|) ET Pr(>|z|)
  coef_valide <- function(coef) {
    p_col <- intersect(colnames(coef), c("Pr(>|t|)", "Pr(>|z|)"))
    !is.null(coef) &&
      nrow(coef) > 0 &&
      "annee" %in% rownames(coef) &&
      length(p_col) > 0 &&
      !is.na(coef["annee", "Estimate"]) &&
      !is.na(coef["annee", p_col])
  }
  
  # Modèle : GLMM Gaussien (lmerTest)
  model <- try(lmerTest::lmer(
    valeur ~ annee + offset(log(ope_surface_calculee)) + pro_libelle + (1 | sta_id),
    data = filtered_data
  ), silent = TRUE)
  
  family <- "Gaussian"
  coef   <- if (!inherits(model, "try-error"))
    summary(model)$coefficients
  else
    NULL
  
  if (inherits(model, "try-error") || !coef_valide(coef))
    return(NULL)
  
  # Uniformise AVANT de construire le résultat
  colnames(coef) <- gsub("Pr\\(>\\|t\\|\\)", "Pr(>|z|)", colnames(coef))
  
  res <- coef %>%
    as.data.frame() %>%
    rename(p_value = "Pr(>|z|)") %>%
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