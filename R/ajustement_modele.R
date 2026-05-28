#' Determiner si les modeles sont bien ajusté aux données
#'
#' @param model objet contenant le modele estimé  
#' @param esp esp_code_alternatif, correspond au code de l'espèce ciblée

#' @return 
#' @export 
#' 
#' @importClassesFrom RVAidememoire, dplyr , lmerTest, glmmTMB, lme4, ggpubr
#'
#' @examples
#' \dontrun{
#' result <- ajustement_model(model = esp_ope_selection)



ajustement_model <- function(model, esp) {
  
  # Condition 1 : Si GLMM lme4 Loi de Poisson 
  if(family == "poisson" )
  ## a) Indépendance entre les résidus du modèle et les valeurs prédites
  res_model <- as.ggplot( ~ plotresid(model))
  
  ## b) homoscédasticité/Absence de sur-dispersion des résidus
  res_disp_model <- overdisp.glmer(model)
  
  # Condition 2 : Si GLMM glmmTMB Loi de Poisson  
  if(family == "poisson")
  ## a) Indépendance entre les résidus du modèle et les valeurs prédites
  res_model <- as.ggplot( ~ plotresid(model))  
 
   # Condition 3 : si GLMM ou LMM lme4 Loi normale 
  if(family == "gaussian")
  ## a) Indépendance entre les résidus du modèle et les valeurs prédites / homoscédasticité / normalité
  res_model <- as.ggplot( ~ plotresid(model))
  
  # Condition 4 : Si GLMM binomiale négative 
  if(family == "poisson")
  
  # Conversion en format ggplot
  res <- ggarrange(res_model,
    ncol = 3,
    nrow = 2,
    labels = family,
      font.label = list(
        size = 13,
        color = "darkred",
        face = "bold",
        family = NULL
      ),
    hjust = -0.1,
    vjust = 1.6,
    align = c("none", "h", "v", "hv")
  )
return(res)
return(res_disp_model)
}

# Exemple 
model <- lmer(valeur ~ annee + offset(log(ope_surface_calculee)) + pro_libelle + (1 |
                                                                                     sta_id), data = filtered_data)

ajustement_model(model, esp)