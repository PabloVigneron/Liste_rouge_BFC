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
  # --- Filtrage et vérifications préalables ---
  filtered_data <- data %>%
    filter(espece == mon_espece)
  
  if (nrow(filtered_data) < 2 ||
      length(unique(filtered_data$pop_id))      < 2 ||
      length(unique(filtered_data$annee))       < 2 ||
      length(unique(filtered_data$pro_libelle)) < 2) {
    return(NULL)
  }
  
  # --- Formule commune ---
  formule <- valeur ~ annee + offset(log(ope_surface_calculee)) + pro_libelle + (1 |
                                                                                          sta_id)
  
  # --- Fonction de validation d'un modèle (convergence + coef/pvalue non NA) ---
  modele_valide <- function(m) {
    if (is.null(m) || inherits(m, "try-error"))
      return(FALSE)
    
    coef <- tryCatch({
      if (inherits(m, "glmerMod"))
        summary(m)$coefficients
      else
        summary(m)$coefficients$cond
    }, error = function(e)
      NULL)
    
    ! is.null(coef) &&
      nrow(coef) > 0 &&
      "annee" %in% rownames(coef) &&
      !is.na(coef["annee", "Estimate"]) &&
      !anyNA(coef["annee", ])
  }
  
  # --- Fonction utilitaire pour mettre en forme le résultat final ---
  formater_resultat <- function(best_model, best_family, best_aic) {
    coef <- if (inherits(best_model, "glmerMod"))
      summary(best_model)$coefficients
    else
      summary(best_model)$coefficients$cond
    
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
        family              = best_family,
        AIC                 = best_aic
      )
    res
  }
  
  # ============================================================
  # ÉTAPE 1 : Ajustement des 4 modèles ZI / hurdle (glmmTMB)
  # ============================================================
  mod_1 <- tryCatch({
    glmmTMB::glmmTMB(formule, data = filtered_data,
                     family    = poisson(link = "log"),
                     ziformula = ~1)
  }, error = function(e) NULL)
  
  mod_2 <- tryCatch({
    glmmTMB::glmmTMB(formule, data = filtered_data,
                     family    = glmmTMB::nbinom2(link = "log"),
                     ziformula = ~1)
  }, error = function(e) NULL)
  
  mod_3 <- tryCatch({
    glmmTMB::glmmTMB(formule, data = filtered_data,
                     family    = glmmTMB::truncated_poisson(link = "log"),
                     ziformula = ~1)
  }, error = function(e) NULL)
  
  mod_4 <- tryCatch({
    glmmTMB::glmmTMB(formule, data = filtered_data,
                     family    = glmmTMB::truncated_nbinom2(link = "log"),
                     ziformula = ~1)
  }, error = function(e) NULL)
  
  modeles <- list(
    list(model = mod_1, family = "ZI_Poisson"),
    list(model = mod_2, family = "ZI_Negative_Binomial"),
    list(model = mod_3, family = "Hurdle_Poisson"),
    list(model = mod_4, family = "Hurdle_Negative_Binomial")
  )
  
  # --- Filtrage : ne garder que les modèles valides ---
  modeles_valides <- Filter(function(x)
    modele_valide(x$model), modeles)
  
  # ============================================================
  # ÉTAPE 1bis : Au moins un modèle valide -> sélection par AIC minimal
  # ============================================================
  if (length(modeles_valides) > 0) {
    aic_values <- sapply(modeles_valides, function(x) {
      tryCatch(
        AIC(x$model),
        error = function(e)
          Inf
      )
    })
    
    best_idx    <- which.min(aic_values)
    best_model  <- modeles_valides[[best_idx]]$model
    best_family <- modeles_valides[[best_idx]]$family
    best_aic    <- aic_values[[best_idx]]
    
    message(
      "Espece : ",
      mon_espece,
      " | Modeles ZI/hurdle valides : ",
      length(modeles_valides),
      "/4",
      " | Meilleur modele : ",
      best_family,
      " | AIC : ",
      round(best_aic, 2)
    )
    
    return(formater_resultat(best_model, best_family, best_aic))
  }
  
  # ============================================================
  # ÉTAPE 2 : Aucun des 4 modèles n'a convergé -> fallback glmer.nb
  # On vérifie ensuite la dispersion avec DHARMa::testDispersion()
  # ============================================================
  message(
    "Aucun des 4 modeles ZI/hurdle n'a converge pour l'espece : ",
    mon_espece,
    " -> tentative du modele Negative Binomiale (glmmTMB::glmmTMB)"
  )
  
  mod_nb <- tryCatch({
    glmmTMB::glmmTMB(formule, data = filtered_data, family = glmmTMB::nbinom2)
  }, error = function(e)
    NULL)
  
  if (is.null(mod_nb) || !modele_valide(mod_nb)) {
    message("Le modele Negative Binomiale n'a pas converge pour l'espece : ",
            mon_espece)
    return(NULL)
  }
  
  # --- Vérification du ratio de dispersion via DHARMa ---
  ratio_dispersion <- tryCatch({
    sim_res <- DHARMa::simulateResiduals(mod_nb, plot = FALSE)
    test_disp <- DHARMa::testDispersion(sim_res, plot = FALSE)
    unname(test_disp$statistic)
  }, error = function(e)
    NA)
  
  if (is.na(ratio_dispersion) ||
      ratio_dispersion < 0.5 ||
      ratio_dispersion > 1.5) {
    message(
      "Modele Negative Binomiale rejete pour surdispersion/sousdispersion (ratio = ",
      round(ratio_dispersion, 3),
      ") pour l'espece : ",
      mon_espece
    )
    return(NULL)
  }
  
  message(
    "Espece : ",
    mon_espece,
    " | Modele retenu : GLMM_Negative_Binomiale",
    " | Ratio de dispersion : ",
    round(ratio_dispersion, 3),
    " | AIC : ",
    round(AIC(mod_nb), 2)
  )
  
  formater_resultat(mod_nb, "GLMM_Negative_Binomiale", AIC(mod_nb))
}