
#' Appliquer zip_glmm_calcul_modele pour chaque espece et chaque periode
#'
#' @param data Dataframe contenant les donnees (toutes especes, toutes annees)
#' @param liste_periodes Liste de vecteurs c(annee_depart, annee_fin)
#' @return Dataframe combinant les resultats de toutes les especes / periodes
#' @export
zip_glmm_application_modele <- function(data,
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
      zip_glmm_calcul_modele(data = period_data, mon_espece = esp)
    }) %>%
      keep(~ !is.null(.)) %>%
      map_dfr(~ mutate(.x, row_name = rownames(.x), periode = period_label))
    
    return(results)
  }) %>%
    bind_rows()
  
  return(results_list)
}



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
#' 
zip_glmm_calcul_modele <- function(data, mon_espece) {
  
  # Filtrage des données et vérifications
  filtered_data <- data %>%
    filter(espece == mon_espece)
  
  if (nrow(filtered_data) < 2 ||
      length(unique(filtered_data$pop_id))      < 2 ||
      length(unique(filtered_data$annee))       < 2 ||
      length(unique(filtered_data$pro_libelle)) < 2) {
    return(NULL)
  }
  
  # Definition d'une formule commune à l'ensemble des modèles 
  formule <- valeur ~ annee + offset(log(ope_surface_calculee)) + pro_libelle + (1 |
                                                                                   sta_id)
  
  # Construction d'une fonction modele_valide () qui verifie la convergence des modèles, 
  # la présence de coefficient et p_value sans NA
  
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
  
  # Création d'une fonction formater_resultat () pour la mise en forme des résultats finaux
  
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
        AIC                 = best_aic, 
      )
    res
  }
  
  ####################################################################
  # Première ÉTAPE : Ajustement des 4 modèles ZI / Hurdle (glmmtmb) : 
  ####################################################################
  
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
  
  # Conserver uniquement les modèles valides 
  
  modeles_valides <- Filter(function(x) modele_valide(x$model), modeles)
  
  #########################################################
  # Deuxième ÉTAPE : Selection de modèle par AIC (minimal): 
  # Au moins 1 modèle valide : 
  #########################################################
  
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
  ########################################################
  # Troisième ÉTAPE : Si aucun modèles n'a convergé on applique 
  # un glmm négative binomiale
  #########################################################
  
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
  
  # On vérifie le ratio de dispersion avec DHARMa::testDispersion()  
  
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

# zip_glmm_calcul_modele <- function(data, mon_espece) {
#   
#   # Filtrage des données et vérifications
#   filtered_data <- data %>%
#     filter(espece == mon_espece)
#   
#   if (nrow(filtered_data) < 2 ||
#       length(unique(filtered_data$pop_id))      < 2 ||
#       length(unique(filtered_data$annee))       < 2 ||
#       length(unique(filtered_data$pro_libelle)) < 2) {
#     return(NULL)
#   }
#   
#   # Definition d'une formule commune à l'ensemble des modèles 
#   formule <- valeur ~ annee + offset(log(ope_surface_calculee)) + pro_libelle + (1 |
#                                                                                    sta_id)
#   
#   # Construction d'une fonction modele_valide () qui verifie la convergence des modèles, 
#   # la présence de coefficient et p_value sans NA
#   
#   modele_valide <- function(m) {
#     if (is.null(m) || inherits(m, "try-error"))
#       return(FALSE)
#     
#     coef <- tryCatch({
#       if (inherits(m, "glmerMod"))
#         summary(m)$coefficients
#       else
#         summary(m)$coefficients$cond
#     }, error = function(e)
#       NULL)
#     
#     ! is.null(coef) &&
#       nrow(coef) > 0 &&
#       "annee" %in% rownames(coef) &&
#       !is.na(coef["annee", "Estimate"]) &&
#       !anyNA(coef["annee", ])
#   }
#   
#   # Creation d'une fonction calculer_rate_change () qui calcul 
#   # le taux de changement des espèces sur la période à partir de prédiction des modèles
#   
#   calculer_rate_change <- function(best_model, filtered_data) {
#     
#     niveaux_cibles <- intersect(
#       c("Pêche complète à un ou plusieurs passages",
#         "Pêche partielle par points (grand milieu)"),
#       unique(as.character(filtered_data$pro_libelle))
#     )
#     
#     if (length(niveaux_cibles) == 0) return(NA_real_)
#     
#     new_data <- expand.grid(
#       annee       = c(min(filtered_data$annee), max(filtered_data$annee)),
#       pro_libelle = niveaux_cibles,
#       stringsAsFactors = FALSE
#     )
#     
#     # Variables nécessaires à predict()
#     new_data$ope_surface_calculee <- 1          # offset = log(1) = 0 -> densité
#     new_data$sta_id <- filtered_data$sta_id[1]  # valeur bidon, ignorée avec re.form = NA
#     
#     # Respecter les niveaux de facteur du modèle
#     if (is.factor(filtered_data$pro_libelle)) {
#       new_data$pro_libelle <- factor(new_data$pro_libelle,
#                                      levels = levels(filtered_data$pro_libelle))
#     }
#     
#     pred <- tryCatch(
#       predict(best_model, newdata = new_data, type = "response",
#               re.form = NA, allow.new.levels = TRUE),
#       error = function(e) {
#         message("predict() a échoué : ", conditionMessage(e))
#         rep(NA_real_, nrow(new_data))
#       }
#     )
#     new_data$prediction <- pred
#     
#     rate_change_df <- new_data %>%
#       dplyr::group_by(pro_libelle) %>%
#       dplyr::arrange(annee, .by_group = TRUE) %>%
#       dplyr::summarise(
#         p_first = dplyr::first(prediction),
#         p_last  = dplyr::last(prediction),
#         rate_change = dplyr::if_else(
#           !is.na(p_first) & p_first > 0,
#           (p_last - p_first) / p_first * 100,
#           NA_real_
#         ),
#         .groups = "drop"
#       )
#     
#     if (all(is.na(rate_change_df$rate_change))) return(NA_real_)
#     
#     mean(rate_change_df$rate_change, na.rm = TRUE)
#   }
#   
#   
#   # Création d'une fonction formater_resultat () pour la mise en forme des résultats finaux
#   
#   formater_resultat <- function(best_model, best_family, best_aic, filtered_data) {
#     coef <- if (inherits(best_model, "glmerMod"))
#       summary(best_model)$coefficients
#     else
#       summary(best_model)$coefficients$cond
#     
#     colnames(coef) <- gsub("Pr\\(>\\|t\\|\\)", "Pr(>|z|)", colnames(coef))
#     
#     rate_change_esp <- calculer_rate_change(best_model, filtered_data)
#     
#     res <- coef %>%
#       as.data.frame() %>%
#       rename(p_value = "Pr(>|z|)") %>%
#       mutate(
#         sig = case_when(
#           p_value < 0.001 ~ "***",
#           p_value < 0.01  ~ "**",
#           p_value < 0.05  ~ "*",
#           TRUE            ~ "NS"
#         ),
#         esp_code_alternatif = mon_espece,
#         family              = best_family,
#         AIC                 = best_aic, 
#         rate_change         = rate_change_esp
#       )
#     res
#   }
#   
#   ####################################################################
#   # Première ÉTAPE : Ajustement des 4 modèles ZI / Hurdle (glmmtmb) : 
#   ####################################################################
#   
#   mod_1 <- tryCatch({
#     glmmTMB::glmmTMB(formule, data = filtered_data,
#                      family    = poisson(link = "log"),
#                      ziformula = ~1)
#   }, error = function(e) NULL)
#   
#   mod_2 <- tryCatch({
#     glmmTMB::glmmTMB(formule, data = filtered_data,
#                      family    = glmmTMB::nbinom2(link = "log"),
#                      ziformula = ~1)
#   }, error = function(e) NULL)
#   
#   mod_3 <- tryCatch({
#     glmmTMB::glmmTMB(formule, data = filtered_data,
#                      family    = glmmTMB::truncated_poisson(link = "log"),
#                      ziformula = ~1)
#   }, error = function(e) NULL)
#   
#   mod_4 <- tryCatch({
#     glmmTMB::glmmTMB(formule, data = filtered_data,
#                      family    = glmmTMB::truncated_nbinom2(link = "log"),
#                      ziformula = ~1)
#   }, error = function(e) NULL)
#   
#   modeles <- list(
#     list(model = mod_1, family = "ZI_Poisson"),
#     list(model = mod_2, family = "ZI_Negative_Binomial"),
#     list(model = mod_3, family = "Hurdle_Poisson"),
#     list(model = mod_4, family = "Hurdle_Negative_Binomial")
#   )
#   
#   # Conserver uniquement les modèles valides 
#   
#   modeles_valides <- Filter(function(x) modele_valide(x$model), modeles)
#   
#   #########################################################
#   # Deuxième ÉTAPE : Selection de modèle par AIC (minimal): 
#   # Au moins 1 modèle valide : 
#   #########################################################
#   
#   if (length(modeles_valides) > 0) {
#     aic_values <- sapply(modeles_valides, function(x) {
#       tryCatch(
#         AIC(x$model),
#         error = function(e)
#           Inf
#       )
#     })
#     
#     best_idx    <- which.min(aic_values)
#     best_model  <- modeles_valides[[best_idx]]$model
#     best_family <- modeles_valides[[best_idx]]$family
#     best_aic    <- aic_values[[best_idx]]
#     
#     message(
#       "Espece : ",
#       mon_espece,
#       " | Modeles ZI/hurdle valides : ",
#       length(modeles_valides),
#       "/4",
#       " | Meilleur modele : ",
#       best_family,
#       " | AIC : ",
#       round(best_aic, 2)
#     )
#     
#     return(formater_resultat(best_model, best_family, best_aic, filtered_data))
#   }
#   ########################################################
#   # Troisième ÉTAPE : Si aucun modèles n'a convergé on applique 
#   # un glmm négative binomiale
#   #########################################################
#   
#   message(
#     "Aucun des 4 modeles ZI/hurdle n'a converge pour l'espece : ",
#     mon_espece,
#     " -> tentative du modele Negative Binomiale (glmmTMB::glmmTMB)"
#   )
#   
#   mod_nb <- tryCatch({
#     glmmTMB::glmmTMB(formule, data = filtered_data, family = glmmTMB::nbinom2)
#   }, error = function(e)
#     NULL)
#   
#   if (is.null(mod_nb) || !modele_valide(mod_nb)) {
#     message("Le modele Negative Binomiale n'a pas converge pour l'espece : ",
#             mon_espece)
#     return(NULL)
#   }
#   
#   # On vérifie le ratio de dispersion avec DHARMa::testDispersion()  
#   
#   ratio_dispersion <- tryCatch({
#     sim_res <- DHARMa::simulateResiduals(mod_nb, plot = FALSE)
#     test_disp <- DHARMa::testDispersion(sim_res, plot = FALSE)
#     unname(test_disp$statistic)
#   }, error = function(e)
#     NA)
#   
#   if (is.na(ratio_dispersion) ||
#       ratio_dispersion < 0.5 ||
#       ratio_dispersion > 1.5) {
#     message(
#       "Modele Negative Binomiale rejete pour surdispersion/sousdispersion (ratio = ",
#       round(ratio_dispersion, 3),
#       ") pour l'espece : ",
#       mon_espece
#     )
#     return(NULL)
#   }
#   
#   message(
#     "Espece : ",
#     mon_espece,
#     " | Modele retenu : GLMM_Negative_Binomiale",
#     " | Ratio de dispersion : ",
#     round(ratio_dispersion, 3),
#     " | AIC : ",
#     round(AIC(mod_nb), 2)
#   )
#   
#   formater_resultat(mod_nb, "GLMM_Negative_Binomiale", AIC(mod_nb), filtered_data)
# }