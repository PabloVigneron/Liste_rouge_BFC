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
  formule <- valeur ~ annee + offset(log(ope_surface_calculee)) + pro_libelle + (1 | sta_id)
  
  # --- Désactivation du finalizer TMB (évite isNullPointer sur gc) ---
  tryCatch(
    TMB::openmp(n = 1, DLL = "glmmTMB"),
    error = function(e) NULL
  )
  
  # --- Wrapper robuste ---
  fit_safe <- function(expr) {
    withCallingHandlers(
      tryCatch(expr, error = function(e) NULL),
      warning = function(w) {
        msg <- conditionMessage(w)
        # Warnings FATALS → on étouffe et retourne NULL via tryCatch
        if (grepl("convergence code [^0]|isNullPointer|TMBconfig|PACKAGE",
                  msg, ignore.case = TRUE)) {
          invokeRestart("muffleWarning")
        }
        # Warnings NON FATALS → on étouffe mais le modèle continue
        if (grepl("NA/NaN|nearly unidentifiable|large eigenvalue|Rescale",
                  msg, ignore.case = TRUE)) {
          invokeRestart("muffleWarning")
        }
      }
    )
  }
  # --- Fonction de refit lme4 avec optimiseur alternatif ---
  refit_lme4 <- function(m) {
    if (!inherits(m, "glmerMod")) return(m)
    
    grad <- tryCatch(
      max(abs(m@optinfo$derivs$gradient)),
      error = function(e) Inf
    )
    
    if (!is.finite(grad) || grad <= 0.002) return(m)  # déjà OK
    
    # Liste d'optimiseurs alternatifs à essayer
    optimiseurs <- c("bobyqa", "Nelder_Mead", "nlminbwrap")
    
    for (opt in optimiseurs) {
      m_refit <- tryCatch(
        lme4::refit(m, 
                    control = lme4::glmerControl(
                      optimizer    = opt,
                      optCtrl      = list(maxfun = 2e5)
                    )
        ),
        error   = function(e) NULL,
        warning = function(w) NULL
      )
      
      if (!is.null(m_refit)) {
        grad_refit <- tryCatch(
          max(abs(m_refit@optinfo$derivs$gradient)),
          error = function(e) Inf
        )
        if (is.finite(grad_refit) && grad_refit <= 0.002) {
          message("  -> Refit lme4 OK avec optimiseur : ", opt, 
                  " | grad = ", round(grad_refit, 6))
          return(m_refit)
        }
      }
    }
    
    # Aucun optimiseur n'a amélioré : retourner le modèle original
    return(m)
  }
  # --- Fonction de validation d'un modèle ---
  # --- Fonction de validation ---
  modele_valide <- function(m) {
    if (is.null(m) || inherits(m, "try-error")) return(FALSE)
    
    # Vérification convergence glmmTMB
    if (inherits(m, "glmmTMB")) {
      conv <- m$fit$convergence
      grad <- tryCatch(
        max(abs(m$sdr$gradient.fixed)),
        error = function(e) Inf
      )
      if (!is.null(conv) && conv != 0)    return(FALSE)
      if (is.finite(grad) && grad > 0.01) return(FALSE)
    }
    
    # Vérification convergence lme4
    if (inherits(m, "glmerMod")) {
      grad <- tryCatch(
        max(abs(m@optinfo$derivs$gradient)),
        error = function(e) Inf
      )
      # Hessian dégénéré → rejet immédiat
      if (any(grepl("degenerate|negative eigenvalue",
                    m@optinfo$conv$lme4$messages))) return(FALSE)
      # Gradient trop élevé même après refit
      if (is.finite(grad) && grad > 0.1) return(FALSE)
    }
    
    coef <- tryCatch({
      if (inherits(m, "glmerMod")) summary(m)$coefficients
      else                         summary(m)$coefficients$cond
    }, error = function(e) NULL)
    
    !is.null(coef) &&
      nrow(coef) > 0 &&
      "annee" %in% rownames(coef) &&
      !is.na(coef["annee", "Estimate"]) &&
      !anyNA(coef["annee", ])
  }
  
  # --- Ajustement des 5 modèles ---
  mod_0 <- fit_safe(
    lme4::glmer.nb(formule, data = filtered_data)
  )
  mod_0 <- refit_lme4(mod_0)   # <-- refit si gradient > tolérance
  
  mod_1 <- fit_safe(local({
    glmmTMB::glmmTMB(formule, data = filtered_data,
                     family = poisson(link = "log"))
  }))
  
  mod_2 <- fit_safe(local({
    glmmTMB::glmmTMB(formule, data = filtered_data,
                     family = glmmTMB::nbinom2(link = "log"))
  }))
  
  mod_3 <- fit_safe(local({
    glmmTMB::glmmTMB(formule, data = filtered_data,
                     family    = glmmTMB::truncated_poisson(link = "log"),
                     ziformula = ~1)
  }))
  
  mod_4 <- fit_safe(local({
    glmmTMB::glmmTMB(formule, data = filtered_data,
                     family    = glmmTMB::truncated_nbinom2(link = "log"),
                     ziformula = ~1)
  }))
  
  # --- Catalogue des modèles ---
  modeles <- list(
    list(model = mod_0, family = "GLMM_Negative_Binomiale"),
    list(model = mod_1, family = "GLMM_Poisson"),
    list(model = mod_2, family = "GLMM_Negative_Binomial"),
    list(model = mod_3, family = "Hurdle_Poisson"),
    list(model = mod_4, family = "Hurdle_Negative_Binomial")
  )
  
  # --- Filtrage : ne garder que les modèles valides ---
  modeles_valides <- Filter(function(x) modele_valide(x$model), modeles)
  
  if (length(modeles_valides) == 0) {
    message("Aucun modele n'a converge pour l'espece : ", mon_espece)
    return(NULL)
  }
  
  # --- Calcul de l'AIC pour chaque modèle valide ---
  aic_values <- sapply(modeles_valides, function(x) {
    tryCatch(AIC(x$model), error = function(e) Inf)
  })
  
  # --- Sélection du meilleur modèle (AIC minimal) ---
  best_idx    <- which.min(aic_values)
  best_model  <- modeles_valides[[best_idx]]$model
  best_family <- modeles_valides[[best_idx]]$family
  best_aic    <- aic_values[[best_idx]]
  
  message(
    "Espece : ", mon_espece,
    " | Modeles valides : ", length(modeles_valides), "/5",
    " | Meilleur modele : ", best_family,
    " | AIC : ", round(best_aic, 2)
  )
  
  # --- Extraction des coefficients du meilleur modèle ---
  coef <- if (inherits(best_model, "glmerMod")) summary(best_model)$coefficients
  else                                   summary(best_model)$coefficients$cond
  
  colnames(coef) <- gsub("Pr\\(>\\|t\\|\\)", "Pr(>|z|)", colnames(coef))
  
  # --- Mise en forme du résultat ---
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
  
  return(res)
}


# zip_glmm_calcul_modele <- function(data, mon_espece) {
#   filtered_data <- data %>%
#     filter(espece == mon_espece)
#   
#   if (nrow(filtered_data) < 2 ||
#       length(unique(filtered_data$pop_id)) < 2 ||
#       length(unique(filtered_data$annee)) < 2 ||
#       length(unique(filtered_data$pro_libelle)) < 2) {
#     return(NULL)
#   }
#   
#   # Fonction pour vérifier la validité des coef
#   coef_valide <- function(coef) {
#     !is.null(coef) && nrow(coef) > 0 &&
#       "annee" %in% rownames(coef) &&
#       !is.na(coef["annee", "Estimate"]) &&
#       !is.na(coef["annee", "Pr(>|z|)"])
#   }
#   
#   # Modèle 1 : BINOMIALE NEGATIVE (lme4)
#   model  <- try(glmer.nb(
#     valeur ~ annee + offset(log(ope_surface_calculee)) + pro_libelle +(1 |
#                                                                           sta_id),
#     data = filtered_data,
#     family = poisson(link = "log")
#   ),
#   silent = TRUE)
#   family <- "Negative_Binomial"
#   coef   <- if (!inherits(model, "try-error"))
#     summary(model)$coefficients
#   else
#     NULL
#   
#   
#   
#   # Fallback modèle 2 : Hurdle Negative Binomial - truncated_nbinom2 (si try-error ou coef invalides) (glmmTMB)
#   
#   if (inherits(model, "try-error") || !coef_valide(coef)) {
#     model  <- try(glmmTMB(
#       valeur ~ annee + offset(log(ope_surface_calculee)) + pro_libelle + (1 |
#                                                                             sta_id),
#       family = truncated_nbinom2(link = "log"),
#       data  = filtered_data,
#       ziformula = ~ 1
#     ),
#     silent = TRUE)
#     
#     family <- "Hurdle_Negative_Binomial"
#     coef   <- if (!inherits(model, "try-error"))
#       summary(model)$coefficients$cond
#     else
#       NULL
#     
#     # Fallback modèle 3 : Zero Inflated Negative Binomial - nbinom2  (glmmTMB)
#     if (inherits(model, "try-error") || !coef_valide(coef)) {
#       model <- try(glmmTMB(
#         valeur ~ annee + offset(log(ope_surface_calculee)) + pro_libelle + (1 |
#                                                                               sta_id),
#         family = nbinom2(link = "log"),
#         data = filtered_data,
#         ziformula = ~ 1
#       ),
#       silent = TRUE)
#       
#       family <- "Zero_Inflated_Negative_Binomial"
#       coef <- if (!inherits(model, "try-error"))
#         summary(model)$coefficients$cond
#       else
#         NULL
#       
#       if (inherits(model, "try-error") ||
#           !coef_valide(coef))
#         return(NULL)
#     }
#   }
#     # Uniformise le nom de la colonne p-value avant le rename
#     
#     colnames(coef) <- gsub("Pr\\(>\\|t\\|\\)", "Pr(>|z|)", colnames(coef))
#     
#     res <- coef %>%
#       as.data.frame() %>%
#       rename(p_value = 'Pr(>|z|)') %>%
#       mutate(
#         sig = case_when(
#           p_value < 0.001 ~ "***",
#           p_value < 0.01  ~ "**",
#           p_value < 0.05  ~ "*",
#           TRUE            ~ "NS"
#         ),
#         esp_code_alternatif = mon_espece,
#         family = family
#       )
#     
#     return(res)
#   }
  

