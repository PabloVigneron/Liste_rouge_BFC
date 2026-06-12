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
  
  # --- Filtrage et vérifications préalables ---
  filtered_data <- data %>%
    filter(espece == mon_espece)
  
  if (nrow(filtered_data) < 2 ||
      length(unique(filtered_data$pop_id))      < 2 ||
      length(unique(filtered_data$annee))       < 2 ||
      length(unique(filtered_data$pro_libelle)) < 2) {
    return(NULL)
  }
  
  # --- Wrapper robuste (parallèle à fit_safe du ZIP_GLMM) ---
  fit_safe <- function(expr) {
    withCallingHandlers(
      tryCatch(expr, error = function(e) NULL),
      warning = function(w) {
        msg <- conditionMessage(w)
        if (grepl("convergence|iteration limit|NA/NaN|not positive definite|PIRLS",
                  msg, ignore.case = TRUE)) {
          invokeRestart("muffleWarning")
        }
      }
    )
  }
  
  # --- Fonction de validation d'un modèle ---
  modele_valide <- function(m) {
    if (is.null(m) || inherits(m, "try-error")) return(FALSE)
    
    # Pour les GAMM : vérifier que $gam et $lme existent
    if (!is.null(m$gam) && !is.null(m$lme)) {
      coef <- tryCatch(summary(m$gam)$p.coeff, error = function(e) NULL)
      if (is.null(coef) || length(coef) == 0)    return(FALSE)
      if (anyNA(coef))                            return(FALSE)
      # Vérifier que l'AIC est calculable
      aic <- tryCatch(AIC(m$lme), error = function(e) NULL)
      if (is.null(aic) || !is.finite(aic))        return(FALSE)
      return(TRUE)
    }
    
    # Pour le GAM (mod_1.3) : vérifier que le modèle a convergé
    if (inherits(m, "gam")) {
      coef <- tryCatch(summary(m)$p.coeff, error = function(e) NULL)
      if (is.null(coef) || length(coef) == 0)    return(FALSE)
      if (anyNA(coef))                            return(FALSE)
      aic <- tryCatch(AIC(m), error = function(e) NULL)
      if (is.null(aic) || !is.finite(aic))        return(FALSE)
      return(TRUE)
    }
    
    return(FALSE)
  }
  
  # --- Ajustement des 4 modèles ---
  mod_1.0 <- fit_safe(
    gamm(
      valeur ~ s(annee) + offset(log(ope_surface_calculee)) + pro_libelle,
      random = list(sta_id = ~1),
      data   = filtered_data,
      family = poisson(link = "log"),
      method = "REML"
    )
  )
  
  mod_1.1 <- fit_safe(
    gamm(
      valeur ~ s(annee) + offset(log(ope_surface_calculee)) + pro_libelle,
      random = list(sta_id = ~1),
      data   = filtered_data,
      family = poisson(link = "identity"),
      method = "REML"
    )
  )
  
  mod_1.2 <- fit_safe(
    gamm(
      valeur ~ s(annee) + offset(log(ope_surface_calculee)) + pro_libelle,
      random = list(sta_id = ~1),
      data   = filtered_data,
      family = poisson(link = "sqrt"),
      method = "REML"
    )
  )
  
  mod_1.3 <- fit_safe(
    gamm(
      valeur ~ s(annee) + offset(log(ope_surface_calculee)) + pro_libelle + s(x, y),
      random = list(sta_id = ~1),
      data   = filtered_data,
      family = poisson(link = "log"),
      method = "REML"
    )
  )
  
  # --- Catalogue des modèles ---
  modeles <- list(
    list(model = mod_1.0, name = "mod_1.0_gamm_log",          type = "gamm", family_label = "poisson_log"),
    list(model = mod_1.1, name = "mod_1.1_gamm_identity",      type = "gamm", family_label = "poisson_identity"),
    list(model = mod_1.2, name = "mod_1.2_gamm_sqrt",          type = "gamm", family_label = "poisson_sqrt"),
    list(model = mod_1.3, name = "mod_1.3_gam_log_spatial",    type = "gamm",  family_label = "poisson_log")
  )
  
  # --- Filtrage : ne garder que les modèles valides ---
  modeles_valides <- Filter(function(x) modele_valide(x$model), modeles)
  
  if (length(modeles_valides) == 0) {
    message("Aucun modele n'a converge pour l'espece : ", mon_espece)
    return(NULL)
  }
  
  # --- Calcul de l'AIC pour chaque modèle valide ---
  aic_values <- sapply(modeles_valides, function(x) {
    tryCatch(
      if (x$type == "gamm") AIC(x$model$lme) else AIC(x$model),
      error = function(e) Inf
    )
  })
  
  # --- Sélection du meilleur modèle (AIC minimal) ---
  best_idx          <- which.min(aic_values)
  best_model        <- modeles_valides[[best_idx]]$model
  best_name         <- modeles_valides[[best_idx]]$name
  best_type         <- modeles_valides[[best_idx]]$type
  best_family_label <- modeles_valides[[best_idx]]$family_label
  best_aic          <- aic_values[[best_idx]]
  
  message(
    "Espece : ", mon_espece,
    " | Modeles valides : ", length(modeles_valides), "/4",
    " | Meilleur modele : ", best_name,
    " | Family : ", best_family_label,
    " | AIC : ", round(best_aic, 2)
  )
  
  # --- Extraction des coefficients du meilleur modèle ---
  gam_obj <- if (best_type == "gamm") best_model$gam else best_model
  
  coef <- tryCatch(
    summary(gam_obj)$p.coeff,
    error = function(e) NULL
  )
  
  if (is.null(coef)) {
    message("Impossible d'extraire les coefficients pour l'espece : ", mon_espece)
    return(NULL)
  }
  
  coef_table <- tryCatch(
    summary(gam_obj)$p.table,
    error = function(e) NULL
  )
  
  # --- Mise en forme du résultat ---
  res <- coef_table %>%
    as.data.frame() %>%
    rename(p_value = `Pr(>|z|)`) %>%
    mutate(
      sig = case_when(
        p_value < 0.001 ~ "***",
        p_value < 0.01  ~ "**",
        p_value < 0.05  ~ "*",
        TRUE            ~ "NS"
      ),
      esp_code_alternatif = mon_espece,
      modele_selectionne  = best_name,
      family        = best_family_label,
      AIC                 = best_aic
    )
  
  return(res)
}

# gamm_calcul_modele <- function(data, mon_espece) {
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
#   
#   model <- try(gamm(
#     valeur ~ s(annee) + s(ope_surface_calculee)  + pro_libelle,
#     random = list(sta_id =  ~ 1),
#     data = filtered_data,
#     family = poisson,
#     method = "REML"
#   ),
#   silent = TRUE
#   )
#   
#   if (inherits(model, "try-error")) {
#     return(NULL)
#   }
#   
#   res <- summary(model$gam)$coefficients %>%
#     as.data.frame() %>%
#     rename(p_value = `Pr(>|z|)`) %>%
#     mutate(sig = case_when(
#       p_value < 0.001 ~ "***",
#       p_value < 0.01 ~ "**",
#       p_value < 0.05 ~ "*",
#       TRUE ~ ""
#     )) %>%
#     mutate(esp_code_alternatif = mon_espece)
#   
#   return(res)
# }

