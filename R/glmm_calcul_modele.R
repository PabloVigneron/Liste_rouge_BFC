#' Calculer les résultats des modèles GLMM pour un df et une espèce choisit
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
  formule <- valeur ~ annee + pro_libelle + (1 | sta_id)
  
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
  
  # Creation d'une fonction calculer_rate_change () qui calcul 
  # le taux de changement des espèces sur la période à partir de prédiction des modèles
  
  calculer_rate_change <- function(best_model, filtered_data) {
    # Conserver uniquement les modalités de pro_libelle réellement présentes
    # dans les données ajustées, pour éviter une erreur de predict()
    # sur un niveau de facteur inconnu du modèle.
    niveaux_cibles <- intersect(
      c(
        "Pêche complète à un ou plusieurs passages",
        "Pêche partielle par points (grand milieu)"
      ),
      unique(as.character(filtered_data$pro_libelle))
    )
    
    if (length(niveaux_cibles) == 0)
      return(NA_real_)
    
    new_data <- expand.grid(
      annee = c(min(filtered_data$annee), max(filtered_data$annee)),
      pro_libelle = niveaux_cibles,
      stringsAsFactors = FALSE
    )
    
    new_data$prediction <- tryCatch({
      predict(best_model, newdata = new_data, type = "response", re.form = NA)
    }, error = function(e) rep(NA_real_, nrow(new_data)))
    
    rate_change_df <- new_data %>%
      group_by(pro_libelle) %>%
      arrange(annee, .by_group = TRUE) %>%
      summarise(
        p_first = first(prediction),
        p_last  = last(prediction),
        rate_change = (p_last - p_first) / p_first * 100,
        .groups = "drop"
      )
    
    mean(rate_change_df$rate_change, na.rm = TRUE)
  }
  
  # Création d'une fonction formater_resultat () pour la mise en forme des résultats finaux
  
  formater_resultat <- function(best_model, best_family, best_aic, filtered_data) {
    coef <- if (inherits(best_model, "glmerMod"))
      summary(best_model)$coefficients
    else
      summary(best_model)$coefficients$cond
    
    colnames(coef) <- gsub("Pr\\(>\\|t\\|\\)", "Pr(>|z|)", colnames(coef))
    
    rate_change_esp <- calculer_rate_change(best_model, filtered_data)
    
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
        rate_change         = rate_change_esp
      )
    res
  }
  
  #####################################################
  # Première ÉTAPE : Ajustement des 4 modèles (lme4) : 
  #####################################################
  
  mod_1 <- tryCatch({
    lme4::glmer(
      valeur ~ annee  + pro_libelle + (1 | sta_id),
      data = filtered_data,
      family = binomial(link = "logit")
    )
  }, error = function(e) NULL)
  
  mod_2 <- tryCatch({
    lme4::glmer(
      valeur ~ annee  + pro_libelle + (1 | sta_id),
      data = filtered_data,
      family = binomial(link = "probit")
    )
  }, error = function(e) NULL)
  
  mod_3 <- tryCatch({
    lme4::glmer(
      valeur ~ annee  + pro_libelle + (1 | sta_id),
      data = filtered_data,
      family = binomial(link = "cauchit")
    )
  }, error = function(e) NULL)
  
  mod_4 <- tryCatch({
    lme4::glmer(
      valeur ~ annee  + pro_libelle + (1 | sta_id),
      data = filtered_data,
      family = binomial(link = "cloglog")
    )
  }, error = function(e) NULL)
  
  modeles <- list(
    list(model = mod_1, family = "Logit"),
    list(model = mod_2, family = "Probit"),
    list(model = mod_3, family = "Cauchit"),
    list(model = mod_4, family = "Cloglog")
  )
  
  # Conserver uniquement les modèles valides 
  modeles_valides <- Filter(function(x) modele_valide(x$model), modeles)
  
  
  ####################################################################################
  # Deuxième ÉTAPE : Selection de modèle par AIC (minimal): Au moins 1 modèle valide : 
  ####################################################################################
  
  if (length(modeles_valides) > 0) {
    aic_values <- sapply(modeles_valides, function(x) {
      tryCatch(AIC(x$model), error = function(e) Inf)
    })
    
    best_idx    <- which.min(aic_values)
    best_model  <- modeles_valides[[best_idx]]$model
    best_family <- modeles_valides[[best_idx]]$family
    best_aic    <- aic_values[[best_idx]]
    
    message(
      "Espece : ", mon_espece,
      " | Modeles GLMM valides : ", length(modeles_valides), "/4",
      " | Meilleur modele : ", best_family,
      " | AIC : ", round(best_aic, 2)
    )
    
    return(formater_resultat(best_model, best_family, best_aic, filtered_data))
  }
  # Si aucun modèle n'a convergé : return null
  
  message("Aucun des 4 modeles GLMM n'a converge pour l'espece : ", mon_espece)
  return(NULL)
}