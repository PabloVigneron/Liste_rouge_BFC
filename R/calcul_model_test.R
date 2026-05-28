# ==============================================================================
# zip_glmm_calcul_modele()
# Ajuste le meilleur modèle disponible et calcule les diagnostics associés.
# Retourne une liste : $res (df coefficients), $plot_resid, $overdisp, $family, $espece
# ==============================================================================

#' @importFrom dplyr filter mutate rename case_when
#' @importFrom lme4 glmer.nb
#' @importFrom glmmTMB glmmTMB truncated_nbinom2 nbinom2
#' @importFrom RVAidememoire plotresid overdisp.glmer
#' @importFrom ggpubr as.ggplot

# ==============================================================================
# zip_glmm_calcul_modele() — version corrigée
# ==============================================================================
zip_glmm_calcul_modele <- function(data, mon_espece) {
  
  filtered_data <- data %>%
    filter(espece == mon_espece)
  
  if (nrow(filtered_data) < 2 ||
      length(unique(filtered_data$pop_id))      < 2 ||
      length(unique(filtered_data$annee))        < 2 ||
      length(unique(filtered_data$pro_libelle)) < 2) {
    return(NULL)
  }
  
  coef_valide <- function(coef) {
    !is.null(coef) &&
      nrow(coef) > 0 &&
      "annee" %in% rownames(coef) &&
      !is.na(coef["annee", "Estimate"]) &&
      !is.na(coef["annee", "Pr(>|z|)"])
  }
  
  # --- Diagnostics DHARMa (fonctionne sur lme4 ET glmmTMB) ---
  safe_dharma_diag <- function(mod, titre) {
    tryCatch({
      sim <- simulateResiduals(mod, n = 500, plot = FALSE)
      
      # Plot résidus → ggplot
      p <- as.ggplot(function() {
        plot(sim, main = titre)
      })
      
      # Test sur-dispersion
      disp <- testDispersion(sim, plot = FALSE)
      
      list(plot = p, overdisp = disp)
      
    }, error = function(e) {
      list(plot = NULL, overdisp = NULL)
    })
  }
  
  plot_resid <- NULL
  overdisp   <- NULL
  
  # ============================================================
  # Modèle 1 : Binomiale Négative (lme4 / glmer.nb)
  # ✅ family = poisson() supprimé
  # ============================================================
  model <- try(
    glmer.nb(
      valeur ~ annee + offset(log(ope_surface_calculee)) + pro_libelle + (1 | sta_id),
      data = filtered_data
    ),
    silent = TRUE
  )
  family <- "Negative_Binomial"
  coef   <- if (!inherits(model, "try-error")) summary(model)$coefficients else NULL
  
  if (!inherits(model, "try-error") && coef_valide(coef)) {
    diag       <- safe_dharma_diag(model, paste0(mon_espece, " | ", family))
    plot_resid <- diag$plot
    overdisp   <- diag$overdisp
  }
  
  # ============================================================
  # Fallback Modèle 2 : Hurdle Negative Binomial (glmmTMB)
  # ============================================================
  if (inherits(model, "try-error") || !coef_valide(coef)) {
    
    model <- try(
      glmmTMB(
        valeur ~ annee + offset(log(ope_surface_calculee)) + pro_libelle + (1 | sta_id),
        family    = truncated_nbinom2(link = "log"),
        ziformula = ~1,
        data      = filtered_data
      ),
      silent = TRUE
    )
    family <- "Hurdle_Negative_Binomial"
    coef   <- if (!inherits(model, "try-error")) summary(model)$coefficients$cond else NULL
    
    if (!inherits(model, "try-error") && coef_valide(coef)) {
      diag       <- safe_dharma_diag(model, paste0(mon_espece, " | ", family))
      plot_resid <- diag$plot
      overdisp   <- diag$overdisp
    }
    
    # ============================================================
    # Fallback Modèle 3 : Zero-Inflated Negative Binomial (glmmTMB)
    # ============================================================
    if (inherits(model, "try-error") || !coef_valide(coef)) {
      
      model <- try(
        glmmTMB(
          valeur ~ annee + offset(log(ope_surface_calculee)) + pro_libelle + (1 | sta_id),
          family    = nbinom2(link = "log"),
          ziformula = ~1,
          data      = filtered_data
        ),
        silent = TRUE
      )
      family <- "Zero_Inflated_Negative_Binomial"
      coef   <- if (!inherits(model, "try-error")) summary(model)$coefficients$cond else NULL
      
      if (!inherits(model, "try-error") && coef_valide(coef)) {
        diag       <- safe_dharma_diag(model, paste0(mon_espece, " | ", family))
        plot_resid <- diag$plot
        overdisp   <- diag$overdisp
      }
      
      if (inherits(model, "try-error") || !coef_valide(coef)) return(NULL)
    }
  }
  
  # --- Tableau coefficients ---
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
      family              = family
    )
  
  return(list(
    res        = res,
    plot_resid = plot_resid,
    overdisp   = overdisp,
    family     = family,
    espece     = mon_espece
  ))
}
