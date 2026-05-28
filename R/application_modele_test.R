# ==============================================================================
# zip_glmm_application_modele()
# Applique zip_glmm_calcul_modele() sur toutes les espèces × périodes.
# Retourne : $results_table  (df global)
#            $combined_plot  (ggarrange de tous les plotresid)
#            $overdisp_list  (liste des tests de sur-dispersion)
# ==============================================================================

#' @importFrom purrr map map_dfr keep pmap
#' @importFrom dplyr select distinct filter mutate bind_rows
#' @importFrom ggpubr ggarrange annotate_figure text_grob ggplotify

# ==============================================================================
# zip_glmm_application_modele() — version corrigée
# ==============================================================================
zip_glmm_application_modele <- function(data, liste_periodes) {
  
  results_list <- map(liste_periodes, function(period) {
    
    mon_annee_depart <- period[1]   # ✅ fonctionne si period = c(2007, 2025)
    mon_annee_fin    <- period[2]
    period_label     <- paste0(mon_annee_depart, "-", mon_annee_fin)
    
    period_data <- data %>%
      filter(annee >= mon_annee_depart & annee <= mon_annee_fin)
    
    especes <- period_data %>%
      select(espece) %>%
      distinct() %>%
      pull(espece)
    
    raw_results <- map(especes, function(esp) {
      zip_glmm_calcul_modele(data = period_data, mon_espece = esp)
    }) %>%
      keep(~ !is.null(.))
    
    # --- Tableau coefficients ---
    results_table <- raw_results %>%
      map_dfr(function(r) {
        r$res %>% mutate(row_name = rownames(r$res), periode = period_label)
      })
    
    # --- Plots résidus ---
    plots_list <- raw_results %>%
      map(function(r) {
        if (!is.null(r$plot_resid)) {
          r$plot_resid +
            ggplot2::labs(
              title    = paste0(r$espece, "  |  ", r$family),
              subtitle = period_label
            ) +
            ggplot2::theme(
              plot.title    = ggplot2::element_text(size = 10, face = "bold", color = "darkred"),
              plot.subtitle = ggplot2::element_text(size = 8)
            )
        } else NULL
      }) %>%
      keep(~ !is.null(.))
    
    combined_plot <- if (length(plots_list) > 0) {
      n_col <- min(3L, length(plots_list))
      n_row <- ceiling(length(plots_list) / n_col)
      ggarrange(plotlist = plots_list, ncol = n_col, nrow = n_row, align = "hv") %>%
        annotate_figure(
          top = text_grob(
            paste0("Diagnostics des résidus — Période : ", period_label),
            face = "bold", size = 13, color = "darkred"
          )
        )
    } else NULL
    
    # --- Liste overdisp avec métadonnées ✅ ---
    overdisp_list <- raw_results %>%
      keep(~ !is.null(.x$overdisp)) %>%
      map(~ list(
        espece   = .x$espece,
        famille  = .x$family,
        periode  = period_label,
        overdisp = .x$overdisp
      )) %>%
      setNames(map_chr(., ~ paste0(.x$espece, "_", .x$periode)))  # nommage pour accès facile
    
    return(list(
      periode       = period_label,
      results_table = results_table,
      combined_plot = combined_plot,
      overdisp_list = overdisp_list
    ))
  })
  
  # --- Agrégation finale ---
  final_table  <- map(results_list, "results_table") %>% bind_rows()
  all_overdisp <- map(results_list, "overdisp_list") %>% unlist(recursive = FALSE)
  all_plots    <- map(results_list, "combined_plot") %>% keep(~ !is.null(.))
  
  final_plot <- if (length(all_plots) == 1L) {
    all_plots[[1]]
  } else if (length(all_plots) > 1L) {
    ggarrange(plotlist = all_plots, ncol = 1L)
  } else NULL
  
  return(list(
    results_table = final_table,
    combined_plot = final_plot,
    overdisp      = all_overdisp
  ))
}