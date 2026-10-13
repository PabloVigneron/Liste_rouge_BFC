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