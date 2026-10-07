#' Fonction pour compiler plusieurs espèces avec la fonction time_series_chart_sp() 
#' qui permet de réaliser un graphique des indicateurs régionaux des espèces
#' 
#' @param data Dataframe contenant les donnees (toutes especes)
#' @param mes_especes L'espèce à traiter selon le code espèce poisson SANDRE de l'OFB
#' @return ggplot 
#' @export 
#' 
#' @examples
#' \dontrun{
#' graph <- time_series_charts_sp (data = reg_indicateur, mes_especes == c("BRO","TRF))
#' }


time_series_charts_sp <- function(data, mes_especes, ...) {
  # Listes des espèces demandées
  especes <- intersect(mes_especes, unique(data$espece))
  
  # Avertissement si certaines espèces sont absentes du df
  esp_manquantes <- setdiff(mes_especes, especes)
  if(length(esp_manquantes) > 0){
    warning("Espèces absentes du dataframe : ", paste(esp_manquantes, collapse = ", "))
  }
  
  # Un graphique par espèce :
  results <- map(especes, function(esp) {
    time_series_chart_sp (data = data, mon_espece = esp, ...)
  }) %>%
    set_names(especes) %>%
    keep( ~ !is.null(.))
  return(results)
}


#' Fonction qui permet de réaliser un graphique des indicateurs régionaux des espèces
#' 
#' @param data Dataframe contenant les donnees (toutes especes)
#' @param mon_espece L'espèce à traiterselon le code espèce poisson SANDRE de l'OFB
#' @param mes_indicateurs Les indicateurs que l'on souhaite afficher sur le graphique
#' @return ggplot 
#' @export 
#' 
#' @examples
#' \dontrun{
#' graph <- distribution_map_sp (data = reg_indicateur, mon_espece == "BRO")
#' }

time_series_chart_sp <- function (data, mon_espece, mes_indicateurs)
{
  
  # Filtrage des data des indicateurs sélectionnés  
  filtered_data <- data %>% 
    filter(stade == "ind",indicateur %in% mes_indicateurs) %>% 
  # Filtrage des data des espèces sélectionnés
    filter(espece == mon_espece)
  
  if(nrow(filtered_data) == 0) return(NULL)
  # Renommer les labels des indicateurs régionaux
  indicateur_labels <- c(
    "densite_surface" = "Densité surfacique",
    "effectif_total" = "Effectif total",
    "pourcentage_juveniles" = "Pourcentage de juvéniles",
    "taux_occurrence" = "Taux d'occurrence",
    "biomasse" = "Biomasse")
  # Tracer le graphique
  graph <- ggplot(data = filtered_data,aes(x = annee, y = valeur)) +
    facet_grid2(
      espece ~ indicateur,
      scales = "free",
      independent = "y",
      axes = "all",
      labeller = labeller(indicateur = indicateur_labels)
    ) +
    geom_point(shape = 21, size = 2) +
    geom_line(linetype = 3, linewidth = 0.5) +
    labs(
      # title = "Indicateurs de tendances à l'échelle régionale des espèces de poissons d'eau douce de BFC",
      x = "Années",
      y = "Valeur indicateur"
    ) +
    geom_smooth(method = "loess",
                se = F,
                color = "darkred") +
    theme(
      strip.background = element_rect(color = "grey35", fill = "grey90"),
      legend.position = "bottom",
      panel.background = element_rect(fill = "grey95"),
      panel.border = element_rect(color = "grey35"),
      panel.grid.major = element_line(color = "white", size = 0.1),
      panel.grid.minor = element_line(color = "white"),
      axis.text.x = element_text(angle = 45),
      strip.text = element_text(size = 12)
    )+
    ggtitle(mon_espece)
  
  return(graph)
  
}

