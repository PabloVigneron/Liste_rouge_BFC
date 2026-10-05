#' Fonction pour réaliser une cartographie de répartition des pourcentages d'occurences des espèces
#' selon 2 période différente (first et second decade)
#' 
#' @param data Dataframe contenant les donnees (toutes especes)
#' @param espece L'espèce à traiterselon le code espèce poisson SANDRE de l'OFB
#' @return ggplot map
#' @export 
#' 
#' @examples
#' \dontrun{
#' graph <- map_decade_sp (data = occur, mon_espece == "BRO")
#' }
 

map_decade_sp <- function (data, mon_espece)
{
  
  # Filtrage des data
  filtered_data <- data %>%
    filter(espece == mon_espece)
  # Tracer le graphique
  graph <- ggplot() +
    geom_spatraster_rgb(data = basemap) +
    # Ajouter la couche SIG Bassins versants avec la limite régionale
    geom_sf(
      data = bassins_versants_bfc,
      mapping = aes(fill = lb_bh),
      show.legend = F
    ) +
    scale_fill_manual(
      values = c(
        "Seine-Normandie" = "darkseagreen1",
        "Loire-Bretagne" = "cadetblue1",
        "Rhône-Méditerranée" = "darkseagreen3"
      )
    ) +
    new_scale_fill() +
    # Ajouter la couche SIG des cours d'eau de la région
    geom_sf(
      data = cours_eau_bfc,
      aes(linewidth = strahler),
      color = "cyan4",
      show.legend = F
    ) +                      scale_linewidth(range = c(1, 3))  +                                             # Ajouter les données espèces d'occurence
    geom_sf(
      data = filtered_data,
      aes(fill = n_occur),
      shape = 21,
      size = 4,
      color = "grey35"
    ) +
    # scale_fill_discrete()+
    scale_fill_gradient(low = "red",
                        high = "green",
                        name = "Pourcentage d'occurence") +
    # scale_size_area(max_size = 5, name = "Pourcentage d'occurence")+
    facet_wrap(
      ~ decade,
      ncol = 2,
      nrow = 1,
      labeller = labeller(
        decade = c(first = "Période 2007 - 2016", second = "Période 2017 - 2025")
      )
    ) +
    # labs(title = "Carte de répartition du pourcentage d'occurence de la Truite commune entre deux décennies sur la région Bourgogne-Franche-Comté") +
    coord_sf(crs = 2154) +
    theme (
      plot.title = element_text(
        hjust = 0,
        vjust = 1,
        face = "bold",
        size = 20
      ),
      legend.background = element_rect(fill = "grey97", color = "grey35"),
      strip.background = element_rect(color = "grey35", fill = "grey88"),
      panel.background = element_rect(fill = "grey97"),
      panel.border = element_rect(color = "grey35"),
      panel.grid.major = element_line(color = "#faffff", size = 0.1),
      panel.grid.minor = element_line(color = "#faffff"),
      strip.text = element_text(size = 14, face = "bold")
    ) +
    annotation_scale(location = "br", line_width = .5) +
    annotation_north_arrow(
      location = "bl",
      height = unit(0.7, "cm"),
      width = unit(0.7, "cm")
    )+
    ggtitle(mon_espece)
  return(graph)
  
  }

