#' Fonction pour compiler plusieurs espèces avec la fonction calcul_n_sta_ope_ind() 
#' qui permet de de créer un tableau recapitulatif du nb ope et nb de stations 
#' par espèce selectionne
#' 
#' @param data Dataframe contenant les donnees (toutes especes)
#' @param mes_especes L'espèce à traiter selon le code espèce poisson SANDRE de l'OFB
#' @return flextable 
#' @export 
#' 
#' @examples
#' \dontrun{
#' tab <- calcul_n_sta_ope (data = reg_indicateur, mes_especes == c("BRO","TRF))
#' }


calcul_n_sta_ope <- function(data, mes_especes) {
  # Listes des espèces demandées
  especes <- intersect(mes_especes, unique(data$espece))
  
  # Avertissement si certaines espèces sont absentes du df
  esp_manquantes <- setdiff(mes_especes, especes)
  if(length(esp_manquantes) > 0){
    warning("Espèces absentes du dataframe : ", paste(esp_manquantes, collapse = ", "))
  }
  
  # Un tableau par espèce :
  results <- map(especes, function(esp) {
    calcul_n_sta_ope_ind (data = data, mon_espece = esp)
  }) %>%
    set_names(especes) %>%
    keep( ~ !is.null(.))
  return(results)
}


#' Fonction qui permet de créer un tableau recapitulatif du nb ope et nb de stations 
#' par espèce selectionne
#' 
#' @param data Dataframe contenant les donnees (toutes especes)
#' @param mon_espece L'espèce à traiter selon le code espèce poisson SANDRE de l'OFB
#' @return flextable 
#' @export 
#' 
#' @examples
#' \dontrun{
#' tab <- calcul_n_sta_ope_ind (data = ope_effectif, mon_espece == "BRO")
#' }

calcul_n_sta_ope_ind <- function (data, mon_espece)
{
  
  # Filtrage des data des indicateurs sélectionnés  
  filtered_data <- data %>% 
    # Filtrage des data des espèces sélectionnés
    filter(espece == mon_espece)
  
  if(nrow(filtered_data) == 0) return(NULL)
  
  # Creation d'un premier df qui resume le nombre de stations/opérations/protocoles par espèce 
   summary_tab_ope <- filtered_data %>%
    group_by(pro_libelle) %>%
    summarise(
      total_sta = n_distinct(sta_id),
      total_ope = n_distinct(ope_id),
      total_ope_occur = n_distinct(ope_id[valeur > 0]),
      .groups = "drop"
    ) %>%
    select(pro_libelle, total_sta, total_ope, total_ope_occur)
   # Creation d'une ligne qui calcul le total des valeurs du premier df
  total_row <- summary_tab_ope %>%
    summarise(
      pro_libelle = "TOTAL",
      total_sta = sum(total_sta, na.rm = TRUE),
      total_ope = sum(total_ope, na.rm = TRUE),
      total_ope_occur = sum(total_ope_occur, na.rm = TRUE)
    )
  # Fusionner les deux df
  summary_ope <- bind_rows(summary_tab_ope, total_row)
  # Tracer le tableau
  tab <- flextable(summary_ope) %>%
    set_header_labels(pro_libelle = "Protocole",
                      total_sta = "Nombre de stations où l'espèce est présente au moins 1 fois",
                      total_ope = "Nombre d'opérations", 
                      total_ope_occur = "Nombre d'opérations où l'espèce est présente") %>%
    flextable::autofit() %>%
    flextable::theme_booktabs() %>%
    flextable::font(fontname = "Calibri", part = "header") %>%
    flextable::font(fontname = "Calibri", part = "body") %>%
    flextable::fontsize(size = 17, part = "all") %>%
    flextable::padding(padding = 10, part = "all") %>%
    flextable::align(align = "center", part = "all") %>%
    bold(i = ~ pro_libelle == "TOTAL", bold = TRUE) %>%
    flextable::hline(border = officer::fp_border(width = 2.5, color="grey80"), part = "header") %>% 
    flextable::hline_bottom(border = officer::fp_border(width = 2.5, color="grey80")) %>% 
    flextable::hline_top(border = officer::fp_border(width = 2.5, color="grey80")) %>% 
    flextable::border_inner_h(border = officer::fp_border(width = 1.8, color="grey80")) %>% 
    flextable::border_outer(border = officer::fp_border(width = 2.5, color="grey70")) %>% 
    flextable::vline_right(border = officer::fp_border(color="white")) %>% 
    flextable::vline_left(border = officer::fp_border(color="white")) %>% 
    flextable::bg(bg = "white", part = "all") %>%
    flextable::bg(i = 1, bg =  "grey88") %>% 
    flextable::bg(i = 2, bg = "grey78") %>% 
    flextable::width(width = 2)%>% 
    flextable::add_header_lines(values = mon_espece) 

  return(tab)
  
}

