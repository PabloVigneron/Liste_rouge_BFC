
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
# zip_glmm_application_modele <- function(data, 
#                                     liste_periodes) {
#   
#   results_list <- map(liste_periodes, function(period) {
#     mon_annee_depart <- period[1]
#     mon_annee_fin <- period[2]
#     
#     # Filtrer les données pour la période courante
#     period_data <- data %>%
#       filter(annee >= mon_annee_depart & annee <= mon_annee_fin)
#     
#     combinations <- period_data %>%
#       select(espece) %>%
#       distinct()
#     
#     period_label <- paste0(mon_annee_depart, "-", mon_annee_fin)
#     
#     results <- combinations %>%
#       pmap(function(espece, 
#                     stade, 
#                     indicateur, 
#                     ope_surface_calculee, 
#                     pro_libelle) 
#       {
#         zip_glmm_calcul_modele(data = period_data,
#                            mon_espece = espece)
#       }) %>%
#       keep(~ !is.null(.)) %>%
#       map_dfr(~ mutate(.x, row_name = rownames(.x), periode = period_label))
#     
#     return(results)
#   }) %>%
#     bind_rows()
#   
#   return(results_list)
# }
