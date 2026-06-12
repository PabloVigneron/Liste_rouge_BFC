#' Appliquer le modèle GAMM sur plusieurs périodes et espèces
#'
#' @param data Dataframe contenant les données
#' @param liste_periodes Liste de vecteurs c(annee_depart, annee_fin)
#' @return Dataframe avec les résultats de tous les modèles par période et espèce
#' @export
#'
#' @importFrom purrr map pmap keep map_dfr
#' @importFrom dplyr filter select distinct mutate bind_rows
#'
#' @examples
#' \dontrun{
#' periodes <- list(c(2000, 2010), c(2010, 2020))
#' resultats <- gamm_application_modele(data = mon_df, liste_periodes = periodes)
#' }
gamm_application_modele <- function(data, liste_periodes) {
  
  results_list <- map(liste_periodes, function(period) {
    
    mon_annee_depart <- period[1]
    mon_annee_fin    <- period[2]
    period_label     <- paste0(mon_annee_depart, "-", mon_annee_fin)
    
    message("\n========================================")
    message(">>> Période : ", period_label)
    message("========================================")
    
    period_data <- data %>%
      filter(annee >= mon_annee_depart & annee <= mon_annee_fin)
    
    message("Lignes dans period_data : ", nrow(period_data))
    
    especes_uniques <- period_data %>%
      select(espece) %>%
      distinct() %>%
      pull(espece)
    
    message("Nombre d'espèces : ", length(especes_uniques))
    message("Espèces : ", paste(especes_uniques, collapse = ", "))
    
    # ---- Diagnostic espèce par espèce ----
    results <- map(especes_uniques, function(esp) {
      
      esp_data <- period_data %>% filter(espece == esp)
      
      # Vérifications préalables explicites
      checks <- list(
        n_lignes    = nrow(esp_data),
        n_pop_id    = length(unique(esp_data$pop_id)),
        n_annee     = length(unique(esp_data$annee)),
        n_prolibelle = length(unique(esp_data$pro_libelle))
      )
      
      # Afficher le diagnostic même si NULL sera retourné
      message(
        "\n  Espèce : ", esp,
        " | n=", checks$n_lignes,
        " | pop_id=", checks$n_pop_id,
        " | annee=", checks$n_annee,
        " | pro_libelle=", checks$n_prolibelle
      )
      
      # Identifier la raison du NULL avant même d'appeler le modèle
      if (checks$n_lignes     < 2) { message("    -> NULL : pas assez de lignes") ;     return(NULL) }
      if (checks$n_pop_id     < 2) { message("    -> NULL : pop_id insuffisant") ;       return(NULL) }
      if (checks$n_annee      < 2) { message("    -> NULL : années insuffisantes") ;     return(NULL) }
      if (checks$n_prolibelle < 2) { message("    -> NULL : pro_libelle insuffisant") ;  return(NULL) }
      
      res <- tryCatch(
        gamm_calcul_modele(data = period_data, mon_espece = esp),
        error = function(e) {
          message("    -> ERREUR : ", e$message)
          NULL
        }
      )
      
      if (is.null(res)) {
        message("    -> gamm_calcul_modele a retourné NULL")
        return(NULL)
      }
      
      message("    -> OK : ", nrow(res), " lignes de résultats")
      
      res %>% mutate(row_name = rownames(res), periode = period_label)
      
    }) %>%
      keep(~ !is.null(.)) %>%
      bind_rows()
    
    message("\n  Résultats pour la période ", period_label, " : ", nrow(results), " lignes")
    
    return(results)
    
  }) %>%
    bind_rows()
  
  message("\n>>> TOTAL résultats : ", nrow(results_list), " lignes")
  
  return(results_list)
}
# gamm_application_modele <- function(data, 
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
#         gamm_calcul_modele(data = period_data,
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
