#' Determiner si le nombre de donnees est suffisant pour appliquer le modele ZIPGLMM
#'
#' @param df Dataframe contenant les donnees (colonnes espece, sta_id, etc.)
#' @param esp Vecteur de esp_code_alternatif, correspond au(x) code(s) de(s) espece(s) ciblee(s)
#' @return Un tibble recapitulatif avec, pour chaque espece : nombre de lignes, 
#'   nombre de parametres requis, statut (ABSENTE / SUFFISANTE / INSUFFISANTE) et message
#' @export
#'
#' @importFrom dplyr filter group_by summarise ungroup pull bind_rows
#'
#' @examples
#' \dontrun{
#' result <- adequate_data_for_model(df = esp_ope_selection, esp = c("TRF", "CHA", "VAI"))
#' }
adequate_data_for_model <- function(df, esp) {
  
  # purrr::map_df applique la fonction a chaque espece et empile les resultats
  # dans un seul data.frame (equivalent a rbind(lapply(...)))
  purrr::map_df(esp, function(e) {
    
    # Filtrer le df avec l'espece ciblee
    filtered_data <- df %>%
      filter(espece == e)
    
    nb_rows <- nrow(filtered_data)
    
    # Cas ou l'espece est absente du jeu de donnees
    if (nb_rows == 0) {
      return(dplyr::tibble(
        espece = e,
        nb_stations = NA_integer_,
        nb_lignes = 0L,
        nb_parametres_requis = NA_integer_,
        statut = "ABSENTE",
        message = paste("L'espece", e, "est absente du jeu de donnees.")
      ))
    }
    
    # Extraire le nombre de stations du df
    nb_station <- filtered_data %>%
      group_by(sta_id) %>%
      summarise(n = dplyr::n(), .groups = "drop") %>%
      nrow()
    
    # Fixer le cout des parametres du modele
    annee <- 1                     # co-variable : coute 1 parametre
    ope_surface_calcule <- 1       # co-variable : coute 1 parametre
    pro_libelle <- 2 - 1           # facteur a 2 modalites : coute (2-1) parametre
    sta_id <- nb_station - 1       # facteur a n modalites : coute (n-1) parametre
    intercept <- 1                 # l'ordonnee a l'origine coute 1 parametre
    
    # Somme des couts des parametres du modele, x10 (1 parametre = 10 donnees requises)
    param_nb <- (annee + ope_surface_calcule + pro_libelle + sta_id + intercept) * 10
    
    statut <- if (param_nb <= nb_rows) "SUFFISANTE" else "INSUFFISANTE"
    
    msg <- paste(
      "Le nombre de donnees de l'espece", e,
      "est", statut, "pour appliquer le modele avec",
      nb_rows, "donnees pour", param_nb, "requis."
    )
    
    dplyr::tibble(
      espece = e,
      nb_stations = nb_station,
      nb_lignes = nb_rows,
      nb_parametres_requis = param_nb,
      statut = statut,
      message = msg
    )
  })
}


#' #' Determiner si le nomnre de donnes est suffissante pour appliquer le modele ZIPGLMM 
#' #'
#' #' @param df Dataframe contenant les données, nombre de stations et nombre de lignes 
#' #' @param esp esp_code_alternatif, correspond au code de l'espèce ciblée
#' 
#' #' @return Message positif ou négatif pour l'utilisation du model avec ces données 
#' #' @export 
#' #' 
#' #' @importClassesFrom dplyr 
#' #'
#' #' @examples
#' #' \dontrun{
#' result <- adequate_data_for_model(df = esp_ope_selection)
# adequate_data_for_model <- function(df, esp) {
#   for (e in esp) {
#     # Filter le df avec l'espece ciblée
#     filtered_data <- df %>%
#       filter(espece == e)
#     
#     # Extraire le nombre de stations du df
#     nb_station <- filtered_data %>%
#       group_by(sta_id) %>%
#       summarise(n = n()) %>%
#       ungroup() %>%
#       summarise(total = n()) %>%
#       pull(total)
#     
#     # Fixer le coût des parametre du model
#     annee <- 1 # co-variable : coûte 1 parametre
#     ope_surface_calcule <- 1 # co-variable : coûte 1 parametre
#     pro_libelle <- 2 - 1 # facteur à 2 modalité : coûte (2-1) parametre
#     sta_id <- nb_station - 1 # facteur à n modalité : coûte (n-1) parametre
#     intercept <- 1 # l'ordonnée à lorigine coûte 1 parametre
#     
#     # Fixer la somme des coût des parametre du model
#     param_nb <- (annee + ope_surface_calcule + pro_libelle + sta_id + intercept) *
#       10 # On multiplie par 10 car 1 parametre coûte 10 données donc 10 lignes
#     
#     # Calculer le nombre de lignes du df pour comparer avec le nombre de parametre demandé
#     nb_rows <- nrow(filtered_data)
#     
#     # Retourner message si esp absente du df
#     if (nb_rows == 0) {
#       paste("L'espèce", e, "est absente du jeu de données.")
#     }
#     # Retourner message positif si param_nb <= nb_rows
#     else if (param_nb <= nb_rows) {
#       paste(
#         "Le nombre de données de l'espèce",
#         e,
#         "est SUFFISANTE pour appliquer le modèle avec",
#         nb_rows,
#         "données pour",
#         param_nb,
#         "requis."
#       )
#     }
#     # Retourner message negatif si param_nb > nb_rows
#     else{
#       paste(
#         "Le nombre de données de l'espèce",
#         e,
#         "est INSUFFISANTE pour appliquer le modèle avec",
#         nb_rows,
#         "données pour",
#         param_nb,
#         "requis."
#       )
#     }
#   }
# 
# }
# 
