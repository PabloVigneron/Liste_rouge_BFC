#' Determiner et corriger les erreurs humaines de protocoles utilisés 
#' sur chaque stations dans les series temporelles
#'
#' @param df Dataframe contenant les données
#' @return Dataframe corriger 
#' @export
#' 
#' @importClassesFrom dplyr group_by arrange mutate filter summarise ungroup select n
#'
#' @examples
#' \dontrun{
#' result <- detect_pro_errors(df = ope_selection)
#' erreurs <- attr(ope_selection_clean, "erreurs_protocoles")

detect_pro_errors <- function(df) {
  #Definition du protocole majoritaire sur le nombre d'années par station
  
  pro_ref <- ope_selection %>%
    group_by(sta_id, pro_libelle) %>%
    summarise(nb_annees = n_distinct(annee), .groups = "drop") %>%
    group_by(sta_id) %>%
    slice_max(nb_annees, n = 1, with_ties = F) %>%
    ungroup() %>%
    select(sta_id, pro_majoritaire = pro_libelle)
  
  
  #Jointure avec le df
  df_check <- ope_selection %>%
    left_join(pro_ref, by = "sta_id")
  
  #Iddentification des erreurs (protocole non majoritaire)
  
  erreurs <- df_check %>%
    filter(pro_libelle != pro_majoritaire)
  
  #Supprimer les erreurs du df
  
  df_clean <- df_check %>%
    filter(pro_libelle == pro_majoritaire) %>%
    select(-pro_majoritaire)
  
  
  #Stocker les erreurs du df pour verif
  attr(df_clean, "erreurs_protocoles") <- erreurs
  
  return(df_clean)
  
}