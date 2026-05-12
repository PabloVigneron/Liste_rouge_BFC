#' Determiner si le nomnre de donnes est suffissante pour appliquer le modele ZIPGLMM 
#'
#' @param df Dataframe contenant les données, nombre de stations et nombre de lignes 
#' @param esp esp_code_alternatif, correspond au code de l'espèce ciblée

#' @return Message positif ou négatif pour l'utilisation du model avec ces données 
#' @export 
#' 
#' @importClassesFrom dplyr 
#'
#' @examples
#' \dontrun{
#' result <- adequate_data_for_model(df = esp_ope_selection)

 adequate_data_for_model <- function(df, esp){
   
   # Filter le df avec l'espece ciblée
   filtered_data <- df %>% 
     filter(esp_code_alternatif == esp)
   
   # Extraire le nombre de stations du df
   nb_station <- filtered_data %>%
     group_by(sta_id) %>%
     summarise(n = n()) %>%
     ungroup() %>%
     summarise(total = n())
   
   # Fixer le coût des parametre du model
   annee <- 1 # co-variable : coûte 1 parametre
   ope_surface_calcule <- 1 # co-variable : coûte 1 parametre
   pro_libelle <- 2 # facteur à 2 modalité : coûte (2-1) parametre
   sta_id <- nb_station - 1 # facteur à n modalité : coûte (n-1) parametre
   intercept <- 1 # l'ordonnée à lorigine coûte 1 parametre
   
   # Fixer la somme des coût des parametre du model
   param_nb <- (annee + ope_surface_calcule + pro_libelle + sta_id + intercept)*10 # On multiplie par 10 car 1 parametre coûte 10 données donc 10 lignes
  
   # Calculer le nombre de lignes du df pour comparer avec le nombre de parametre demandé
   nb_rows <- df %>% 
     summarise(n = n()) 
   
   # Retourner message positif si param_nb <= nb_rows
   if(param_nb <= nb_rows){
     paste("Le nombre de données de l'espèce", esp, "est suffisante pour appliquer le modèle")
   } 
   # Retourner message negatif si param_nb > nb_rows  
   else{
     paste("Le nombre de données de l'espèce", esp, "est insuffisante pour appliquer le modèle")
   }
 }
