#' Calculer les résultats des modèles GAMM avec poptrend pour un df et une espèce choisit
#'
#' @param df Dataframe contenant les données
#' @param mon_espece esp_code_alternatif : le code espèce de l'espèce ciblé par l'analyse
#' @return Dataframe avec resultats 
#' @export 
#' 
#' @importClassesFrom dplyr; mutate filter rename ;lme4, glmer; base summary  nrow rename length unique ; glmmTMB
#' @importClassesFrom ggplot
#' 
#' @examples
#' \dontrun{
#' mod <- gamm_poptrend (data = esp_ope_selection, mon_espece == "BOU")
#' }
gamm_poptrend_application <- function(data, liste_periodes) {
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
      gamm_poptrend(data = period_data, mon_espece = esp)
    }) %>%
      keep(~ !is.null(.))
    return(results)
  })
}
