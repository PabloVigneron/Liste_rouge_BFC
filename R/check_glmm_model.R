#' Determiner si les modeles sont bien ajusté aux données
#'
#' @param model objet contenant le modele estimé  
#' @param mon_espece esp_code_alternatif, correspond au code de l'espèce ciblée
#' @param indicateur correspond à l'indicateur utilisé dans l'analyse
#' @param family correspond à la famille de distribution utilisé dans les modèles
#' @param link correspond à la fonction de lien utilisé dans les modèles

#' @return 
#' @export 
#' 
#' @importClassesFrom RVAidememoire, dplyr , lmerTest, glmmTMB, lme4, ggpubr, DHARMa
#'
#' @examples
#' \dontrun{
#' result <- ajustement_model(model = esp_ope_selection)
#' 
 
check_glmm_model <- function (data, mon_espece, indicateur, family, link, ...){
  
  # Definition des variables 
  filtered_data <- data %>%
    filter(indicateur == indicateur) %>% 
    filter(espece == mon_espece)
  
  family <- family
  
  link <- link
  
  # zero_inf <- zero_inf
     
  # Etape 1 : Reconstruction du modèle

  # Cas occurence : family == "Cloglog"  "Cauchit"  "Probit"  "Logit"   
    
    if(family == "binomial")
      model <- lme4::glmer(
        valeur ~ annee  + pro_libelle + (1 | sta_id),
        data = filtered_data,
        family = binomial(link = link)
      )
  
  # Cas densité / effectif : 
  if(
    family == "poisson"|
    family == "nbinom2"|
    family == "truncated_poisson"|
    family == "truncated_nbinom2"
    )
    
    model <- glmmTMB::glmmTMB(
      valeur ~ annee + offset(log(ope_surface_calculee)) + pro_libelle + (1 |
                                                                            sta_id),
      data = filtered_data,
      family    = family(link = link),
      ziformula = ~ 1
    )
  
 # Cas biomasse : 
  if(family == "gaussian")
    model <- glmer(
      valeur ~ annee  + pro_libelle + (1 |
                                         sta_id),
      data = filtered_data,
      family = family(link = link)
    )

  # Etape 2 : conditions d'application des modèles 
  
  if(
    family == "poisson"|
    family == "nbinom2"|
    family == "truncated_poisson"|
    family == "truncated_nbinom2"
  )
    ## a) Indépendance entre les résidus du modèle et les valeurs prédites
    independance <- as.ggplot( ~ plot(DHARMa::simulateResiduals(fittedModel = modele, n = 1000), quantreg = T)) +
    labs (title = mon_espece, subtitle = family) +
    theme(
      plot.title = element_text(
        hjust = 0,
        vjust = 1,
        face = "bold",
        size = 15
      ),
      plot.subtitle = element_text(
        hjust = 0,
        vjust = 1,
        face = "bold.italic",
        size = 12
      )
    )
  ## b) homoscédasticité/Absence de sur-dispersion des résidus
  disp <- overdisp.glmer(model)
  
  # Condition 3 : si GLMM ou LMM lme4 Loi normale
  if (family == "gaussian")
    ## a) Indépendance entre les résidus du modèle et les valeurs prédites / homoscédasticité / normalité
    independance <- as.ggplot( ~ plot(DHARMa::simulateResiduals(fittedModel = modele, n = 1000), quantreg = T)) +
    labs (title = mon_espece, subtitle = family) +
    theme(
      plot.title = element_text(
        hjust = 0,
        vjust = 1,
        face = "bold",
        size = 15
      ),
      plot.subtitle = element_text(
        hjust = 0,
        vjust = 1,
        face = "bold.italic",
        size = 12
      )
    )
  
  # Condition 4 : Si GLMM binomiale négative
  if (family == "binomial")
    ## a) Indépendance entre les résidus du modèle et les valeurs prédites / homoscédasticité / normalité
    independance <- as.ggplot( ~ plot(DHARMa::simulateResiduals(fittedModel = modele, n = 1000), quantreg = T)) +
    labs (title = mon_espece, subtitle = family) +
    theme(
      plot.title = element_text(
        hjust = 0,
        vjust = 1,
        face = "bold",
        size = 15
      ),
      plot.subtitle = element_text(
        hjust = 0,
        vjust = 1,
        face = "bold.italic",
        size = 12
      )
    )
  ## b) homoscédasticité/Absence de sur-dispersion des résidus
  disp <- overdisp.glmer(model)
    #exemple : Residual deviance: 217256329.801 on 772 degrees of freedom (ratio: 281420.116)
  
  # Etape 3 : renvoyer les résultats 

}


#' Vérifier l'ajustement des modèles aux données
#'
#' @param data data.frame contenant les colonnes indicateur, espece, valeur, annee, pro_libelle, sta_id (et ope_surface_calculee pour les comptages)
#' @param mon_espece code de l'espèce ciblée
#' @param indicateur indicateur utilisé dans l'analyse
#' @param family famille de distribution : "binomial", "gaussian", "poisson", "nbinom2", "truncated_poisson", "truncated_nbinom2"
#' @param link fonction de lien (ex. "logit", "log", "identity")
#' @param n_sim nombre de simulations DHARMa
#'
#' @return une liste : plot (ggplot), model, ratio_dispersion
#' @export
#'
#' @importFrom dplyr filter
#' @importFrom rlang .data .env
#' @importFrom ggplot2 labs theme element_text
#' @importFrom ggplotify as.ggplot
#' @importFrom utils capture.output
#'
#' @examples
#' \dontrun{
#' res <- check_glmm_model(data = mes_donnees, mon_espece = "TRF",
#'                         indicateur = "densite", family = "nbinom2", link = "log")
#' res$plot
#' }
check_glmm_model <- function(data, mon_espece,  family, link, n_sim = 1000, ...) {
  
  families_tmb <- c("poisson", "nbinom2", "truncated_poisson", "truncated_nbinom2")
  
  # Etape 0 : filtre (.env évite que "indicateur" soit comparé à la colonne elle-même)
  filtered_data <- data %>%
    dplyr::filter(
      # .data$indicateur == .env$indicateur,
      .data$espece == .env$mon_espece
    )
  
  # Etape 1 : reconstruction du modèle
  if (family == "binomial") {
    
    model <- lme4::glmer(
      valeur ~ annee + pro_libelle + (1 | sta_id),
      data = filtered_data,
      family = binomial(link = link)
    )
    
  } else if (family %in% families_tmb) {
    
    # truncated_* et nbinom2 sont exportées par glmmTMB, poisson vient de stats
    fam_fun <- if (family == "poisson") stats::poisson else getExportedValue("glmmTMB", family)
    
    model <- glmmTMB::glmmTMB(
      valeur ~ annee + offset(log(ope_surface_calculee)) + pro_libelle + (1 | sta_id),
      data = filtered_data,
      family = fam_fun(link = link),
      ziformula = ~ 1
    )
    
  } else if (family == "gaussian") {
    
    model <- if (link == "identity") {
      lme4::lmer(valeur ~ annee + pro_libelle + (1 | sta_id), data = filtered_data)
    } else {
      lme4::glmer(valeur ~ annee + pro_libelle + (1 | sta_id),
                  data = filtered_data, family = gaussian(link = link))
    }
    
  } else {
    stop("family non gérée : ", family)
  }
  
  # Etape 2 : résidus simulés (indépendance / homoscédasticité / normalité)
  sim <- DHARMa::simulateResiduals(fittedModel = model, n = n_sim)
  
  # Etape 3 : test de surdispersion (uniquement binomial / comptages)
  ratio <- NA_real_
  label_ratio <- NULL
  
  if (family == "binomial") {
    
    ratio <- extraire_ratio_overdisp(model)
    label_ratio <- "Ratio de surdispersion"
    
  } else if (family %in% families_tmb) {
    
    # overdisp.glmer ne fonctionne pas sur un objet glmmTMB -> test DHARMa
    ratio <- unname(DHARMa::testDispersion(sim, plot = FALSE)$statistic)
    label_ratio <- "Dispersion (DHARMa)"
  }
  
  # Etape 4 : sous-titre + graphique
  subtitle <- paste0(family, " (", link, ")")
  if (!is.null(label_ratio)) {
    subtitle <- paste0(subtitle, "\n", label_ratio, " : ", signif(ratio, 4))
  }
  
  p <- ggplotify::as.ggplot(~ plot(sim, quantreg = TRUE)) +
    ggplot2::labs(title = mon_espece, subtitle = subtitle) +
    ggplot2::theme(
      plot.title    = ggplot2::element_text(hjust = 0, vjust = 1, face = "bold", size = 15),
      plot.subtitle = ggplot2::element_text(hjust = 0, vjust = 1, face = "bold.italic", size = 12)
    )
  
  list(plot = p, model = model, ratio_dispersion = ratio)
}


#' Isoler le ratio renvoyé par RVAideMemoire::overdisp.glmer
#'
#' La fonction affiche "Residual deviance: X on Y degrees of freedom (ratio: Z)".
#' On capture cet affichage et on extrait Z avec une regex.
#' @noRd
extraire_ratio_overdisp <- function(model) {
  
  sortie <- utils::capture.output(res <- RVAideMemoire::overdisp.glmer(model))
  txt <- paste(sortie, collapse = " ")
  
  ratio <- suppressWarnings(
    as.numeric(sub(".*ratio: *([-0-9.eE+]+)\\).*", "\\1", txt))
  )
  
  # secours : si la fonction renvoie un vecteur nommé contenant "ratio"
  if (is.na(ratio) && is.numeric(res) && "ratio" %in% names(res)) {
    ratio <- unname(res["ratio"])
  }
  
  ratio
}
