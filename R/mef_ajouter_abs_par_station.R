#' Ajouter à un dataframe de présences les lignes des absences par stations (avec effectif et densité nuls)
#'
#' La fonction fait appel au dataframe "operation" de la base Aspe qui doit impérativement avoir
#'     été chargé auparavant.
#'
#' @param df Dataframe avec les variables ope_id, esp_code_alternatif, effectif, dens_ind_1000m2 et annee.
#' @param var_id Nom de la variable contenant les identifiants des observations (ex : ope_id), sans guillements.
#' @param var_taxon Nom de la variable contenant les identifiants des taxons (ex : esp_code_alternatif),
#'     sans guillements.
#' @param var_effectif Nom de la variable contenant les effectifs (ex : lop_effectif), sans guillements.
#'
#' @return Le dataframe avec des lignes supplémentaires pour chacune des espèces absentes à chacune
#'     des opérations.
#' @export
#'
#' @importFrom dplyr select left_join enquo 
#' @importFrom tidyr complete
#' @importFrom rlang as_name
#' 
#' @examples
#' \dontrun{
#' df_complet <- mef_ajouter_abs(df = df_presences,
#' var_id = ope_id,
#' var_taxon = esp_code_alternatif,
#' var_effectif = effectif)
#' }


mef_ajouter_abs_par_station <- function(df, var_id, var_taxon, var_effectif)
{
  var_id <- enquo(var_id)
  var_taxon <- enquo(var_taxon)
  var_effectif <- enquo(var_effectif)
  
  df %>%
    droplevels() %>%
    left_join(operation %>%
                select(ope_id, pop_id = ope_pop_id)) %>%
    select(!!var_id, pop_id, !!var_taxon, !!var_effectif) %>%
    group_by(pop_id) %>%
    complete(!!var_id, !!var_taxon) %>%
    ungroup() %>%
    left_join(y = df %>% select(!!var_id, annee) %>% distinct(),
              by = as_name(var_id))
}

# Grâce au test suivant, on a pu montrer que la fonction **mef_ajouter_abs_par_station** fonctionne correctement.

# =============================================================================
# Fonction à tester
# =============================================================================
# mef_ajouter_abs_par_station <- function(df, var_id, var_taxon, var_effectif)
# {
#   var_id <- enquo(var_id)
#   var_taxon <- enquo(var_taxon)
#   var_effectif <- enquo(var_effectif)
#   
#   df %>%
#     droplevels() %>%
#     left_join(operation %>%
#                 select(ope_id, pop_id = ope_pop_id)) %>%
#     select(!!var_id, pop_id, !!var_taxon, !!var_effectif) %>%
#     group_by(pop_id) %>%
#     complete(!!var_id, !!var_taxon) %>%
#     ungroup() %>%
#     left_join(y = df %>% select(!!var_id, annee) %>% distinct(),
#               by = as_name(var_id))
# }
# 
# # =============================================================================
# # Jeu de données jouet — CONFORME AU CAS RÉEL :
# # chaque opération a toujours AU MOINS UNE capture (jamais 0 poisson total)
# # =============================================================================
# #
# # Station 10 : 2 opérations
# #   - ope 1 : espèce A capturée
# #   - ope 2 : espèce D capturée (mais PAS A -> A doit devenir une absence ici)
# #
# # Station 20 : 2 opérations
# #   - ope 3 : espèce B capturée
# #   - ope 4 : espèces B ET E capturées (E est nouvelle -> absence à créer pour ope 3)
# #
# # Station 30 : 1 opération
# #   - ope 5 : espèce C capturée (aucune absence à créer, une seule ope)
# 
# operation <- tibble(
#   ope_id     = c(1, 2, 3, 4, 5),
#   ope_pop_id = c(10, 10, 20, 20, 30)
# )
# 
# df_presences <- tibble(
#   ope_id              = c(1, 2, 3, 4, 4, 5),
#   esp_code_alternatif = c("A", "D", "B", "B", "E", "C"),
#   effectif            = c(5, 3, 8, 2, 1, 4),
#   annee               = c(2020, 2020, 2020, 2021, 2021, 2020)
# )
# 
# cat("--- Données de départ ---\n")
# print(df_presences)
# 
# # =============================================================================
# # Exécution
# # =============================================================================
# resultat <- mef_ajouter_abs_par_station(
#   df_presences, ope_id, esp_code_alternatif, effectif
# )
# 
# cat("\n--- Résultat de mef_ajouter_abs_par_station ---\n")
# print(resultat %>% arrange(pop_id, ope_id, esp_code_alternatif))
# 
# # =============================================================================
# # TESTS
# # =============================================================================
# 
# test_that("Toutes les opérations d'origine sont conservées (aucune perte)", {
#   # Puisque chaque ope_id a déjà au moins une capture dans df, complete()
#   # doit conserver l'intégralité des ope_id présents au départ.
#   expect_setequal(unique(resultat$ope_id), unique(df_presences$ope_id))
# })
# 
# test_that("Le nombre de lignes correspond au produit espèces x opérations, PAR STATION", {
#   # Station 10 : 2 espèces (A, D) x 2 opérations (1, 2) = 4 lignes
#   # Station 20 : 2 espèces (B, E) x 2 opérations (3, 4) = 4 lignes
#   # Station 30 : 1 espèce (C)   x 1 opération (5)       = 1 ligne
#   expect_equal(nrow(resultat), 4 + 4 + 1)
# })
# 
# test_that("Aucune espèce ne fuit vers une autre station", {
#   stations_A <- resultat %>% filter(esp_code_alternatif == "A") %>% pull(pop_id) %>% unique()
#   stations_B <- resultat %>% filter(esp_code_alternatif == "B") %>% pull(pop_id) %>% unique()
#   stations_C <- resultat %>% filter(esp_code_alternatif == "C") %>% pull(pop_id) %>% unique()
#   stations_D <- resultat %>% filter(esp_code_alternatif == "D") %>% pull(pop_id) %>% unique()
#   stations_E <- resultat %>% filter(esp_code_alternatif == "E") %>% pull(pop_id) %>% unique()
# 
#   expect_equal(stations_A, 10)
#   expect_equal(stations_D, 10)
#   expect_equal(stations_B, 20)
#   expect_equal(stations_E, 20)
#   expect_equal(stations_C, 30)
# })
# 
# test_that("Une absence réelle est bien créée : espèce A absente (NA) à l'ope 2", {
#   # A a été capturée à l'ope 1 mais pas à l'ope 2 (où seule D a été capturée)
#   # -> une ligne (ope 2, A) doit exister avec effectif = NA
#   eff_A_ope2 <- resultat %>%
#     filter(ope_id == 2, esp_code_alternatif == "A") %>%
#     pull(effectif)
#   expect_length(eff_A_ope2, 1)
#   expect_true(is.na(eff_A_ope2))
# })
# 
# test_that("Une absence réelle est bien créée : espèce D absente (NA) à l'ope 1", {
#   eff_D_ope1 <- resultat %>%
#     filter(ope_id == 1, esp_code_alternatif == "D") %>%
#     pull(effectif)
#   expect_length(eff_D_ope1, 1)
#   expect_true(is.na(eff_D_ope1))
# })
# 
# test_that("Une absence réelle est bien créée : espèce E absente (NA) à l'ope 3", {
#   # E n'a été capturée qu'à l'ope 4 -> absence attendue à l'ope 3
#   eff_E_ope3 <- resultat %>%
#     filter(ope_id == 3, esp_code_alternatif == "E") %>%
#     pull(effectif)
#   expect_length(eff_E_ope3, 1)
#   expect_true(is.na(eff_E_ope3))
# })
# 
# test_that("Les présences réelles gardent leur effectif d'origine (rien n'est écrasé)", {
#   eff_A_ope1 <- resultat %>% filter(ope_id == 1, esp_code_alternatif == "A") %>% pull(effectif)
#   eff_B_ope3 <- resultat %>% filter(ope_id == 3, esp_code_alternatif == "B") %>% pull(effectif)
#   eff_B_ope4 <- resultat %>% filter(ope_id == 4, esp_code_alternatif == "B") %>% pull(effectif)
#   eff_E_ope4 <- resultat %>% filter(ope_id == 4, esp_code_alternatif == "E") %>% pull(effectif)
# 
#   expect_equal(eff_A_ope1, 5)
#   expect_equal(eff_B_ope3, 8)
#   expect_equal(eff_B_ope4, 2)
#   expect_equal(eff_E_ope4, 1)
# })
# 
# test_that("Une station à opération unique (station 30) n'a aucune absence générée", {
#   n_lignes_30 <- resultat %>% filter(pop_id == 30) %>% nrow()
#   expect_equal(n_lignes_30, 1)
# })
# 
# test_that("annee est bien récupérée pour toutes les lignes d'origine", {
#   annee_ope1 <- resultat %>% filter(ope_id == 1, esp_code_alternatif == "A") %>% pull(annee)
#   expect_equal(annee_ope1, 2020)
# })
# 
# cat("\n=====================================================================\n")
# cat("Si aucun message d'échec (Failure/Error) n'apparaît ci-dessus,\n")
# cat("la fonction se comporte exactement comme attendu pour votre cas :\n")
# cat(" - pas de fuite d'espèce entre stations\n")
# cat(" - absences correctement générées uniquement pour les espèces déjà\n")
# cat("   vues sur la station, sur les opérations où elles n'ont pas été revues\n")
# cat(" - aucune perte d'opération (puisque toutes sont déjà représentées dans df)\n")
# cat("=====================================================================\n")
# 
# # =============================================================================
# # Étape suivante à ne pas oublier : remplacer les NA par 0
# # =============================================================================
# resultat_final <- resultat %>%
#   mutate(effectif = tidyr::replace_na(effectif, 0))
# 
# cat("\n--- Aperçu après remplacement des NA par 0 ---\n")
# print(resultat_final %>% arrange(pop_id, ope_id, esp_code_alternatif))