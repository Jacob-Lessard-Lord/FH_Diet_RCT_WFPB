# Date : 11 juin 2025
# Auteur : Jacob Lessard-Lord

# Fonction pour entraîner les modèles de régression linéaire mixte ----
lmer_reg <- function(data_df, var_depend, formule = "tx + age + sex + hfgenotype + seq + bmi + weight_delta + (1|nomat)", log_transform = FALSE, digits_results = 2) {
  ## data_df = Data.frame des données
  ## var_depend = Variable dépendante
  ## formule = Formule des variables indépendantes
  ## log_transform = Transformation logarithmique de la variable dépendante
  ## digits_results = Nombre de décimales pour les résultats
  
  if(log_transform == FALSE) { # Modèle de régression sans transformation logarithmique
    
    # Entraînement du modèle
    modele <- lmer(as.formula(paste(var_depend, "~", formule)), data = data_df, REML = TRUE)
    
    # Impression du summary du modèle
    print(summary(modele))
    
    # Extraction des résultats : mean +/- SEM de SAD et WFPB, WFPB - SAD difference mean absolute (95% CI), WFPB - SAD difference mean relative (95% CI), p-value
    results_reg <- data.frame(Outcome = var_depend,
                              SAD_mean = ls_means(modele, which = "tx")[1, "Estimate"],
                              SAD_sem = ls_means(modele, which = "tx")[1, "Std. Error"],
                              WFPB_mean = ls_means(modele, which = "tx")[2, "Estimate"],
                              WFPB_sem = ls_means(modele, which = "tx")[2, "Std. Error"],
                              WFPB_SAD_mean = ls_means(modele, which = "tx", pairwise = TRUE)[1, "Estimate"] * -1, # Multiplication par -1, car par defaut, SAD - WFPB
                              WFPB_SAD_lower = ls_means(modele, which = "tx", pairwise = TRUE)[1, "upper"] * -1, # Pour la même raison, inversion lower et upper
                              WFPB_SAD_upper = ls_means(modele, which = "tx", pairwise = TRUE)[1, "lower"] * -1,
                              p_value = ls_means(modele, which = "tx", pairwise = TRUE)[1, "Pr(>|t|)"])
    
    # Arrondissement des résultats
    results_reg[, c("SAD_mean", "SAD_sem", "WFPB_mean", "WFPB_sem", "WFPB_SAD_mean", "WFPB_SAD_lower", "WFPB_SAD_upper")] <- 
      round(results_reg[, c("SAD_mean", "SAD_sem", "WFPB_mean", "WFPB_sem", "WFPB_SAD_mean", "WFPB_SAD_lower", "WFPB_SAD_upper")],
            digits = digits_results)
    
    # Mise en forme du mean +/- sem et mean (95% CI) absolu et relatif et arrondissement de la p-value
    results_reg <- results_reg %>%
      mutate(SAD_mean_sem = paste(SAD_mean, "±", SAD_sem),
             WFPB_mean_sem = paste(WFPB_mean, "±", WFPB_sem),
             WFPB_SAD_mean_ci_abs = paste0(WFPB_SAD_mean, " (", WFPB_SAD_lower, ", ", WFPB_SAD_upper, ")"),
             WFPB_SAD_mean_ci_rel = paste0(round((WFPB_SAD_mean / SAD_mean) * 100, digits = 1),
                                           "% (", round((WFPB_SAD_lower / SAD_mean) * 100, digits = 1), "%, ", round((WFPB_SAD_upper / SAD_mean) * 100, digits = 1), "%)"),
             p_value = case_when(p_value < 0.0001 ~ "<0.0001",
                                 p_value >= 0.0001 & p_value < 0.001 ~ as.character(round(p_value, 4)),
                                 p_value >= 0.001 & p_value < 0.01 ~ as.character(round(p_value, 3)),
                                 p_value >= 0.01 ~ as.character(round(p_value, 2))))
    
    # Extraction des colonnes pertinentes seulement
    results_reg <- results_reg %>% select(Outcome, SAD_mean_sem, WFPB_mean_sem, WFPB_SAD_mean_ci_abs, WFPB_SAD_mean_ci_rel, p_value)
    
    # Stockage des résultats dans une liste
    results_all <- list(modele = modele,
                        results_reg = results_reg)
    return(results_all)
    
  } else { # Modèle de régression avec transformation logarithmique
    
    # Entraînement du modèle
    modele <- lmer(as.formula(paste("log(", var_depend, ") ~", formule)), data = data_df, REML = TRUE)
    
    # Impression du summary du modèle
    print(summary(modele))
    
    # Calculer les moyennes et erreur-type non-ajustés
    data_mean_sem <- data_df %>% group_by(tx) %>% get_summary_stats(all_of(var_depend), type = "mean_se")
    
    # Extraction des résultats : mean +/- SEM de SAD et WFPB, WFPB - SAD difference mean absolute (95% CI), WFPB - SAD difference mean relative (95% CI), p-value
    results_reg <- data.frame(Outcome = var_depend,
             SAD_mean = round(subset(data_mean_sem, tx == "SAD")$mean, digits = 2), 
             SAD_sem = round(subset(data_mean_sem, tx == "SAD")$se, digits = 2), 
             WFPB_mean = round(subset(data_mean_sem, tx == "WFPB")$mean, digits = 2), 
             WFPB_sem = round(subset(data_mean_sem, tx == "WFPB")$se, digits = 2),
             WFPB_SAD_mean = ls_means(modele, which = "tx", pairwise = TRUE)[1, "Estimate"] * -1, # Multiplication par -1, car par defaut, SAD - WFPB
             WFPB_SAD_lower = ls_means(modele, which = "tx", pairwise = TRUE)[1, "upper"] * -1, # Pour la même raison, inversion lower et upper
             WFPB_SAD_upper = ls_means(modele, which = "tx", pairwise = TRUE)[1, "lower"] * -1,
             p_value = ls_means(modele, which = "tx", pairwise = TRUE)[1, "Pr(>|t|)"])
    
    # Transformation du coefficient pour exprimer les différences en % 
    results_reg <- results_reg %>%
      mutate(WFPB_SAD_mean = round((100 * exp(WFPB_SAD_mean)) - 100, digits = 1),
             WFPB_SAD_lower = round((100 * exp(WFPB_SAD_lower)) - 100, digits = 1),
             WFPB_SAD_upper = round((100 * exp(WFPB_SAD_upper)) - 100, digits = 1))
    
    # Arrondissement des résultats
    results_reg[, c("SAD_mean", "SAD_sem", "WFPB_mean", "WFPB_sem", "WFPB_SAD_mean", "WFPB_SAD_lower", "WFPB_SAD_upper")] <- 
      round(results_reg[, c("SAD_mean", "SAD_sem", "WFPB_mean", "WFPB_sem", "WFPB_SAD_mean", "WFPB_SAD_lower", "WFPB_SAD_upper")],
            digits = digits_results)
    
    # Mise en forme du mean +/- sem et mean (95% CI) absolu et relatif et arrondissement de la p-value
    results_reg <- results_reg %>%
      mutate(SAD_mean_sem = paste(SAD_mean, "±", SAD_sem),
             WFPB_mean_sem = paste(WFPB_mean, "±", WFPB_sem),
             WFPB_SAD_mean_ci_abs = NA,
             WFPB_SAD_mean_ci_rel = paste0(WFPB_SAD_mean, "% (", WFPB_SAD_lower, "%, ", WFPB_SAD_upper, "%)"),
             p_value = case_when(p_value < 0.0001 ~ "<0.0001",
                                 p_value >= 0.0001 & p_value < 0.001 ~ as.character(round(p_value, 4)),
                                 p_value >= 0.001 & p_value < 0.01 ~ as.character(round(p_value, 3)),
                                 p_value >= 0.01 ~ as.character(round(p_value, 2))))
    
    # Extraction des colonnes pertinentes seulement
    results_reg <- results_reg %>% select(Outcome, SAD_mean_sem, WFPB_mean_sem, WFPB_SAD_mean_ci_abs, WFPB_SAD_mean_ci_rel, p_value)
    
    # Stockage des résultats dans une liste
    results_all <- list(modele = modele,
                        results_reg = results_reg)
    return(results_all)
  }
  
}