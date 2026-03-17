# Date : 12 juin 2025
# Auteur : Jacob Lessard-Lord

# Fonction pour entraîner les modèles de régression linéaire mixte stratifié (avec interaction) ----
lmer_reg_strat_log <- function(data_df, var_depend, stratification, formule) {
  ## data_df = Data.frame des données
  ## var_depend = Variable dépendante
  ## stratification = Variable catégorielle sur laquelle la stratification est effectuée
  ## formule = Formule des variables indépendantes
  
  # Extraction de chacun des niveaux du facteur de la stratification
  level_1 <- levels(data_df[, stratification])[1]
  level_2 <- levels(data_df[, stratification])[2]
  
  # Recodage de la variable de stratification pour avoir un dataset par niveau de reference
  data_df_1 <- data_df
  data_df_1[, stratification] <- relevel(data_df[, stratification], level_1)
  data_df_2 <- data_df
  data_df_2[, stratification] <- relevel(data_df[, stratification], level_2)
  
  # Entraînement du modèle selon la 1ère référence
  modele_1 <- lmer(as.formula(paste(var_depend, "~", formule)),
                   data = data_df_1,
                   REML = TRUE)
  
  # Entraînement du modèle selon la 2e référence
  modele_2 <- lmer(as.formula(paste(var_depend, "~", formule)),
                   data = data_df_2,
                   REML = TRUE)
  
  # Calcul des betas, des p-values et des 95% CI
  modele_1_summary <- summary(modele_1)
  modele_1_confint <- data.frame(confint(modele_1))
  modele_2_summary <- summary(modele_2)
  modele_2_confint <- data.frame(confint(modele_2))
  
  # Stockage des resultats dans un data.frame
  results_reg_1 <- data.frame(Outcome = var_depend,
                              Stratification = stratification,
                              Strat_level = level_1,
                              d28_d7_mean = modele_1_summary$coefficients[rownames(modele_1_summary$coefficients) == "vas_day_factorAfter 28 days", "Estimate"],
                              d28_d7_lower = modele_1_confint[rownames(modele_1_confint) == "vas_day_factorAfter 28 days", 1],
                              d28_d7_upper = modele_1_confint[rownames(modele_1_confint) == "vas_day_factorAfter 28 days", 2],
                              p_value = modele_1_summary$coefficients[rownames(modele_1_summary$coefficients) == "vas_day_factorAfter 28 days", "Pr(>|t|)"],
                              p_value_interaction = modele_1_summary$coefficients[nrow(modele_1_summary$coefficients), "Pr(>|t|)"])
  
  results_reg_2 <- data.frame(Outcome = var_depend,
                              Stratification = stratification,
                              Strat_level = level_2,
                              d28_d7_mean = modele_2_summary$coefficients[rownames(modele_2_summary$coefficients) == "vas_day_factorAfter 28 days", "Estimate"],
                              d28_d7_lower = modele_2_confint[rownames(modele_2_confint) == "vas_day_factorAfter 28 days", 1],
                              d28_d7_upper = modele_2_confint[rownames(modele_2_confint) == "vas_day_factorAfter 28 days", 2],
                              p_value = modele_2_summary$coefficients[rownames(modele_2_summary$coefficients) == "vas_day_factorAfter 28 days", "Pr(>|t|)"],
                              p_value_interaction = modele_2_summary$coefficients[nrow(modele_2_summary$coefficients), "Pr(>|t|)"])
  
  # Transformation pour mise en forme du mean difference (95% CI) relatif
  results_reg_1 <- results_reg_1 %>%
    mutate(d28_d7_mean = round((100 * exp(d28_d7_mean)) - 100, digits = 1),
           d28_d7_lower = round((100 * exp(d28_d7_lower)) - 100, digits = 1),
           d28_d7_upper = round((100 * exp(d28_d7_upper)) - 100, digits = 1),
           d28_d7_mean_ci_rel = paste0(d28_d7_mean, "% (", d28_d7_lower, "%, ", d28_d7_upper, "%)"))
  
  results_reg_2 <- results_reg_2 %>%
    mutate(d28_d7_mean = round((100 * exp(d28_d7_mean)) - 100, digits = 1),
           d28_d7_lower = round((100 * exp(d28_d7_lower)) - 100, digits = 1),
           d28_d7_upper = round((100 * exp(d28_d7_upper)) - 100, digits = 1),
           d28_d7_mean_ci_rel = paste0(d28_d7_mean, "% (", d28_d7_lower, "%, ", d28_d7_upper, "%)"))
  
  results_reg <- rbind(results_reg_1, results_reg_2)
  
  # Arrondissement des p-values
  results_reg <- results_reg %>%
    mutate(p_value = case_when(p_value < 0.0001 ~ "<0.0001",
                               p_value >= 0.0001 & p_value < 0.001 ~ as.character(round(p_value, 4)),
                               p_value >= 0.001 & p_value < 0.01 ~ as.character(round(p_value, 3)),
                               p_value >= 0.01 ~ as.character(round(p_value, 2))),
           p_value_interaction = case_when(p_value_interaction < 0.0001 ~ "<0.0001",
                                           p_value_interaction >= 0.0001 & p_value_interaction < 0.001 ~ as.character(round(p_value_interaction, 4)),
                                           p_value_interaction >= 0.001 & p_value_interaction < 0.01 ~ as.character(round(p_value_interaction, 3)),
                                           p_value_interaction >= 0.01 ~ as.character(round(p_value_interaction, 2))))
  
  results_all <- list(modele_1 = modele_1,
                      modele_2 = modele_2,
                      results_reg = results_reg)
  return(results_all)
  
}