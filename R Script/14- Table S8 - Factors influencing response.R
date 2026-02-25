# Date : 14 juin 2024
# Auteur : Jacob Lessard-Lord

# Projet FH-Diet

# Modèles de régression lineaire pour évaluer l'effet de différentes variables sur la réduction de LDL-C en réponse à la diète

# Chargement des packages requis ----
library(tidyverse)
library(rstatix)
library(car)
library(olsrr)
library(rsq)
library(openxlsx)

# Set working directory ----
setwd("C:/Users/jackl/OneDrive/Bureau/École/Postdoctorat/Chercheur/Jean-Philippe/FH-Diet/Processing_FH-Diet/Data")

# Importation des données ----
load("Data diff PP.rda")

# Changer l'ordre des variables pour mettre RD comme refence 
data_diff_pp <- data_diff_pp %>% mutate(hfgenotype = fct_relevel(hfgenotype, "RD", "RN", "Other"))

# Vecteur avec le modèle complet
all_variable_formula_ldlc <- paste(c("sex", "age", "hfgenotype", "bmi", "ldlc_ctrl", "lpa_neph_ctrl", "pcsk9_ctrl",
                                     "crp_neph_ctrl", "homa_ir_ctrl", "glp1_ctrl", "sbp_ctrl",
                                     "Lathosterol_norm_ctrl", "Campesterol_norm_ctrl",
                                     "kcal_mean"),
                                   collapse = " + ")

all_variable_formula_apob <- paste(c("sex", "age", "hfgenotype", "bmi", "apob_neph_ctrl", "lpa_neph_ctrl", "pcsk9_ctrl",
                                     "crp_neph_ctrl", "homa_ir_ctrl", "glp1_ctrl", "sbp_ctrl",
                                     "Lathosterol_norm_ctrl", "Campesterol_norm_ctrl",
                                     "kcal_mean"),
                                   collapse = " + ")

# Modèles de régression pour réduction LDL-C en relatif (%) ----

# Modèle complet
modele_complet_ldlc_rel <- lm(as.formula(paste("ldlc ~ ", all_variable_formula_ldlc)), data = data_diff_pp)

# Multicolinearite - les doivent etre < 10 
ols_vif_tol(modele_complet_ldlc_rel)

# Vérification des hypothèses du modèle
plot(x = modele_complet_ldlc_rel$fitted.values, y = modele_complet_ldlc_rel$residuals) # Homogénéité
qqnorm(resid(modele_complet_ldlc_rel)) # Normalité des résidus Q-Q Plot (point)
qqline(resid(modele_complet_ldlc_rel)) # Normalité des résidus Q-Q Plot (ligne)
shapiro_test(resid(modele_complet_ldlc_rel)) # Normalité des résidus (Shapiro test)

# Influence des observations
influencePlot(modele_complet_ldlc_rel)
influenceIndexPlot(modele_complet_ldlc_rel)

# Résultats du modèle complet
summary(modele_complet_ldlc_rel)

# Extraction des résultats
results_reg_complet_ldlc_rel <- merge(x = rownames_to_column(as.data.frame(summary(modele_complet_ldlc_rel)$coefficients), var = "Variable"),
                                      y = rownames_to_column(as.data.frame(confint(modele_complet_ldlc_rel)), var = "Variable"),
                                      by = "Variable", sort = FALSE)[-1, ] # Retrait de l'intercept

# Renommer les colonnes
colnames(results_reg_complet_ldlc_rel) <- c("Variable", "Estimate", "Std_error", "t_value", "p_value", "lower_ci", "upper_ci")

# Intégrer les r2 partiels aux résultats
results_reg_complet_ldlc_rel <- cbind(subset(results_reg_complet_ldlc_rel, subset = Variable != "hfgenotypeOther"),
                                      data.frame(Variable2 = rsq.partial(modele_complet_ldlc_rel)$variable,
                                                 partial_r2 = rsq.partial(modele_complet_ldlc_rel)$partial.rsq))

# WARNINGS NORMAUX #

# Mise en forme des résultats
results_reg_complet_ldlc_rel <- results_reg_complet_ldlc_rel %>%
  mutate(Estimate = round(Estimate, digits = 2),
         lower_ci = round(lower_ci, digits = 2),
         upper_ci = round(upper_ci, digits = 2),
         beta_ci = paste0(Estimate, " (", lower_ci, ", ", upper_ci, ")"),
         p_value = round(p_value, digits = 4),
         partial_r2 = round((partial_r2 * 100), digits = 1))

# Sélection des résultats pertinents
results_reg_complet_ldlc_rel <- results_reg_complet_ldlc_rel %>% select(c("Variable", "beta_ci", "partial_r2", "p_value"))

# Ajout du r2 du modèle
results_reg_complet_ldlc_rel <- rbind(
  results_reg_complet_ldlc_rel,
  data.frame(Variable = "Model",
             beta_ci = NA,
             partial_r2 = paste0(round(ols_regress(modele_complet_ldlc_rel)$rsq * 100, 1), " (", round(ols_regress(modele_complet_ldlc_rel)$adjr * 100, 1), ")"),
             p_value = round(ols_regress(modele_complet_ldlc_rel)$p, 4))
)

# Arrondissement des p-values
results_reg_complet_ldlc_rel <- results_reg_complet_ldlc_rel %>%
  mutate(p_value = case_when(p_value < 0.0001 ~ "<0.0001",
                             p_value >= 0.0001 & p_value < 0.001 ~ as.character(round(p_value, 4)),
                             p_value >= 0.001 & p_value < 0.01 ~ as.character(round(p_value, 3)),
                             p_value >= 0.01 ~ as.character(round(p_value, 2))))

# Modèles de régression pour réduction ApoB en relatif (%) ----

# Modèle complet
modele_complet_apob_rel <- lm(as.formula(paste("apob_neph ~ ", all_variable_formula_apob)), data = data_diff_pp)

# Multicolinearite - les doivent etre < 10 
ols_vif_tol(modele_complet_apob_rel)

# Vérification des hypothèses du modèle
plot(x = modele_complet_apob_rel$fitted.values, y = modele_complet_apob_rel$residuals) # Homogénéité
qqnorm(resid(modele_complet_apob_rel)) # Normalité des résidus Q-Q Plot (point)
qqline(resid(modele_complet_apob_rel)) # Normalité des résidus Q-Q Plot (ligne)
shapiro_test(resid(modele_complet_apob_rel)) # Normalité des résidus (Shapiro test)

# Influence des observations
influencePlot(modele_complet_apob_rel)
influenceIndexPlot(modele_complet_apob_rel)

# Résultats du modèle complet
summary(modele_complet_apob_rel)

# Extraction des résultats
results_reg_complet_apob_rel <- merge(x = rownames_to_column(as.data.frame(summary(modele_complet_apob_rel)$coefficients), var = "Variable"),
                                      y = rownames_to_column(as.data.frame(confint(modele_complet_apob_rel)), var = "Variable"),
                                      by = "Variable", sort = FALSE)[-1, ] # Retrait de l'intercept

# Renommer les colonnes
colnames(results_reg_complet_apob_rel) <- c("Variable", "Estimate", "Std_error", "t_value", "p_value", "lower_ci", "upper_ci")

# Intégrer les r2 partiels aux résultats
results_reg_complet_apob_rel <- cbind(subset(results_reg_complet_apob_rel, subset = Variable != "hfgenotypeOther"),
                                      data.frame(Variable2 = rsq.partial(modele_complet_apob_rel)$variable,
                                                 partial_r2 = rsq.partial(modele_complet_apob_rel)$partial.rsq))

# WARNINGS NORMAUX #

# Mise en forme des résultats
results_reg_complet_apob_rel <- results_reg_complet_apob_rel %>%
  mutate(Estimate = round(Estimate, digits = 2),
         lower_ci = round(lower_ci, digits = 2),
         upper_ci = round(upper_ci, digits = 2),
         beta_ci = paste0(Estimate, " (", lower_ci, ", ", upper_ci, ")"),
         p_value = round(p_value, digits = 4),
         partial_r2 = round((partial_r2 * 100), digits = 1))

# Sélection des résultats pertinents
results_reg_complet_apob_rel <- results_reg_complet_apob_rel %>% select(c("Variable", "beta_ci", "partial_r2", "p_value"))

# Ajout du r2 du modèle
results_reg_complet_apob_rel <- rbind(
  results_reg_complet_apob_rel,
  data.frame(Variable = "Model",
             beta_ci = NA,
             partial_r2 = paste0(round(ols_regress(modele_complet_apob_rel)$rsq * 100, 1), " (", round(ols_regress(modele_complet_apob_rel)$adjr * 100, 1), ")"),
             p_value = round(ols_regress(modele_complet_apob_rel)$p, 4))
)

# Arrondissement des p-values
results_reg_complet_apob_rel <- results_reg_complet_apob_rel %>%
  mutate(p_value = case_when(p_value < 0.0001 ~ "<0.0001",
                             p_value >= 0.0001 & p_value < 0.001 ~ as.character(round(p_value, 4)),
                             p_value >= 0.001 & p_value < 0.01 ~ as.character(round(p_value, 3)),
                             p_value >= 0.01 ~ as.character(round(p_value, 2))))



# Mise en forme des résultats et exportation ----

# Combinaison des resultats
results_relative <- rbind(results_reg_complet_ldlc_rel, results_reg_complet_apob_rel)

# Exportation des résultats
write.xlsx(results_relative, file = "Table S8.xlsx")

