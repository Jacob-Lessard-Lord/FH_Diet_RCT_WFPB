# Date : 6 juin 2024
# Auteur : Jacob Lessard-Lord

# Projet FH-Diet

# Modèles de régression mixte pour évaluer l'adéquation du protocole

# Intention to treat analysis #

# Chargement des packages requis ----
library(openxlsx)
library(tidyverse)
library(lme4)
library(lmerTest)
library(influence.ME)
library(rstatix)

# Set working directory ----
setwd("C:/Users/jackl/OneDrive/Bureau/École/Postdoctorat/Chercheur/Jean-Philippe/FH-Diet/Processing_FH-Diet/Script R")

# Importation de la fonction ----
source("0B- Mixed linear regression model function.R")

# Set working directory ----
setwd("C:/Users/jackl/OneDrive/Bureau/École/Postdoctorat/Chercheur/Jean-Philippe/FH-Diet/Processing_FH-Diet/Data")

# Importation des données ----
load("Data post-diet ITT.rda")
load("Data weight ITT.rda")

# Mixed models for Protocol quality control / adequacy ----

#### Energy intake ####

# Modèle + résultats du modèle
model_energy <- lmer_reg(data_post_itt, "kcal_mean", "tx + seq + bmi + (1|nomat)", digits_results = 0)

# Validation des hypothèses du modèle
plot(influence(lmer(as.formula(paste("kcal_mean", "~", "tx + seq + bmi + (1|nomat)")),
                    data = data_post_itt, REML = TRUE), obs = TRUE), which = "cook") # Distance de Cook
plot(model_energy$modele) # Homogénéité de la variance
qqnorm(resid(model_energy$modele)) # Normalité des résidus Q-Q Plot (point)
qqline(resid(model_energy$modele)) # Normalité des résidus Q-Q Plot (ligne)
shapiro_test(resid(model_energy$modele)) # Normalité des résidus (Shapiro test)

#### Compliance ####

# Modèle + résultats du modèle
model_compliance <- lmer_reg(data_post_itt, "comp_mean", "tx + seq + (1|nomat)", digits_results = 1)

# Validation des hypothèses du modèle
plot(influence(lmer(as.formula(paste("comp_mean", "~", "tx + seq + (1|nomat)")),
                    data = data_post_itt, REML = TRUE), obs = TRUE), which = "cook") # Distance de Cook
plot(model_compliance$modele) # Homogénéité de la variance
qqnorm(resid(model_compliance$modele)) # Normalité des résidus Q-Q Plot (point)
qqline(resid(model_compliance$modele)) # Normalité des résidus Q-Q Plot (ligne)
shapiro_test(resid(model_compliance$modele)) # Normalité des résidus (Shapiro test)

#### Weight post intervention ####

# Modèle + résultats du modèle
model_weight_post <- lmer_reg(data_post_itt, "weight", "tx + seq + (1|nomat)", digits_results = 1)

# Validation des hypothèses du modèle
plot(influence(lmer(as.formula(paste("weight", "~", "tx + seq + (1|nomat)")),
                    data = data_post_itt, REML = TRUE), obs = TRUE), which = "cook") # Distance de Cook
plot(model_weight_post$modele) # Homogénéité de la variance
qqnorm(resid(model_weight_post$modele)) # Normalité des résidus Q-Q Plot (point)
qqline(resid(model_weight_post$modele)) # Normalité des résidus Q-Q Plot (ligne)
shapiro_test(resid(model_weight_post$modele)) # Normalité des résidus (Shapiro test)

#### VAT post intervention ####

# Modèle + résultats du modèle
model_vat_post <- lmer_reg(data_post_itt, "Masse_Gras_Androide_Viscerale", "tx + bmi + seq + (1|nomat)", digits_results = 0)

# Validation des hypothèses du modèle
plot(influence(lmer(as.formula(paste("Masse_Gras_Androide_Viscerale", "~", "tx + seq + (1|nomat)")),
                    data = data_post_itt, REML = TRUE), obs = TRUE), which = "cook") # Distance de Cook
plot(model_vat_post$modele) # Homogénéité de la variance
qqnorm(resid(model_vat_post$modele)) # Normalité des résidus Q-Q Plot (point)
qqline(resid(model_vat_post$modele)) # Normalité des résidus Q-Q Plot (ligne)
shapiro_test(resid(model_vat_post$modele)) # Normalité des résidus (Shapiro test)

#### Weight change during interventions ####

# Modèle + résultats du modèle
model_weight_change <- lmer_reg(data_post_itt, "weight_delta", "tx + seq + bmi + (1|nomat)")

# Validation des hypothèses du modèle
plot(influence(lmer(as.formula(paste("weight_delta", "~", "tx + seq + bmi + (1|nomat)")),
                    data = data_post_itt, REML = TRUE), obs = TRUE), which = "cook") # Distance de Cook
plot(model_weight_change$modele) # Homogénéité de la variance
qqnorm(resid(model_weight_change$modele)) # Normalité des résidus Q-Q Plot (point)
qqline(resid(model_weight_change$modele)) # Normalité des résidus Q-Q Plot (ligne)
shapiro_test(resid(model_weight_change$modele)) # Normalité des résidus (Shapiro test)

# Vérification si la perte de poids est significative pour chacune des interventions ----

# Regression #

#### SAD ####

# Modèle + résultats du modèle
model_weight_sad <- lmer(SAD ~ Visites + seq + (1|nomat), data = data_weight_itt, REML = TRUE)
summary(model_weight_sad)

# Validation des hypothèses du modèle
plot(influence(lmer(SAD ~ Visites + seq + (1|nomat), data = data_weight_itt, REML = TRUE), obs = TRUE), which = "cook") # Distance de Cook
plot(model_weight_sad) # Homogénéité de la variance
qqnorm(resid(model_weight_sad)) # Normalité des résidus Q-Q Plot (point)
qqline(resid(model_weight_sad)) # Normalité des résidus Q-Q Plot (ligne)
shapiro_test(resid(model_weight_sad)) # Normalité des résidus (Shapiro test)

#### WFPB ####

# Modèle + résultats du modèle
model_weight_wfpb <- lmer(WFPB ~ Visites + seq + (1|nomat), data = data_weight_itt, REML = TRUE)
summary(model_weight_wfpb)

# Validation des hypothèses du modèle
plot(influence(lmer(WFPB ~ Visites + seq + (1|nomat), data = data_weight_itt, REML = TRUE), obs = TRUE), which = "cook") # Distance de Cook
plot(model_weight_wfpb) # Homogénéité de la variance
qqnorm(resid(model_weight_wfpb)) # Normalité des résidus Q-Q Plot (point)
qqline(resid(model_weight_wfpb)) # Normalité des résidus Q-Q Plot (ligne)
shapiro_test(resid(model_weight_wfpb)) # Normalité des résidus (Shapiro test)


# Exportation des donnees ----
results_reg <- rbind(model_compliance$results_reg,
                     model_energy$results_reg,
                     model_weight_post$results_reg,
                     model_vat_post$results_reg,
                     model_weight_change$results_reg)


write.xlsx(results_reg[, c("Outcome", "SAD_mean_sem", "WFPB_mean_sem", "WFPB_SAD_mean_ci_abs", "WFPB_SAD_mean_ci_rel", "p_value")], file = "Table S2.xlsx")


