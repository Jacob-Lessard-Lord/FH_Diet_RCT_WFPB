# Date : 13 juin 2024
# Auteur : Jacob Lessard-Lord

# Projet FH-Diet

# Modèles de régression mixte pour évaluer l'effet de la diète sur les échelles visuelles analogues

# Effet du temps entre première semaine et fin de l'intervention

# Chargement des packages requis ----
library(openxlsx)
library(tidyverse)
library(lme4)
library(lmerTest)
library(influence.ME)
library(rstatix)
library(ggpubr)

# Set working directory ----
setwd("C:/Users/jackl/OneDrive/Bureau/École/Postdoctorat/Chercheur/Jean-Philippe/FH-Diet/Processing_FH-Diet/Script R")

# Importation de la fonction ----
source("0E- Mixed linear regression model function stratified - Time.R")
source("0F- Mixed linear regression model function stratified - Time log.R")

# Set working directory ----
setwd("C:/Users/jackl/OneDrive/Bureau/École/Postdoctorat/Chercheur/Jean-Philippe/FH-Diet/Processing_FH-Diet/Data")

# Importation des données ----
load("Data VAS.rda")

# Variables explicatives par défaut ----
formule_default <- "tx*vas_day_factor + tx + meal + vas_wkday + vas_day_factor + seq + bmi + kcal + (1|nomat)"

# Men ----

#### Desire to eat ####

# Before # 
model_desire_to_eat_before <- lmer_reg_strat(data_vas_male, "desire_to_eat_before", "tx", formule_default)

# Validation des hypothèses du modèle
plot(influence(lmer(as.formula(paste("desire_to_eat_before", "~", formule_default)),
                    data = data_vas_male, REML = TRUE), obs = TRUE), which = "cook") # Distance de Cook
plot(model_desire_to_eat_before$modele_1) # Homogénéité de la variance
qqnorm(resid(model_desire_to_eat_before$modele_1)) # Normalité des résidus Q-Q Plot (point)
qqline(resid(model_desire_to_eat_before$modele_1)) # Normalité des résidus Q-Q Plot (ligne)
shapiro_test(resid(model_desire_to_eat_before$modele_1)) # Normalité des résidus (Shapiro test)

# After # 
model_desire_to_eat_after <- lmer_reg_strat(data_vas_male, "desire_to_eat_after", "tx", formule_default)

# Validation des hypothèses du modèle
plot(influence(lmer(as.formula(paste("desire_to_eat_after", "~", formule_default)),
                    data = data_vas_male, REML = TRUE), obs = TRUE), which = "cook") # Distance de Cook
plot(model_desire_to_eat_after$modele_1) # Homogénéité de la variance
qqnorm(resid(model_desire_to_eat_after$modele_1)) # Normalité des résidus Q-Q Plot (point)
qqline(resid(model_desire_to_eat_after$modele_1)) # Normalité des résidus Q-Q Plot (ligne)
shapiro_test(resid(model_desire_to_eat_after$modele_1)) # Normalité des résidus (Shapiro test)

# Problème d'homogénéité de normalité des résidus !!! Transformation log !
model_desire_to_eat_after <- lmer_reg_strat_log(data_vas_log_male, "desire_to_eat_after", "tx", formule_default)

# Validation des hypothèses du modèle
plot(model_desire_to_eat_after$modele_1) # Homogénéité de la variance
qqnorm(resid(model_desire_to_eat_after$modele_1)) # Normalité des résidus Q-Q Plot (point)
qqline(resid(model_desire_to_eat_after$modele_1)) # Normalité des résidus Q-Q Plot (ligne)
shapiro_test(resid(model_desire_to_eat_after$modele_1)) # Normalité des résidus (Shapiro test)

#### Hunger ####

# Before # 
model_hunger_before <- lmer_reg_strat(data_vas_male, "hunger_before", "tx", formule_default)

# Validation des hypothèses du modèle
plot(influence(lmer(as.formula(paste("hunger_before", "~", formule_default)),
                    data = data_vas_male, REML = TRUE), obs = TRUE), which = "cook") # Distance de Cook
plot(model_hunger_before$modele_1) # Homogénéité de la variance
qqnorm(resid(model_hunger_before$modele_1)) # Normalité des résidus Q-Q Plot (point)
qqline(resid(model_hunger_before$modele_1)) # Normalité des résidus Q-Q Plot (ligne)
shapiro_test(resid(model_hunger_before$modele_1)) # Normalité des résidus (Shapiro test)

# After # 
model_hunger_after <- lmer_reg_strat(data_vas_male, "hunger_after", "tx", formule_default)

# Validation des hypothèses du modèle
plot(influence(lmer(as.formula(paste("hunger_after", "~", formule_default)),
                    data = data_vas_male, REML = TRUE), obs = TRUE), which = "cook") # Distance de Cook
plot(model_hunger_after$modele_1) # Homogénéité de la variance
qqnorm(resid(model_hunger_after$modele_1)) # Normalité des résidus Q-Q Plot (point)
qqline(resid(model_hunger_after$modele_1)) # Normalité des résidus Q-Q Plot (ligne)
shapiro_test(resid(model_hunger_after$modele_1)) # Normalité des résidus (Shapiro test)

# Problème d'homogénéité de normalité des résidus !!! Transformation log !
model_hunger_after <- lmer_reg_strat_log(data_vas_log_male, "hunger_after", "tx", formule_default)

# Validation des hypothèses du modèle
plot(model_hunger_after$modele_1) # Homogénéité de la variance
qqnorm(resid(model_hunger_after$modele_1)) # Normalité des résidus Q-Q Plot (point)
qqline(resid(model_hunger_after$modele_1)) # Normalité des résidus Q-Q Plot (ligne)
shapiro_test(resid(model_hunger_after$modele_1)) # Normalité des résidus (Shapiro test)

#### Fullness ####

# Before # 
model_fullness_before <- lmer_reg_strat(data_vas_male, "fullness_before", "tx", formule_default)

# Validation des hypothèses du modèle
plot(influence(lmer(as.formula(paste("fullness_before", "~", formule_default)),
                    data = data_vas_male, REML = TRUE), obs = TRUE), which = "cook") # Distance de Cook
plot(model_fullness_before$modele_1) # Homogénéité de la variance
qqnorm(resid(model_fullness_before$modele_1)) # Normalité des résidus Q-Q Plot (point)
qqline(resid(model_fullness_before$modele_1)) # Normalité des résidus Q-Q Plot (ligne)
shapiro_test(resid(model_fullness_before$modele_1)) # Normalité des résidus (Shapiro test)

# Les hypothèses du modèles ne sont vraiment pas respectés, mais la donnée est terrible, je ne vois pas de moyen de remédier le problème ..
# Comme c'est non significatif, je n'y vois pas vraiment de problème

# After # 
model_fullness_after <- lmer_reg_strat(data_vas_male, "fullness_after", "tx", formule_default)

# Validation des hypothèses du modèle
plot(influence(lmer(as.formula(paste("fullness_after", "~", formule_default)),
                    data = data_vas_male, REML = TRUE), obs = TRUE), which = "cook") # Distance de Cook
plot(model_fullness_after$modele_1) # Homogénéité de la variance
qqnorm(resid(model_fullness_after$modele_1)) # Normalité des résidus Q-Q Plot (point)
qqline(resid(model_fullness_after$modele_1)) # Normalité des résidus Q-Q Plot (ligne)
shapiro_test(resid(model_fullness_after$modele_1)) # Normalité des résidus (Shapiro test)

# Hétéroscédasticité des résidus # Appart retirer les extrêmes il n'y a pas grand chose à faire

#### Prospective food consumption ####

# Before # 
model_prospective_food_consumption_before <- lmer_reg_strat(data_vas_male, "prospective_food_consumption_before", "tx", formule_default)

# Validation des hypothèses du modèle
plot(influence(lmer(as.formula(paste("prospective_food_consumption_before", "~", formule_default)),
                    data = data_vas_male, REML = TRUE), obs = TRUE), which = "cook") # Distance de Cook
plot(model_prospective_food_consumption_before$modele_1) # Homogénéité de la variance
qqnorm(resid(model_prospective_food_consumption_before$modele_1)) # Normalité des résidus Q-Q Plot (point)
qqline(resid(model_prospective_food_consumption_before$modele_1)) # Normalité des résidus Q-Q Plot (ligne)
shapiro_test(resid(model_prospective_food_consumption_before$modele_1)) # Normalité des résidus (Shapiro test)

# Les hypothèses du modèles ne sont vraiment pas respectés, mais la donnée est terrible, je ne vois pas de moyen de remédier le problème ..
# Comme c'est non significatif, je n'y vois pas vraiment de problème

# After # 
model_prospective_food_consumption_after <- lmer_reg_strat(data_vas_male, "prospective_food_consumption_after", "tx", formule_default)

# Validation des hypothèses du modèle
plot(influence(lmer(as.formula(paste("prospective_food_consumption_after", "~", formule_default)),
                    data = data_vas_male, REML = TRUE), obs = TRUE), which = "cook") # Distance de Cook
plot(model_prospective_food_consumption_after$modele_1) # Homogénéité de la variance
qqnorm(resid(model_prospective_food_consumption_after$modele_1)) # Normalité des résidus Q-Q Plot (point)
qqline(resid(model_prospective_food_consumption_after$modele_1)) # Normalité des résidus Q-Q Plot (ligne)
shapiro_test(resid(model_prospective_food_consumption_after$modele_1)) # Normalité des résidus (Shapiro test)

# Problème d'homogénéité de normalité des résidus !!! Transformation log !
model_prospective_food_consumption_after <- lmer_reg_strat_log(data_vas_log_male, "prospective_food_consumption_after", "tx", formule_default)

# Validation des hypothèses du modèle
plot(model_prospective_food_consumption_after$modele_1) # Homogénéité de la variance
qqnorm(resid(model_prospective_food_consumption_after$modele_1)) # Normalité des résidus Q-Q Plot (point)
qqline(resid(model_prospective_food_consumption_after$modele_1)) # Normalité des résidus Q-Q Plot (ligne)
shapiro_test(resid(model_prospective_food_consumption_after$modele_1)) # Normalité des résidus (Shapiro test)

#### Appetite score ####

# Before # 
model_appetite_score_before <- lmer_reg_strat(data_vas_male, "appetite_score_before", "tx", formule_default)

# Validation des hypothèses du modèle
plot(influence(lmer(as.formula(paste("appetite_score_before", "~", formule_default)),
                    data = data_vas_male, REML = TRUE), obs = TRUE), which = "cook") # Distance de Cook
plot(model_appetite_score_before$modele_1) # Homogénéité de la variance
qqnorm(resid(model_appetite_score_before$modele_1)) # Normalité des résidus Q-Q Plot (point)
qqline(resid(model_appetite_score_before$modele_1)) # Normalité des résidus Q-Q Plot (ligne)
shapiro_test(resid(model_appetite_score_before$modele_1)) # Normalité des résidus (Shapiro test)

# After # 
model_appetite_score_after <- lmer_reg_strat(data_vas_male, "appetite_score_after", "tx", formule_default)

# Validation des hypothèses du modèle
plot(influence(lmer(as.formula(paste("appetite_score_after", "~", formule_default)),
                    data = data_vas_male, REML = TRUE), obs = TRUE), which = "cook") # Distance de Cook
plot(model_appetite_score_after$modele_1) # Homogénéité de la variance
qqnorm(resid(model_appetite_score_after$modele_1)) # Normalité des résidus Q-Q Plot (point)
qqline(resid(model_appetite_score_after$modele_1)) # Normalité des résidus Q-Q Plot (ligne)
shapiro_test(resid(model_appetite_score_after$modele_1)) # Normalité des résidus (Shapiro test)

# Problème d'homogénéité de normalité des résidus !!! Transformation log !
model_appetite_score_after <- lmer_reg_strat_log(data_vas_log_male, "appetite_score_after", "tx", formule_default)

# Validation des hypothèses du modèle
plot(model_appetite_score_after$modele_1) # Homogénéité de la variance
qqnorm(resid(model_appetite_score_after$modele_1)) # Normalité des résidus Q-Q Plot (point)
qqline(resid(model_appetite_score_after$modele_1)) # Normalité des résidus Q-Q Plot (ligne)
shapiro_test(resid(model_appetite_score_after$modele_1)) # Normalité des résidus (Shapiro test)

#### Satiety quotient ####
model_satiety_quotient <- lmer_reg_strat(data_vas_male, "satiety_quotient_after", "tx", formule_default)

# Validation des hypothèses du modèle
plot(influence(lmer(as.formula(paste("satiety_quotient_after", "~", formule_default)),
                    data = data_vas_male, REML = TRUE), obs = TRUE), which = "cook") # Distance de Cook
plot(model_satiety_quotient$modele_1) # Homogénéité de la variance
qqnorm(resid(model_satiety_quotient$modele_1)) # Normalité des résidus Q-Q Plot (point)
qqline(resid(model_satiety_quotient$modele_1)) # Normalité des résidus Q-Q Plot (ligne)
shapiro_test(resid(model_satiety_quotient$modele_1)) # Normalité des résidus (Shapiro test)

# Hétéroscédasticité des résidus n'est pas idéale #  Problème avec les extrêmes, donc appart retirer des observations, il n'y a pas grand chose à faire

#### Appreciation ####
model_appreciation <- lmer_reg_strat(data_vas_male, "appreciation_after", "tx", formule_default)

# Validation des hypothèses du modèle
plot(influence(lmer(as.formula(paste("appreciation_after", "~", formule_default)),
                    data = data_vas_male, REML = TRUE), obs = TRUE), which = "cook") # Distance de Cook
plot(model_appreciation$modele_1) # Homogénéité de la variance
qqnorm(resid(model_appreciation$modele_1)) # Normalité des résidus Q-Q Plot (point)
qqline(resid(model_appreciation$modele_1)) # Normalité des résidus Q-Q Plot (ligne)
shapiro_test(resid(model_appreciation$modele_1)) # Normalité des résidus (Shapiro test)


# Extraction des résultats ----

# Noms des colonnes a conserver
nom_colonne <- c("Outcome", "Stratification", "Strat_level", "d28_d7_mean_ci_rel", "p_value", "p_value_interaction")

# Création du dataset des résultats de la régression
results_reg_male <- rbind(model_desire_to_eat_before$results_reg[, nom_colonne],
                         model_hunger_before$results_reg[, nom_colonne],
                         model_fullness_before$results_reg[, nom_colonne],
                         model_prospective_food_consumption_before$results_reg[, nom_colonne],
                         model_appetite_score_before$results_reg[, nom_colonne],
                         model_desire_to_eat_after$results_reg[, nom_colonne],
                         model_hunger_after$results_reg[, nom_colonne],
                         model_fullness_after$results_reg[, nom_colonne],
                         model_prospective_food_consumption_after$results_reg[, nom_colonne],
                         model_appetite_score_after$results_reg[, nom_colonne],
                         model_satiety_quotient$results_reg[, nom_colonne],
                         model_appreciation$results_reg[, nom_colonne])
results_reg_male$sex <- "Male"


# Women ----

#### Appetite score ####

# Before # 
model_desire_to_eat_before <- lmer_reg_strat(data_vas_female, "desire_to_eat_before", "tx", formule_default)

# Validation des hypothèses du modèle
plot(influence(lmer(as.formula(paste("desire_to_eat_before", "~", formule_default)),
                    data = data_vas_female, REML = TRUE), obs = TRUE), which = "cook") # Distance de Cook
plot(model_desire_to_eat_before$modele_1) # Homogénéité de la variance
qqnorm(resid(model_desire_to_eat_before$modele_1)) # Normalité des résidus Q-Q Plot (point)
qqline(resid(model_desire_to_eat_before$modele_1)) # Normalité des résidus Q-Q Plot (ligne)
shapiro_test(resid(model_desire_to_eat_before$modele_1)) # Normalité des résidus (Shapiro test)

# After # 
model_desire_to_eat_after <- lmer_reg_strat(data_vas_female, "desire_to_eat_after", "tx", formule_default)

# Validation des hypothèses du modèle
plot(influence(lmer(as.formula(paste("desire_to_eat_after", "~", formule_default)),
                    data = data_vas_female, REML = TRUE), obs = TRUE), which = "cook") # Distance de Cook
plot(model_desire_to_eat_after$modele_1) # Homogénéité de la variance
qqnorm(resid(model_desire_to_eat_after$modele_1)) # Normalité des résidus Q-Q Plot (point)
qqline(resid(model_desire_to_eat_after$modele_1)) # Normalité des résidus Q-Q Plot (ligne)
shapiro_test(resid(model_desire_to_eat_after$modele_1)) # Normalité des résidus (Shapiro test)

# Problème d'homogénéité de normalité des résidus !!! Transformation log !
model_desire_to_eat_after <- lmer_reg_strat_log(data_vas_log_female, "desire_to_eat_after", "tx", formule_default)

# Validation des hypothèses du modèle
plot(model_desire_to_eat_after$modele_1) # Homogénéité de la variance
qqnorm(resid(model_desire_to_eat_after$modele_1)) # Normalité des résidus Q-Q Plot (point)
qqline(resid(model_desire_to_eat_after$modele_1)) # Normalité des résidus Q-Q Plot (ligne)
shapiro_test(resid(model_desire_to_eat_after$modele_1)) # Normalité des résidus (Shapiro test)

#### Hunger ####

# Before # 
model_hunger_before <- lmer_reg_strat(data_vas_female, "hunger_before", "tx", formule_default)

# Validation des hypothèses du modèle
plot(influence(lmer(as.formula(paste("hunger_before", "~", formule_default)),
                    data = data_vas_female, REML = TRUE), obs = TRUE), which = "cook") # Distance de Cook
plot(model_hunger_before$modele_1) # Homogénéité de la variance
qqnorm(resid(model_hunger_before$modele_1)) # Normalité des résidus Q-Q Plot (point)
qqline(resid(model_hunger_before$modele_1)) # Normalité des résidus Q-Q Plot (ligne)
shapiro_test(resid(model_hunger_before$modele_1)) # Normalité des résidus (Shapiro test)

# After # 
model_hunger_after <- lmer_reg_strat(data_vas_female, "hunger_after", "tx", formule_default)

# Validation des hypothèses du modèle
plot(influence(lmer(as.formula(paste("hunger_after", "~", formule_default)),
                    data = data_vas_female, REML = TRUE), obs = TRUE), which = "cook") # Distance de Cook
plot(model_hunger_after$modele_1) # Homogénéité de la variance
qqnorm(resid(model_hunger_after$modele_1)) # Normalité des résidus Q-Q Plot (point)
qqline(resid(model_hunger_after$modele_1)) # Normalité des résidus Q-Q Plot (ligne)
shapiro_test(resid(model_hunger_after$modele_1)) # Normalité des résidus (Shapiro test)

# Problème d'homogénéité de normalité des résidus !!! Transformation log !
model_hunger_after <- lmer_reg_strat_log(data_vas_log_female, "hunger_after", "tx", formule_default)

# Validation des hypothèses du modèle
plot(model_hunger_after$modele_1) # Homogénéité de la variance
qqnorm(resid(model_hunger_after$modele_1)) # Normalité des résidus Q-Q Plot (point)
qqline(resid(model_hunger_after$modele_1)) # Normalité des résidus Q-Q Plot (ligne)
shapiro_test(resid(model_hunger_after$modele_1)) # Normalité des résidus (Shapiro test)

#### Fullness ####

# Before # 
model_fullness_before <- lmer_reg_strat(data_vas_female, "fullness_before", "tx", formule_default)

# Validation des hypothèses du modèle
plot(influence(lmer(as.formula(paste("fullness_before", "~", formule_default)),
                    data = data_vas_female, REML = TRUE), obs = TRUE), which = "cook") # Distance de Cook
plot(model_fullness_before$modele_1) # Homogénéité de la variance
qqnorm(resid(model_fullness_before$modele_1)) # Normalité des résidus Q-Q Plot (point)
qqline(resid(model_fullness_before$modele_1)) # Normalité des résidus Q-Q Plot (ligne)
shapiro_test(resid(model_fullness_before$modele_1)) # Normalité des résidus (Shapiro test)

# After # 
model_fullness_after <- lmer_reg_strat(data_vas_female, "fullness_after", "tx", formule_default)

# Validation des hypothèses du modèle
plot(influence(lmer(as.formula(paste("fullness_after", "~", formule_default)),
                    data = data_vas_female, REML = TRUE), obs = TRUE), which = "cook") # Distance de Cook
plot(model_fullness_after$modele_1) # Homogénéité de la variance
qqnorm(resid(model_fullness_after$modele_1)) # Normalité des résidus Q-Q Plot (point)
qqline(resid(model_fullness_after$modele_1)) # Normalité des résidus Q-Q Plot (ligne)
shapiro_test(resid(model_fullness_after$modele_1)) # Normalité des résidus (Shapiro test)

# Les hypothèses du modèles ne sont vraiment pas respectés, mais la donnée est terrible, je ne vois pas de moyen de remédier le problème ..
# Comme c'est non significatif, je n'y vois pas vraiment de problème

#### Prospective food consumption ####

# Before # 
model_prospective_food_consumption_before <- lmer_reg_strat(data_vas_female, "prospective_food_consumption_before", "tx", formule_default)

# Validation des hypothèses du modèle
plot(influence(lmer(as.formula(paste("prospective_food_consumption_before", "~", formule_default)),
                    data = data_vas_female, REML = TRUE), obs = TRUE), which = "cook") # Distance de Cook
plot(model_prospective_food_consumption_before$modele_1) # Homogénéité de la variance
qqnorm(resid(model_prospective_food_consumption_before$modele_1)) # Normalité des résidus Q-Q Plot (point)
qqline(resid(model_prospective_food_consumption_before$modele_1)) # Normalité des résidus Q-Q Plot (ligne)
shapiro_test(resid(model_prospective_food_consumption_before$modele_1)) # Normalité des résidus (Shapiro test)

# After # 
model_prospective_food_consumption_after <- lmer_reg_strat(data_vas_female, "prospective_food_consumption_after", "tx", formule_default)

# Validation des hypothèses du modèle
plot(influence(lmer(as.formula(paste("prospective_food_consumption_after", "~", formule_default)),
                    data = data_vas_female, REML = TRUE), obs = TRUE), which = "cook") # Distance de Cook
plot(model_prospective_food_consumption_after$modele_1) # Homogénéité de la variance
qqnorm(resid(model_prospective_food_consumption_after$modele_1)) # Normalité des résidus Q-Q Plot (point)
qqline(resid(model_prospective_food_consumption_after$modele_1)) # Normalité des résidus Q-Q Plot (ligne)
shapiro_test(resid(model_prospective_food_consumption_after$modele_1)) # Normalité des résidus (Shapiro test)

# Problème d'homogénéité de normalité des résidus !!! Transformation log !
model_prospective_food_consumption_after <- lmer_reg_strat_log(data_vas_log_female, "prospective_food_consumption_after", "tx", formule_default)

# Validation des hypothèses du modèle
plot(model_prospective_food_consumption_after$modele_1) # Homogénéité de la variance
qqnorm(resid(model_prospective_food_consumption_after$modele_1)) # Normalité des résidus Q-Q Plot (point)
qqline(resid(model_prospective_food_consumption_after$modele_1)) # Normalité des résidus Q-Q Plot (ligne)
shapiro_test(resid(model_prospective_food_consumption_after$modele_1)) # Normalité des résidus (Shapiro test)

#### Appetite score ####

# Before # 
model_appetite_score_before <- lmer_reg_strat(data_vas_female, "appetite_score_before", "tx", formule_default)

# Validation des hypothèses du modèle
plot(influence(lmer(as.formula(paste("appetite_score_before", "~", formule_default)),
                    data = data_vas_female, REML = TRUE), obs = TRUE), which = "cook") # Distance de Cook
plot(model_appetite_score_before$modele_1) # Homogénéité de la variance
qqnorm(resid(model_appetite_score_before$modele_1)) # Normalité des résidus Q-Q Plot (point)
qqline(resid(model_appetite_score_before$modele_1)) # Normalité des résidus Q-Q Plot (ligne)
shapiro_test(resid(model_appetite_score_before$modele_1)) # Normalité des résidus (Shapiro test)

# After # 
model_appetite_score_after <- lmer_reg_strat(data_vas_female, "appetite_score_after", "tx", formule_default)

# Validation des hypothèses du modèle
plot(influence(lmer(as.formula(paste("appetite_score_after", "~", formule_default)),
                    data = data_vas_female, REML = TRUE), obs = TRUE), which = "cook") # Distance de Cook
plot(model_appetite_score_after$modele_1) # Homogénéité de la variance
qqnorm(resid(model_appetite_score_after$modele_1)) # Normalité des résidus Q-Q Plot (point)
qqline(resid(model_appetite_score_after$modele_1)) # Normalité des résidus Q-Q Plot (ligne)
shapiro_test(resid(model_appetite_score_after$modele_1)) # Normalité des résidus (Shapiro test)

# Problème d'homogénéité de normalité des résidus !!! Transformation log !
model_appetite_score_after <- lmer_reg_strat_log(data_vas_log_female, "appetite_score_after", "tx", formule_default)

# Validation des hypothèses du modèle
plot(model_appetite_score_after$modele_1) # Homogénéité de la variance
qqnorm(resid(model_appetite_score_after$modele_1)) # Normalité des résidus Q-Q Plot (point)
qqline(resid(model_appetite_score_after$modele_1)) # Normalité des résidus Q-Q Plot (ligne)
shapiro_test(resid(model_appetite_score_after$modele_1)) # Normalité des résidus (Shapiro test)

#### Satiety quotient ####
model_satiety_quotient <- lmer_reg_strat(data_vas_female, "satiety_quotient_after", "tx", formule_default)

# Validation des hypothèses du modèle
plot(influence(lmer(as.formula(paste("satiety_quotient_after", "~", formule_default)),
                    data = data_vas_female, REML = TRUE), obs = TRUE), which = "cook") # Distance de Cook
plot(model_satiety_quotient$modele_1) # Homogénéité de la variance
qqnorm(resid(model_satiety_quotient$modele_1)) # Normalité des résidus Q-Q Plot (point)
qqline(resid(model_satiety_quotient$modele_1)) # Normalité des résidus Q-Q Plot (ligne)
shapiro_test(resid(model_satiety_quotient$modele_1)) # Normalité des résidus (Shapiro test)

# Hétéroscédasticité des résidus n'est pas idéale # Pas grand chose à faire appart retirer des observation

#### Appreciation ####
model_appreciation <- lmer_reg_strat(data_vas_female, "appreciation_after", "tx", formule_default)

# Validation des hypothèses du modèle
plot(influence(lmer(as.formula(paste("appreciation_after", "~", formule_default)),
                    data = data_vas_female, REML = TRUE), obs = TRUE), which = "cook") # Distance de Cook
plot(model_appreciation$modele_1) # Homogénéité de la variance
qqnorm(resid(model_appreciation$modele_1)) # Normalité des résidus Q-Q Plot (point)
qqline(resid(model_appreciation$modele_1)) # Normalité des résidus Q-Q Plot (ligne)
shapiro_test(resid(model_appreciation$modele_1)) # Normalité des résidus (Shapiro test)

# Extraction des résultats ----

# Noms des colonnes a conserver
nom_colonne <- c("Outcome", "Stratification", "Strat_level", "d28_d7_mean_ci_rel", "p_value", "p_value_interaction")

# Création du dataset des résultats de la régression
results_reg_female <- rbind(model_desire_to_eat_before$results_reg[, nom_colonne],
                           model_hunger_before$results_reg[, nom_colonne],
                           model_fullness_before$results_reg[, nom_colonne],
                           model_prospective_food_consumption_before$results_reg[, nom_colonne],
                           model_appetite_score_before$results_reg[, nom_colonne],
                           model_desire_to_eat_after$results_reg[, nom_colonne],
                           model_hunger_after$results_reg[, nom_colonne],
                           model_fullness_after$results_reg[, nom_colonne],
                           model_prospective_food_consumption_after$results_reg[, nom_colonne],
                           model_appetite_score_after$results_reg[, nom_colonne],
                           model_satiety_quotient$results_reg[, nom_colonne],
                           model_appreciation$results_reg[, nom_colonne])
results_reg_female$sex <- "Female"

# Combiner les résultats males/femelles
results_reg <- rbind(results_reg_female, results_reg_male)

# Exportation des résultats
write.xlsx(results_reg[order(results_reg$sex, results_reg$Strat_level), ], file = "Table S9.xlsx")

