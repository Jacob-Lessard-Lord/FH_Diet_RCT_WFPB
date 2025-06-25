# Date : 12 juin 2024
# Auteur : Jacob Lessard-Lord

# Projet FH-Diet

# Modèles de régression mixte pour évaluer l'effet de la diète sur les échelles visuelles analogues

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
source("0C- Mixed linear regression model function stratified.R")
source("0D- Mixed linear regression model function stratified log.R")

# Set working directory ----
setwd("C:/Users/jackl/OneDrive/Bureau/École/Postdoctorat/Chercheur/Jean-Philippe/FH-Diet/Processing_FH-Diet/Data")

# Importation des données ----
load("Data VAS post-diet.rda")

# Variables explicatives par défaut ----
formule_default <- "tx*sex + tx + meal + vas_wkday + age + sex + seq + bmi + kcal + (1|nomat)"

# Modèles de régression ----

#### Desire to eat ####

# Before # 
model_desire_to_eat_before <- lmer_reg_strat(data_vas_w4, "desire_to_eat_before", "sex",
                                             "desire_to_eat_before ~ tx*sex + tx + meal + vas_wkday + age + sex + seq + bmi + kcal + (1|nomat)")

# Validation des hypothèses du modèle
plot(influence(lmer(as.formula(paste("desire_to_eat_before", "~", formule_default)),
                    data = data_vas_w4, REML = TRUE), obs = TRUE), which = "cook") # Distance de Cook
plot(model_desire_to_eat_before$modele_1) # Homogénéité de la variance
qqnorm(resid(model_desire_to_eat_before$modele_1)) # Normalité des résidus Q-Q Plot (point)
qqline(resid(model_desire_to_eat_before$modele_1)) # Normalité des résidus Q-Q Plot (ligne)
shapiro_test(resid(model_desire_to_eat_before$modele_1)) # Normalité des résidus (Shapiro test)

# After # 
model_desire_to_eat_after <- lmer_reg_strat(data_vas_w4, "desire_to_eat_after", "sex",
                                            "desire_to_eat_after ~ tx*sex + tx + meal + vas_wkday + age + sex + seq + bmi + kcal + (1|nomat)")

# Validation des hypothèses du modèle
plot(influence(lmer(as.formula(paste("desire_to_eat_after", "~", formule_default)),
                    data = data_vas_w4, REML = TRUE), obs = TRUE), which = "cook") # Distance de Cook
plot(model_desire_to_eat_after$modele_1) # Homogénéité de la variance
qqnorm(resid(model_desire_to_eat_after$modele_1)) # Normalité des résidus Q-Q Plot (point)
qqline(resid(model_desire_to_eat_after$modele_1)) # Normalité des résidus Q-Q Plot (ligne)
shapiro_test(resid(model_desire_to_eat_after$modele_1)) # Normalité des résidus (Shapiro test)

# Problème d'homogénéité de normalité des résidus !!! Transformation log !
model_desire_to_eat_after <- lmer_reg_strat_log(data_vas_log_w4, "desire_to_eat_after", "sex",
                                                "desire_to_eat_after ~ tx*sex + tx + meal + vas_wkday + age + sex + seq + bmi + kcal + (1|nomat)")

# Validation des hypothèses du modèle
plot(model_desire_to_eat_after$modele_1) # Homogénéité de la variance
qqnorm(resid(model_desire_to_eat_after$modele_1)) # Normalité des résidus Q-Q Plot (point)
qqline(resid(model_desire_to_eat_after$modele_1)) # Normalité des résidus Q-Q Plot (ligne)
shapiro_test(resid(model_desire_to_eat_after$modele_1)) # Normalité des résidus (Shapiro test)

# C'est réglé !

#### Hunger ####

# Before # 
model_hunger_before <- lmer_reg_strat(data_vas_w4, "hunger_before", "sex",
                                      "hunger_before ~ tx*sex + tx + meal + vas_wkday + age + sex + seq + bmi + kcal + (1|nomat)")

# Validation des hypothèses du modèle
plot(influence(lmer(as.formula(paste("hunger_before", "~", formule_default)),
                    data = data_vas_w4, REML = TRUE), obs = TRUE), which = "cook") # Distance de Cook
plot(model_hunger_before$modele_1) # Homogénéité de la variance
qqnorm(resid(model_hunger_before$modele_1)) # Normalité des résidus Q-Q Plot (point)
qqline(resid(model_hunger_before$modele_1)) # Normalité des résidus Q-Q Plot (ligne)
shapiro_test(resid(model_hunger_before$modele_1)) # Normalité des résidus (Shapiro test)

# L'homogénéité de la variance n'est pas parfaite, mais il n'y a rien a faire ...

# After # 
model_hunger_after <- lmer_reg_strat(data_vas_w4, "hunger_after", "sex",
                                     "hunger_after ~ tx*sex + tx + meal + vas_wkday + age + sex + seq + bmi + kcal + (1|nomat)")

# Validation des hypothèses du modèle
plot(influence(lmer(as.formula(paste("hunger_after", "~", formule_default)),
                    data = data_vas_w4, REML = TRUE), obs = TRUE), which = "cook") # Distance de Cook
plot(model_hunger_after$modele_1) # Homogénéité de la variance
qqnorm(resid(model_hunger_after$modele_1)) # Normalité des résidus Q-Q Plot (point)
qqline(resid(model_hunger_after$modele_1)) # Normalité des résidus Q-Q Plot (ligne)
shapiro_test(resid(model_hunger_after$modele_1)) # Normalité des résidus (Shapiro test)

# Problème d'homogénéité de normalité des résidus !!! Transformation log !
model_hunger_after <- lmer_reg_strat_log(data_vas_log_w4, "hunger_after", "sex",
                                         "hunger_after ~ tx*sex + tx + meal + vas_wkday + age + sex + seq + bmi + kcal + (1|nomat)")

# Validation des hypothèses du modèle
plot(model_hunger_after$modele_1) # Homogénéité de la variance
qqnorm(resid(model_hunger_after$modele_1)) # Normalité des résidus Q-Q Plot (point)
qqline(resid(model_hunger_after$modele_1)) # Normalité des résidus Q-Q Plot (ligne)
shapiro_test(resid(model_hunger_after$modele_1)) # Normalité des résidus (Shapiro test)

# C'est réglé !

#### Fullness ####

# Before # 
model_fullness_before <- lmer_reg_strat(data_vas_w4, "fullness_before", "sex",
                                        "fullness_before ~ tx*sex + tx + meal + vas_wkday + age + sex + seq + bmi + kcal + (1|nomat)")

# Validation des hypothèses du modèle
plot(influence(lmer(as.formula(paste("fullness_before", "~", formule_default)),
                    data = data_vas_w4, REML = TRUE), obs = TRUE), which = "cook") # Distance de Cook
plot(model_fullness_before$modele_1) # Homogénéité de la variance
qqnorm(resid(model_fullness_before$modele_1)) # Normalité des résidus Q-Q Plot (point)
qqline(resid(model_fullness_before$modele_1)) # Normalité des résidus Q-Q Plot (ligne)
shapiro_test(resid(model_fullness_before$modele_1)) # Normalité des résidus (Shapiro test)

# Hétéroscédasticité des résidus n'est pas idéale # Pas grand chose à faire appart retirer des observations

# After # 
model_fullness_after <- lmer_reg_strat(data_vas_w4, "fullness_after", "sex",
                                       "fullness_after ~ tx*sex + tx + meal + vas_wkday + age + sex + seq + bmi + kcal + (1|nomat)")

# Validation des hypothèses du modèle
plot(influence(lmer(as.formula(paste("fullness_after", "~", formule_default)),
                    data = data_vas_w4, REML = TRUE), obs = TRUE), which = "cook") # Distance de Cook
plot(model_fullness_after$modele_1) # Homogénéité de la variance
qqnorm(resid(model_fullness_after$modele_1)) # Normalité des résidus Q-Q Plot (point)
qqline(resid(model_fullness_after$modele_1)) # Normalité des résidus Q-Q Plot (ligne)
shapiro_test(resid(model_fullness_after$modele_1)) # Normalité des résidus (Shapiro test)

#### Prospective food consumption ####

# Before # 
model_prospective_food_consumption_before <- lmer_reg_strat(data_vas_w4, "prospective_food_consumption_before", "sex",
                                                            "prospective_food_consumption_before ~ tx*sex + tx + meal + vas_wkday + age + sex + seq + bmi + kcal + (1|nomat)")

# Validation des hypothèses du modèle
plot(influence(lmer(as.formula(paste("prospective_food_consumption_before", "~", formule_default)),
                    data = data_vas_w4, REML = TRUE), obs = TRUE), which = "cook") # Distance de Cook
plot(model_prospective_food_consumption_before$modele_1) # Homogénéité de la variance
qqnorm(resid(model_prospective_food_consumption_before$modele_1)) # Normalité des résidus Q-Q Plot (point)
qqline(resid(model_prospective_food_consumption_before$modele_1)) # Normalité des résidus Q-Q Plot (ligne)
shapiro_test(resid(model_prospective_food_consumption_before$modele_1)) # Normalité des résidus (Shapiro test)

# Hétéroscédasticité des résidus n'est pas idéale # Pas grand chose à faire appart retirer des observations 

# After # 
model_prospective_food_consumption_after <- lmer_reg_strat(data_vas_w4, "prospective_food_consumption_after", "sex",
                                                           "prospective_food_consumption_after ~ tx*sex + tx + meal + vas_wkday + age + sex + seq + bmi + kcal + (1|nomat)")

# Validation des hypothèses du modèle
plot(influence(lmer(as.formula(paste("prospective_food_consumption_after", "~", formule_default)),
                    data = data_vas_w4, REML = TRUE), obs = TRUE), which = "cook") # Distance de Cook
plot(model_prospective_food_consumption_after$modele_1) # Homogénéité de la variance
qqnorm(resid(model_prospective_food_consumption_after$modele_1)) # Normalité des résidus Q-Q Plot (point)
qqline(resid(model_prospective_food_consumption_after$modele_1)) # Normalité des résidus Q-Q Plot (ligne)
shapiro_test(resid(model_prospective_food_consumption_after$modele_1)) # Normalité des résidus (Shapiro test)

# Problème d'homogénéité de normalité des résidus !!! Transformation log !
model_prospective_food_consumption_after <- lmer_reg_strat_log(data_vas_log_w4, "prospective_food_consumption_after", "sex",
                                                               "prospective_food_consumption_after ~ tx*sex + tx + meal + vas_wkday + age + sex + seq + bmi + kcal + (1|nomat)")

# Validation des hypothèses du modèle
plot(model_prospective_food_consumption_after$modele_1) # Homogénéité de la variance
qqnorm(resid(model_prospective_food_consumption_after$modele_1)) # Normalité des résidus Q-Q Plot (point)
qqline(resid(model_prospective_food_consumption_after$modele_1)) # Normalité des résidus Q-Q Plot (ligne)
shapiro_test(resid(model_prospective_food_consumption_after$modele_1)) # Normalité des résidus (Shapiro test)

# C'est réglé !

#### Appetite score ####

# Before # 
model_appetite_score_before <- lmer_reg_strat(data_vas_w4, "appetite_score_before", "sex",
                                              "appetite_score_before ~ tx*sex + tx + meal + vas_wkday + age + sex + seq + bmi + kcal + (1|nomat)")

# Validation des hypothèses du modèle
plot(influence(lmer(as.formula(paste("appetite_score_before", "~", formule_default)),
                    data = data_vas_w4, REML = TRUE), obs = TRUE), which = "cook") # Distance de Cook
plot(model_appetite_score_before$modele_1) # Homogénéité de la variance
qqnorm(resid(model_appetite_score_before$modele_1)) # Normalité des résidus Q-Q Plot (point)
qqline(resid(model_appetite_score_before$modele_1)) # Normalité des résidus Q-Q Plot (ligne)
shapiro_test(resid(model_appetite_score_before$modele_1)) # Normalité des résidus (Shapiro test)

# After # 
model_appetite_score_after <- lmer_reg_strat(data_vas_w4, "appetite_score_after", "sex",
                                             "appetite_score_after ~ tx*sex + tx + meal + vas_wkday + age + sex + seq + bmi + kcal + (1|nomat)")

# Validation des hypothèses du modèle
plot(influence(lmer(as.formula(paste("appetite_score_after", "~", formule_default)),
                    data = data_vas_w4, REML = TRUE), obs = TRUE), which = "cook") # Distance de Cook
plot(model_appetite_score_after$modele_1) # Homogénéité de la variance
qqnorm(resid(model_appetite_score_after$modele_1)) # Normalité des résidus Q-Q Plot (point)
qqline(resid(model_appetite_score_after$modele_1)) # Normalité des résidus Q-Q Plot (ligne)
shapiro_test(resid(model_appetite_score_after$modele_1)) # Normalité des résidus (Shapiro test)

# Problème d'homogénéité de normalité des résidus !!! Transformation log !
model_appetite_score_after <- lmer_reg_strat_log(data_vas_log_w4, "appetite_score_after", "sex",
                                                 "appetite_score_after ~ tx*sex + tx + meal + vas_wkday + age + sex + seq + bmi + kcal + (1|nomat)")

# Validation des hypothèses du modèle
plot(model_appetite_score_after$modele_1) # Homogénéité de la variance
qqnorm(resid(model_appetite_score_after$modele_1)) # Normalité des résidus Q-Q Plot (point)
qqline(resid(model_appetite_score_after$modele_1)) # Normalité des résidus Q-Q Plot (ligne)
shapiro_test(resid(model_appetite_score_after$modele_1)) # Normalité des résidus (Shapiro test)

# C'est réglé !

#### Satiety quotient ####
model_satiety_quotient <- lmer_reg_strat(data_vas_w4, "satiety_quotient_after", "sex",
                                         "satiety_quotient_after ~ tx*sex + tx + meal + vas_wkday + age + sex + seq + bmi + kcal + (1|nomat)")

# Validation des hypothèses du modèle
plot(influence(lmer(as.formula(paste("satiety_quotient_after", "~", formule_default)),
                    data = data_vas_w4, REML = TRUE), obs = TRUE), which = "cook") # Distance de Cook
plot(model_satiety_quotient$modele_1) # Homogénéité de la variance
qqnorm(resid(model_satiety_quotient$modele_1)) # Normalité des résidus Q-Q Plot (point)
qqline(resid(model_satiety_quotient$modele_1)) # Normalité des résidus Q-Q Plot (ligne)
shapiro_test(resid(model_satiety_quotient$modele_1)) # Normalité des résidus (Shapiro test)

# Hétéroscédasticité des résidus n'est pas idéale # Rien à faire appart retirer des observations

#### Appreciation ####
model_appreciation <- lmer_reg_strat(data_vas_w4, "appreciation_after", "sex",
                                     "appreciation_after ~ tx*sex + tx + meal + vas_wkday + age + sex + seq + bmi + kcal + (1|nomat)")

# Validation des hypothèses du modèle
plot(influence(lmer(as.formula(paste("appreciation_after", "~", formule_default)),
                    data = data_vas_w4, REML = TRUE), obs = TRUE), which = "cook") # Distance de Cook
plot(model_appreciation$modele_1) # Homogénéité de la variance
qqnorm(resid(model_appreciation$modele_1)) # Normalité des résidus Q-Q Plot (point)
qqline(resid(model_appreciation$modele_1)) # Normalité des résidus Q-Q Plot (ligne)
shapiro_test(resid(model_appreciation$modele_1)) # Normalité des résidus (Shapiro test)

# Hétéroscédasticité des résidus n'est pas idéale # Rien à faire appart retirer des observations

# Noms des colonnes a conserver
nom_colonne <- c("Outcome", "Stratification", "Strat_level", "WFPB_SAD_mean_ci_rel", "p_value", "p_value_interaction")

# Création du dataset des résultats de la régression
results_reg <- rbind(model_desire_to_eat_before$results_reg[, nom_colonne],
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

write.xlsx(results_reg[order(results_reg$Strat_level), ],
           file = "Table 6.xlsx")
