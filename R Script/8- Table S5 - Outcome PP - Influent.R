# Date : 6 juin 2024
# Auteur : Jacob Lessard-Lord

# Projet FH-Diet

# Modèles de régression mixte pour évaluer l'effet de la diète sur le LDL-C et autres paramètres biochimiques

# Per protocol analysis #

# Retrait des sujets influents # 

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
load("Data post-diet PP.rda")

# Formule par défault (variables d'ajustement) ----
formule_default <- "tx + seq + bmi + weight_delta + (1|nomat)"

# Modèles de régression mixte ----

#### HDL-C ####

# Modèle + résultats du modèle
model_hdlc <- lmer_reg(data_post_pp, "hdlc")

# Validation des hypothèses du modèle
plot(influence(lmer(as.formula(paste("hdlc", "~", formule_default)), data = data_post_pp, REML = TRUE), obs = TRUE), which = "cook") # Distance de Cook
plot(model_hdlc$modele) # Homogénéité de la variance
qqnorm(resid(model_hdlc$modele)) # Normalité des résidus Q-Q Plot (point)
qqline(resid(model_hdlc$modele)) # Normalité des résidus Q-Q Plot (ligne)
shapiro_test(resid(model_hdlc$modele)) # Normalité des résidus (Shapiro test)

# Un sujet semble influent et nuit à la normalité

# Extraction des distances de Cook
influence_hdlc <- influence(lmer(as.formula(paste("hdlc", "~", formule_default)), data = data_post_pp, REML = TRUE), obs = TRUE)
cook_hdlc <- data.frame(Num_obs = 1:length(cooks.distance(influence_hdlc)),
                        Cook_dist = cooks.distance(influence_hdlc))
cook_hdlc[order(cook_hdlc$Cook_dist, decreasing = TRUE), ]
data_post_pp[41:42, "nomat"]

model_hdlc <- lmer_reg(subset(data_post_pp, nomat != "022"), "hdlc") # Retrait du sujet 022

# Validation des hypothèses du modèle
plot(model_hdlc$modele) # Homogénéité de la variance
qqnorm(resid(model_hdlc$modele)) # Normalité des résidus Q-Q Plot (point)
qqline(resid(model_hdlc$modele)) # Normalité des résidus Q-Q Plot (ligne)
shapiro_test(resid(model_hdlc$modele)) # Normalité des résidus (Shapiro test)

#### Triglycerides ####

# Modèle + résultats du modèle
model_tg <- lmer_reg(data_post_pp, "tg")

# Validation des hypothèses du modèle
plot(influence(lmer(as.formula(paste("tg", "~", formule_default)), data = data_post_pp, REML = TRUE), obs = TRUE), which = "cook") # Distance de Cook
plot(model_tg$modele) # Homogénéité de la variance
qqnorm(resid(model_tg$modele)) # Normalité des résidus Q-Q Plot (point)
qqline(resid(model_tg$modele)) # Normalité des résidus Q-Q Plot (ligne)
shapiro_test(resid(model_tg$modele)) # Normalité des résidus (Shapiro test)

# Problème de normalité des résidus !!! Transformation log
model_tg <- lmer_reg(data_post_pp, "tg", log_transform = TRUE)

# Validation des hypothèses du modèle
plot(influence(lmer(as.formula(paste("log(tg)", "~", formule_default)), data = data_post_pp, REML = TRUE), obs = TRUE), which = "cook") # Distance de Cook
plot(model_tg$modele) # Homogénéité de la variance
qqnorm(resid(model_tg$modele)) # Normalité des résidus Q-Q Plot (point)
qqline(resid(model_tg$modele)) # Normalité des résidus Q-Q Plot (ligne)
shapiro_test(resid(model_tg$modele)) # Normalité des résidus (Shapiro test)

# Un sujet semble influent et nuit à la normalité

# Extraction des distances de Cook
influence_tg <- influence(lmer(as.formula(paste("log(tg)", "~", formule_default)), data = data_post_pp, REML = TRUE), obs = TRUE)
cook_tg <- data.frame(Num_obs = 1:length(cooks.distance(influence_tg)),
                      Cook_dist = cooks.distance(influence_tg))
cook_tg[order(cook_tg$Cook_dist, decreasing = TRUE), ]
data_post_pp[c(3:4), "nomat"]

model_tg <- lmer_reg(subset(data_post_pp, nomat != "002"), "tg", log_transform = TRUE) # Retrait du sujet 002

# Validation des hypothèses du modèle
plot(model_tg$modele) # Homogénéité de la variance
qqnorm(resid(model_tg$modele)) # Normalité des résidus Q-Q Plot (point)
qqline(resid(model_tg$modele)) # Normalité des résidus Q-Q Plot (ligne)
shapiro_test(resid(model_tg$modele)) # Normalité des résidus (Shapiro test)


#### Lp(a) ####

# Modèle + résultats du modèle
model_lpa_neph <- lmer_reg(data_post_pp, "lpa_neph", digits_results = 1)

# Validation des hypothèses du modèle
plot(influence(lmer(as.formula(paste("lpa_neph", "~", formule_default)), data = data_post_pp, REML = TRUE), obs = TRUE), which = "cook") # Distance de Cook
plot(model_lpa_neph$modele) # Homogénéité de la variance
qqnorm(resid(model_lpa_neph$modele)) # Normalité des résidus Q-Q Plot (point)
qqline(resid(model_lpa_neph$modele)) # Normalité des résidus Q-Q Plot (ligne)
shapiro_test(resid(model_lpa_neph$modele)) # Normalité des résidus (Shapiro test)

# Un sujet semble influent et nuit à la normalité

# Extraction des distances de Cook
influence_lpa_neph <- influence(lmer(as.formula(paste("lpa_neph", "~", formule_default)), data = data_post_pp, REML = TRUE), obs = TRUE)
cook_lpa_neph <- data.frame(Num_obs = 1:length(cooks.distance(influence_lpa_neph)),
                            Cook_dist = cooks.distance(influence_lpa_neph))
cook_lpa_neph[order(cook_lpa_neph$Cook_dist, decreasing = TRUE), ]
data_post_pp[c(59:60), "nomat"]

model_lpa_neph <- lmer_reg(subset(data_post_pp, nomat != "031"), "lpa_neph", digits_results = 1) # Retrait du sujet 031

# Validation des hypothèses du modèle
plot(model_lpa_neph$modele) # Homogénéité de la variance
qqnorm(resid(model_lpa_neph$modele)) # Normalité des résidus Q-Q Plot (point)
qqline(resid(model_lpa_neph$modele)) # Normalité des résidus Q-Q Plot (ligne)
shapiro_test(resid(model_lpa_neph$modele)) # Normalité des résidus (Shapiro test)

#### Diastolic blood pressure ####

# Modèle + résultats du modèle
model_dbp <- lmer_reg(data_post_pp, "dbp", digits_results = 1)

# Validation des hypothèses du modèle
plot(influence(lmer(as.formula(paste("dbp", "~", formule_default)), data = data_post_pp, REML = TRUE), obs = TRUE), which = "cook") # Distance de Cook
plot(model_dbp$modele) # Homogénéité de la variance
qqnorm(resid(model_dbp$modele)) # Normalité des résidus Q-Q Plot (point)
qqline(resid(model_dbp$modele)) # Normalité des résidus Q-Q Plot (ligne)
shapiro_test(resid(model_dbp$modele)) # Normalité des résidus (Shapiro test)

# Un sujet semble influent et nuit à la normalité

# Extraction des distances de Cook
influence_dbp <- influence(lmer(as.formula(paste("dbp", "~", formule_default)), data = data_post_pp, REML = TRUE), obs = TRUE)
cook_dbp <- data.frame(Num_obs = 1:length(cooks.distance(influence_dbp)),
                       Cook_dist = cooks.distance(influence_dbp))
cook_dbp[order(cook_dbp$Cook_dist, decreasing = TRUE), ]
data_post_pp[c(9:10), "nomat"]

model_dbp <- lmer_reg(subset(data_post_pp, nomat != "005"), "dbp", digits_results = 1) # Retrait du sujet 005

# Validation des hypothèses du modèle
plot(model_dbp$modele) # Homogénéité de la variance
qqnorm(resid(model_dbp$modele)) # Normalité des résidus Q-Q Plot (point)
qqline(resid(model_dbp$modele)) # Normalité des résidus Q-Q Plot (ligne)
shapiro_test(resid(model_dbp$modele)) # Normalité des résidus (Shapiro test)

# Création du dataset des résultats de la régression
results_reg <- rbind(model_hdlc$results_reg,
                     model_tg$results_reg,
                     model_lpa_neph$results_reg,
                     model_dbp$results_reg)

# Exportation des résultats
write.xlsx(results_reg, file = "Table S5.xlsx")