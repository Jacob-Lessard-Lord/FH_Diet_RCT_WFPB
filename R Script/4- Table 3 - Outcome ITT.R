# Date : 6 juin 2024
# Auteur : Jacob Lessard-Lord

# Projet FH-Diet

# Modèles de régression mixte pour évaluer l'effet de la diète sur le LDL-C et autres paramètres biochimiques

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

# Formule par défault (variables d'ajustement) ----
formule_default <- "tx + seq + bmi + weight_delta + (1|nomat)"

# Modèles de régression mixte ----

#### LDL-C ####

# Modèle + résultats du modèle
model_ldlc <- lmer_reg(data_post_itt, "ldlc")

# Validation des hypothèses du modèle
plot(influence(lmer(as.formula(paste("ldlc", "~", formule_default)), data = data_post_itt, REML = TRUE), obs = TRUE), which = "cook") # Distance de Cook
plot(model_ldlc$modele) # Homogénéité de la variance
qqnorm(resid(model_ldlc$modele)) # Normalité des résidus Q-Q Plot (point)
qqline(resid(model_ldlc$modele)) # Normalité des résidus Q-Q Plot (ligne)
shapiro_test(resid(model_ldlc$modele)) # Normalité des résidus (Shapiro test)

#### LDL-C corrigé pour Lp(a)####

# Modèle + résultats du modèle
model_ldlc_lpa_corr <- lmer_reg(data_post_itt, "ldlc_lpa_corr")

# Validation des hypothèses du modèle
plot(influence(lmer(as.formula(paste("ldlc_lpa_corr", "~", formule_default)), data = data_post_itt, REML = TRUE), obs = TRUE), which = "cook") # Distance de Cook
plot(model_ldlc_lpa_corr$modele) # Homogénéité de la variance
qqnorm(resid(model_ldlc_lpa_corr$modele)) # Normalité des résidus Q-Q Plot (point)
qqline(resid(model_ldlc_lpa_corr$modele)) # Normalité des résidus Q-Q Plot (ligne)
shapiro_test(resid(model_ldlc_lpa_corr$modele)) # Normalité des résidus (Shapiro test)


#### Cholesterol ####

# Modèle + résultats du modèle
model_chol <- lmer_reg(data_post_itt, "chol")

# Validation des hypothèses du modèle
plot(influence(lmer(as.formula(paste("chol", "~", formule_default)), data = data_post_itt, REML = TRUE), obs = TRUE), which = "cook") # Distance de Cook
plot(model_chol$modele) # Homogénéité de la variance
qqnorm(resid(model_chol$modele)) # Normalité des résidus Q-Q Plot (point)
qqline(resid(model_chol$modele)) # Normalité des résidus Q-Q Plot (ligne)
shapiro_test(resid(model_chol$modele)) # Normalité des résidus (Shapiro test)

#### HDL-C ####

# Modèle + résultats du modèle
model_hdlc <- lmer_reg(data_post_itt, "hdlc")

# Validation des hypothèses du modèle
plot(influence(lmer(as.formula(paste("hdlc", "~", formule_default)), data = data_post_itt, REML = TRUE), obs = TRUE), which = "cook") # Distance de Cook
plot(model_hdlc$modele) # Homogénéité de la variance
qqnorm(resid(model_hdlc$modele)) # Normalité des résidus Q-Q Plot (point)
qqline(resid(model_hdlc$modele)) # Normalité des résidus Q-Q Plot (ligne)
shapiro_test(resid(model_hdlc$modele)) # Normalité des résidus (Shapiro test)

# Un sujet a une grande influence, son retrait est testé en matériel supplémentaire

#### Triglycerides ####

# Modèle + résultats du modèle
model_tg <- lmer_reg(data_post_itt, "tg")

# Validation des hypothèses du modèle
plot(influence(lmer(as.formula(paste("tg", "~", formule_default)), data = data_post_itt, REML = TRUE), obs = TRUE), which = "cook") # Distance de Cook
plot(model_tg$modele) # Homogénéité de la variance
qqnorm(resid(model_tg$modele)) # Normalité des résidus Q-Q Plot (point)
qqline(resid(model_tg$modele)) # Normalité des résidus Q-Q Plot (ligne)
shapiro_test(resid(model_tg$modele)) # Normalité des résidus (Shapiro test)

# Problème de normalité des résidus !!! Transformation log
model_tg <- lmer_reg(data_post_itt, "tg", log_transform = TRUE)

# Validation des hypothèses du modèle
plot(influence(lmer(as.formula(paste("log(tg)", "~", formule_default)), data = data_post_itt, REML = TRUE), obs = TRUE), which = "cook") # Distance de Cook
plot(model_tg$modele) # Homogénéité de la variance
qqnorm(resid(model_tg$modele)) # Normalité des résidus Q-Q Plot (point)
qqline(resid(model_tg$modele)) # Normalité des résidus Q-Q Plot (ligne)
shapiro_test(resid(model_tg$modele)) # Normalité des résidus (Shapiro test)

# Un sujet a une grande influence, son retrait est testé en matériel supplémentaire

#### Non HDL-C ####

# Modèle + résultats du modèle
model_nhdlc <- lmer_reg(data_post_itt, "nhdlc")

# Validation des hypothèses du modèle
plot(influence(lmer(as.formula(paste("nhdlc", "~", formule_default)), data = data_post_itt, REML = TRUE), obs = TRUE), which = "cook") # Distance de Cook
plot(model_nhdlc$modele) # Homogénéité de la variance
qqnorm(resid(model_nhdlc$modele)) # Normalité des résidus Q-Q Plot (point)
qqline(resid(model_nhdlc$modele)) # Normalité des résidus Q-Q Plot (ligne)
shapiro_test(resid(model_nhdlc$modele)) # Normalité des résidus (Shapiro test)

#### ApoA1 ####

# Modèle + résultats du modèle
model_apoa1_neph <- lmer_reg(data_post_itt, "apoa1_neph")

# Validation des hypothèses du modèle
plot(influence(lmer(as.formula(paste("apoa1_neph", "~", formule_default)), data = data_post_itt, REML = TRUE), obs = TRUE), which = "cook") # Distance de Cook
plot(model_apoa1_neph$modele) # Homogénéité de la variance
qqnorm(resid(model_apoa1_neph$modele)) # Normalité des résidus Q-Q Plot (point)
qqline(resid(model_apoa1_neph$modele)) # Normalité des résidus Q-Q Plot (ligne)
shapiro_test(resid(model_apoa1_neph$modele)) # Normalité des résidus (Shapiro test)

#### ApoB ####

# Modèle + résultats du modèle
model_apob_neph <- lmer_reg(data_post_itt, "apob_neph")

# Validation des hypothèses du modèle
plot(influence(lmer(as.formula(paste("apob_neph", "~", formule_default)), data = data_post_itt, REML = TRUE), obs = TRUE), which = "cook") # Distance de Cook
plot(model_apob_neph$modele) # Homogénéité de la variance
qqnorm(resid(model_apob_neph$modele)) # Normalité des résidus Q-Q Plot (point)
qqline(resid(model_apob_neph$modele)) # Normalité des résidus Q-Q Plot (ligne)
shapiro_test(resid(model_apob_neph$modele)) # Normalité des résidus (Shapiro test)

#### Lp(a) ####

# Modèle + résultats du modèle
model_lpa_neph <- lmer_reg(data_post_itt, "lpa_neph", formule_default, digits_results = 1)

# Validation des hypothèses du modèle
plot(influence(lmer(as.formula(paste("lpa_neph", "~", formule_default)), data = data_post_itt, REML = TRUE), obs = TRUE), which = "cook") # Distance de Cook
plot(model_lpa_neph$modele) # Homogénéité de la variance
qqnorm(resid(model_lpa_neph$modele)) # Normalité des résidus Q-Q Plot (point)
qqline(resid(model_lpa_neph$modele)) # Normalité des résidus Q-Q Plot (ligne)
shapiro_test(resid(model_lpa_neph$modele)) # Normalité des résidus (Shapiro test)

# Un sujet a une grande influence, son retrait est testé en matériel supplémentaire

#### C-Reactive protein ####

# Modèle + résultats du modèle
model_crp_neph <- lmer_reg(data_post_itt, "crp_neph")

# Validation des hypothèses du modèle
plot(influence(lmer(as.formula(paste("crp_neph", "~", formule_default)), data = data_post_itt, REML = TRUE), obs = TRUE), which = "cook") # Distance de Cook
plot(model_crp_neph$modele) # Homogénéité de la variance
qqnorm(resid(model_crp_neph$modele)) # Normalité des résidus Q-Q Plot (point)
qqline(resid(model_crp_neph$modele)) # Normalité des résidus Q-Q Plot (ligne)
shapiro_test(resid(model_crp_neph$modele)) # Normalité des résidus (Shapiro test)

# Problème de normalité des résidus !!! Transformation log
model_crp_neph <- lmer_reg(data_post_itt, "crp_neph", log_transform = TRUE) # Transformation log

# Validation des hypothèses du modèle
plot(influence(lmer(as.formula(paste("log(crp_neph)", "~", formule_default)), data = data_post_itt, REML = TRUE), obs = TRUE), which = "cook") # Distance de Cook
plot(model_crp_neph$modele) # Homogénéité de la variance
qqnorm(resid(model_crp_neph$modele)) # Normalité des résidus Q-Q Plot (point)
qqline(resid(model_crp_neph$modele)) # Normalité des résidus Q-Q Plot (ligne)
shapiro_test(resid(model_crp_neph$modele)) # Normalité des résidus (Shapiro test)

#### Fasting glucose ####

# Modèle + résultats du modèle
model_glufast <- lmer_reg(data_post_itt, "glufast")

# Validation des hypothèses du modèle
plot(influence(lmer(as.formula(paste("glufast", "~", formule_default)), data = data_post_itt, REML = TRUE), obs = TRUE), which = "cook") # Distance de Cook
plot(model_glufast$modele) # Homogénéité de la variance
qqnorm(resid(model_glufast$modele)) # Normalité des résidus Q-Q Plot (point)
qqline(resid(model_glufast$modele)) # Normalité des résidus Q-Q Plot (ligne)
shapiro_test(resid(model_glufast$modele)) # Normalité des résidus (Shapiro test)

#### Fasting insulin ####

# Modèle + résultats du modèle
model_ins2018 <- lmer_reg(data_post_itt, "ins2018", digits_results = 1)

# Validation des hypothèses du modèle
plot(influence(lmer(as.formula(paste("ins2018", "~", formule_default)), data = data_post_itt, REML = TRUE), obs = TRUE), which = "cook") # Distance de Cook
plot(model_ins2018$modele) # Homogénéité de la variance
qqnorm(resid(model_ins2018$modele)) # Normalité des résidus Q-Q Plot (point)
qqline(resid(model_ins2018$modele)) # Normalité des résidus Q-Q Plot (ligne)
shapiro_test(resid(model_ins2018$modele)) # Normalité des résidus (Shapiro test)

#### HbA1c ####

# Modèle + résultats du modèle
model_hba1c <- lmer_reg(data_post_itt, "hba1c")

# Validation des hypothèses du modèle
plot(influence(lmer(as.formula(paste("hba1c", "~", formule_default)), data = data_post_itt, REML = TRUE), obs = TRUE), which = "cook") # Distance de Cook
plot(model_hba1c$modele) # Homogénéité de la variance
qqnorm(resid(model_hba1c$modele)) # Normalité des résidus Q-Q Plot (point)
qqline(resid(model_hba1c$modele)) # Normalité des résidus Q-Q Plot (ligne)
shapiro_test(resid(model_hba1c$modele)) # Normalité des résidus (Shapiro test)


#### Systolic blood pressure ####

# Modèle + résultats du modèle
model_sbp <- lmer_reg(data_post_itt, "sbp", digits_results = 1)

# Validation des hypothèses du modèle
plot(influence(lmer(as.formula(paste("sbp", "~", formule_default)), data = data_post_itt, REML = TRUE), obs = TRUE), which = "cook") # Distance de Cook
plot(model_sbp$modele) # Homogénéité de la variance
qqnorm(resid(model_sbp$modele)) # Normalité des résidus Q-Q Plot (point)
qqline(resid(model_sbp$modele)) # Normalité des résidus Q-Q Plot (ligne)
shapiro_test(resid(model_sbp$modele)) # Normalité des résidus (Shapiro test)


#### Diastolic blood pressure ####

# Modèle + résultats du modèle
model_dbp <- lmer_reg(data_post_itt, "dbp", digits_results = 1)

# Validation des hypothèses du modèle
plot(influence(lmer(as.formula(paste("dbp", "~", formule_default)), data = data_post_itt, REML = TRUE), obs = TRUE), which = "cook") # Distance de Cook
plot(model_dbp$modele) # Homogénéité de la variance
qqnorm(resid(model_dbp$modele)) # Normalité des résidus Q-Q Plot (point)
qqline(resid(model_dbp$modele)) # Normalité des résidus Q-Q Plot (ligne)
shapiro_test(resid(model_dbp$modele)) # Normalité des résidus (Shapiro test)

# Un sujet a une grande influence, son retrait est testé en matériel supplémentaire

#### Ten-Year Risk of ASCVD event ####

# Modèle + résultats du modèle
model_ten_year_risk_ascvd <- lmer_reg(data_post_itt, "ten_year_risk_ascvd", "tx + seq + bmi + weight_delta + (1|nomat)", digits_results = 1)

# Validation des hypothèses du modèle
plot(influence(lmer(as.formula(paste("ten_year_risk_ascvd", "~", "tx + seq + bmi + weight_delta + (1|nomat)")), data = data_post_itt, REML = TRUE), obs = TRUE),
     which = "cook") # Distance de Cook
plot(model_ten_year_risk_ascvd$modele) # Homogénéité de la variance
qqnorm(resid(model_ten_year_risk_ascvd$modele)) # Normalité des résidus Q-Q Plot (point)
qqline(resid(model_ten_year_risk_ascvd$modele)) # Normalité des résidus Q-Q Plot (ligne)
shapiro_test(resid(model_ten_year_risk_ascvd$modele)) # Normalité des résidus (Shapiro test)

# Problème de normalité des résidus !!! Transformation log
model_ten_year_risk_ascvd <- lmer_reg(data_post_itt, "ten_year_risk_ascvd", formule = "tx + seq + bmi + weight_delta + (1|nomat)",
                                      log_transform = TRUE) # Transformation log

# Validation des hypothèses du modèle
plot(influence(lmer(as.formula(paste("log(ten_year_risk_ascvd)", "~", "tx + seq + bmi + weight_delta + (1|nomat)")), data = data_post_itt, REML = TRUE),
               obs = TRUE), which = "cook") # Distance de Cook
plot(model_ten_year_risk_ascvd$modele) # Homogénéité de la variance
qqnorm(resid(model_ten_year_risk_ascvd$modele)) # Normalité des résidus Q-Q Plot (point)
qqline(resid(model_ten_year_risk_ascvd$modele)) # Normalité des résidus Q-Q Plot (ligne)
shapiro_test(resid(model_ten_year_risk_ascvd$modele)) # Normalité des résidus (Shapiro test)

# Création du dataset des résultats de la régression
results_reg <- rbind(model_ldlc$results_reg,
                     model_chol$results_reg,
                     model_ldlc_lpa_corr$results_reg,
                     model_hdlc$results_reg,
                     model_tg$results_reg,
                     model_nhdlc$results_reg,
                     model_apoa1_neph$results_reg,
                     model_apob_neph$results_reg,
                     model_lpa_neph$results_reg,
                     model_crp_neph$results_reg,
                     model_glufast$results_reg,
                     model_ins2018$results_reg,
                     model_hba1c$results_reg,
                     model_sbp$results_reg,
                     model_dbp$results_reg,
                     model_ten_year_risk_ascvd$results_reg)

# Exportation des résultats
write.xlsx(results_reg, file = "Table 3.xlsx")
