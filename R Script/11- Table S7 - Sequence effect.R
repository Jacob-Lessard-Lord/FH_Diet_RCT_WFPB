# Date : 12 juin 2025
# Auteur : Jacob Lessard-Lord

# Projet FH-Diet

# Modèles de régression mixte stratifié pour évaluer l'effet de la diète sur le LDL-C et autres paramètres biochimiques par séquence

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
source("0C- Mixed linear regression model function stratified.R")
source("0D- Mixed linear regression model function stratified log.R")

# Set working directory ----
setwd("C:/Users/jackl/OneDrive/Bureau/École/Postdoctorat/Chercheur/Jean-Philippe/FH-Diet/Processing_FH-Diet/Data")

# Importation des données ----
load("Data post-diet ITT.rda")

# Modèles de régression mixte ----

#### LDL-C ####

# Modèle + résultats du modèle
model_ldlc <- lmer_reg_strat(data_post_itt, "ldlc", "seq", "ldlc ~ tx*seq +  tx + seq + bmi + weight_delta + (1|nomat)")

# Validation des hypothèses du modèle
plot(influence(lmer(ldlc ~ tx*seq +  tx + seq + bmi + weight_delta + (1|nomat),
                    data = data_post_itt, REML = TRUE), obs = TRUE), which = "cook") # Distance de Cook
plot(model_ldlc$modele_1) # Homogénéité de la variance
qqnorm(resid(model_ldlc$modele_1)) # Normalité des résidus Q-Q Plot (point)
qqline(resid(model_ldlc$modele_1)) # Normalité des résidus Q-Q Plot (ligne)
shapiro_test(resid(model_ldlc$modele_1)) # Normalité des résidus (Shapiro test)

#### LDL-C corrigé pour Lp(a)####

# Modèle + résultats du modèle
model_ldlc_lpa_corr <- lmer_reg_strat(data_post_itt, "ldlc_lpa_corr", "seq", "ldlc_lpa_corr ~ tx*seq +  tx + seq + bmi + weight_delta + (1|nomat)")

# Validation des hypothèses du modèle
plot(influence(lmer(ldlc_lpa_corr ~ tx*seq +  tx + seq + bmi + weight_delta + (1|nomat),
                    data = data_post_itt, REML = TRUE), obs = TRUE), which = "cook") # Distance de Cook
plot(model_ldlc_lpa_corr$modele_1) # Homogénéité de la variance
qqnorm(resid(model_ldlc_lpa_corr$modele_1)) # Normalité des résidus Q-Q Plot (point)
qqline(resid(model_ldlc_lpa_corr$modele_1)) # Normalité des résidus Q-Q Plot (ligne)
shapiro_test(resid(model_ldlc_lpa_corr$modele_1)) # Normalité des résidus (Shapiro test)


#### Cholesterol ####

# Modèle + résultats du modèle
model_chol <- lmer_reg_strat(data_post_itt, "chol", "seq", "chol ~ tx*seq +  tx + seq + bmi + weight_delta + (1|nomat)")

# Validation des hypothèses du modèle
plot(influence(lmer(chol ~ tx*seq +  tx + seq + bmi + weight_delta + (1|nomat),
                    data = data_post_itt, REML = TRUE), obs = TRUE), which = "cook") # Distance de Cook
plot(model_chol$modele_1) # Homogénéité de la variance
qqnorm(resid(model_chol$modele_1)) # Normalité des résidus Q-Q Plot (point)
qqline(resid(model_chol$modele_1)) # Normalité des résidus Q-Q Plot (ligne)
shapiro_test(resid(model_chol$modele_1)) # Normalité des résidus (Shapiro test)

#### HDL-C ####

# Modèle + résultats du modèle
model_hdlc <- lmer_reg_strat(data_post_itt, "hdlc", "seq", "hdlc ~ tx*seq +  tx + seq + bmi + weight_delta + (1|nomat)")

# Validation des hypothèses du modèle
plot(influence(lmer(hdlc ~ tx*seq +  tx + seq + bmi + weight_delta + (1|nomat),
                    data = data_post_itt, REML = TRUE), obs = TRUE), which = "cook") # Distance de Cook
plot(model_hdlc$modele_1) # Homogénéité de la variance
qqnorm(resid(model_hdlc$modele_1)) # Normalité des résidus Q-Q Plot (point)
qqline(resid(model_hdlc$modele_1)) # Normalité des résidus Q-Q Plot (ligne)
shapiro_test(resid(model_hdlc$modele_1)) # Normalité des résidus (Shapiro test)

#### Triglycerides ####

# Modèle + résultats du modèle
model_tg <- lmer_reg_strat_log(data_post_itt, "tg", "seq", "log(tg) ~ tx*seq +  tx + seq + bmi + weight_delta + (1|nomat)")

# Validation des hypothèses du modèle
plot(influence(lmer(tg ~ tx*seq +  tx + seq + bmi + weight_delta + (1|nomat),
                    data = data_post_itt, REML = TRUE), obs = TRUE), which = "cook") # Distance de Cook
plot(model_tg$modele_1) # Homogénéité de la variance
qqnorm(resid(model_tg$modele_1)) # Normalité des résidus Q-Q Plot (point)
qqline(resid(model_tg$modele_1)) # Normalité des résidus Q-Q Plot (ligne)
shapiro_test(resid(model_tg$modele_1)) # Normalité des résidus (Shapiro test)

#### Non HDL-C ####

# Modèle + résultats du modèle
model_nhdlc <- lmer_reg_strat(data_post_itt, "nhdlc", "seq", "nhdlc ~ tx*seq +  tx + seq + bmi + weight_delta + (1|nomat)")

# Validation des hypothèses du modèle
plot(influence(lmer(nhdlc ~ tx*seq +  tx + seq + bmi + weight_delta + (1|nomat),
                    data = data_post_itt, REML = TRUE), obs = TRUE), which = "cook") # Distance de Cook
plot(model_nhdlc$modele_1) # Homogénéité de la variance
qqnorm(resid(model_nhdlc$modele_1)) # Normalité des résidus Q-Q Plot (point)
qqline(resid(model_nhdlc$modele_1)) # Normalité des résidus Q-Q Plot (ligne)
shapiro_test(resid(model_nhdlc$modele_1)) # Normalité des résidus (Shapiro test)

#### ApoA1 ####

# Modèle + résultats du modèle
model_apoa1_neph <- lmer_reg_strat(data_post_itt, "apoa1_neph", "seq", "apoa1_neph ~ tx*seq +  tx + seq + bmi + weight_delta + (1|nomat)")

# Validation des hypothèses du modèle
plot(influence(lmer(apoa1_neph ~ tx*seq +  tx + seq + bmi + weight_delta + (1|nomat),
                    data = data_post_itt, REML = TRUE), obs = TRUE), which = "cook") # Distance de Cook
plot(model_apoa1_neph$modele_1) # Homogénéité de la variance
qqnorm(resid(model_apoa1_neph$modele_1)) # Normalité des résidus Q-Q Plot (point)
qqline(resid(model_apoa1_neph$modele_1)) # Normalité des résidus Q-Q Plot (ligne)
shapiro_test(resid(model_apoa1_neph$modele_1)) # Normalité des résidus (Shapiro test)

#### ApoB ####

# Modèle + résultats du modèle
model_apob_neph <- lmer_reg_strat(data_post_itt, "apob_neph", "seq", "apob_neph ~ tx*seq +  tx + seq + bmi + weight_delta + (1|nomat)")

# Validation des hypothèses du modèle
plot(influence(lmer(apob_neph ~ tx*seq +  tx + seq + bmi + weight_delta + (1|nomat),
                    data = data_post_itt, REML = TRUE), obs = TRUE), which = "cook") # Distance de Cook
plot(model_apob_neph$modele_1) # Homogénéité de la variance
qqnorm(resid(model_apob_neph$modele_1)) # Normalité des résidus Q-Q Plot (point)
qqline(resid(model_apob_neph$modele_1)) # Normalité des résidus Q-Q Plot (ligne)
shapiro_test(resid(model_apob_neph$modele_1)) # Normalité des résidus (Shapiro test)

#### Lp(a) ####

# Modèle + résultats du modèle
model_lpa_neph <- lmer_reg_strat(data_post_itt, "lpa_neph", "seq", "lpa_neph ~ tx*seq +  tx + seq + bmi + weight_delta + (1|nomat)")

# Validation des hypothèses du modèle
plot(influence(lmer(lpa_neph ~ tx*seq +  tx + seq + bmi + weight_delta + (1|nomat),
                    data = data_post_itt, REML = TRUE), obs = TRUE), which = "cook") # Distance de Cook
plot(model_lpa_neph$modele_1) # Homogénéité de la variance
qqnorm(resid(model_lpa_neph$modele_1)) # Normalité des résidus Q-Q Plot (point)
qqline(resid(model_lpa_neph$modele_1)) # Normalité des résidus Q-Q Plot (ligne)
shapiro_test(resid(model_lpa_neph$modele_1)) # Normalité des résidus (Shapiro test)

#### C-Reactive protein ####

# Modèle + résultats du modèle
model_crp_neph <- lmer_reg_strat_log(data_post_itt, "crp_neph", "seq", "log(crp_neph) ~ tx*seq +  tx + seq + bmi + weight_delta + (1|nomat)")

# Validation des hypothèses du modèle
plot(influence(lmer(log(crp_neph) ~ tx*seq +  tx + seq + bmi + weight_delta + (1|nomat),
                    data = data_post_itt, REML = TRUE), obs = TRUE), which = "cook") # Distance de Cook
plot(model_crp_neph$modele_1) # Homogénéité de la variance
qqnorm(resid(model_crp_neph$modele_1)) # Normalité des résidus Q-Q Plot (point)
qqline(resid(model_crp_neph$modele_1)) # Normalité des résidus Q-Q Plot (ligne)
shapiro_test(resid(model_crp_neph$modele_1)) # Normalité des résidus (Shapiro test)

#### Fasting glucose ####

# Modèle + résultats du modèle
model_glufast <- lmer_reg_strat(data_post_itt, "glufast", "seq", "glufast ~ tx*seq +  tx + seq + bmi + weight_delta + (1|nomat)")

# Validation des hypothèses du modèle
plot(influence(lmer(glufast ~ tx*seq +  tx + seq + bmi + weight_delta + (1|nomat),
                    data = data_post_itt, REML = TRUE), obs = TRUE), which = "cook") # Distance de Cook
plot(model_glufast$modele_1) # Homogénéité de la variance
qqnorm(resid(model_glufast$modele_1)) # Normalité des résidus Q-Q Plot (point)
qqline(resid(model_glufast$modele_1)) # Normalité des résidus Q-Q Plot (ligne)
shapiro_test(resid(model_glufast$modele_1)) # Normalité des résidus (Shapiro test)

#### Fasting insulin ####

# Modèle + résultats du modèle
model_ins2018 <- lmer_reg_strat(data_post_itt, "ins2018", "seq", "ins2018 ~ tx*seq +  tx + seq + bmi + weight_delta + (1|nomat)")

# Validation des hypothèses du modèle
plot(influence(lmer(ins2018 ~ tx*seq +  tx + seq + bmi + weight_delta + (1|nomat),
                    data = data_post_itt, REML = TRUE), obs = TRUE), which = "cook") # Distance de Cook
plot(model_ins2018$modele_1) # Homogénéité de la variance
qqnorm(resid(model_ins2018$modele_1)) # Normalité des résidus Q-Q Plot (point)
qqline(resid(model_ins2018$modele_1)) # Normalité des résidus Q-Q Plot (ligne)
shapiro_test(resid(model_ins2018$modele_1)) # Normalité des résidus (Shapiro test)

#### HbA1c ####

# Modèle + résultats du modèle
model_hba1c <- lmer_reg_strat(data_post_itt, "hba1c", "seq", "hba1c ~ tx*seq +  tx + seq + bmi + weight_delta + (1|nomat)")

# Validation des hypothèses du modèle
plot(influence(lmer(hba1c ~ tx*seq +  tx + seq + bmi + weight_delta + (1|nomat),
                    data = data_post_itt, REML = TRUE), obs = TRUE), which = "cook") # Distance de Cook
plot(model_hba1c$modele_1) # Homogénéité de la variance
qqnorm(resid(model_hba1c$modele_1)) # Normalité des résidus Q-Q Plot (point)
qqline(resid(model_hba1c$modele_1)) # Normalité des résidus Q-Q Plot (ligne)
shapiro_test(resid(model_hba1c$modele_1)) # Normalité des résidus (Shapiro test)

#### Systolic blood pressure ####

# Modèle + résultats du modèle
model_sbp <- lmer_reg_strat(data_post_itt, "sbp", "seq", "sbp ~ tx*seq +  tx + seq + bmi + weight_delta + (1|nomat)")

# Validation des hypothèses du modèle
plot(influence(lmer(sbp ~ tx*seq +  tx + seq + bmi + weight_delta + (1|nomat),
                    data = data_post_itt, REML = TRUE), obs = TRUE), which = "cook") # Distance de Cook
plot(model_sbp$modele_1) # Homogénéité de la variance
qqnorm(resid(model_sbp$modele_1)) # Normalité des résidus Q-Q Plot (point)
qqline(resid(model_sbp$modele_1)) # Normalité des résidus Q-Q Plot (ligne)
shapiro_test(resid(model_sbp$modele_1)) # Normalité des résidus (Shapiro test)


#### Diastolic blood pressure ####

# Modèle + résultats du modèle
model_dbp <- lmer_reg_strat(data_post_itt, "dbp", "seq", "dbp ~ tx*seq +  tx + seq + bmi + weight_delta + (1|nomat)")

# Validation des hypothèses du modèle
plot(influence(lmer(dbp ~ tx*seq +  tx + seq + bmi + weight_delta + (1|nomat),
                    data = data_post_itt, REML = TRUE), obs = TRUE), which = "cook") # Distance de Cook
plot(model_dbp$modele_1) # Homogénéité de la variance
qqnorm(resid(model_dbp$modele_1)) # Normalité des résidus Q-Q Plot (point)
qqline(resid(model_dbp$modele_1)) # Normalité des résidus Q-Q Plot (ligne)
shapiro_test(resid(model_dbp$modele_1)) # Normalité des résidus (Shapiro test)

#### Ten-Year Risk of ASCVD event ####

# Modèle + résultats du modèle
model_ten_year_risk_ascvd <- lmer_reg_strat_log(data_post_itt, "ten_year_risk_ascvd", "seq",
                                                "log(ten_year_risk_ascvd) ~ tx*seq +  tx + seq + bmi + weight_delta + (1|nomat)")

# Validation des hypothèses du modèle
plot(influence(lmer(ten_year_risk_ascvd ~ tx*seq +  tx + seq + bmi + weight_delta + (1|nomat),
                    data = data_post_itt, REML = TRUE), obs = TRUE), which = "cook") # Distance de Cook
plot(model_ten_year_risk_ascvd$modele_1) # Homogénéité de la variance
qqnorm(resid(model_ten_year_risk_ascvd$modele_1)) # Normalité des résidus Q-Q Plot (point)
qqline(resid(model_ten_year_risk_ascvd$modele_1)) # Normalité des résidus Q-Q Plot (ligne)
shapiro_test(resid(model_ten_year_risk_ascvd$modele_1)) # Normalité des résidus (Shapiro test)

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
                     model_ten_year_risk_ascvd$results_reg) %>%
  select(Outcome, Strat_level, n, WFPB_SAD_mean_ci_rel, p_value, p_value_interaction)

# Mise en forme des résultats
results_reg <- merge(x = subset(results_reg, subset = Strat_level == "SAD-WFPB", select = -p_value_interaction),
                     y = subset(results_reg, subset = Strat_level == "WFPB-SAD"),
                     by = "Outcome")

# Exportation des résultats
write.xlsx(results_reg, file = "Table S7.xlsx")
