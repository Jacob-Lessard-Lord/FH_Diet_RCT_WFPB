# Date : 11 juin 2024
# Auteur : Jacob Lessard-Lord

# Projet FH-Diet

# Modèles de régression mixte pour évaluer l'effet de la diète sur le LDL-C stratifié selon différents facteurs non-modifiables

# Per protocol analysis #

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

# Set working directory ----
setwd("C:/Users/jackl/OneDrive/Bureau/École/Postdoctorat/Chercheur/Jean-Philippe/FH-Diet/Processing_FH-Diet/Data")

# Importation des données ----
load("Data post-diet PP.rda")


# Modèles de régression mixte stratifiés ----


############# LDL-C #############


#### Sex ####
model_ldlc_sex <- lmer_reg_strat(data_post_pp, "ldlc", "sex", 
                                 "ldlc ~ tx*sex +  tx + age + sex + hfgenotype + seq + bmi + weight_delta + (1|nomat)")

# Validation des hypothèses du modèle
plot(influence(lmer(ldlc ~ tx*sex +  tx + age + sex + hfgenotype + seq + bmi + weight_delta + (1|nomat),
                    data = data_post_pp, REML = TRUE), obs = TRUE), which = "cook") # Distance de Cook
plot(model_ldlc_sex$modele_1) # Homogénéité de la variance
qqnorm(resid(model_ldlc_sex$modele_1)) # Normalité des résidus Q-Q Plot (point)
qqline(resid(model_ldlc_sex$modele_1)) # Normalité des résidus Q-Q Plot (ligne)
shapiro_test(resid(model_ldlc_sex$modele_1)) # Normalité des résidus (Shapiro test)

#### LDLR ####
model_ldlc_hfgenotype <- lmer_reg_strat(subset(data_post_pp, subset = hfgenotype != "Other") %>% mutate(hfgenotype = droplevels(hfgenotype)),
                                        "ldlc", "hfgenotype",
                                        "ldlc ~ tx*hfgenotype +  tx + age + sex + hfgenotype + seq + bmi + weight_delta + (1|nomat)")

# Validation des hypothèses du modèle
plot(influence(lmer(ldlc ~ tx*hfgenotype +  tx + age + sex + hfgenotype + seq + bmi + weight_delta + (1|nomat),
                    data = data_post_pp, REML = TRUE), obs = TRUE), which = "cook") # Distance de Cook
plot(model_ldlc_hfgenotype$modele_1) # Homogénéité de la variance
qqnorm(resid(model_ldlc_hfgenotype$modele_1)) # Normalité des résidus Q-Q Plot (point)
qqline(resid(model_ldlc_hfgenotype$modele_1)) # Normalité des résidus Q-Q Plot (ligne)
shapiro_test(resid(model_ldlc_hfgenotype$modele_1)) # Normalité des résidus (Shapiro test)

#### Age stratifié à 40 ans ####
model_ldlc_age_cat_40 <- lmer_reg_strat(data_post_pp, "ldlc", "age_cat_40",
                                        "ldlc ~ tx*age_cat_40 +  tx + age_cat_40 + sex + hfgenotype + seq + bmi + weight_delta + (1|nomat)")

# Validation des hypothèses du modèle
plot(influence(lmer(ldlc ~ tx*age_cat_40 +  tx + age_cat_40 + sex + hfgenotype + seq + bmi + weight_delta + (1|nomat),
                    data = data_post_pp, REML = TRUE), obs = TRUE), which = "cook") # Distance de Cook
plot(model_ldlc_age_cat_40$modele_1) # Homogénéité de la variance
qqnorm(resid(model_ldlc_age_cat_40$modele_1)) # Normalité des résidus Q-Q Plot (point)
qqline(resid(model_ldlc_age_cat_40$modele_1)) # Normalité des résidus Q-Q Plot (ligne)
shapiro_test(resid(model_ldlc_age_cat_40$modele_1)) # Normalité des résidus (Shapiro test)

# Test de l'interaction en continu # 
model_ldlc_age <- lmer(ldlc ~ tx*age + tx + age + sex + hfgenotype + seq + bmi + weight_delta + (1|nomat), data = data_post_pp, REML = TRUE)
summary(model_ldlc_age)

# P-value continue 
summary(model_ldlc_age)$coefficients[nrow(summary(model_ldlc_age)$coefficients), "Pr(>|t|)"]

############# Apo B #############

#### Sex ####
model_apob_neph_sex <- lmer_reg_strat(data_post_pp, "apob_neph", "sex", 
                                      "apob_neph ~ tx*sex +  tx + age + sex + hfgenotype + seq + bmi + weight_delta + (1|nomat)")

# Validation des hypothèses du modèle
plot(influence(lmer(apob_neph ~ tx*sex +  tx + age + sex + hfgenotype + seq + bmi + weight_delta + (1|nomat),
                    data = data_post_pp, REML = TRUE), obs = TRUE), which = "cook") # Distance de Cook
plot(model_apob_neph_sex$modele_1) # Homogénéité de la variance
qqnorm(resid(model_apob_neph_sex$modele_1)) # Normalité des résidus Q-Q Plot (point)
qqline(resid(model_apob_neph_sex$modele_1)) # Normalité des résidus Q-Q Plot (ligne)
shapiro_test(resid(model_apob_neph_sex$modele_1)) # Normalité des résidus (Shapiro test)

#### LDLR ####
model_apob_neph_hfgenotype <- lmer_reg_strat(subset(data_post_pp, subset = hfgenotype != "Other") %>% mutate(hfgenotype = droplevels(hfgenotype)),
                                        "apob_neph", "hfgenotype",
                                        "apob_neph ~ tx*hfgenotype +  tx + age + sex + hfgenotype + seq + bmi + weight_delta + (1|nomat)")

# Validation des hypothèses du modèle
plot(influence(lmer(apob_neph ~ tx*hfgenotype + tx + age + sex + hfgenotype + seq + bmi + weight_delta + (1|nomat),
                    data = data_post_pp, REML = TRUE), obs = TRUE), which = "cook") # Distance de Cook
plot(model_apob_neph_hfgenotype$modele_1) # Homogénéité de la variance
qqnorm(resid(model_apob_neph_hfgenotype$modele_1)) # Normalité des résidus Q-Q Plot (point)
qqline(resid(model_apob_neph_hfgenotype$modele_1)) # Normalité des résidus Q-Q Plot (ligne)
shapiro_test(resid(model_apob_neph_hfgenotype$modele_1)) # Normalité des résidus (Shapiro test)

#### Age stratifié à 40 ans ####
model_apob_neph_age_cat_40 <- lmer_reg_strat(data_post_pp, "apob_neph", "age_cat_40",
                                             "apob_neph ~ tx*age_cat_40 +  tx + age_cat_40 + sex + hfgenotype + seq + bmi + weight_delta + (1|nomat)")

# Validation des hypothèses du modèle
plot(influence(lmer(apob_neph ~ tx*age_cat_40 +  tx + age_cat_40 + sex + hfgenotype + seq + bmi + weight_delta + (1|nomat),
                    data = data_post_pp, REML = TRUE), obs = TRUE), which = "cook") # Distance de Cook
plot(model_apob_neph_age_cat_40$modele_1) # Homogénéité de la variance
qqnorm(resid(model_apob_neph_age_cat_40$modele_1)) # Normalité des résidus Q-Q Plot (point)
qqline(resid(model_apob_neph_age_cat_40$modele_1)) # Normalité des résidus Q-Q Plot (ligne)
shapiro_test(resid(model_apob_neph_age_cat_40$modele_1)) # Normalité des résidus (Shapiro test)

# Test de l'interaction en continu # 
model_apob_neph_age <- lmer(apob_neph ~ tx*age + tx + age + sex + hfgenotype + seq + bmi + weight_delta + (1|nomat), data = data_post_pp, REML = TRUE)
summary(model_apob_neph_age)

# P-value continue 
summary(model_apob_neph_age)$coefficients[nrow(summary(model_apob_neph_age)$coefficients), "Pr(>|t|)"]


# Création du dataset des résultats de la régression ----

## LDL-C ##
results_reg_ldlc <- rbind(model_ldlc_sex$results_reg,
                          model_ldlc_hfgenotype$results_reg,
                          model_ldlc_age_cat_40$results_reg) %>%
  select(Outcome, Stratification, Strat_level, n, WFPB_SAD_mean_ci_rel, p_value, p_value_interaction)

## ApoB ##
results_reg_apob_neph <- rbind(model_apob_neph_sex$results_reg,
                               model_apob_neph_hfgenotype$results_reg,
                               model_apob_neph_age_cat_40$results_reg)%>%
  select(Outcome, Stratification, Strat_level, n, WFPB_SAD_mean_ci_rel, p_value, p_value_interaction)

# Exportation des résultats
write.xlsx(cbind(results_reg_ldlc, results_reg_apob_neph), file = "Table S7.xlsx")
