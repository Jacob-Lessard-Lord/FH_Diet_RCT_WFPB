# Date : 10 fevrier 2025
# Auteur : Jacob Lessard-Lord

# Projet FH-Diet

# Modèles de régression mixte pour évaluer l'effet de la diète sur PCSK9 et les phytosterols

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

#### GLP-1 ####

# Modèle + résultats du modèle
model_glp1 <- lmer_reg(data_post_pp, "glp1")

# Validation des hypothèses du modèle
plot(influence(lmer(as.formula(paste("glp1", "~", formule_default)), data = data_post_pp, REML = TRUE), obs = TRUE), which = "cook") # Distance de Cook
plot(model_glp1$modele) # Homogénéité de la variance
qqnorm(resid(model_glp1$modele)) # Normalité des résidus Q-Q Plot (point)
qqline(resid(model_glp1$modele)) # Normalité des résidus Q-Q Plot (ligne)
shapiro_test(resid(model_glp1$modele)) # Normalité des résidus (Shapiro test)


#### PYY ####

# Modèle + résultats du modèle
model_pyy <- lmer_reg(data_post_pp, "pyy")

# Validation des hypothèses du modèle
plot(influence(lmer(as.formula(paste("pyy", "~", formule_default)), data = data_post_pp, REML = TRUE), obs = TRUE), which = "cook") # Distance de Cook
plot(model_pyy$modele) # Homogénéité de la variance
qqnorm(resid(model_pyy$modele)) # Normalité des résidus Q-Q Plot (point)
qqline(resid(model_pyy$modele)) # Normalité des résidus Q-Q Plot (ligne)
shapiro_test(resid(model_pyy$modele)) # Normalité des résidus (Shapiro test)

#### PCSK9 ####

# Modèle + résultats du modèle
model_pcsk9 <- lmer_reg(data_post_pp, "pcsk9")

# Validation des hypothèses du modèle
plot(influence(lmer(as.formula(paste("pcsk9", "~", formule_default)), data = data_post_pp, REML = TRUE), obs = TRUE), which = "cook") # Distance de Cook
plot(model_pcsk9$modele) # Homogénéité de la variance
qqnorm(resid(model_pcsk9$modele)) # Normalité des résidus Q-Q Plot (point)
qqline(resid(model_pcsk9$modele)) # Normalité des résidus Q-Q Plot (ligne)
shapiro_test(resid(model_pcsk9$modele)) # Normalité des résidus (Shapiro test)

# Problème d'homogénéiété de la variance et distribution non-normal des résidus !!! Transformation log
model_pcsk9 <- lmer_reg(data_post_pp, "pcsk9", log_transform = TRUE) # Transformation log

# Validation des hypothèses du modèle
plot(influence(lmer(as.formula(paste("log(pcsk9)", "~", formule_default)), data = data_post_pp, REML = TRUE), obs = TRUE), which = "cook") # Distance de Cook
plot(model_pcsk9$modele) # Homogénéité de la variance
qqnorm(resid(model_pcsk9$modele)) # Normalité des résidus Q-Q Plot (point)
qqline(resid(model_pcsk9$modele)) # Normalité des résidus Q-Q Plot (ligne)
shapiro_test(resid(model_pcsk9$modele)) # Normalité des résidus (Shapiro test)

#### Cholestanol normalisée ####

# Modèle + résultats du modèle
model_Cholestanol_norm <- lmer_reg(data_post_pp, "Cholestanol_norm", digits_results = 1)

# Validation des hypothèses du modèle
plot(influence(lmer(as.formula(paste("Cholestanol_norm", "~", formule_default)), data = data_post_pp, REML = TRUE), obs = TRUE), which = "cook") # Distance de Cook
plot(model_Cholestanol_norm$modele) # Homogénéité de la variance
qqnorm(resid(model_Cholestanol_norm$modele)) # Normalité des résidus Q-Q Plot (point)
qqline(resid(model_Cholestanol_norm$modele)) # Normalité des résidus Q-Q Plot (ligne)
shapiro_test(resid(model_Cholestanol_norm$modele)) # Normalité des résidus (Shapiro test)

#### Desmosterol normalisée ####

# Modèle + résultats du modèle
model_Desmosterol_norm <- lmer_reg(data_post_pp, "Desmosterol_norm", digits_results = 1)

# Validation des hypothèses du modèle
plot(influence(lmer(as.formula(paste("Desmosterol_norm", "~", formule_default)), data = data_post_pp, REML = TRUE), obs = TRUE), which = "cook") # Distance de Cook
plot(model_Desmosterol_norm$modele) # Homogénéité de la variance
qqnorm(resid(model_Desmosterol_norm$modele)) # Normalité des résidus Q-Q Plot (point)
qqline(resid(model_Desmosterol_norm$modele)) # Normalité des résidus Q-Q Plot (ligne)
shapiro_test(resid(model_Desmosterol_norm$modele)) # Normalité des résidus (Shapiro test)

#### Lathosterol normalisée ####

# Modèle + résultats du modèle
model_Lathosterol_norm <- lmer_reg(data_post_pp, "Lathosterol_norm")

# Validation des hypothèses du modèle
plot(influence(lmer(as.formula(paste("Lathosterol_norm", "~", formule_default)), data = data_post_pp, REML = TRUE), obs = TRUE), which = "cook") # Distance de Cook
plot(model_Lathosterol_norm$modele) # Homogénéité de la variance
qqnorm(resid(model_Lathosterol_norm$modele)) # Normalité des résidus Q-Q Plot (point)
qqline(resid(model_Lathosterol_norm$modele)) # Normalité des résidus Q-Q Plot (ligne)
shapiro_test(resid(model_Lathosterol_norm$modele)) # Normalité des résidus (Shapiro test)

#### Campesterol normalisée ####

# Modèle + résultats du modèle
model_Campesterol_norm <- lmer_reg(data_post_pp, "Campesterol_norm")

# Validation des hypothèses du modèle
plot(influence(lmer(as.formula(paste("Campesterol_norm", "~", formule_default)), data = data_post_pp, REML = TRUE), obs = TRUE), which = "cook") # Distance de Cook
plot(model_Campesterol_norm$modele) # Homogénéité de la variance
qqnorm(resid(model_Campesterol_norm$modele)) # Normalité des résidus Q-Q Plot (point)
qqline(resid(model_Campesterol_norm$modele)) # Normalité des résidus Q-Q Plot (ligne)
shapiro_test(resid(model_Campesterol_norm$modele)) # Normalité des résidus (Shapiro test)

#### Lanosterol normalisée ####

# Modèle + résultats du modèle
model_Lanosterol_norm <- lmer_reg(data_post_pp, "Lanosterol_norm")

# Validation des hypothèses du modèle
plot(influence(lmer(as.formula(paste("Lanosterol_norm", "~", formule_default)), data = data_post_pp, REML = TRUE), obs = TRUE), which = "cook") # Distance de Cook
plot(model_Lanosterol_norm$modele) # Homogénéité de la variance
qqnorm(resid(model_Lanosterol_norm$modele)) # Normalité des résidus Q-Q Plot (point)
qqline(resid(model_Lanosterol_norm$modele)) # Normalité des résidus Q-Q Plot (ligne)
shapiro_test(resid(model_Lanosterol_norm$modele)) # Normalité des résidus (Shapiro test)

#### beta_Sitosterol normalisée ####

# Modèle + résultats du modèle
model_beta_Sitosterol_norm <- lmer_reg(data_post_pp, "beta_Sitosterol_norm")

# Validation des hypothèses du modèle
plot(influence(lmer(as.formula(paste("beta_Sitosterol_norm", "~", formule_default)), data = data_post_pp, REML = TRUE), obs = TRUE), which = "cook") # Distance de Cook
plot(model_beta_Sitosterol_norm$modele) # Homogénéité de la variance
qqnorm(resid(model_beta_Sitosterol_norm$modele)) # Normalité des résidus Q-Q Plot (point)
qqline(resid(model_beta_Sitosterol_norm$modele)) # Normalité des résidus Q-Q Plot (ligne)
shapiro_test(resid(model_beta_Sitosterol_norm$modele)) # Normalité des résidus (Shapiro test)

# Problème d'homogénéiété de la variance !!! Transformation log

# Modèle + résultats du modèle
model_beta_Sitosterol_norm <- lmer_reg(data_post_pp, "beta_Sitosterol_norm", log_transform = TRUE)

# Validation des hypothèses du modèle
plot(influence(lmer(as.formula(paste("log(beta_Sitosterol_norm)", "~", formule_default)), data = data_post_pp, REML = TRUE), obs = TRUE), which = "cook") # Distance de Cook
plot(model_beta_Sitosterol_norm$modele) # Homogénéité de la variance
qqnorm(resid(model_beta_Sitosterol_norm$modele)) # Normalité des résidus Q-Q Plot (point)
qqline(resid(model_beta_Sitosterol_norm$modele)) # Normalité des résidus Q-Q Plot (ligne)
shapiro_test(resid(model_beta_Sitosterol_norm$modele)) # Normalité des résidus (Shapiro test)

# C'est beaucoup mieux probleme regle !

#### Lathosterol / Cholestanol ####

# Modèle + résultats du modèle
model_Lathosterol_Cholestanol <- lmer_reg(data_post_pp, "Lathosterol_Cholestanol")

# Validation des hypothèses du modèle
plot(influence(lmer(as.formula(paste("Lathosterol_Cholestanol", "~", formule_default)), data = data_post_pp, REML = TRUE), obs = TRUE), which = "cook") # Distance de Cook
plot(model_Lathosterol_Cholestanol$modele) # Homogénéité de la variance
qqnorm(resid(model_Lathosterol_Cholestanol$modele)) # Normalité des résidus Q-Q Plot (point)
qqline(resid(model_Lathosterol_Cholestanol$modele)) # Normalité des résidus Q-Q Plot (ligne)
shapiro_test(resid(model_Lathosterol_Cholestanol$modele)) # Normalité des résidus (Shapiro test)

# Création du dataset des résultats de la régression ----
results_reg <- rbind(model_pcsk9$results_reg,
                     
                     model_Desmosterol_norm$results_reg,
                     model_Lathosterol_norm$results_reg,
                     model_Lanosterol_norm$results_reg,
                     model_Campesterol_norm$results_reg,
                     model_beta_Sitosterol_norm$results_reg,
                     model_Cholestanol_norm$results_reg,
                     
                     model_Lathosterol_Cholestanol$results_reg,
                     
                     model_glp1$results_reg,
                     model_pyy$results_reg)

# Exportation des résultats ----
write.xlsx(results_reg, file = "Table 5.xlsx")

