# Date : 14 juin 2024
# Auteur : Jacob Lessard-Lord

# Projet FH-Diet

# Modèles de régression mixte pour évaluer l'effet de la diète sur les échelles visuelles analogues

# Effet du temps entre première semaine et fin de l'intervention en faisant l'analyse sur toutes les données en même temps (plutôt que femme et homme séparé)

# Chargement des packages requis ----
library(tidyverse)
library(lme4)
library(lmerTest)
library(influence.ME)
library(rstatix)
library(ggpubr)

# Set working directory ----
setwd("C:/Users/jackl/OneDrive/Bureau/École/Postdoctorat/Chercheur/Jean-Philippe/FH-Diet/Processing_FH-Diet/Script R")

# Importation de la fonction ----
source("0C- Mixed linear regression model function stratified.R")
source("0D- Mixed linear regression model function stratified log.R")

# Set working directory ----
setwd("C:/Users/jackl/OneDrive/Bureau/École/Postdoctorat/Chercheur/Jean-Philippe/FH-Diet/Processing_FH-Diet/Data")

# Importation des données ----
load("Data VAS diff.rda")

# Variables explicatives par défaut ----
formule_default <- "tx*sex + tx + meal + kcal_diff + sex + seq + bmi + (1|nomat)"

# Modèles de régression ----

#### Desire to eat ####

# Before # 
model_desire_to_eat_before <- lmer_reg_strat(data_vas_diff, "desire_to_eat_before", "sex",
                                             "desire_to_eat_before ~ tx*sex + tx + meal + kcal_diff + sex + seq + bmi + (1|nomat)")

# Validation des hypothèses du modèle
plot(influence(lmer(as.formula(paste("desire_to_eat_before", "~", formule_default)),
                    data = data_vas_diff, REML = TRUE), obs = TRUE), which = "cook") # Distance de Cook
plot(model_desire_to_eat_before$modele_1) # Homogénéité de la variance
qqnorm(resid(model_desire_to_eat_before$modele_1)) # Normalité des résidus Q-Q Plot (point)
qqline(resid(model_desire_to_eat_before$modele_1)) # Normalité des résidus Q-Q Plot (ligne)
shapiro_test(resid(model_desire_to_eat_before$modele_1)) # Normalité des résidus (Shapiro test)

# After # 
model_desire_to_eat_after <- lmer_reg_strat(data_vas_diff, "desire_to_eat_after", "sex",
                                            "desire_to_eat_after ~ tx*sex + tx + meal + kcal_diff + sex + seq + bmi + (1|nomat)")

# Validation des hypothèses du modèle
plot(influence(lmer(as.formula(paste("desire_to_eat_after", "~", formule_default)),
                    data = data_vas_diff, REML = TRUE), obs = TRUE), which = "cook") # Distance de Cook
plot(model_desire_to_eat_after$modele_1) # Homogénéité de la variance
qqnorm(resid(model_desire_to_eat_after$modele_1)) # Normalité des résidus Q-Q Plot (point)
qqline(resid(model_desire_to_eat_after$modele_1)) # Normalité des résidus Q-Q Plot (ligne)
shapiro_test(resid(model_desire_to_eat_after$modele_1)) # Normalité des résidus (Shapiro test)

#### Hunger ####

# Before # 
model_hunger_before <- lmer_reg_strat(data_vas_diff, "hunger_before", "sex",
                                      "hunger_before ~ tx*sex + tx + meal + kcal_diff + sex + seq + bmi + (1|nomat)")

# Validation des hypothèses du modèle
plot(influence(lmer(as.formula(paste("hunger_before", "~", formule_default)),
                    data = data_vas_diff, REML = TRUE), obs = TRUE), which = "cook") # Distance de Cook
plot(model_hunger_before$modele_1) # Homogénéité de la variance
qqnorm(resid(model_hunger_before$modele_1)) # Normalité des résidus Q-Q Plot (point)
qqline(resid(model_hunger_before$modele_1)) # Normalité des résidus Q-Q Plot (ligne)
shapiro_test(resid(model_hunger_before$modele_1)) # Normalité des résidus (Shapiro test)

# L'homogénéité de la variance n'est pas parfaite, mais il n'y a rien a faire ...

# After # 
model_hunger_after <- lmer_reg_strat(data_vas_diff, "hunger_after", "sex",
                                     "hunger_after ~ tx*sex + tx + meal + kcal_diff + sex + seq + bmi + (1|nomat)")

# Validation des hypothèses du modèle
plot(influence(lmer(as.formula(paste("hunger_after", "~", formule_default)),
                    data = data_vas_diff, REML = TRUE), obs = TRUE), which = "cook") # Distance de Cook
plot(model_hunger_after$modele_1) # Homogénéité de la variance
qqnorm(resid(model_hunger_after$modele_1)) # Normalité des résidus Q-Q Plot (point)
qqline(resid(model_hunger_after$modele_1)) # Normalité des résidus Q-Q Plot (ligne)
shapiro_test(resid(model_hunger_after$modele_1)) # Normalité des résidus (Shapiro test)

#### Fullness ####

# Before # 
model_fullness_before <- lmer_reg_strat(data_vas_diff, "fullness_before", "sex",
                                        "fullness_before ~ tx*sex + tx + meal + kcal_diff + sex + seq + bmi + (1|nomat)")

# Validation des hypothèses du modèle
plot(influence(lmer(as.formula(paste("fullness_before", "~", formule_default)),
                    data = data_vas_diff, REML = TRUE), obs = TRUE), which = "cook") # Distance de Cook
plot(model_fullness_before$modele_1) # Homogénéité de la variance
qqnorm(resid(model_fullness_before$modele_1)) # Normalité des résidus Q-Q Plot (point)
qqline(resid(model_fullness_before$modele_1)) # Normalité des résidus Q-Q Plot (ligne)
shapiro_test(resid(model_fullness_before$modele_1)) # Normalité des résidus (Shapiro test)

# After # 
model_fullness_after <- lmer_reg_strat(data_vas_diff, "fullness_after", "sex",
                                       "fullness_after ~ tx*sex + tx + meal + kcal_diff + sex + seq + bmi + (1|nomat)")

# Validation des hypothèses du modèle
plot(influence(lmer(as.formula(paste("fullness_after", "~", formule_default)),
                    data = data_vas_diff, REML = TRUE), obs = TRUE), which = "cook") # Distance de Cook
plot(model_fullness_after$modele_1) # Homogénéité de la variance
qqnorm(resid(model_fullness_after$modele_1)) # Normalité des résidus Q-Q Plot (point)
qqline(resid(model_fullness_after$modele_1)) # Normalité des résidus Q-Q Plot (ligne)
shapiro_test(resid(model_fullness_after$modele_1)) # Normalité des résidus (Shapiro test)

#### Prospective food consumption ####

# Before # 
model_prospective_food_consumption_before <- lmer_reg_strat(data_vas_diff, "prospective_food_consumption_before", "sex",
                                                            "prospective_food_consumption_before ~ tx*sex + tx + meal + kcal_diff + sex + seq + bmi + (1|nomat)")

# Validation des hypothèses du modèle
plot(influence(lmer(as.formula(paste("prospective_food_consumption_before", "~", formule_default)),
                    data = data_vas_diff, REML = TRUE), obs = TRUE), which = "cook") # Distance de Cook
plot(model_prospective_food_consumption_before$modele_1) # Homogénéité de la variance
qqnorm(resid(model_prospective_food_consumption_before$modele_1)) # Normalité des résidus Q-Q Plot (point)
qqline(resid(model_prospective_food_consumption_before$modele_1)) # Normalité des résidus Q-Q Plot (ligne)
shapiro_test(resid(model_prospective_food_consumption_before$modele_1)) # Normalité des résidus (Shapiro test)

# After # 
model_prospective_food_consumption_after <- lmer_reg_strat(data_vas_diff, "prospective_food_consumption_after", "sex",
                                                           "prospective_food_consumption_after ~ tx*sex + tx + meal + kcal_diff + sex + seq + bmi + (1|nomat)")

# Validation des hypothèses du modèle
plot(influence(lmer(as.formula(paste("prospective_food_consumption_after", "~", formule_default)),
                    data = data_vas_diff, REML = TRUE), obs = TRUE), which = "cook") # Distance de Cook
plot(model_prospective_food_consumption_after$modele_1) # Homogénéité de la variance
qqnorm(resid(model_prospective_food_consumption_after$modele_1)) # Normalité des résidus Q-Q Plot (point)
qqline(resid(model_prospective_food_consumption_after$modele_1)) # Normalité des résidus Q-Q Plot (ligne)
shapiro_test(resid(model_prospective_food_consumption_after$modele_1)) # Normalité des résidus (Shapiro test)

#### Appetite score ####

# Before # 
model_appetite_score_before <- lmer_reg_strat(data_vas_diff, "appetite_score_before", "sex",
                                              "appetite_score_before ~ tx*sex + tx + meal + kcal_diff + sex + seq + bmi + (1|nomat)")

# Validation des hypothèses du modèle
plot(influence(lmer(as.formula(paste("appetite_score_before", "~", formule_default)),
                    data = data_vas_diff, REML = TRUE), obs = TRUE), which = "cook") # Distance de Cook
plot(model_appetite_score_before$modele_1) # Homogénéité de la variance
qqnorm(resid(model_appetite_score_before$modele_1)) # Normalité des résidus Q-Q Plot (point)
qqline(resid(model_appetite_score_before$modele_1)) # Normalité des résidus Q-Q Plot (ligne)
shapiro_test(resid(model_appetite_score_before$modele_1)) # Normalité des résidus (Shapiro test)

# After # 
model_appetite_score_after <- lmer_reg_strat(data_vas_diff, "appetite_score_after", "sex",
                                             "appetite_score_after ~ tx*sex + tx + meal + kcal_diff + sex + seq + bmi + (1|nomat)")

# Validation des hypothèses du modèle
plot(influence(lmer(as.formula(paste("appetite_score_after", "~", formule_default)),
                    data = data_vas_diff, REML = TRUE), obs = TRUE), which = "cook") # Distance de Cook
plot(model_appetite_score_after$modele_1) # Homogénéité de la variance
qqnorm(resid(model_appetite_score_after$modele_1)) # Normalité des résidus Q-Q Plot (point)
qqline(resid(model_appetite_score_after$modele_1)) # Normalité des résidus Q-Q Plot (ligne)
shapiro_test(resid(model_appetite_score_after$modele_1)) # Normalité des résidus (Shapiro test)

#### Satiety quotient ####
model_satiety_quotient <- lmer_reg_strat(data_vas_diff, "satiety_quotient_after", "sex",
                                         "satiety_quotient_after ~ tx*sex + tx + meal + kcal_diff + sex + seq + bmi + (1|nomat)")

# Validation des hypothèses du modèle
plot(influence(lmer(as.formula(paste("satiety_quotient_after", "~", formule_default)),
                    data = data_vas_diff, REML = TRUE), obs = TRUE), which = "cook") # Distance de Cook
plot(model_satiety_quotient$modele_1) # Homogénéité de la variance
qqnorm(resid(model_satiety_quotient$modele_1)) # Normalité des résidus Q-Q Plot (point)
qqline(resid(model_satiety_quotient$modele_1)) # Normalité des résidus Q-Q Plot (ligne)
shapiro_test(resid(model_satiety_quotient$modele_1)) # Normalité des résidus (Shapiro test)

#### Appreciation ####
model_appreciation <- lmer_reg_strat(data_vas_diff, "appreciation_after", "sex",
                                     "appreciation_after ~ tx*sex + tx + meal + kcal_diff + sex + seq + bmi + (1|nomat)")

# Validation des hypothèses du modèle
plot(influence(lmer(as.formula(paste("appreciation_after", "~", formule_default)),
                    data = data_vas_diff, REML = TRUE), obs = TRUE), which = "cook") # Distance de Cook
plot(model_appreciation$modele_1) # Homogénéité de la variance
qqnorm(resid(model_appreciation$modele_1)) # Normalité des résidus Q-Q Plot (point)
qqline(resid(model_appreciation$modele_1)) # Normalité des résidus Q-Q Plot (ligne)
shapiro_test(resid(model_appreciation$modele_1)) # Normalité des résidus (Shapiro test)




# Création du dataset des résultats de la régression----
results_reg <- rbind(model_desire_to_eat_before$results_reg,
                     model_hunger_before$results_reg,
                     model_fullness_before$results_reg,
                     model_prospective_food_consumption_before$results_reg,
                     model_appetite_score_before$results_reg,
                     model_desire_to_eat_after$results_reg,
                     model_hunger_after$results_reg,
                     model_fullness_after$results_reg,
                     model_prospective_food_consumption_after$results_reg,
                     model_appetite_score_after$results_reg,
                     model_satiety_quotient$results_reg,
                     model_appreciation$results_reg)

# Extraire seulement les résultats significatifs
subset(results_reg, subset = p_value_interaction <= 0.05)
# Desire to eat after
# Hunger after
# Appetite score after
# Appreciation after

# Fonction pour error plot
errorplot_diff <- function(var_y, titre, data_df = data_vas_diff) {
  
  ggerrorplot(data = data_df, x = "tx", y = var_y, color = "sex",
              xlab = "Diet", ylab = "Difference W4 - W1 (mm)", title = titre,
              desc_stat = "mean_se", legend = "bottom") +
    geom_hline(yintercept = 0, linetype = "dashed") +
    coord_flip() +
    scale_x_discrete(labels = c("WFPB", "SAD"), limits = rev) +
    scale_color_manual(values = c("#fa2a61", "#3e46af")) +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"),
          legend.title = element_blank(),
          legend.text = element_text(size = 12))
}

# Création d'un data.frame contenant les annotations pour chacun des graphiques
annotation_df_p <- data.frame(Outcome = c("desire_to_eat_after", "hunger_after", "appetite_score_after", "appreciation_after"),
                            Texte = c("P diet*sex = 0.001", "P diet*sex = 0.04", "P diet*sex = 0.01", "P diet*sex = 0.05"),
                            y_pos = c(-3, -0, -0, -1.5),
                            x_pos = c(2.4, 2.4, 2.4, 2.4))

# Création d'un data.frame pour les étiquettes avec les flèches
annotation_df_fleche <- data.frame(Outcome = c("desire_to_eat_after", "desire_to_eat_after", "hunger_after", "hunger_after",
                                               "appetite_score_after", "appetite_score_after", "appreciation_after", "appreciation_after"),
                                   Texte = c("\u2193 desire", "\u2191 desire", "\u2193 hunger", "\u2191 hunger",
                                             "\u2193 appetite", "\u2191 appetite", "\u2193 appreciat.", "\u2191 appreciat."),
                                   y_pos = c(-12.5, 15, -5.5, 10, -5.5, 9, -4.5, 5),
                                   x_pos = c(0.6, 0.6, 0.6, 0.6, 0.6, 0.6, 0.6, 0.6))

# Concaténer les 2 data.frames
annotation_df <- rbind(annotation_df_p, annotation_df_fleche)

setwd("C:/Users/jackl/OneDrive/Bureau/École/Postdoctorat/Chercheur/Jean-Philippe/FH-Diet/Article/Figures")

tiff(filename = "Figure S4.tiff", width = 17.5, height = 17.5, units = "cm", res = 600, compression = "lzw")
ggarrange(
  errorplot_diff("desire_to_eat_after", "Desire to eat after meals") +
    geom_label(data = subset(annotation_df, subset = Outcome == "desire_to_eat_after"), aes(x = x_pos, y = y_pos, label = Texte), fontface = "bold"),
  errorplot_diff("hunger_after", "Hunger after meals") +
    geom_label(data = subset(annotation_df, subset = Outcome == "hunger_after"), aes(x = x_pos, y = y_pos, label = Texte), fontface = "bold"),
  errorplot_diff("appetite_score_after", "Appetite score after meals") +
    geom_label(data = subset(annotation_df, subset = Outcome == "appetite_score_after"), aes(x = x_pos, y = y_pos, label = Texte), fontface = "bold"),
  errorplot_diff("appreciation_after", "Appreciation after meals") +
    geom_label(data = subset(annotation_df, subset = Outcome == "appreciation_after"), aes(x = x_pos, y = y_pos, label = Texte), fontface = "bold"),
  nrow = 2, ncol = 2, common.legend = TRUE, legend = "bottom", labels = "AUTO"
)
dev.off()

