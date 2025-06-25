# Date : 5 juin 2024
# Auteur : Jacob Lessard-Lord

# Projet FH-Diet

# Caractéristiques au baseline des participants - Tableau 2

# Chargement des packages requis ----
library(tidyverse)
library(rstatix)
library(openxlsx)

# Set working directory ----
setwd("C:/Users/jackl/OneDrive/Bureau/École/Postdoctorat/Chercheur/Jean-Philippe/FH-Diet/Processing_FH-Diet/Data")

# Importation des données ----
load("Data baseline.rda")

# Mise en forme des résultats ----

# Valeur extrême de CRP - Recodage en NA

# Evaluation du probleme avec la CRP
data_baseline[which.max(data_baseline$crp_neph), c("nomat", "crp_neph")]
data_baseline[which.max(data_baseline$crp_neph), "crp_neph"] <- NA

# Statistiques sur variables numériques
data_var_quant <- data_baseline %>% get_summary_stats(type = "mean_sd")
data_var_quant_SAD_WFPB <- subset(data_baseline, subset = seq == "SAD-WFPB") %>% get_summary_stats(type = "mean_sd")
data_var_quant_WFPB_SAD <- subset(data_baseline, subset = seq == "WFPB-SAD") %>% get_summary_stats(type = "mean_sd")

# Mise en forme des données sur les variables numériques
data_var_quant <- data_var_quant %>%
  mutate(Results_all = paste(round(mean, 2), " ± ", round(sd, 2)))
data_var_quant_SAD_WFPB <- data_var_quant_SAD_WFPB %>%
  mutate(Results_SAD_WFPB = paste(round(mean, 2), " ± ", round(sd, 2)))
data_var_quant_WFPB_SAD <- data_var_quant_WFPB_SAD %>%
  mutate(Results_WFPB_SAD = paste(round(mean, 2), " ± ", round(sd, 2)))

# Merge des 3 datasets sur les variables numériques
data_var_quant <- merge(x = merge(x = data_var_quant[, c("variable", "Results_all")],
                                  y = data_var_quant_SAD_WFPB[, c("variable", "Results_SAD_WFPB")], by = "variable", sort = FALSE),
                        y = data_var_quant_WFPB_SAD[, c("variable", "Results_WFPB_SAD")], by = "variable", sort = FALSE)
write.xlsx(data_cat, file = "Table 2 - Quant.xlsx")

rm(data_var_quant_SAD_WFPB, data_var_quant_WFPB_SAD)

# Statistiques du BMI + poids stratifié sur le sexe
data_baseline %>% 
  group_by(sex) %>%
  get_summary_stats(c(bmi, waistc), type = "mean_sd")

data_baseline %>% 
  group_by(seq, sex) %>%
  get_summary_stats(c(bmi, waistc), type = "mean_sd")


# Statistiques sur variables catégorielles
data_cat <- rbind(
  data_baseline %>% freq_table(sex) %>% rename(variable = sex),
  data_baseline %>% freq_table(hfgenotype) %>% rename(variable = hfgenotype),
  
  data_baseline %>% freq_table(fh_detection) %>% rename(variable = fh_detection),
  
  data_baseline %>% freq_table(rx_case) %>% rename(variable = rx_case)
)



# Analyse par séquence de la diète
data_baseline %>% freq_table(seq)

data_cat_seq <- rbind(
  data_baseline %>% freq_table(seq, sex) %>% rename(variable = sex),
  data_baseline %>% freq_table(seq, hfgenotype) %>% rename(variable = hfgenotype),
  
  data_baseline %>% freq_table(seq, fh_detection) %>% rename(variable = fh_detection),
  data_baseline %>% freq_table(seq, rx_case) %>% rename(variable = rx_case)
)

# Mise en forme des données sur les variables catégorielles
data_cat <- data_cat %>%
  mutate(Results_all = paste0(n, " (", round(prop), "%)"))
data_cat_seq <- data_cat_seq %>%
  mutate(Results_all = paste0(n, " (", round(prop), "%)"))

data_cat_seq <- pivot_wider(data_cat_seq[, c("seq", "variable", "Results_all")], names_from = "seq", values_from = "Results_all")

# Merge des dataset
data_cat <- merge(x = data_cat[, c("variable", "Results_all")], y = data_cat_seq, by = "variable", sort = FALSE)
write.xlsx(data_cat, file = "Table 2 - Cat.xlsx")

