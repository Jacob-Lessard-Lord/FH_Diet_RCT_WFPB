# Date : 18 juin 2024
# Auteur : Jacob Lessard-Lord

# Projet FH-Diet

# Comparaison du niveau de détection du FH selon guideline CCS

# Chargement des packages requis ----
library(tidyverse)
library(rstatix)
library(ggpubr)

# Set working directory ----
setwd("C:/Users/jackl/OneDrive/Bureau/École/Postdoctorat/Chercheur/Jean-Philippe/FH-Diet/Processing_FH-Diet/Data")

# Importation des données ----
load("Data post-diet PP.rda")

# Analyse de la proportion qui atteigne les cibles de surveillance ----

# Test de McNemar
stat_test <- pairwise_mcnemar_test(data_post_pp, fh_detection ~ tx|nomat) 
stat_test

# Ajustement de la position et arrondissement de la p-value
stat_test <- stat_test %>%
  mutate(y.position = 102,
         p = round(p, digits = 2))

# Creation d'un tableau de frequence
df_freq <- data_post_pp %>% freq_table(tx, fh_detection)
