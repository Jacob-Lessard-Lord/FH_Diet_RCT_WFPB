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

# Graphique
setwd("C:/Users/jackl/OneDrive/Bureau/École/Postdoctorat/Chercheur/Jean-Philippe/FH-Diet/Processing_FH-Diet/Data")

tiff(filename = "Figure S2.tiff", width = 5.75, height = 12, units = "cm", res = 600, compression = "lzw")
ggbarplot(data = subset(df_freq, subset = fh_detection == "FH detected"),
          x = "tx", y = "prop", fill = "tx",
          ylab = "Proportion of FH-detected subjects (%)",
          label = TRUE, palette = c("#df5f01", "#0160cd"), legend = "none")+ 
  scale_x_discrete(labels = c("SAD", "WFPB"), name = NULL) + 
  stat_pvalue_manual(stat_test, label = "P = {p}", tip.length = 0.15)
dev.off()
