# Date : 12 juin 2024
# Auteur : Jacob Lessard-Lord

# Projet FH-Diet

# Modèles de régression mixte pour évaluer l'effet de la diète sur les échelles visuelles analogues

# Chargement des packages requis ----
library(tidyverse)
library(ggpubr)

# Set working directory ----
setwd("C:/Users/jackl/OneDrive/Bureau/École/Postdoctorat/Chercheur/Jean-Philippe/FH-Diet/Processing_FH-Diet/Data")

# Importation des données ----
load("Data VAS post-diet.rda")

# Figure appreciation ----
setwd("C:/Users/jackl/OneDrive/Bureau/École/Postdoctorat/Chercheur/Jean-Philippe/FH-Diet/Article/Figures")

tiff(filename = "Figure S5.tiff", width = 12, height = 12, units = "cm", res = 600, compression = "lzw")
ggboxplot(data = data_vas_w4, x = "tx", y = "appreciation_after", fill = "tx",
          ylab = "Meal appreciation (mm)", add = "jitter", facet.by = "sex",
          panel.labs.font = list(size = 11, face = "bold", color = "white"), panel.labs.background = list(fill = "black"),
          palette = c("#df5f01", "#0160cd"), legend = "none") + 
  scale_x_discrete(labels = c("SAD", "WFPB"), name = NULL)
dev.off()
