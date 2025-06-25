# Date : 14 juin 2024
# Auteur : Jacob Lessard-Lord

# Projet FH-Diet

# Modèles de régression lineaire pour évaluer l'effet de différentes variables sur la réduction de LDL-C en réponse à la diète

# Chargement des packages requis ----
library(tidyverse)
library(rstatix)
library(car)
library(olsrr)
library(rsq)
library(ggpubr)
library(openxlsx)

# Set working directory ----
setwd("C:/Users/jackl/OneDrive/Bureau/École/Postdoctorat/Chercheur/Jean-Philippe/FH-Diet/Processing_FH-Diet/Data")

# Importation des données ----
load("Data diff PP.rda")

# Graphique réponse vs niveau SAD LDL-C / ApoB ----

# Ajout de variable bidon pour faire les titres de facets
data_diff_pp <- data_diff_pp %>%
  mutate(titre_1 = factor("titre_1"),
         titre_2 = factor("titre_2"))

# Graphique réponse LDL-C vs post-SAD LDL-C
graph_ldlc <- ggscatter(data = data_diff_pp, x = "ldlc_ctrl", y = "ldlc",
                        xlab = "SAD level", ylab = "WFPB–SAD relative change",
                        facet.by = c("titre_1", "titre_2"),
                        panel.labs.font = list(size = 11, face = "bold", color = "white"), panel.labs.background = list(fill = "black"),
                        panel.labs = list(titre_1 = "LDL-C\n(%)",
                                          titre_2 = "LDL-C\n(mmol/L)"),
                        add = "reg.line",
                        add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
                        conf.int = TRUE # Add confidence interval
)

# Graphique réponse LDL-C vs post-SAD LDL-C
graph_apob <- ggscatter(data = data_diff_pp, x = "apob_neph_ctrl", y = "apob_neph",
                        xlab = "SAD level", ylab = "WFPB–SAD relative change",
                        facet.by = c("titre_1", "titre_2"),
                        panel.labs.font = list(size = 11, face = "bold", color = "white"), panel.labs.background = list(fill = "black"),
                        panel.labs = list(titre_1 = "ApoB\n(%)",
                                          titre_2 = "ApoB\n(g/L)"),
                        add = "reg.line",
                        add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
                        conf.int = TRUE # Add confidence interval
)


# Graphique réponse vs CRP et GLP-1 ----

# Sélection des données
data_graph <- data_diff_pp %>% select(c("nomat", "ldlc", "apob_neph",  "crp_neph_ctrl", "glp1_ctrl"))

# Mise en forme des données pour faire un grid
data_graph <- data_graph %>% 
  pivot_longer(cols = c("ldlc", "apob_neph"), values_to = "Valeur_var_rep", names_to = "Nom_var_rep") %>%
  pivot_longer(cols = c("crp_neph_ctrl", "glp1_ctrl"), values_to = "Valeur_var_exp", names_to = "Nom_var_exp") %>%
  mutate(Nom_var_rep = factor(Nom_var_rep, levels = c("ldlc", "apob_neph")),
         Nom_var_exp = factor(Nom_var_exp, c("crp_neph_ctrl", "glp1_ctrl")))

# Graphique CRP
graph_crp <- ggscatter(data = subset(data_graph, subset = Nom_var_exp  == "crp_neph_ctrl") %>% mutate(Nom_var_exp = droplevels(Nom_var_exp)),
                       x = "Valeur_var_exp", y = "Valeur_var_rep",
                       xlab = "SAD level", ylab = "WFPB–SAD relative change",
                       facet.by = c("Nom_var_rep", "Nom_var_exp"), scales = "free",
                       panel.labs.font = list(size = 11, face = "bold", color = "white"), panel.labs.background = list(fill = "black"),
                       panel.labs = list(Nom_var_exp = c("C-reactive\nprotein (mg/L)"),
                                         Nom_var_rep = c("LDL-C\n(%)", "ApoB\n(%)")),
                       add = "reg.line",
                       add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
                       conf.int = TRUE # Add confidence interval
)

# Graphique GLP-1
graph_glp1 <- ggscatter(data = subset(data_graph, subset = Nom_var_exp  == "glp1_ctrl") %>% mutate(Nom_var_exp = droplevels(Nom_var_exp)),
                       x = "Valeur_var_exp", y = "Valeur_var_rep",
                       xlab = "SAD level", ylab = "WFPB–SAD relative change",
                       facet.by = c("Nom_var_rep", "Nom_var_exp"), scales = "free",
                       panel.labs.font = list(size = 11, face = "bold", color = "white"), panel.labs.background = list(fill = "black"),
                       panel.labs = list(Nom_var_exp = c("GLP-1\n(pmol/L)"),
                                         Nom_var_rep = c("LDL-C\n(%)", "ApoB\n(%)")),
                       add = "reg.line",
                       add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
                       conf.int = TRUE # Add confidence interval
)

setwd("C:/Users/jackl/OneDrive/Bureau/École/Postdoctorat/Chercheur/Jean-Philippe/FH-Diet/Article/Figures")

tiff(filename = "Figure S3.tiff", width = 18, height = 15, units = "cm", res = 600, compression = "lzw")
ggarrange(ggarrange(graph_ldlc, graph_apob, nrow = 2, labels = c("A", NULL)),
          graph_crp, graph_glp1, ncol = 3, labels = "AUTO"
          )
dev.off()
