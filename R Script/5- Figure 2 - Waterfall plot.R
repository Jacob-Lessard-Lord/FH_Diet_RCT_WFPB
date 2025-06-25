# Date : 14 juin 2024
# Auteur : Jacob Lessard-Lord

# Projet FH-Diet

# Waterfall plot de la réponse LDL-C et ApoB

# Chargement des packages requis ----
library(tidyverse)
library(ggpubr)
library(rstatix)

# Set working directory ----
setwd("C:/Users/jackl/OneDrive/Bureau/École/Postdoctorat/Chercheur/Jean-Philippe/FH-Diet/Processing_FH-Diet/Data")

# Importation des données ----
load("Data diff ITT.rda")

# Waterfall plot de la réduction en LDL-C ----

# Difference relative (%)

## Ajout de la couleur pour les 5 sujets avec un augmentation du LDL-C
data_diff_itt <- data_diff_itt %>%
  mutate(Sujet_col = factor(case_match(nomat,
                                       "002" ~ "COVID",
                                       "042" ~ "Accutane",
                                       "047" ~ "High_TG",
                                       "050" ~ "Marathon",
                                       "014" ~ "IDK",
                                       .default = "Normal"), 
                            levels = c("COVID", "Accutane", "High_TG", "Marathon", "IDK", "Normal")))

## Graphique
ldlc_rel <- ggbarplot(data_diff_itt %>% mutate(nomat = factor(as.character(nomat), levels = as.character(nomat)[order(ldlc)])),
                      x = "nomat", y = "ldlc", fill = "Sujet_col",
                      legend = "none", palette = c("#f12d9f", "#00974c", "#026bbf", "#cd8600", "#b9c90e", "lightgray"),
                      ylab = "WFPB–SAD LDL-C\nrelative difference (%)") +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank()) +
  scale_y_continuous(limits = c(-35, 30), breaks = seq(-30, 20, 10))

apob_rel <- ggbarplot(data_diff_itt %>% mutate(nomat = factor(as.character(nomat), levels = as.character(nomat)[order(apob_neph)])),
                      x = "nomat", y = "apob_neph", fill = "Sujet_col",
                      legend = "none", palette = c("#f12d9f", "#00974c", "#026bbf", "#cd8600", "#b9c90e", "lightgray"),
                      ylab = "WFPB–SAD ApoB\nrelative difference (%)") +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank()) +
  scale_y_continuous(limits = c(-37, 5), breaks = seq(-35, 5, 10))


# Enregistrement de la figure
setwd("C:/Users/jackl/OneDrive/Bureau/École/Postdoctorat/Chercheur/Jean-Philippe/FH-Diet/Article/Figures")

tiff(filename = "Figure 2.tiff", width = 17.5, height = 22.5, units = "cm", res = 600, compression = "lzw")
ggarrange(ldlc_rel, apob_rel, nrow = 2, labels = "AUTO")
dev.off()

# Atteinte des différentes cibles
data_diff_itt <- data_diff_itt %>%
  mutate(ldlc_5 = ifelse(test = ldlc <= -5, yes = "Yes", no = "No"),
         ldlc_10 = ifelse(test = ldlc <= -10, yes = "Yes", no = "No"),
         ldlc_15 = ifelse(test = ldlc <= -15, yes = "Yes", no = "No"),
         ldlc_20 = ifelse(test = ldlc <= -20, yes = "Yes", no = "No"),
         ldlc_25 = ifelse(test = ldlc <= -25, yes = "Yes", no = "No"),
         ldlc_30 = ifelse(test = ldlc <= -30, yes = "Yes", no = "No"),
         
         apob_neph_5 = ifelse(test = apob_neph <= -5, yes = "Yes", no = "No"),
         apob_neph_10 = ifelse(test = apob_neph <= -10, yes = "Yes", no = "No"),
         apob_neph_15 = ifelse(test = apob_neph <= -15, yes = "Yes", no = "No"),
         apob_neph_20 = ifelse(test = apob_neph <= -20, yes = "Yes", no = "No"),
         apob_neph_25 = ifelse(test = apob_neph <= -25, yes = "Yes", no = "No"),
         apob_neph_30 = ifelse(test = apob_neph <= -30, yes = "Yes", no = "No"))

# LDL-C
data_diff_itt %>% freq_table(ldlc_5) %>% mutate(Results_all = paste0(n, " (", round(prop), "%)"))
data_diff_itt %>% freq_table(ldlc_10) %>% mutate(Results_all = paste0(n, " (", round(prop), "%)"))

data_diff_itt %>% freq_table(ldlc_15) %>% mutate(Results_all = paste0(n, " (", round(prop), "%)"))
data_diff_itt %>% freq_table(ldlc_20) %>% mutate(Results_all = paste0(n, " (", round(prop), "%)"))

data_diff_itt %>% freq_table(ldlc_25) %>% mutate(Results_all = paste0(n, " (", round(prop), "%)"))
data_diff_itt %>% freq_table(ldlc_30) %>% mutate(Results_all = paste0(n, " (", round(prop), "%)"))


# ApoB
data_diff_itt %>% freq_table(apob_neph_5) %>% mutate(Results_all = paste0(n, " (", round(prop), "%)"))
data_diff_itt %>% freq_table(apob_neph_10) %>% mutate(Results_all = paste0(n, " (", round(prop), "%)"))

data_diff_itt %>% freq_table(apob_neph_15) %>% mutate(Results_all = paste0(n, " (", round(prop), "%)"))
data_diff_itt %>% freq_table(apob_neph_20) %>% mutate(Results_all = paste0(n, " (", round(prop), "%)"))

data_diff_itt %>% freq_table(apob_neph_25) %>% mutate(Results_all = paste0(n, " (", round(prop), "%)"))
data_diff_itt %>% freq_table(apob_neph_30) %>% mutate(Results_all = paste0(n, " (", round(prop), "%)"))

