# Date : 30 mai 2025
# Auteur : Jacob Lessard-Lord

# Projet FH-Diet

# Préparation des données

# Chargement des packages requis ----
library(tidyverse)
library(readxl)

# Set working directory ----
setwd("C:/Users/jackl/OneDrive/Bureau/École/Postdoctorat/Chercheur/Jean-Philippe/FH-Diet/Processing_FH-Diet/Script R")

# Importation de la fonction pour calculer le FH-risk score
source("0A- FH-risk score function.R")

# Set working directory ----
setwd("C:/Users/jackl/OneDrive/Bureau/École/Postdoctorat/Chercheur/Jean-Philippe/FH-Diet/Processing_FH-Diet/Data")

# Stockage des variables mesurées dans un vecteur
variable_mesure <- c("chol", "tg", "hdlc", "ldlc", "ldlc_lpa_corr", "nhdlc", "chol_hdlc", "apoa1_neph", "apob_neph", "glufast", "ins2018", "hba1c", "crp_neph", "lpa_neph",
                     "sbp", "dbp", "ApoC3_total", "ApoC3_HDL", "Ratio_ApoC3_total_HDL",
                     "pcsk9", "glp1", "pyy", 
                     "Cholestanol", "Desmosterol", "Lathosterol", "Campesterol", "Stigmasterol", "Lanosterol", "beta_Sitosterol", "Lathosterol_Cholestanol",
                     paste0(c("Cholestanol", "Desmosterol", "Lathosterol", "Campesterol", "Stigmasterol", "Lanosterol", "beta_Sitosterol"), "_norm"))

# Importation des données FANI ----
data_all <- read_excel(path = "2024-10-28 Data FH-Diet.xlsx", skip = 3, na = ".")

# Ajout des données DEXA ----

## Importation des données
data_dexa <- read_excel(path = "2024-07-28 Transposition DXA.xlsx")

## Mise en forme des données
data_dexa <- data_dexa %>%
  mutate(nomat = gsub(x = Nomat, pattern = "FH-Diet-", replacement = ""),
         date_visite = substr(x = Date_Exam, start = 1, stop = 10))

## Merge des données
data_all <- merge(x = data_all, y = data_dexa, by = c("nomat", "date_visite"), all = TRUE, sort = FALSE)
rm(data_dexa)

# Ajout des données de PCSK9 ----

## Importation des données
data_pcsk9 <- read_excel("2024-11-15 Résultat ELISA PCSK9.xlsx")

## Mise en forme des données
data_pcsk9 <- pivot_longer(data = data_pcsk9, cols = c("V0", "V1", "V2", "V4"), names_to = "Visites", values_to = "pcsk9")

## Merge des données
data_all <- merge(x = data_all, y = data_pcsk9, by = c("nomat", "Visites"), all = TRUE, sort = FALSE)
rm(data_pcsk9)

# Ajout des données de GLP-1 ----

## Importation des données
data_glp1 <- read_excel("2025-01-20 Résultat ELISA GLP-1.xlsx")

## Mise en forme des données
data_glp1 <- pivot_longer(data = data_glp1, cols = c("V2", "V4"), names_to = "Visites", values_to = "glp1")

## Merge des données
data_all <- merge(x = data_all, y = data_glp1, by = c("nomat", "Visites"), all = TRUE, sort = FALSE)
rm(data_glp1)

# Ajout des données de PYY ----

## Importation des données
data_pyy <- read_excel("2025-01-20 Résultat ELISA PYY.xlsx")

## Mise en forme des données
data_pyy <- pivot_longer(data = data_pyy, cols = c("V2", "V4"), names_to = "Visites", values_to = "pyy")

## Merge des données
data_all <- merge(x = data_all, y = data_pyy, by = c("nomat", "Visites"), all = TRUE, sort = FALSE)
rm(data_pyy)

# Ajout des données de phytosterol ----

## Importation des données
data_phytosterol <- read_excel("2025-02-10 Résultat Phytosterols.xlsx")

## Mise en forme des données
data_phytosterol <- data_phytosterol %>%
  separate_wider_delim(cols = "Sample_ID", delim = " ", names = c("nomat", "Visites"))

## Merge des données
data_all <- merge(x = data_all, y = data_phytosterol, by = c("nomat", "Visites"), all = TRUE, sort = FALSE)
rm(data_phytosterol)

## Normalisation des données sur le cholesterol total
data_all <- data_all %>%
  mutate(Cholestanol_norm = (Cholestanol / chol) * 100,
         Desmosterol_norm = (Desmosterol / chol) * 100,
         Lathosterol_norm = (Lathosterol / chol) * 100,
         Campesterol_norm = (Campesterol / chol) * 100,
         Stigmasterol_norm = (Stigmasterol / chol) * 100,
         Lanosterol_norm = (Lanosterol / chol) * 100,
         beta_Sitosterol_norm = (beta_Sitosterol / chol) * 100,
         Lathosterol_Cholestanol = Lathosterol / Cholestanol)

# Ajout des données d'ApoC3 ----

## Importation des données
data_apoc3 <- read_excel("2025-06-13 Résultat ApoCIII.xlsx")

## Mise en forme des données
data_apoc3 <- data_apoc3 %>%
  mutate(nomat = gsub(x = nomat, pattern = "FH-diet ", replacement = ""),
         nomat = substr(nomat, 1, 3))

## Merge des données
data_all <- merge(x = data_all, y = data_apoc3, by = c("nomat", "Visites"), all = TRUE, sort = FALSE)
rm(data_apoc3)

# Recodage des genotypes des sujets autres qui avait été mal classifié dans FANI ----
data_all[data_all$Visites == "V0" & data_all$nomat %in% c("033", "034", "036", "053"), "hfgenotype"] <- NA

# Recodage des variables dans le bon format + ajout de nouvelles variables (HOMA-IR, LDL-C corrected for Lpa) + catégories pour classification ----
data_all <- data_all %>%
  mutate(date_visite = weekdays(as.POSIXlt(date_visite)),
         sex = factor(case_match(sex, 1 ~ "Female", 2 ~ "Male")),
         age_cat_40 = factor(ifelse(test = age < 40, yes = "Young", no = "Old")),
         hfgenotype = factor(case_match(hfgenotype, 1 ~ "RD", 2 ~ "RN", NA ~ "Other")),
         hbp = factor(case_match(hbp, 1 ~ "Yes", 2 ~ "No")),
         smknow = factor(case_match(smknow, 1 ~ "Everyday", 2 ~ "Occasionally", 3 ~ "Never")),
         tx = factor(case_match(tx, "Contrôle" ~ "SAD", "GAC" ~ "WFPB"), levels = c("SAD", "WFPB")),
         seq = factor(case_match(seq, "Contrôle-GAC" ~ "SAD-WFPB", "GAC-Contrôle" ~ "WFPB-SAD")),
         ldlc_lpa_corr = (((ldlc * 38.67) - (lpa_neph * 0.077)) / 38.67),
         hba1c = (hba1c * 100),
         homa_ir = (((ins2018 / 6) * glufast) / 22.5)) # https://pmc.ncbi.nlm.nih.gov/articles/PMC6501531/

# Préparation des métadonnées ----

# Extraction des métadonnées (sexe, age, taille, genotype, high blood pressure et tabagisme)
metadata <- subset(data_all,
                   subset = Visites == "V0",
                   select = c("nomat", "sex", "age", "height", "hfgenotype", "hbp", "smknow"))

# Retrait des dropouts et les observation du volet 2
metadata <- subset(metadata, subset = nomat %in% subset(data_all, subset = Visites == "V4" & as.numeric(substr(nomat, 1, 1)) == 0 & !is.na(date_visite))$nomat)

# Préparation des données au baseline (V1 - Sans médicament) ----

# Extraction des données
data_baseline <- subset(data_all,
                        subset = Visites == "V1",
                        select = c("nomat", "seq", "Visites", "weight", "waistc",
                                   "chol", "tg", "hdlc", "ldlc", "ldlc_lpa_corr", "nhdlc", "chol_hdlc", "apoa1_neph", "apob_neph",
                                   "glufast", "ins2018", "hba1c", "crp_neph", "lpa_neph", "sbp", "dbp"))

# Merge avec les métadonnées
data_baseline <- merge(x = metadata, y = data_baseline, by = "nomat") %>%
  # Calcul du BMI
  mutate(bmi = (weight / (height^2)))

# Extraction de l'information sur la médication hypolipémiante ----

# Extraction et mise en forme
data_med <- subset(data_all,
                   subset = Visites == "V0",
                   select = c("nomat", paste0("rx_name_", 1:14), paste0("rx_fin_", 1:14))) %>%
  # Mise en forme longue
  pivot_longer(cols = c(paste0("rx_name_", 1:14), paste0("rx_fin_", 1:14)), names_to = "Parametre", values_to = "Valeur") %>%
  # Recodage de la variable Parametre
  mutate(Parametre = gsub(x = Parametre, pattern = "rx_name_", replacement = "rx_name rx"),
         Parametre = gsub(x = Parametre, pattern = "rx_fin_", replacement = "rx_fin rx")) %>%
  # Separation de la variable Parametre
  separate_wider_delim(cols = Parametre, delim = " ", names = c("Parametre", "rx_number")) %>%
  # Mise en forme large 
  pivot_wider(names_from = "Parametre", values_from = "Valeur") %>%
  # Retrait des médicaments arrêtés
  subset(subset = is.na(rx_fin), select = -rx_fin) %>% 
  # Sélection des médicaments hypolipémiants seulement
  subset(subset = grepl(x = rx_name, pattern = "Praluent") | grepl(x = rx_name, pattern = "Repatha") | grepl(x = rx_name, pattern = "Leqvio") |
           grepl(x = rx_name, pattern = "Ezetimibe") | grepl(x = rx_name, pattern = "Ezetrol") | 
           grepl(x = rx_name, pattern = "statin") | grepl(x = rx_name, pattern = "Crestor") | grepl(x = rx_name, pattern = "Lipitor")) %>%
  # Ajout d'une colonne identifiant la consommation de 1) statin 2) ezetimibe 3) PCSK9
  mutate(statin = ifelse(test = grepl(x = rx_name, pattern = "statin") | grepl(x = rx_name, pattern = "Crestor") | grepl(x = rx_name, pattern = "Lipitor"),
                         yes = 1, no = 0),
         ezetimibe = ifelse(test = grepl(x = rx_name, pattern = "Ezetimibe") | grepl(x = rx_name, pattern = "Ezetrol"),
                            yes = 1, no = 0),
         pcsk9 = ifelse(test = grepl(x = rx_name, pattern = "Praluent") | grepl(x = rx_name, pattern = "Repatha") | grepl(x = rx_name, pattern = "Leqvio"),
                        yes = 1, no = 0))

# Aggregation par sujet
data_med <- aggregate(data_med[, c("statin", "ezetimibe", "pcsk9")], by = list(nomat = data_med$nomat), FUN = sum)

# Identification de chacune des situations 1) statin monotherapy, 2) Statin + ezetimibe, 3) PCSK9
data_med <- data_med %>%
  mutate(rx_case = case_when(statin > 0 & ezetimibe == 0 & pcsk9 == 0 ~ "Statin monotherapy",
                             statin > 0 & ezetimibe > 0 & pcsk9 == 0 ~ "Statin and ezetimibe",
                             pcsk9 > 0 ~ "PCSK9 inhibitor"))
# Merge les données baseline
data_baseline <- merge(x = data_baseline, y = data_med %>% select(nomat, rx_case), by = "nomat", all.x = TRUE) %>%
  # Changement des NA en None
  mutate(rx_case = factor(ifelse(test = is.na(rx_case), yes = "None", no = rx_case)))

rm(data_med)

# Calcul du FH-Risk score
data_baseline <- fh_risk_score(data_baseline)

# Ajout de la stratiification à 40 et la proportion de FH détectée : Atteinte de cible (LDL-C > 5 mmol pour 40 ans et plus et LDL-C >= 4.5 mmol/L pour moins de 40 ans)
data_baseline <- data_baseline %>%
  mutate(age_cat_40 = factor(ifelse(test = age < 40, yes = "Young", no = "Old")),
         fh_detection = factor(ifelse(test = ldlc > 5 & age_cat_40 == "Old", yes = "FH detected",
                                      no = ifelse(test = ldlc > 4.5 & age_cat_40 == "Young", yes = "FH detected", no = "FH non detected"))))

# Extraction du temps de wash-out
temps_washout <- data_all %>%
  subset(subset = Visites == "V3", select = c(nomat, washout))

# Merge les données baseline
data_baseline <- merge(x = data_baseline, y =temps_washout, by = "nomat")

rm(temps_washout)

# Préparation des données post-diète ----

# Extraction des données
data_post <- subset(data_all,
                    subset = Visites %in% c("V2", "V4"),
                    select = c("nomat", "Visites", "seq", "tx", "weight", "comp_mean", "kcal_mean", "Masse_Gras_Androide_Viscerale", variable_mesure))

# Merge avec les métadonnées
data_post <- merge(x = metadata, y = data_post, by = "nomat") %>%
  # Calcul du BMI
  mutate(bmi = (weight / (height^2)))

# Calcul du FH-Risk score
data_post <- fh_risk_score(data_post)

# Ajout de la stratiification à 40 et la proportion de FH détectée : Atteinte de cible (LDL-C > 5 mmol pour 40 ans et plus et LDL-C >= 4.5 mmol/L pour moins de 40 ans)
data_post <- data_post %>%
  mutate(age_cat_40 = factor(ifelse(test = age < 40, yes = "Young", no = "Old")),
         fh_detection = factor(ifelse(test = ldlc > 5 & age_cat_40 == "Old", yes = "FH detected",
                                      no = ifelse(test = ldlc > 4.5 & age_cat_40 == "Young", yes = "FH detected", no = "FH non detected"))))

# Calcul de la variation de poids post - pré diète (relatif)  ----

# Extraction, calcul et mise en forme
data_weight_delta <- subset(data_all,
                      subset = Visites %in% c("V1", "V2", "V3", "V4"),
                      select = c("nomat", "Visites", "weight")) %>%
  # Mise en forme large
  pivot_wider(names_from = "Visites", values_from = "weight") %>%  
  # Calcul de la difference relative V2-V1 et V4-V3
  mutate(Weight_delta_V2V1 = ((V2 - V1) / V1) * 100,
         Weight_delta_V4V3 = ((V4 - V3) / V3) * 100) %>% 
  select(c("nomat", "Weight_delta_V2V1", "Weight_delta_V4V3")) %>%
  # Mise en forme longue
  pivot_longer(cols = c("Weight_delta_V2V1", "Weight_delta_V4V3"), names_to = "Visites", values_to = "weight_delta") %>%
  # Recodage de la variable Visites
  mutate(Visites = factor(case_match(Visites, "Weight_delta_V2V1" ~ "V2", "Weight_delta_V4V3" ~ "V4")))

# Merge avec les données post-diète
data_post <- merge(x = data_post, y = data_weight_delta, by = c("nomat", "Visites"))

rm(data_weight_delta)

# Extraction des données de poids pour pouvoir vérifier que la perte de poids par diète est significative ----

# Extraction des données
data_weight <- subset(data_all,
                      subset = Visites %in% c("V1", "V2", "V3", "V4"),
                      select = c("nomat", "Visites", "tx", "weight", "seq")) %>%
  # Selection des sujets
  subset(subset = nomat %in% subset(data_all, subset = Visites == "V4" & as.numeric(substr(nomat, 1, 1)) == 0 & !is.na(date_visite))$nomat) %>%
  # Recodage de la variable Visites
  mutate(Visites = case_match(Visites, "V1" ~ "Pre", "V2" ~ "Post", "V3" ~ "Pre", "V4" ~ "Post")) %>%
  # Mise en forme large
  pivot_wider(names_from = "tx", values_from = "weight")

# Merge avec les metadata
data_weight <- merge(x = metadata, y = data_weight, by = "nomat")

# Préparation du dataset pour l'analyse des déterminants de la réponse LDL-C/ApoB ----

# Calcul de la réponse LDL-C/ApoB (différence relative) et le mean daily calorie intake
data_diff <- subset(data_post, subset = !is.na(tx), select = c("nomat", "tx", "ldlc", "apob_neph", "kcal_mean")) %>%
  # Mise en forme longue
  pivot_longer(cols = all_of(c("ldlc", "apob_neph")), names_to = "Parametre", values_to = "Valeur") %>%
  # Mise en forme large
  pivot_wider(names_from = "tx", values_from = c("Valeur", "kcal_mean")) %>%
  # Calcul de la différence relative et moyenne des calories
  mutate(Diff_rel = ((Valeur_WFPB - Valeur_SAD) / Valeur_SAD) * 100,
         kcal_mean = ((kcal_mean_WFPB + kcal_mean_SAD) / 2) / 100) %>% # Division par 100 pour avoir des beta interprétables (par 100 calories)
  select(c(nomat, Parametre, Diff_rel, kcal_mean)) %>%
  # Remise en forme large
  pivot_wider(names_from = "Parametre", values_from = "Diff_rel")

# Extraction des résultats post-SAD
data_controle <- subset(data_all,
                        subset = tx == "SAD" & Visites %in% c("V2", "V4"),
                        select = c("nomat", variable_mesure))

# Ajout d'un suffixe devant les variables
colnames(data_controle)[colnames(data_controle) %in% variable_mesure] <- paste0(variable_mesure, "_ctrl")

# Calcul du HOMA-IR, conversion du HbA1c en % et ajustement de la valeur de Lp(a) et insuline pour rendre les beta + interpretable
data_controle <- data_controle %>%
  mutate(hba1c_ctrl = hba1c_ctrl * 100,
         homa_ir_ctrl = (((ins2018_ctrl / 6) * glufast_ctrl) / 22.5), # https://pmc.ncbi.nlm.nih.gov/articles/PMC6501531/
         lpa_neph_ctrl = (lpa_neph_ctrl / 10), # Pour avoir des betas plus facilement interprétable
         ins2018_ctrl = (ins2018_ctrl / 100)) # Pour avoir des betas plus facilement interprétable

# Merge du dataset avec les données contrôle
data_diff <- merge(x = data_diff, y = data_controle, by = "nomat")
rm(data_controle)

# Merge du dataset avec les métadonnées
data_diff <- merge(x = metadata, y = data_diff, by = "nomat")

# Ajout du BMI controle
data_diff <- merge(x = data_diff,
                   y = subset(data_post, subset = tx == "SAD", select = c(nomat, bmi)),
                   by = "nomat")

# Préparation des données de VAS ----

# Extraction des données
data_vas <- subset(data_all,
                   subset = Visites %in% c("EVA1", "EVA2", "ÉVA3", "ÉVA4"),
                   select = c("nomat", "Visites", "seq", "tx", "vas_wkday", "vas_day",
                              c(paste0(paste0("vas", 1:4, "_a"), c(rep("vdej", 4), rep("vdin", 4), rep("vsoup", 4))),
                                paste0(paste0("vas", 1:5, "_a"), c(rep("pdej", 5), rep("pdin", 5), rep("psoup", 5))))))

# Retrait des sujets qui n'ont pas complété l'ensemble des VAS
sujet_manquant <- data_vas %>% select(nomat, Visites, vas_wkday) %>% pivot_wider(names_from = Visites, values_from = vas_wkday) %>%
  filter(!is.na(EVA1) & !is.na(EVA2) & !is.na(ÉVA3) & !is.na(ÉVA4))

data_vas <- subset(data_vas, subset = nomat %in% sujet_manquant$nomat)

rm(sujet_manquant)

# Merge avec les métadonnées
data_vas <- merge(x = metadata, y = data_vas, by = "nomat")

# Extraction des données de poids pour pouvoir calculer le BMI
data_weight_vas <- subset(data_all, subset = Visites %in% c("V1", "V2", "V3", "V4"), select = c(nomat, Visites, weight)) %>%
  mutate(Visites = case_match(Visites, 
                              "V1" ~ "EVA1",
                              "V2" ~ "EVA2",
                              "V3" ~ "ÉVA3",
                              "V4" ~ "ÉVA4"))

# Merge des données de poids avec les données VAS
data_vas <- merge(x = data_vas, y = data_weight_vas, by = c("nomat", "Visites")) %>%
  # Calcul du BMI
  mutate(bmi = (weight / (height^2)))

rm(data_weight_vas)

# Extraction des données d'énergie pour pouvoir calculer le satiety quotient
energie_w1 <- subset(data_all, subset = Visites %in% c("V2", "V4"), select = c("nomat", "Visites", "kcal_w1"))
energie_w4 <- subset(data_all, subset = Visites %in% c("V2", "V4"), select = c("nomat", "Visites", "kcal_w4"))

# Mise en forme des données d'énergie

## Semaine 1
energie_w1 <- energie_w1 %>%
  mutate(Visites = case_match(Visites, "V2" ~ "EVA1", "V4" ~ "ÉVA3")) %>%
  rename(kcal = kcal_w1)

## Semaine 4
energie_w4 <- energie_w4 %>%
  mutate(Visites = case_match(Visites, "V2" ~ "EVA2", "V4" ~ "ÉVA4")) %>%
  rename(kcal = kcal_w4)

# Merge de l'énergie avec les données
data_vas <- merge(x = data_vas, y = rbind(energie_w1, energie_w4), by = c("nomat", "Visites"))

rm(energie_w1, energie_w4)

# Calcul du satiety quotient
data_vas <- data_vas %>%
  mutate(satietyquotient_apdej = ((vas3_apdej - vas3_avdej) / (kcal * 0.3)) * 100,
         satietyquotient_apdin = ((vas3_apdin - vas3_avdin) / (kcal * 0.35)) * 100,
         satietyquotient_apsoup = ((vas3_apsoup - vas3_avsoup) / (kcal * 0.35)) * 100)

# Mise en forme longue des données
data_vas <- pivot_longer(data = data_vas,
                         cols = c(paste0(paste0("vas", 1:4, "_a"), c(rep("vdej", 4), rep("vdin", 4), rep("vsoup", 4))),
                                  paste0(paste0("vas", 1:5, "_a"), c(rep("pdej", 5), rep("pdin", 5), rep("psoup", 5))),
                                  "satietyquotient_apdej", "satietyquotient_apdin", "satietyquotient_apsoup"), 
                         names_to = "Variable", values_to = "Score")

# Creation des variables meal (breakfast, lunch, dinner) et measure (desire to eat, hunger, fullness, prospective food consumption, appetite score)
data_vas <- data_vas %>%
  mutate(Variable = gsub(x = Variable, pattern = "_av", replacement = "_av "),
         Variable = gsub(x = Variable, pattern = "_ap", replacement = "_ap "))

data_vas <- separate(data = data_vas, col = Variable, into = c("measure", "meal"), sep = " ")

# Recodage de la variable measure
data_vas <- data_vas %>%
  mutate(measure = gsub(x = measure, pattern = "av", replacement = "before"),
         measure = gsub(x = measure, pattern = "ap", replacement = "after"),
         measure = gsub(x = measure, pattern = "vas1", replacement = "desire_to_eat"),
         measure = gsub(x = measure, pattern = "vas2", replacement = "hunger"),
         measure = gsub(x = measure, pattern = "vas3", replacement = "fullness"),
         measure = gsub(x = measure, pattern = "vas4", replacement = "prospective_food_consumption"),
         measure = gsub(x = measure, pattern = "vas5", replacement = "appreciation"),
         measure = gsub(x = measure, pattern = "satietyquotient", replacement = "satiety_quotient"))

# Transformation logarithmique des score pour régler les problèmes d'hétéroscédasticité
data_vas_log <- data_vas %>%
  mutate(Score = Score + 1,
         Score = log(Score))

# Erreur pour la transformation log du satiety quotient car certaines valeurs sont negatives !

# Mise en forme large des données pour avoir une colonne par variable mesurée
data_vas <- pivot_wider(data_vas, names_from = "measure", values_from = "Score")
data_vas_log <- pivot_wider(data_vas_log, names_from = "measure", values_from = "Score")

# Reformatage des variables catégorielles en facteurs
data_vas <- data_vas %>%
  mutate(nomat = factor(nomat),
         vas_day_factor = factor(case_match(Visites, "EVA1" ~ "After 7 days", "EVA2" ~ "After 28 days", "ÉVA3" ~ "After 7 days", "ÉVA4" ~ "After 28 days"),
                                 levels = c("After 7 days", "After 28 days")),
         seq = factor(seq),
         vas_wkday = factor(case_match(vas_wkday, 1 ~ "Monday", 2 ~ "Tuesday", 3 ~ "Wednesday", 4 ~ "Thursday", 5 ~ "Friday", 6 ~ "Satursday", 7 ~ "Sunday")),
         meal = factor(case_match(meal, "dej" ~ "Breakfast", "din" ~ "lunch", "soup" ~ "Dinner")),
         appetite_score_before = ((desire_to_eat_before + hunger_before + (150 - fullness_before) + prospective_food_consumption_before) / 4),
         appetite_score_after = ((desire_to_eat_after + hunger_after + (150 - fullness_after) + prospective_food_consumption_after) / 4))

data_vas_log <- data_vas_log %>%
  mutate(nomat = factor(nomat),
         vas_day_factor = factor(case_match(Visites, "EVA1" ~ "After 7 days", "EVA2" ~ "After 28 days", "ÉVA3" ~ "After 7 days", "ÉVA4" ~ "After 28 days"),
                                 levels = c("After 7 days", "After 28 days")),
         seq = factor(seq),
         vas_wkday = factor(case_match(vas_wkday, 1 ~ "Monday", 2 ~ "Tuesday", 3 ~ "Wednesday", 4 ~ "Thursday", 5 ~ "Friday", 6 ~ "Satursday", 7 ~ "Sunday")),
         meal = factor(case_match(meal, "dej" ~ "Breakfast", "din" ~ "lunch", "soup" ~ "Dinner")),
         appetite_score_before = ((desire_to_eat_before + hunger_before + (150 - fullness_before) + prospective_food_consumption_before) / 4),
         appetite_score_after = ((desire_to_eat_after + hunger_after + (150 - fullness_after) + prospective_food_consumption_after) / 4))

# Séparation du data.frame selon le genre
data_vas_female <- as.data.frame(subset(data_vas, subset = sex == "Female"))
data_vas_male <- as.data.frame(subset(data_vas, subset = sex == "Male"))

data_vas_log_female <- as.data.frame(subset(data_vas_log, subset = sex == "Female"))
data_vas_log_male <- as.data.frame(subset(data_vas_log, subset = sex == "Male"))

# Sélection des seulement des VAS à la fin des interventions (EVA2 et EVA4)
data_vas_w4 <- subset(data_vas, subset = Visites %in% c("EVA2", "ÉVA4"))
data_vas_log_w4 <- subset(data_vas_log, subset = Visites %in% c("EVA2", "ÉVA4"))

# Calcul de la différence entre W4 - W1 (pour figure S4) ----

# Mise en forme des données
data_vas_diff <- data_vas %>%
  select(-c(Visites, vas_day, vas_wkday, bmi, weight)) %>%
  pivot_longer(cols = c("desire_to_eat_before", "hunger_before", "fullness_before", "prospective_food_consumption_before", "desire_to_eat_after",
                        "hunger_after", "fullness_after", "prospective_food_consumption_after", "appreciation_after","satiety_quotient_after",
                        "appetite_score_before", "appetite_score_after"),
               names_to = "measure", values_to = "Score") %>%
  mutate(vas_day_factor = case_match(vas_day_factor, "After 7 days" ~ "W1", "After 28 days" ~ "W4")) %>%
  pivot_wider(names_from = "vas_day_factor", values_from = c("Score", "kcal")) %>%
  mutate(Score_diff = Score_W4 - Score_W1, 
         kcal_diff = kcal_W4 - kcal_W1) %>%
  select(-c(Score_W1, Score_W4, kcal_W1, kcal_W4)) %>%
  pivot_wider(names_from = "measure", values_from = "Score_diff")

# Ajout des données de BMI de fin d'intervention
data_vas_diff <- merge(x = data_post %>% select(nomat, tx, bmi),
                       y = data_vas_diff,
                       by = c("nomat", "tx"))


#### Approche intention-to-treat vs per protocol ####

## Per protocol : Retrait des sujet 041 et 042 ##

# Données post-diète
data_post_itt <- data_post
data_post_pp <- subset(data_post, subset = !nomat %in% c("041", "042"))
rm(data_post)

# Données de réponse LDL-C/ApoB (pour waterfall plot - ITT et pour analyse des déterminants - per protocol)
data_diff_itt <- data_diff
data_diff_pp <- subset(data_diff, subset = !nomat %in% c("041", "042"))

rm(data_diff)

# Données de poids (ITT seulement)
data_weight_itt <- data_weight

# Données VAS (per protocol uniquement)
data_vas <- subset(data_vas, subset = !nomat %in% c("041", "042"))
data_vas_log <- subset(data_vas_log, subset = !nomat %in% c("041", "042"))

data_vas_w4 <- subset(data_vas_w4, subset = !nomat %in% c("041", "042"))
data_vas_log_w4 <- subset(data_vas_log_w4, subset = !nomat %in% c("041", "042"))

data_vas_male <- subset(data_vas_male, subset = !nomat %in% c("041", "042"))
data_vas_log_male <- subset(data_vas_log_male, subset = !nomat %in% c("041", "042"))

data_vas_female <- subset(data_vas_female, subset = !nomat %in% c("041", "042"))
data_vas_log_female <- subset(data_vas_log_female, subset = !nomat %in% c("041", "042"))

data_vas_diff <- subset(data_vas_diff, subset = !nomat %in% c("041", "042"))

# Enregistrement des données ----

# Données baseline
save(data_baseline, file = "Data baseline.rda")

# Données post-diète
save(data_post_itt, file = "Data post-diet ITT.rda")
save(data_post_pp, file = "Data post-diet PP.rda")

# Données de poids
save(data_weight_itt, file = "Data weight ITT.rda")

# Données de réponse LDL-C/ApoB 
save(data_diff_itt, file = "Data diff ITT.rda")
save(data_diff_pp, file = "Data diff PP.rda")

# Données VAS
save(data_vas_w4, data_vas_log_w4, file = "Data VAS post-diet.rda")
save(data_vas, data_vas_log, data_vas_male, data_vas_log_male, data_vas_female, data_vas_log_female, file = "Data VAS.rda")
save(data_vas_diff, file = "Data VAS diff.rda")
