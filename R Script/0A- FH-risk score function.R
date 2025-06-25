# Date : 12 juin 2024
# Auteur : Jacob Lessard-Lord

# Fonction pour calculer le risk score

fh_risk_score <- function(data_df) {
  data_df %>%
    mutate(FHRS_sex = case_match(sex, "Male" ~ 0.721, "Female" ~ 0),
           FHRS_age = case_when(age >= 18 & age <= 30 ~ 0,
                                age >= 31 & age <= 35 ~ 0.938,
                                age >= 36 & age <= 40 ~ 1.383,
                                age >= 41 & age <= 45 ~ 1.621,
                                age >= 46 & age <= 50 ~ 1.738,
                                age >= 51 & age <= 55 ~ 1.804,
                                age >= 56 & age <= 60 ~ 1.964,
                                age > 60 ~ 2.256),
           FHRS_hdlc = case_when(hdlc > 1.3 ~ 0,
                                 hdlc >= 1.01 & hdlc <= 1.3 ~ 0.298,
                                 hdlc >= 0.85 & hdlc <= 1 ~ 0.712,
                                 hdlc < 0.85 ~ 0.752),
           FHRS_ldlc = case_when(ldlc <= 5.5 ~ 0,
                                 ldlc >= 5.51 & ldlc <= 7.5 ~ 0.315,
                                 ldlc >= 7.51 & ldlc <= 8.5 ~ 0.718,
                                 ldlc >= 8.51 & ldlc <= 9.5 ~ 0.918,
                                 ldlc >= 9.5  ~ 1.136),
           FHRS_hypertension = case_match(hbp, "Yes" ~ 0.644, "No" ~ 0),
           FHRS_smoking = case_when(smknow %in% c("Everyday", "Occasionally") ~ 0.625,
                                    smknow == "Never" ~ 0),
           FHRS_lpa = case_when((lpa_neph/2.15) >= 50 ~ 0.434,
                                (lpa_neph/2.15) < 50 ~ 0),
           FHRS_sum = (FHRS_sex + FHRS_age + FHRS_hdlc + FHRS_ldlc + FHRS_hypertension + FHRS_smoking + FHRS_lpa),
           ten_year_risk_ascvd = ((1 - (0.889^exp(FHRS_sum - 3))) * 100) 
    )
}
