#Alter

# Entfernen der Spalte V2-V4 (brauche ich nicht) 
Demographics [2:4] <- list(NULL)

# Manuelles Zuweisen der Spaltennamen, falls bekannt
colnames(Demographics) <- c("study_id", "age", "gender","smoking","cigarettesnr",
"education","occupation", "hours_parttime", "hours_fulltime","medication_none","medication_pain",
"medication_opioid","medication_depr","medication_MmRelax","medication_Cann","medication_other",
"frequency_med","care_none","care_GP","care_Specialist","care_PT","care_Chiro","care_Psych","care_Mass","care_other",
"nr_GP_treatment","imaging_none","imaging_Rö","imaging_MRI","imaging_other","nr_physio_treatment","nr_chiro_treatment",
"nr_psych_treatment","nr_massage_treatment","nr_other_treatment")

# Löschen der ersten Zeile
Demographics <- Demographics [-1, ]

# Umwandeln der Spalte age in numerische Werte
Demographics$age <- as.numeric(Demographics$age)

# Berechnung der Spannweite und des Mittelwerts der Spalte V5
age_range <- range(Demographics$age, na.rm = TRUE)
age_mean <- mean(Demographics$age, na.rm = TRUE)
age_sd <- sd(Demographics$age, na.rm = TRUE)

# Anzahl der Probanden
n <- length(Demographics$age[!is.na(Demographics$age)])

# Z-Wert für 95% Konfidenzintervall
z <- qnorm(0.975)  # Zweischwänziger Test, also 0.975 statt 0.95

# Berechnung des Konfidenzintervalls
ci_width <- z * (age_sd / sqrt(n))
lower_ci <- age_mean - ci_width
upper_ci <- age_mean + ci_width

# Ausgabe der Ergebnisse
print(paste("Mittelwert des Alters:", age_mean))
print(paste("95% Konfidenzintervall für das Alter: [", lower_ci, ",", upper_ci, "]", sep=""))


# Ausgabe der Ergebnisse
cat("Spannweite des Alters: ", age_range, "\n")
cat("Mittelwert des Alters: ", age_mean, "\n")


#Gender:

# Umwandeln der Spalte gender in numerische Werte
Demographics$gender <- as.numeric(Demographics$gender)

# Zählen der Werte in der Spalte gender
gender_counts <- table(Demographics$gender, useNA = "no")

# Berechnung der Prozentsätze
gender_percentages <- round(gender_counts / sum(gender_counts) * 100, 2)

# Erstellen eines DataFrames mit den Ergebnissen
gender_df <- data.frame(
  gender = c("männlich", "weiblich"),
  count = as.vector(gender_counts),
  percentage = as.vector(gender_percentages)
)

# Ausgabe der Ergebnisse
print(gender_df)

# Berechnung der Spannweite und des Mittelwerts des Alters
age_range <- range(Demographics$age, na.rm = TRUE)
age_mean <- mean(Demographics$age, na.rm = TRUE)
age_sd <- sd(Demographics$age, na.rm = TRUE)
gender_sd <- sd(Demographics$gender, na.rm = TRUE)

# Ausgabe der Ergebnisse
cat("Alter:\n")
cat("Spannweite: ", age_range, "\n")
cat("Mittelwert: ", age_mean, "\n")
cat("Standardabweichung: ", age_sd, "\n\n")


#nochmals neu einlesen der Daten
# Entfernen der Spalte V2-V4 (brauche ich nicht) 
Demographics [2:4] <- list(NULL)

# Manuelles Zuweisen der Spaltennamen, falls bekannt
colnames(Demographics) <- c("study_id", "age", "gender","smoking","cigarettesnr",
                            "education","occupation", "hours_parttime", "hours_fulltime","medication_none","medication_pain",
                            "medication_opioid","medication_depr","medication_MmRelax","medication_Cann","medication_other",
                            "frequency_med","care_none","care_GP","care_Specialist","care_PT","care_Chiro","care_Psych","care_Mass","care_other",
                            "nr_GP_treatment","imaging_none","imaging_Rö","imaging_MRI","imaging_other","nr_physio_treatment","nr_chiro_treatment",
                            "nr_psych_treatment","nr_massage_treatment","nr_other_treatment")

# Löschen der ersten Zeile
Demographics <- Demographics [-1, ]

# Umwandeln der Spalte age in numerische Werte
Demographics$age <- as.numeric(Demographics$age)

# Umwandeln der Spalte gender in numerische Werte
Demographics$gender <- as.numeric(Demographics$gender)



# Umwandeln der Spalte smoking in numerische Werte
Demographics$smoking <- as.numeric(Demographics$smoking)

# Zählen der Werte in der Spalte gender
smoking_counts <- table(Demographics$smoking, useNA = "no")

# Berechnung der Prozentsätze
smoking_percentages <- round(smoking_counts / sum(smoking_counts) * 100, 2)

# Erstellen eines DataFrames mit den Ergebnissen
smoking_df <- data.frame(
  smoking = c("no", "occasionally", "smoker"),
  count = as.vector(smoking_counts),
  percentage = as.vector(smoking_percentages)
)

# Ausgabe der Ergebnisse
print(smoking_df)

# Berechnung des Mittelwerts und SD der Raucher
smoking_mean <- mean(Demographics$smoking, na.rm = TRUE)
smoking_sd <- sd(Demographics$smoking, na.rm = TRUE)

# Ausgabe der Ergebnisse
cat("Raucher:\n")
cat("Mittelwert: ", smoking_mean, "\n")
cat("Standardabweichung: ", smoking_sd, "\n\n")



# Umwandeln der Spalte education in numerische Werte
Demographics$education <- as.numeric(Demographics$education)

# Zählen der Werte in der Spalte gender
education_counts <- table(Demographics$education, useNA = "no")

# Berechnung der Prozentsätze
education_percentages <- round(education_counts / sum(education_counts) * 100, 2)

# Erstellen eines DataFrames mit den Ergebnissen
education_df <- data.frame(
  education = c("apprenticeship", "uni", "other"),
  count = as.vector(education_counts),
  percentage = as.vector(education_percentages)
)

# Ausgabe der Ergebnisse
print(education_df)

# Berechnung des Mittelwerts und SD der Raucher
education_mean <- mean(Demographics$education, na.rm = TRUE)
education_sd <- sd(Demographics$education, na.rm = TRUE)

# Ausgabe der Ergebnisse
cat("Ausbildung:\n")
cat("Mittelwert: ", education_mean, "\n")
cat("Standardabweichung: ", education_sd, "\n\n")



# Umwandeln der Spalte occupation in numerische Werte
Demographics$occupation <- as.numeric(Demographics$occupation)

# Zählen der Werte in der Spalte gender
occupation_counts <- table(Demographics$occupation, useNA = "no")

# Berechnung der Prozentsätze
occupation_percentages <- round(occupation_counts / sum(occupation_counts) * 100, 2)

# Erstellen eines DataFrames mit den Ergebnissen
occupation_df <- data.frame(
  occupation = c("part-time", "full-time", "unemployed", "uni"),
  count = as.vector(occupation_counts),
  percentage = as.vector(occupation_percentages)
)

# Ausgabe der Ergebnisse
print(occupation_df)

# Berechnung des Mittelwerts und SD der Raucher
occupation_mean <- mean(Demographics$occupation, na.rm = TRUE)
occupation_sd <- sd(Demographics$occupation, na.rm = TRUE)

# Ausgabe der Ergebnisse
cat("Beschäftigung:\n")
cat("Mittelwert: ", occupation_mean, "\n")
cat("Standardabweichung: ", occupation_sd, "\n\n")




# Umwandeln der Spalten medication in numerische Werte
Demographics$medication_none <- as.numeric(Demographics$medication_none)
Demographics$medication_pain <- as.numeric(Demographics$medication_pain)
Demographics$medication_opioid <- as.numeric(Demographics$medication_opioid)
Demographics$medication_depr <- as.numeric(Demographics$medication_depr)
Demographics$medication_MmRelax <- as.numeric(Demographics$medication_MmRelax)
Demographics$medication_Cann <- as.numeric(Demographics$medication_Cann)


# Zählen der Werte in der Spalten Medication
medication_none_counts <- table(Demographics$medication_none, useNA = "no")
medication_pain_counts <- table(Demographics$medication_pain, useNA = "no")
medication_opioid_counts <- table(Demographics$medication_opioid, useNA = "no")
medication_depr_counts <- table(Demographics$medication_depr, useNA = "no")
medication_MmRelax_counts <- table(Demographics$medication_MmRelax, useNA = "no")
medication_Cann_counts <- table(Demographics$medication_Cann, useNA = "no")

# Berechnung der Prozentsätze
medication_none_percentages <- round(medication_none_counts / sum(medication_none_counts) * 100, 2)
medication_pain_percentages <- round(medication_pain_counts / sum(medication_pain_counts) * 100, 2)
medication_opioid_percentages <- round(medication_opioid_counts / sum(medication_opioid_counts) * 100, 2)
medication_depr_percentages <- round(medication_depr_counts / sum(medication_depr_counts) * 100, 2)
medication_MmRelax_percentages <- round(medication_MmRelax_counts / sum(medication_MmRelax_counts) * 100, 2)
medication_Cann_percentages <- round(medication_Cann_counts / sum(medication_Cann_counts) * 100, 2)

# Erstellen eines DataFrames mit den Ergebnissen
medication_none_df <- data.frame(
  medication_none = c("Yes", "No"),
  count = as.vector(medication_none_counts),
  percentage = as.vector(medication_none_percentages))

medication_pain_df <- data.frame(
  medication_pain = c("No", "Yes"),
  count = as.vector(medication_pain_counts),
  percentage = as.vector(medication_pain_percentages))

medication_opioid_df <- data.frame(
  medication_opioid = c("No", "Yes"),
  count = as.vector(medication_opioid_counts),
  percentage = as.vector(medication_opioid_percentages))

medication_depr_df <- data.frame(
  medication_depr = c("No", "Yes"),
  count = as.vector(medication_depr_counts),
  percentage = as.vector(medication_depr_percentages))

medication_MmRelax_df <- data.frame(
  medication_MmRelax = c("No", "Yes"),
  count = as.vector(medication_MmRelax_counts),
  percentage = as.vector(medication_MmRelax_percentages))

medication_Cann_df <- data.frame(
  medication_Cann = c("No", "Yes"),
  count = as.vector(medication_Cann_counts),
  percentage = as.vector(medication_Cann_percentages))

# Ausgabe der Ergebnisse
print(medication_none_df)
print(medication_pain_df)
print(medication_opioid_df)
print(medication_depr_df)
print(medication_MmRelax_df)
print(medication_Cann_df)

# Berechnung des Mittelwerts und SD der Medikamenteneinnahme
medication_none_mean <- mean(Demographics$medication_none, na.rm = TRUE)
medication_none_sd <- sd(Demographics$medication_none, na.rm = TRUE)
medication_pain_mean <- mean(Demographics$medication_pain, na.rm = TRUE)
medication_pain_sd <- sd(Demographics$medication_pain, na.rm = TRUE)
medication_opioid_mean <- mean(Demographics$medication_opioid, na.rm = TRUE)
medication_opioid_sd <- sd(Demographics$medication_opioid, na.rm = TRUE)
medication_depr_mean <- mean(Demographics$medication_depr, na.rm = TRUE)
medication_depr_sd <- sd(Demographics$medication_depr, na.rm = TRUE)
medication_MmRelax_mean <- mean(Demographics$medication_MmRelax, na.rm = TRUE)
medication_MmRelax_sd <- sd(Demographics$medication_MmRelax, na.rm = TRUE)
medication_Cann_mean <- mean(Demographics$medication_Cann, na.rm = TRUE)
medication_Cann_sd <- sd(Demographics$medication_Cann, na.rm = TRUE)

# Ausgabe der Ergebnisse
cat("keine Medikamente:\n")
cat("Mittelwert: ", medication_none_mean, "\n")
cat("Standardabweichung: ", medication_none_sd, "\n\n")

cat("Schmerzmedis:\n")
cat("Mittelwert: ", medication_pain_mean, "\n")
cat("Standardabweichung: ", medication_pain_sd, "\n\n")

cat("Opioide:\n")
cat("Mittelwert: ", medication_opioid_mean, "\n")
cat("Standardabweichung: ", medication_opioid_sd, "\n\n")

cat("Antidepressiva:\n")
cat("Mittelwert: ", medication_depr_mean, "\n")
cat("Standardabweichung: ", medication_depr_sd, "\n\n")

cat("Muskelrelaxanz:\n")
cat("Mittelwert: ", medication_MmRelax_mean, "\n")
cat("Standardabweichung: ", medication_MmRelax_sd, "\n\n")

cat("Cannabis:\n")
cat("Mittelwert: ", medication_Cann_mean, "\n")
cat("Standardabweichung: ", medication_Cann_sd, "\n\n")





# Umwandeln der Spalten Care in numerische Werte
Demographics$care_none <- as.numeric(Demographics$care_none)
Demographics$care_GP <- as.numeric(Demographics$care_GP)
Demographics$care_Specialist <- as.numeric(Demographics$care_Specialist)
Demographics$care_PT <- as.numeric(Demographics$care_PT)
Demographics$care_Chiro <- as.numeric(Demographics$care_Chiro)
Demographics$care_Psych <- as.numeric(Demographics$care_Psych)
Demographics$care_Mass <- as.numeric(Demographics$care_Mass)

# Zählen der Werte in der Spalten Care
care_none_counts <- table(Demographics$care_none, useNA = "no")
care_GP_counts <- table(Demographics$care_GP, useNA = "no")
care_Specialist_counts <- table(Demographics$care_Specialist, useNA = "no")
care_PT_counts <- table(Demographics$care_PT, useNA = "no")
care_Chiro_counts <- table(Demographics$care_Chiro, useNA = "no")
care_Psych_counts <- table(Demographics$care_Psych, useNA = "no")
care_Mass_counts <- table(Demographics$care_Mass, useNA = "no")

# Berechnung der Prozentsätze
care_none_percentages <- round(care_none_counts / sum(care_none_counts) * 100, 2)
care_GP_percentages <- round(care_GP_counts / sum(care_GP_counts) * 100, 2)
care_Specialist_percentages <- round(care_Specialist_counts / sum(care_Specialist_counts) * 100, 2)
care_PT_percentages <- round(care_PT_counts / sum(care_PT_counts) * 100, 2)
care_Chiro_percentages <- round(care_Chiro_counts / sum(care_Chiro_counts) * 100, 2)
care_Psych_percentages <- round(care_Psych_counts / sum(care_Psych_counts) * 100, 2)
care_Mass_percentages <- round(care_Mass_counts / sum(care_Mass_counts) * 100, 2)

# Erstellen eines DataFrames mit den Ergebnissen
care_none_df <- data.frame(
  care_none = c("Yes", "No"),
  count = as.vector(care_none_counts),
  percentage = as.vector(care_none_percentages))

care_GP_df <- data.frame(
  care_GP = c("No", "Yes"),
  count = as.vector(care_GP_counts),
  percentage = as.vector(care_GP_percentages))

care_Specialist_df <- data.frame(
  care_Specialist = c("No", "Yes"),
  count = as.vector(care_Specialist_counts),
  percentage = as.vector(care_Specialist_percentages))

care_PT_df <- data.frame(
  care_PT = c("No", "Yes"),
  count = as.vector(care_PT_counts),
  percentage = as.vector(care_PT_percentages))

care_Chiro_df <- data.frame(
  care_Chiro = c("No", "Yes"),
  count = as.vector(care_Chiro_counts),
  percentage = as.vector(care_Chiro_percentages))

care_Psych_df <- data.frame(
  care_Psych = c("No", "Yes"),
  count = as.vector(care_Psych_counts),
  percentage = as.vector(care_Psych_percentages))

care_Mass_df <- data.frame(
  care_Mass = c("No", "Yes"),
  count = as.vector(care_Mass_counts),
  percentage = as.vector(care_Mass_percentages))

# Ausgabe der Ergebnisse
print(care_none_df)
print(care_GP_df)
print(care_Specialist_df)
print(care_PT_df)
print(care_Chiro_df)
print(care_Psych_df)
print(care_Mass_df)

# Berechnung des Mittelwerts und SD der Care
care_none_mean <- mean(Demographics$care_none, na.rm = TRUE)
care_none_sd <- sd(Demographics$care_none, na.rm = TRUE)
care_GP_mean <- mean(Demographics$care_GP, na.rm = TRUE)
care_GP_sd <- sd(Demographics$care_GP, na.rm = TRUE)
care_Specialist_mean <- mean(Demographics$care_Specialist, na.rm = TRUE)
care_Specialist_sd <- sd(Demographics$care_Specialist, na.rm = TRUE)
care_PT_mean <- mean(Demographics$care_PT, na.rm = TRUE)
care_PT_sd <- sd(Demographics$care_PT, na.rm = TRUE)
care_Chiro_mean <- mean(Demographics$care_Chiro, na.rm = TRUE)
care_Chiro_sd <- sd(Demographics$care_Chiro, na.rm = TRUE)
care_Psych_mean <- mean(Demographics$care_Psych, na.rm = TRUE)
care_Psych_sd <- sd(Demographics$care_Psych, na.rm = TRUE)
care_Mass_mean <- mean(Demographics$care_Mass, na.rm = TRUE)
care_Mass_sd <- sd(Demographics$care_Mass, na.rm = TRUE)

# Ausgabe der Ergebnisse
cat("keine Care:\n")
cat("Mittelwert: ", care_none_mean, "\n")
cat("Standardabweichung: ", care_none_sd, "\n\n")

cat("GP:\n")
cat("Mittelwert: ", care_GP_mean, "\n")
cat("Standardabweichung: ", care_GP_sd, "\n\n")

cat("Spezialist:\n")
cat("Mittelwert: ", care_Specialist_mean, "\n")
cat("Standardabweichung: ", care_Specialist_sd, "\n\n")

cat("PT:\n")
cat("Mittelwert: ", care_PT_mean, "\n")
cat("Standardabweichung: ", care_PT_sd, "\n\n")

cat("Chiro:\n")
cat("Mittelwert: ", care_Chiro_mean, "\n")
cat("Standardabweichung: ", care_Chiro_sd, "\n\n")

cat("Psychologe:\n")
cat("Mittelwert: ", care_Psych_mean, "\n")
cat("Standardabweichung: ", care_Psych_sd, "\n\n")

cat("Massage:\n")
cat("Mittelwert: ", care_Mass_mean, "\n")
cat("Standardabweichung: ", care_Mass_sd, "\n\n")




#Neue Berechnungen vom 26.06.2024

# Entfernen der Spalte V2-V4 (brauche ich nicht) 
Demographics [2:4] <- list(NULL)

# Manuelles Zuweisen der Spaltennamen, falls bekannt
colnames(Demographics) <- c("study_id", "age", "gender","smoking","cigarettesnr",
                            "education","occupation", "hours_parttime", "hours_fulltime","medication_none","medication_pain",
                            "medication_opioid","medication_depr","medication_MmRelax","medication_Cann","medication_other",
                            "frequency_med","care_none","care_GP","care_Specialist","care_PT","care_Chiro","care_Psych","care_Mass","care_other",
                            "nr_GP_treatment","imaging_none","imaging_Rö","imaging_MRI","imaging_other","nr_physio_treatment","nr_chiro_treatment",
                            "nr_psych_treatment","nr_massage_treatment","nr_other_treatment")

# Löschen der ersten Zeile
Demographics <- Demographics [-1, ]


# Umwandeln der Spalte imaging_none in numerische Werte
Demographics$imaging_none <- as.numeric(Demographics$imaging_none)

# Zählen der Werte in der Spalte imaging_none
imaging_counts <- table(Demographics$imaging_none, useNA = "no")

# Berechnung der Prozentsätze
imaging_percentages <- round(imaging_counts / sum(imaging_counts) * 100, 2)

# Erstellen eines DataFrames mit den Ergebnissen
imaging_df <- data.frame(
  imaging = c("no", "yes"),
  count = as.vector(imaging_counts),
  percentage = as.vector(imaging_percentages)
)

# Ausgabe der Ergebnisse
print(imaging_df)

# Berechnung des Mittelwerts und SD des imaging
imaging_mean <- mean(Demographics$imaging_none, na.rm = TRUE)
imaging_sd <- sd(Demographics$imaging_none, na.rm = TRUE)

# Ausgabe der Ergebnisse
cat("Imaging:\n")
cat("Mittelwert: ", imaging_mean, "\n")
cat("Standardabweichung: ", imaging_sd, "\n\n")




#Neue Berechnungen vom 03.09.2024 für Baseline Characteristics Tabelle (AE.FS und HCC fehlten)

# Entfernen der Spalte V2-V4 (brauche ich nicht) 
AE.FS [2:4] <- list(NULL)

#FAR Gruppe bilden mit den if_else Vorgaben
AE.FS$FAR <- ifelse(AE.FS$score_pps < 3 & AE.FS$dms_score == 2, TRUE, FALSE)

#DER Gruppe bilden mit den if_else Vorgaben
AE.FS$DER <- ifelse(AE.FS$score_pps >= 3 & AE.FS$dms_score == 2, TRUE, FALSE)

#EER Gruppe bilden mit den if_else Vorgaben
AE.FS$EER <- ifelse(AE.FS$score_pps >= 3 & AE.FS$dms_score < 2, TRUE, FALSE)

#AR Gruppe bilden mit den if_else Vorgaben
AE.FS$AR <- ifelse(AE.FS$score_pps < 3 & AE.FS$dms_score < 2, TRUE, FALSE)

#neue Spalten mit den Summen bilden
AE.FS$Anzahl_FAR <- sum(AE.FS$FAR)
AE.FS$Anzahl_DER <- sum(AE.FS$DER)
AE.FS$Anzahl_EER <- sum(AE.FS$EER)
AE.FS$Anzahl_AR <- sum(AE.FS$AR)

# Beispiel-Daten
groups <- c("FAR", "DER", "EER", "AR")
summen <- c(AE.FS$Anzahl_FAR, AE.FS$Anzahl_DER, AE.FS$Anzahl_EER, AE.FS$Anzahl_AR)
summen_Zahlen <- c(7, 24, 64, 28)


# Umwandeln der Spalten in numerische Werte
AE.FS$score_pps <- as.numeric(AE.FS$score_pps)
AE.FS$dms_score <- as.numeric(AE.FS$dms_score)

# Zählen der Werte in der Spalte imaging_none
pps_counts <- table(AE.FS$score_pps, useNA = "no")
dms_counts <- table(AE.FS$dms_score, useNA = "no")

# Berechnung der Prozentsätze
pps_percentages <- round(pps_counts / sum(pps_counts) * 100, 2)
dms_percentages <- round(dms_counts / sum(dms_counts) * 100, 2)

# Erstellen eines DataFrames mit den Ergebnissen
pps_df <- data.frame(
  count = as.vector(pps_counts),
  percentage = as.vector(pps_percentages)
)
dms_df <- data.frame(
  count = as.vector(dms_counts),
  percentage = as.vector(dms_percentages)
)

# Ausgabe der Ergebnisse
print(pps_df)
print(dms_df)


# Berechnung des Mittelwerts und SD
AE.FS_mean_pps <- mean(AE.FS$score_pps, na.rm = TRUE)
AE.FS_mean_dms <- mean(AE.FS$dms_score, na.rm = TRUE)
AE.FS_sd_pps <- sd(AE.FS$score_pps, na.rm = TRUE)
AE.FS_sd_dms <- sd(AE.FS$dms_score, na.rm = TRUE)




#HCC Mittelwerte und SD

# Entfernen der Spalte V2-V4 (brauche ich nicht) 
HCC [2:4] <- list(NULL)

# Manuelles Zuweisen der Spaltennamen, falls bekannt
colnames(HCC) <- c("study_id","cortisol1","cortisol2","cortisone1","cortisone2","hairmass1","hairmass2","age","gender")

# Löschen der ersten Zeile
HCC <- HCC [-1, ]

# Spalten definieren, in denen leere Stellen mit NA ersetzt werden sollen
columns_to_fill <- c("cortisol1", "cortisol2", "cortisone1", "cortisone2", "hairmass1", "hairmass2")

# Leere Stellen (repräsentiert als leere Strings oder explizite NAs) in diesen Spalten mit NA ersetzen
HCC[columns_to_fill] <- lapply(HCC[columns_to_fill], function(x) {
  x[x == "" | is.na(x)] <- NA
  return(x)
})

# Umwandeln der Spalten in numerische Werte
HCC$cortisol1 <- as.numeric(HCC$cortisol1)
HCC$cortisol2 <- as.numeric(HCC$cortisol2)

library(dplyr)

# Umwandeln der Spalten in numerische Werte
HCC$cortisol1 <- as.numeric(HCC$cortisol1)
HCC$cortisol2 <- as.numeric(HCC$cortisol2)

# Stelle sicher, dass die benötigten Pakete installiert und geladen sind
if (!require(dplyr)) {
  install.packages("dplyr")
}
if (!require(openxlsx)) {
  install.packages("openxlsx")
}

library(dplyr)
library(openxlsx)

# Gruppiere Daten nach Geschlecht und führe die Berechnungen durch
gender_stats <- HCC %>%
  group_by(gender) %>%
  summarise(
    # Zählen der Werte und Berechnen der Prozentsätze für Cortisol1
    cortisol1_count = n(),
    cortisol1_mean = mean(cortisol1, na.rm = TRUE),
    cortisol1_sd = sd(cortisol1, na.rm = TRUE),
    cortisol1_median = median(cortisol1, na.rm = TRUE),
    
    # Zählen der Werte und Berechnen der Prozentsätze für Cortisol2
    cortisol2_count = n(),
    cortisol2_mean = mean(cortisol2, na.rm = TRUE),
    cortisol2_sd = sd(cortisol2, na.rm = TRUE),
    cortisol2_median = median(cortisol2, na.rm = TRUE)
  )

# Ausgabe der gruppierten Statistiken
print(gender_stats)

# Spezifiziere den vollständigen Pfad
file_path <- "U:/PhD/LongNeck/Statistik/R Berechnungen/Akutdaten/Tables and Figures/HCC_Gender_Specific_Statistics.xlsx"

# Speichere die Workbook-Datei an diesem Pfad
saveWorkbook(wb, file_path, overwrite = TRUE)
print(paste("Die Daten wurden gespeichert unter:", file_path))

