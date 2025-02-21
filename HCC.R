#Akutdaten HCC#
#05.08.2024#


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

# Sicherstellen, dass die Werte numerisch sind
HCC$cortisol1 <- as.numeric(HCC$cortisol1)
HCC$cortisol2 <- as.numeric(HCC$cortisol2)

library(dplyr)

# Ausreißer für Cortisol1 und Cortisol2 basierend auf dem IQR entfernen
clean_HCC <- HCC %>%
  mutate(cortisol1 = as.numeric(cortisol1), cortisol2 = as.numeric(cortisol2)) %>%
  group_by(gender) %>%
  mutate(
    Q1_cortisol1 = quantile(cortisol1, 0.25, na.rm = TRUE),
    Q3_cortisol1 = quantile(cortisol1, 0.75, na.rm = TRUE),
    IQR_cortisol1 = Q3_cortisol1 - Q1_cortisol1,
    Q1_cortisol2 = quantile(cortisol2, 0.25, na.rm = TRUE),
    Q3_cortisol2 = quantile(cortisol2, 0.75, na.rm = TRUE),
    IQR_cortisol2 = Q3_cortisol2 - Q1_cortisol2
  ) %>%
  filter(
    cortisol1 > (Q1_cortisol1 - 1.5 * IQR_cortisol1) & cortisol1 < (Q3_cortisol1 + 1.5 * IQR_cortisol1),
    cortisol2 > (Q1_cortisol2 - 1.5 * IQR_cortisol2) & cortisol2 < (Q3_cortisol2 + 1.5 * IQR_cortisol2)
  ) %>%
  select(-starts_with("Q"), -starts_with("IQR"))

# Sicherstellen, dass alle anderen notwendigen Pakete geladen sind
if (!require(openxlsx)) {
  install.packages("openxlsx")
  library(openxlsx)
}
# Statistiken nach Geschlecht berechnen und exportieren
gender_stats <- clean_HCC %>%
  group_by(gender) %>%
  summarise(
    mean_cortisol1 = mean(cortisol1, na.rm = TRUE),
    sd_cortisol1 = sd(cortisol1, na.rm = TRUE),
    range_low_cortisol1 = min(cortisol1, na.rm = TRUE),
    range_high_cortisol1 = max(cortisol1, na.rm = TRUE),
    mean_cortisol2 = mean(cortisol2, na.rm = TRUE),
    sd_cortisol2 = sd(cortisol2, na.rm = TRUE),
    range_low_cortisol2 = min(cortisol2, na.rm = TRUE),
    range_high_cortisol2 = max(cortisol2, na.rm = TRUE)
  ) %>%
  mutate(
    ci_width_cortisol1 = qnorm(0.975) * (sd_cortisol1 / sqrt(n())),
    lower_ci_cortisol1 = mean_cortisol1 - ci_width_cortisol1,
    upper_ci_cortisol1 = mean_cortisol1 + ci_width_cortisol1,
    ci_width_cortisol2 = qnorm(0.975) * (sd_cortisol2 / sqrt(n())),
    lower_ci_cortisol2 = mean_cortisol2 - ci_width_cortisol2,
    upper_ci_cortisol2 = mean_cortisol2 + ci_width_cortisol2
  )

# Excel-Datei erstellen und speichern
wb <- createWorkbook()
addWorksheet(wb, "Cleaned Cortisol by Gender")
writeData(wb, "Cleaned Cortisol by Gender", gender_stats, startRow = 1, colNames = TRUE)

# Spezifizieren des Pfades für die Excel-Datei
file_path <- "U:/PhD/LongNeck/Statistik/R Berechnungen/Akutdaten/Tables and Figures/Cleaned_Cortisol_by_Gender.xlsx"
saveWorkbook(wb, file_path, overwrite = TRUE)

# Bestätigungsnachricht
cat("Die bereinigte Excel-Datei wurde erfolgreich unter dem Pfad gespeichert:", file_path)



# Neue Spalten für jede Altersgruppe erstellen
HCC$Age_Group_11_20 <- ifelse(HCC$age >= 11 & HCC$age <= 20, 1, 0)
HCC$Age_Group_21_30 <- ifelse(HCC$age >= 21 & HCC$age <= 30, 1, 0)
HCC$Age_Group_31_40 <- ifelse(HCC$age >= 31 & HCC$age <= 40, 1, 0)
HCC$Age_Group_41_50 <- ifelse(HCC$age >= 41 & HCC$age <= 50, 1, 0)
HCC$Age_Group_51_60 <- ifelse(HCC$age >= 51 & HCC$age <= 60, 1, 0)
HCC$Age_Group_61_70 <- ifelse(HCC$age >= 61 & HCC$age <= 70, 1, 0)

# Überprüfung der erstellten Spalten
head(HCC[c("age", "Age_Group_11_20", "Age_Group_21_30", "Age_Group_31_40", "Age_Group_41_50", "Age_Group_51_60", "Age_Group_61_70")])


# Erstellen des DataFrames für die Referenzwerte der Frauen
reference_values <- data.frame(
  Age_Group = c("4-10", "11-20", "21-30", "31-40", "41-50", "51-60", "61-70", "71-80", ">80"),
  P5 = c(0.49, 0.74, 1.64, 1.60, 1.42, 1.36, 1.32, 1.38, 1.50),
  P50 = c(2.31, 3.62, 4.95, 4.63, 4.93, 4.76, 4.64, 4.72, 4.92),
  P95 = c(16.13, 13.49, 19.46, 15.91, 21.65, 23.09, 29.71, 34.45, 28.69)
)

# Referenzwerte für Männer als DataFrame
reference_values_male <- data.frame(
  Age_Group = c("4-10", "11-20", "21-30", "31-40", "41-50", "51-60", "61-70", "71-80", ">80"),
  P5 = c(0.68, 0.97, 2.58, 2.23, 2.25, 1.78, 1.43, 1.66, 1.78),
  P50 = c(2.96, 3.57, 4.77, 5.53, 5.71, 5.62, 5.00, 5.74, 5.27),
  P95 = c(16.25, 14.66, 19.43, 19.48, 17.76, 29.83, 27.88, 32.04, 25.16)
)

# Daten für weibliche Teilnehmer filtern (Frauen sind als '2' definiert)
female_data <- HCC[HCC$gender == 2,]

# Daten für männliche Teilnehmer filtern (Männer sind als '1' definiert)
male_data <- HCC[HCC$gender == 1,]

# Referenzwerte kombinieren für einfachere Nutzung
all_reference_values <- rbind(
  cbind(Sex = "Female", reference_values),
  cbind(Sex = "Male", reference_values_male)
)

# Daten nach Geschlecht und Altersgruppe gruppieren und Durchschnitte berechnen
library(dplyr)


# Stelle sicher, dass die 'age' Spalte numerisch ist
HCC$age <- as.numeric(HCC$age)

# Überprüfe, ob die Umwandlung erfolgreich war
str(HCC$age)

# Sicherstellen, dass die Cortisol-Spalten numerisch sind
HCC$cortisol1 <- as.numeric(HCC$cortisol1)
HCC$cortisol2 <- as.numeric(HCC$cortisol2)
HCC$cortisone1 <- as.numeric(HCC$cortisone1)
HCC$cortisone2 <- as.numeric(HCC$cortisone2)

# Überprüfung der Umwandlung und Fehlerbehandlung
HCC <- HCC %>%
  mutate(
    cortisol1 = ifelse(is.na(as.numeric(cortisol1)), NA_real_, as.numeric(cortisol1)),
    cortisol2 = ifelse(is.na(as.numeric(cortisol2)), NA_real_, as.numeric(cortisol2)),
    cortisone1 = ifelse(is.na(as.numeric(cortisone1)), NA_real_, as.numeric(cortisone1)),
    cortisone2 = ifelse(is.na(as.numeric(cortisone2)), NA_real_, as.numeric(cortisone2))
  )


library(dplyr)
library(openxlsx)


# Daten vorbereiten
HCC <- HCC %>%
  mutate(
    age = as.numeric(age),
    cortisol1 = as.numeric(cortisol1),
    cortisol2 = as.numeric(cortisol2),
    cortisone1 = as.numeric(cortisone1),
    cortisone2 = as.numeric(cortisone2),
    Sex = ifelse(gender == 1, "Male", "Female"),
    Age_Group = cut(age, breaks = c(4, 10, 20, 30, 40, 50, 60, 70, 80, Inf), 
                    labels = c("4-10", "11-20", "21-30", "31-40", "41-50", "51-60", "61-70", "71-80", ">80"),
                    right = FALSE)
  ) %>%
  group_by(Sex, Age_Group) %>%
  mutate(
    mean_cortisol1 = mean(cortisol1, na.rm = TRUE),
    sd_cortisol1 = sd(cortisol1, na.rm = TRUE),
    mean_cortisol2 = mean(cortisol2, na.rm = TRUE),
    sd_cortisol2 = sd(cortisol2, na.rm = TRUE)
  ) %>%
  filter(
    (cortisol1 > (mean_cortisol1 - 2 * sd_cortisol1)) & (cortisol1 < (mean_cortisol1 + 2 * sd_cortisol1)),
    (cortisol2 > (mean_cortisol2 - 2 * sd_cortisol2)) & (cortisol2 < (mean_cortisol2 + 2 * sd_cortisol2))
  ) %>%
  summarise(
    Average_Cortisol1 = mean(cortisol1, na.rm = TRUE),
    Average_Cortisol2 = mean(cortisol2, na.rm = TRUE),
    .groups = 'drop'
  )

# Excel-Datei erstellen und speichern
wb <- createWorkbook()
addWorksheet(wb, "Adjusted Hormone Averages")
writeData(wb, "Adjusted Hormone Averages", HCC)

# Spezifizieren des Pfades für die Excel-Datei
file_path <- "U:/PhD/LongNeck/Statistik/R Berechnungen/Akutdaten/Tables and Figures/Adjusted_Hormone_Averages.xlsx"
saveWorkbook(wb, file_path, overwrite = TRUE)

# Bestätigungsnachricht
print(paste("Die Excel-Datei wurde erfolgreich gespeichert unter:", file_path))






















# Fortsetzung der Berechnung der Durchschnittswerte von den Cortisol1 Werten
average_cortisol <- HCC %>%
  mutate(Age_Group = cut(age, 
                         breaks = c(4, 10, 20, 30, 40, 50, 60, 70, 80, Inf), 
                         labels = c("4-10", "11-20", "21-30", "31-40", "41-50", "51-60", "61-70", "71-80", ">80"),
                         right = FALSE),
         Sex = ifelse(gender == 1, "Male", "Female")) %>%
  group_by(Sex, Age_Group) %>%
  summarise(Average_Cortisol1 = mean(cortisol1, na.rm = TRUE), .groups = 'drop') %>%
  left_join(all_reference_values, by = c("Sex", "Age_Group"))

# Anzeigen der Ergebnistabelle
print(average_cortisol)

# Fortsetzung der Berechnung der Durchschnittswerte und Zählung der Probanden
average_cortisol_count <- HCC %>%
  mutate(Age_Group = cut(age, 
                         breaks = c(4, 10, 20, 30, 40, 50, 60, 70, 80, Inf), 
                         labels = c("4-10", "11-20", "21-30", "31-40", "41-50", "51-60", "61-70", "71-80", ">80"),
                         right = FALSE),
         Sex = ifelse(gender == 1, "Male", "Female")) %>%
  group_by(Sex, Age_Group) %>%
  summarise(
    Average_Cortisol1 = mean(cortisol1, na.rm = TRUE),
    Count = n(),  # Zählt die Anzahl der Beobachtungen in jeder Gruppe
    .groups = 'drop'
  ) %>%
  left_join(all_reference_values, by = c("Sex", "Age_Group"))

# Anzeigen der erweiterten Ergebnistabelle
print(average_cortisol_count)

# Stelle sicher, dass das Paket openxlsx installiert ist
if (!require("openxlsx")) {
  install.packages("openxlsx")
  library(openxlsx)
} else {
  library(openxlsx)
}

# Erstellen einer neuen Excel-Datei
wb <- createWorkbook()

# Füge ein Arbeitsblatt hinzu
addWorksheet(wb, "Cortisol Results")

# Schreibe die Daten in das Arbeitsblatt
writeData(wb, sheet = "Cortisol Results", average_cortisol_count)

# Speichere die Workbook
saveWorkbook(wb, "Average_Cortisol_Results.xlsx", overwrite = TRUE)

# Ausgabe des Speicherpfads
print("Die Daten wurden gespeichert unter: Average_Cortisol_Results.xlsx")


# Fortsetzung der Berechnung der Durchschnittswerte von den Cortisol2 Werten

# Berechne Durchschnitte für Cortisol2 basierend auf Geschlecht und Altersgruppe
average_cortisol2 <- HCC %>%
  mutate(Age_Group = cut(age, 
                         breaks = c(4, 10, 20, 30, 40, 50, 60, 70, 80, Inf), 
                         labels = c("4-10", "11-20", "21-30", "31-40", "41-50", "51-60", "61-70", "71-80", ">80"),
                         right = FALSE),
         Sex = ifelse(gender == 1, "Male", "Female")) %>%
  group_by(Sex, Age_Group) %>%
  summarise(Average_Cortisol2 = mean(cortisol2, na.rm = TRUE), Count = n(), .groups = 'drop') %>%
  left_join(all_reference_values, by = c("Sex", "Age_Group"))

# Überprüfung der Ergebnisse
print(average_cortisol2)

# Speichern der Ergebnisse in einer Excel-Datei
# Stelle sicher, dass das Paket openxlsx installiert ist
if (!require("openxlsx")) {
  install.packages("openxlsx")
  library(openxlsx)
} else {
  library(openxlsx)
}

# Erstellen einer neuen Excel-Datei oder Verwenden einer bestehenden
wb <- createWorkbook()

# Füge ein Arbeitsblatt hinzu
addWorksheet(wb, "Cortisol2 Results")

# Schreibe die Daten in das Arbeitsblatt
writeData(wb, sheet = "Cortisol2 Results", average_cortisol2)

# Speichere die Workbook
saveWorkbook(wb, "Average_Cortisol2_Results.xlsx", overwrite = TRUE)

# Ausgabe des Speicherpfads
print("Die Daten wurden gespeichert unter: Average_Cortisol2_Results.xlsx")



# Sicherstellen, dass die Cortison-Spalten numerisch sind
HCC$cortisone1 <- as.numeric(HCC$cortisone1)
HCC$cortisone2 <- as.numeric(HCC$cortisone2)


# Referenzwerte für Cortisone bei Frauen und Männern als DataFrames
reference_values_female <- data.frame(
  Age_Group = c("4-10", "11-20", "21-30", "31-40", "41-50", "51-60", "61-70", "71-80", ">80"),
  P5 = c(0.54, 1.24, 2.26, 3.26, 3.26, 2.02, 1.77, 2.09, 2.44),
  P50 = c(4.70, 8.48, 12.43, 11.95, 12.82, 8.02, 6.22, 6.39, 7.87),
  P95 = c(17.51, 28.38, 41.71, 35.40, 38.51, 29.24, 20.36, 18.75, 19.50),
  Sex = rep("Female", 9)
)

reference_values_male <- data.frame(
  Age_Group = c("4-10", "11-20", "21-30", "31-40", "41-50", "51-60", "61-70", "71-80", ">80"),
  P5 = c(1.53, 2.07, 5.52, 5.55, 6.91, 4.11, 3.29, 3.25, 3.81),
  P50 = c(7.17, 11.28, 13.54, 15.46, 14.31, 10.68, 8.00, 7.72, 8.12),
  P95 = c(25.50, 32.28, 43.28, 43.24, 44.44, 31.33, 24.14, 21.79, 19.04),
  Sex = rep("Male", 9)
)

# Kombiniere die Referenzwerte für beide Geschlechter
all_reference_values <- rbind(reference_values_female, reference_values_male)

# Analyse des Cortisone1 für beide Geschlechter
average_cortisone1 <- HCC %>%
  mutate(
    Age_Group = cut(age, breaks = c(4, 10, 20, 30, 40, 50, 60, 70, 80, Inf),
                    labels = c("4-10", "11-20", "21-30", "31-40", "41-50", "51-60", "61-70", "71-80", ">80"),
                    right = FALSE),
    Sex = ifelse(gender == 1, "Male", "Female")
  ) %>%
  group_by(Sex, Age_Group) %>%
  summarise(Average_Cortisone1 = mean(cortisone1, na.rm = TRUE), Count = n(), .groups = 'drop') %>%
  left_join(all_reference_values, by = c("Sex", "Age_Group"))


# Erstellen einer neuen Excel-Datei
wb <- createWorkbook()

# Füge ein Arbeitsblatt hinzu
addWorksheet(wb, "Cortisone1 Results")

# Schreibe die Daten in das Arbeitsblatt
writeData(wb, sheet = "Cortisone1 Results", average_cortisone1)

# Speichere die Workbook
saveWorkbook(wb, "Comprehensive_Cortisone1_Results.xlsx", overwrite = TRUE)

# Ausgabe des Speicherpfads
print("Die Daten wurden gespeichert unter: Comprehensive_Cortisone1_Results.xlsx")




# Analyse des Cortisone2 für beide Geschlechter
average_cortisone2 <- HCC %>%
  mutate(
    Age_Group = cut(age, breaks = c(4, 10, 20, 30, 40, 50, 60, 70, 80, Inf),
                    labels = c("4-10", "11-20", "21-30", "31-40", "41-50", "51-60", "61-70", "71-80", ">80"),
                    right = FALSE),
    Sex = ifelse(gender == 1, "Male", "Female")
  ) %>%
  group_by(Sex, Age_Group) %>%
  summarise(Average_Cortisone2 = mean(cortisone2, na.rm = TRUE), Count = n(), .groups = 'drop') %>%
  left_join(all_reference_values, by = c("Sex", "Age_Group"))


# Arbeitsblatt hinzufügen
addWorksheet(wb, "Cortisone2 Results")

# Daten in das Arbeitsblatt schreiben
writeData(wb, sheet = "Cortisone2 Results", average_cortisone2)

# Workbook speichern
saveWorkbook(wb, "Comprehensive_Cortisone2_Results.xlsx", overwrite = TRUE)

# Speicherpfad ausgeben
print("Die Daten wurden gespeichert unter: Comprehensive_Cortisone2_Results.xlsx")





#Schätzung der Standardabweichung: Ausgehend von P95 und P50.
#Berechnung der Z-Scores für jeden Wert: (Wert - P50) / geschätzte SD


# Erweitere den all_reference_values DataFrame um die geschätzte Standardabweichung
all_reference_values$Estimated_SD <- (all_reference_values$P95 - all_reference_values$P50) / 1.645

# Berechnung der Z-Scores für Cortisol und Cortisone
HCC <- HCC %>%
  mutate(
    Sex = ifelse(gender == 1, "Male", "Female"),
    Age_Group = cut(age, breaks = c(4, 10, 20, 30, 40, 50, 60, 70, 80, Inf),
                    labels = c("4-10", "11-20", "21-30", "31-40", "41-50", "51-60", "61-70", "71-80", ">80"),
                    right = FALSE)
  ) %>%
  left_join(all_reference_values, by = c("Sex", "Age_Group")) %>%
  mutate(
    Z_Score_Cortisol1 = (cortisol1 - P50) / Estimated_SD,
    Z_Score_Cortisol2 = (cortisol2 - P50) / Estimated_SD,
    Z_Score_Cortisone1 = (cortisone1 - P50) / Estimated_SD,
    Z_Score_Cortisone2 = (cortisone2 - P50) / Estimated_SD
  )

# Auswahl relevanter Spalten zur besseren Übersicht
result <- HCC %>%
  select(age, gender, cortisol1, cortisol2, cortisone1, cortisone2,
         Z_Score_Cortisol1, Z_Score_Cortisol2, Z_Score_Cortisone1, Z_Score_Cortisone2)

# Anzeigen der Ergebnisse
print(result)

# Erstelle eine neue Excel-Datei
wb <- createWorkbook()

# Füge ein Arbeitsblatt hinzu für die Ergebnisse
addWorksheet(wb, "Z-Score Results")

# Schreibe die Daten in das Arbeitsblatt
writeData(wb, sheet = "Z-Score Results", result)

# Speichere die Workbook
saveWorkbook(wb, "Z_Scores_Results.xlsx", overwrite = TRUE)

# Ausgabe des Speicherpfads
print("Die Daten wurden gespeichert unter: Z_Scores_Results.xlsx")



#Validierung der Ergebnisse
#Um die Plausibilität der Z-Scores zu überprüfen, könnten wir statistische Zusammenfassungen und Kontrollen durchführen, um zu sehen, ob es Ausreißer oder unerwartete Werte gibt. Dies könnte einfache deskriptive Statistiken wie Mittelwert, Median, Minima, Maxima und Standardabweichungen umfassen
# Deskriptive Statistiken der Z-Scores berechnen
summary_stats <- summary(result[, c("Z_Score_Cortisol1", "Z_Score_Cortisol2", "Z_Score_Cortisone1", "Z_Score_Cortisone2")])
print(summary_stats)



#Visualisierung
#Visualisierungen können helfen, die Verteilung der Z-Scores zu verstehen und zu visualisieren. Boxplots oder Histogramme sind nützliche Werkzeuge, um die Verteilung der Daten zu zeigen.

# Visualisierung der Z-Scores mit Boxplots
library(ggplot2)

ggplot(result, aes(x = factor(0), y = Z_Score_Cortisol1)) +
  geom_boxplot(fill = "blue", alpha = 0.5) +
  facet_wrap(~ gender, scales = "free") +
  labs(title = "Verteilung der Z-Scores für Cortisol1", y = "Z-Score", x = "") +
  theme_minimal()

# Ähnliche Plots können für Cortisol2, Cortisone1 und Cortisone2 erstellt werden

# Stellen Sie sicher, dass ggplot2 installiert ist
if (!require("ggplot2")) {
  install.packages("ggplot2")
  library(ggplot2)
}

# Plot für Z_Score_Cortisol1
ggplot(result, aes(x = factor(0), y = Z_Score_Cortisol1)) +
  geom_boxplot(fill = "blue", alpha = 0.5) +
  facet_wrap(~ gender, scales = "free") +
  labs(title = "Distribution of Z-Scores for Cortisol1",
       y = "Z-Score", x = "Cortisol1") +
  theme_minimal()

# Plot für Z_Score_Cortisol2
ggplot(result, aes(x = factor(0), y = Z_Score_Cortisol2)) +
  geom_boxplot(fill = "green", alpha = 0.5) +
  facet_wrap(~ gender, scales = "free") +
  labs(title = "Distribution of Z-Scores for Cortisol2",
       y = "Z-Score", x = "Cortisol2") +
  theme_minimal()

# Plot für Z_Score_Cortisone1
ggplot(result, aes(x = factor(0), y = Z_Score_Cortisone1)) +
  geom_boxplot(fill = "red", alpha = 0.5) +
  facet_wrap(~ gender, scales = "free") +
  labs(title = "Distribution of Z-Scores for Cortisone1",
       y = "Z-Score", x = "Cortisone1") +
  theme_minimal()

# Plot für Z_Score_Cortisone2
ggplot(result, aes(x = factor(0), y = Z_Score_Cortisone2)) +
  geom_boxplot(fill = "purple", alpha = 0.5) +
  facet_wrap(~ gender, scales = "free") +
  labs(title = "Distribution of Z-Scores for Cortisone2",
       y = "Z-Score", x = "Cortisone2") +
  theme_minimal()





# Deskriptive Statistiken der Z-Scores berechnen
summary_stats <- summary(result[, c("Z_Score_Cortisol1", "Z_Score_Cortisol2", "Z_Score_Cortisone1", "Z_Score_Cortisone2")])

# Um die Ausgabe von summary() für die Exportierung vorzubereiten
summary_stats_df <- data.frame(
  Statistic = c("Min", "1st Qu.", "Median", "Mean", "3rd Qu.", "Max"),
  Cortisol1 = as.numeric(summary_stats[1:6, "Z_Score_Cortisol1"]),
  Cortisol2 = as.numeric(summary_stats[1:6, "Z_Score_Cortisol2"]),
  Cortisone1 = as.numeric(summary_stats[1:6, "Z_Score_Cortisone1"]),
  Cortisone2 = as.numeric(summary_stats[1:6, "Z_Score_Cortisone2"])
)

# Stelle sicher, dass das Paket openxlsx installiert ist
if (!require("openxlsx")) {
  install.packages("openxlsx")
  library(openxlsx)
} else {
  library(openxlsx)
}

# Erstellen einer neuen Excel-Datei
wb <- createWorkbook()

# Füge ein Arbeitsblatt hinzu für die deskriptiven Statistiken
addWorksheet(wb, "Z-Score Statistics")

# Schreibe die Daten in das Arbeitsblatt
writeData(wb, sheet = "Z-Score Statistics", summary_stats_df)

# Speichere die Workbook
saveWorkbook(wb, "Z_Score_Statistics.xlsx", overwrite = TRUE)

# Ausgabe des Speicherpfads
print("Die deskriptiven Statistiken wurden gespeichert unter: Z_Score_Statistics.xlsx")





#Statistische Analysen
#Um zu prüfen, ob signifikante Unterschiede zwischen Gruppen bestehen, können wir ANOVA oder t-Tests verwenden, wenn die Daten normal verteilt sind. Nicht-parametrische Tests wie Wilcoxon oder Kruskal-Wallis können verwendet werden, wenn die Normalitätsannahmen nicht erfüllt sind.
# ANOVA für Cortisol1 Z-Scores zwischen Geschlechtern

# Installieren und Laden des benötigten Pakets
if (!require("openxlsx")) {
  install.packages("openxlsx")
  library(openxlsx)
}

# Daten vorbereiten
variables <- c("Z_Score_Cortisol1", "Z_Score_Cortisol2", "Z_Score_Cortisone1", "Z_Score_Cortisone2")
test_results <- list()

# Statistische Tests durchführen
for (var in variables) {
  anova_result <- aov(reformulate("gender", response = var), data = result)
  kruskal_result <- kruskal.test(reformulate("gender", response = var), data = result)
  
  # Ergebnisse in DataFrames umwandeln
  anova_df <- as.data.frame(summary(anova_result)[[1]])
  kruskal_df <- data.frame(
    Test = "Kruskal-Wallis",
    Chi_Squared = kruskal_result$statistic,
    p_Value = kruskal_result$p.value,
    Parameter = kruskal_result$parameter,
    stringsAsFactors = FALSE
  )
  
  # Ergebnisse in die Liste speichern
  test_results[[paste0("ANOVA_", var)]] <- anova_df
  test_results[[paste0("Kruskal_", var)]] <- kruskal_df
}

# Erstellen einer neuen Excel-Datei
wb <- createWorkbook()

# Ergebnisse in das Arbeitsblatt schreiben
for (name in names(test_results)) {
  addWorksheet(wb, name)
  writeData(wb, name, test_results[[name]])
}

# Speichern der Workbook
file_path <- "Statistical_Tests_Comprehensive_Results.xlsx"
saveWorkbook(wb, file_path, overwrite = TRUE)

# Ausgabe des Speicherpfads
print(paste("Die statistischen Testergebnisse wurden gespeichert unter:", file_path))









#HCC Berechnungen und Visualisierungen 09.09.24
# Visualisierung der Z-Scores mit Histogrammen
# Berechnen von Mittelwert, Standardabweichung, Bereich und Konfidenzintervall für cortisol1

# Hilfsfunktion zur Berechnung der Statistiken
calculate_stats <- function(data, variable_name) {
  mean_val <- mean(data[[variable_name]], na.rm = TRUE)
  sd_val <- sd(data[[variable_name]], na.rm = TRUE)
  range_val <- range(data[[variable_name]], na.rm = TRUE)
  
  # Anzahl der Beobachtungen
  n <- sum(!is.na(data[[variable_name]]))
  
  # Standardfehler
  se <- sd_val / sqrt(n)
  
  # 95% Konfidenzintervall
  ci <- qt(0.975, df = n - 1) * se
  
  # Ergebnisse zurückgeben
  list(mean = mean_val, sd = sd_val, range = range_val, ci = ci)
}

# Statistiken für Cortisol1 nach Geschlecht
male_stats_cortisol1 <- calculate_stats(HCC[HCC$gender == 1,], "cortisol1")
female_stats_cortisol1 <- calculate_stats(HCC[HCC$gender == 2,], "cortisol1")

# Statistiken für Cortisol2 nach Geschlecht
male_stats_cortisol2 <- calculate_stats(HCC[HCC$gender == 1,], "cortisol2")
female_stats_cortisol2 <- calculate_stats(HCC[HCC$gender == 2,], "cortisol2")

# Ergebnisse ausgeben
cat("Cortisol1 - Male:\n")
cat("Mean: ", male_stats_cortisol1$mean, "\n")
cat("Standard Deviation: ", male_stats_cortisol1$sd, "\n")
cat("Range: ", paste(male_stats_cortisol1$range, collapse = " to "), "\n")
cat("95% Confidence Interval: ±", male_stats_cortisol1$ci, "\n\n")

cat("Cortisol1 - Female:\n")
cat("Mean: ", female_stats_cortisol1$mean, "\n")
cat("Standard Deviation: ", female_stats_cortisol1$sd, "\n")
cat("Range: ", paste(female_stats_cortisol1$range, collapse = " to "), "\n")
cat("95% Confidence Interval: ±", female_stats_cortisol1$ci, "\n\n")

cat("Cortisol2 - Male:\n")
cat("Mean: ", male_stats_cortisol2$mean, "\n")
cat("Standard Deviation: ", male_stats_cortisol2$sd, "\n")
cat("Range: ", paste(male_stats_cortisol2$range, collapse = " to "), "\n")
cat("95% Confidence Interval: ±", male_stats_cortisol2$ci, "\n\n")

cat("Cortisol2 - Female:\n")
cat("Mean: ", female_stats_cortisol2$mean, "\n")
cat("Standard Deviation: ", female_stats_cortisol2$sd, "\n")
cat("Range: ", paste(female_stats_cortisol2$range, collapse = " to "), "\n")
cat("95% Confidence Interval: ±", female_stats_cortisol2$ci, "\n")

# Hilfsfunktion zur Berechnung der Statistiken
calculate_stats <- function(data, variable_name) {
  mean_val <- mean(data[[variable_name]], na.rm = TRUE)
  sd_val <- sd(data[[variable_name]], na.rm = TRUE)
  
  # Anzahl der Beobachtungen
  n <- sum(!is.na(data[[variable_name]]))
  
  # Standardfehler
  se <- sd_val / sqrt(n)
  
  # 95% Konfidenzintervall
  t_val <- qt(0.975, df = n - 1) * se
  ci_lower <- mean_val - t_val
  ci_upper <- mean_val + t_val
  
  # Ergebnisse zurückgeben
  list(mean = mean_val, se = se, ci = c(ci_lower, ci_upper), ci_lower = ci_lower, ci_upper = ci_upper)
}

# Statistiken für Cortisol1 nach Geschlecht
male_stats_cortisol1 <- calculate_stats(HCC[HCC$gender == 1,], "cortisol1")
female_stats_cortisol1 <- calculate_stats(HCC[HCC$gender == 2,], "cortisol1")

# Statistiken für Cortisol2 nach Geschlecht
male_stats_cortisol2 <- calculate_stats(HCC[HCC$gender == 1,], "cortisol2")
female_stats_cortisol2 <- calculate_stats(HCC[HCC$gender == 2,], "cortisol2")

# Ergebnisse ausgeben
cat("Cortisol1 - Male:\n")
cat("Mean: ", male_stats_cortisol1$mean, "\n")
cat("Standard Error: ", male_stats_cortisol1$se, "\n")
cat("95% Confidence Interval: ", paste(male_stats_cortisol1$ci, collapse = " to "), "\n")
cat("95% CI Lower Bound: ", male_stats_cortisol1$ci_lower, "\n")
cat("95% CI Upper Bound: ", male_stats_cortisol1$ci_upper, "\n\n")

cat("Cortisol1 - Female:\n")
cat("Mean: ", female_stats_cortisol1$mean, "\n")
cat("Standard Error: ", female_stats_cortisol1$se, "\n")
cat("95% Confidence Interval: ", paste(female_stats_cortisol1$ci, collapse = " to "), "\n")
cat("95% CI Lower Bound: ", female_stats_cortisol1$ci_lower, "\n")
cat("95% CI Upper Bound: ", female_stats_cortisol1$ci_upper, "\n\n")

cat("Cortisol2 - Male:\n")
cat("Mean: ", male_stats_cortisol2$mean, "\n")
cat("Standard Error: ", male_stats_cortisol2$se, "\n")
cat("95% Confidence Interval: ", paste(male_stats_cortisol2$ci, collapse = " to "), "\n")
cat("95% CI Lower Bound: ", male_stats_cortisol2$ci_lower, "\n")
cat("95% CI Upper Bound: ", male_stats_cortisol2$ci_upper, "\n\n")

cat("Cortisol2 - Female:\n")
cat("Mean: ", female_stats_cortisol2$mean, "\n")
cat("Standard Error: ", female_stats_cortisol2$se, "\n")
cat("95% Confidence Interval: ", paste(female_stats_cortisol2$ci, collapse = " to "), "\n")
cat("95% CI Lower Bound: ", female_stats_cortisol2$ci_lower, "\n")
cat("95% CI Upper Bound: ", female_stats_cortisol2$ci_upper, "\n")



#Berechnungen ohne Ausreisser:
# Entfernen der Spalte V2-V4 (brauche ich nicht) 
HCC_ohne.Ausreisser [2:4] <- list(NULL)

# Manuelles Zuweisen der Spaltennamen, falls bekannt
colnames(HCC_ohne.Ausreisser) <- c("study_id","cortisol1","cortisol2","cortisone1","cortisone2","hairmass1","hairmass2","age","gender")

# Löschen der ersten Zeile
HCC_ohne.Ausreisser <- HCC_ohne.Ausreisser [-1, ]

# Spalten definieren, in denen leere Stellen mit NA ersetzt werden sollen
columns_to_fill <- c("cortisol1", "cortisol2", "cortisone1", "cortisone2", "hairmass1", "hairmass2")

# Leere Stellen (repräsentiert als leere Strings oder explizite NAs) in diesen Spalten mit NA ersetzen
HCC_ohne.Ausreisser[columns_to_fill] <- lapply(HCC[columns_to_fill], function(x) {
  x[x == "" | is.na(x)] <- NA
  return(x)
})

# Sicherstellen, dass die Werte numerisch sind
HCC_ohne.Ausreisser$cortisol1 <- as.numeric(HCC_ohne.Ausreisser$cortisol1)
HCC_ohne.Ausreisser$cortisol2 <- as.numeric(HCC_ohne.Ausreisser$cortisol2)
# Hilfsfunktion zur Berechnung der Statistiken
calculate_stats <- function(data, variable_name) {
  mean_val <- mean(data[[variable_name]], na.rm = TRUE)
  sd_val <- sd(data[[variable_name]], na.rm = TRUE)
  
  # Anzahl der Beobachtungen
  n <- sum(!is.na(data[[variable_name]]))
  
  # Standardfehler
  se <- sd_val / sqrt(n)
  
  # 95% Konfidenzintervall
  t_val <- qt(0.975, df = n - 1) * se
  ci_lower <- mean_val - t_val
  ci_upper <- mean_val + t_val
  
  # Ergebnisse zurückgeben
  list(mean = mean_val, se = se, ci = c(ci_lower, ci_upper), ci_lower = ci_lower, ci_upper = ci_upper)
}

# Statistiken für Cortisol1 nach Geschlecht ohne Ausreißer
male_stats_cortisol1_no_outlier <- calculate_stats(HCC_ohne.Ausreisser[HCC_ohne.Ausreisser$gender == 1,], "cortisol1")
female_stats_cortisol1_no_outlier <- calculate_stats(HCC_ohne.Ausreisser[HCC_ohne.Ausreisser$gender == 2,], "cortisol1")

# Statistiken für Cortisol2 nach Geschlecht ohne Ausreißer
male_stats_cortisol2_no_outlier <- calculate_stats(HCC_ohne.Ausreisser[HCC_ohne.Ausreisser$gender == 1,], "cortisol2")
female_stats_cortisol2_no_outlier <- calculate_stats(HCC_ohne.Ausreisser[HCC_ohne.Ausreisser$gender == 2,], "cortisol2")

# Ergebnisse ausgeben
cat("Cortisol1 ohne Ausreißer - Male:\n")
cat("Mean: ", male_stats_cortisol1_no_outlier$mean, "\n")
cat("Standard Error: ", male_stats_cortisol1_no_outlier$se, "\n")
cat("95% Confidence Interval: ", paste(male_stats_cortisol1_no_outlier$ci, collapse = " to "), "\n")
cat("95% CI Lower Bound: ", male_stats_cortisol1_no_outlier$ci_lower, "\n")
cat("95% CI Upper Bound: ", male_stats_cortisol1_no_outlier$ci_upper, "\n\n")

cat("Cortisol1 ohne Ausreißer - Female:\n")
cat("Mean: ", female_stats_cortisol1_no_outlier$mean, "\n")
cat("Standard Error: ", female_stats_cortisol1_no_outlier$se, "\n")
cat("95% Confidence Interval: ", paste(female_stats_cortisol1_no_outlier$ci, collapse = " to "), "\n")
cat("95% CI Lower Bound: ", female_stats_cortisol1_no_outlier$ci_lower, "\n")
cat("95% CI Upper Bound: ", female_stats_cortisol1_no_outlier$ci_upper, "\n\n")

cat("Cortisol2 ohne Ausreißer - Male:\n")
cat("Mean: ", male_stats_cortisol2_no_outlier$mean, "\n")
cat("Standard Error: ", male_stats_cortisol2_no_outlier$se, "\n")
cat("95% Confidence Interval: ", paste(male_stats_cortisol2_no_outlier$ci, collapse = " to "), "\n")
cat("95% CI Lower Bound: ", male_stats_cortisol2_no_outlier$ci_lower, "\n")
cat("95% CI Upper Bound: ", male_stats_cortisol2_no_outlier$ci_upper, "\n\n")

cat("Cortisol2 ohne Ausreißer - Female:\n")
cat("Mean: ", female_stats_cortisol2_no_outlier$mean, "\n")
cat("Standard Error: ", female_stats_cortisol2_no_outlier$se, "\n")
cat("95% Confidence Interval: ", paste(female_stats_cortisol2_no_outlier$ci, collapse = " to "), "\n")
cat("95% CI Lower Bound: ", female_stats_cortisol2_no_outlier$ci_lower, "\n")
cat("95% CI Upper Bound: ", female_stats_cortisol2_no_outlier$ci_upper, "\n")




# Hilfsfunktion zur Berechnung der Statistiken inklusive Perzentile nach Geschlecht
calculate_extended_stats_by_gender <- function(data, variable_name, gender) {
  # Filtern nach Geschlecht
  gender_data <- data[data$gender == gender, ]
  
  mean_val <- mean(gender_data[[variable_name]], na.rm = TRUE)
  sd_val <- sd(gender_data[[variable_name]], na.rm = TRUE)
  n <- sum(!is.na(gender_data[[variable_name]]))
  se <- sd_val / sqrt(n)
  t_val <- qt(0.975, df = n - 1) * se
  ci_lower <- mean_val - t_val
  ci_upper <- mean_val + t_val
  percentiles <- quantile(gender_data[[variable_name]], probs = c(0.05, 0.50, 0.95), na.rm = TRUE)
  
  # Ergebnisse zurückgeben
  list(mean = mean_val, se = se, ci = c(ci_lower, ci_upper), ci_lower = ci_lower, ci_upper = ci_upper,
       percentile_5th = percentiles[1], percentile_50th = percentiles[2], percentile_95th = percentiles[3])
}

# Statistiken für Cortisol1 ohne Ausreißer nach Geschlecht
male_stats_cortisol1_no_outlier <- calculate_extended_stats_by_gender(HCC_ohne.Ausreisser, "cortisol1", 1)
female_stats_cortisol1_no_outlier <- calculate_extended_stats_by_gender(HCC_ohne.Ausreisser, "cortisol1", 2)

# Statistiken für Cortisol2 ohne Ausreißer nach Geschlecht
male_stats_cortisol2_no_outlier <- calculate_extended_stats_by_gender(HCC_ohne.Ausreisser, "cortisol2", 1)
female_stats_cortisol2_no_outlier <- calculate_extended_stats_by_gender(HCC_ohne.Ausreisser, "cortisol2", 2)

# Ergebnisse für Cortisol1 ohne Ausreißer ausgeben
cat("Cortisol1 ohne Ausreißer - Male:\n")
cat("Mean: ", male_stats_cortisol1_no_outlier$mean, "\n")
cat("Standard Error: ", male_stats_cortisol1_no_outlier$se, "\n")
cat("95% Confidence Interval: ", paste(male_stats_cortisol1_no_outlier$ci, collapse = " to "), "\n")
cat("5th Percentile: ", male_stats_cortisol1_no_outlier$percentile_5th, "\n")
cat("50th Percentile (Median): ", male_stats_cortisol1_no_outlier$percentile_50th, "\n")
cat("95th Percentile: ", male_stats_cortisol1_no_outlier$percentile_95th, "\n\n")

cat("Cortisol1 ohne Ausreißer - Female:\n")
cat("Mean: ", female_stats_cortisol1_no_outlier$mean, "\n")
cat("Standard Error: ", female_stats_cortisol1_no_outlier$se, "\n")
cat("95% Confidence Interval: ", paste(female_stats_cortisol1_no_outlier$ci, collapse = " to "), "\n")
cat("5th Percentile: ", female_stats_cortisol1_no_outlier$percentile_5th, "\n")
cat("50th Percentile (Median): ", female_stats_cortisol1_no_outlier$percentile_50th, "\n")
cat("95th Percentile: ", female_stats_cortisol1_no_outlier$percentile_95th, "\n\n")

# Ergebnisse für Cortisol2 ohne Ausreißer ausgeben
cat("Cortisol2 ohne Ausreißer - Male:\n")
cat("Mean: ", male_stats_cortisol2_no_outlier$mean, "\n")
cat("Standard Error: ", male_stats_cortisol2_no_outlier$se, "\n")
cat("95% Confidence Interval: ", paste(male_stats_cortisol2_no_outlier$ci, collapse = " to "), "\n")
cat("5th Percentile: ", male_stats_cortisol2_no_outlier$percentile_5th, "\n")
cat("50th Percentile (Median): ", male_stats_cortisol2_no_outlier$percentile_50th, "\n")
cat("95th Percentile: ", male_stats_cortisol2_no_outlier$percentile_95th, "\n\n")

cat("Cortisol2 ohne Ausreißer - Female:\n")
cat("Mean: ", female_stats_cortisol2_no_outlier$mean, "\n")
cat("Standard Error: ", female_stats_cortisol2_no_outlier$se, "\n")
cat("95% Confidence Interval: ", paste(female_stats_cortisol2_no_outlier$ci, collapse = " to "), "\n")
cat("5th Percentile: ", female_stats_cortisol2_no_outlier$percentile_5th, "\n")
cat("50th Percentile (Median): ", female_stats_cortisol2_no_outlier$percentile_50th, "\n")
cat("95th Percentile: ", female_stats_cortisol2_no_outlier$percentile_95th, "\n")



library(ggplot2)

# Angenommene Referenzdaten
referenzwerte <- data.frame(
  Geschlecht = c("Female","Male"),
  Alter_Gruppe = c("18-65"),
  P5 = c(1.34,1.74),     # 5. Perzentil
  P50 = c(4.62,5.44),    # 50. Perzentil (Median)
  P95 = c(22.14,24.69),  # 95. Perzentil
  Mittelwert = c(7.17,8.23),  # Referenzmittelwerte
  SE = c(0.10,0.19),  # Standardfehler der Mittelwerte
  CI = c(6.97-7.36, 7.85-8.60)  # 95% Konfidenzintervall der Mittelwerte 
)
# Ihre berechneten Mittelwerte für Cortisol1
berechnete_mittelwerte <- data.frame(
  Geschlecht = c("Female","Male"),
  Alter_Gruppe = c("18-65"),
  P5 = c(1.05,1.30),     # 5. Perzentil
  P50 = c(2.84,3.0),    # 50. Perzentil (Median)
  P95 = c(10.60,9.21),  # 95. Perzentil
  Mittelwert = c(4.14,3.60),  # Referenzmittelwerte
  SE = c(0.44,0.37),  # Standardfehler der Mittelwerte
  CI = c(3.26-5.02, 2.84-4.34)  # 95% Konfidenzintervall der Mittelwerte 
)
# Ihre berechneten Mittelwerte für Cortisol2
berechnete_mittelwerte <- data.frame(
  Geschlecht = c("Female","Male"),
  Alter_Gruppe = c("18-65"),
  P5 = c(0.67,1.38),     # 5. Perzentil
  P50 = c(2.55,2.81),    # 50. Perzentil (Median)
  P95 = c(10.40,7.98),  # 95. Perzentil
  Mittelwert = c(4.13,3.40),  # Referenzmittelwerte
SE = c(0.63,0.34),  # Standardfehler der Mittelwerte
CI = c(2.87-5.39, 2.70-4.10)  # 95% Konfidenzintervall der Mittelwerte 
)

# Erstellen der Grafik
p <- ggplot() +
  geom_rect(data = referenzwerte, aes(xmin = P5, xmax = P95, ymin = 0, ymax = Inf, fill = Geschlecht), alpha = 0.2) +
  geom_point(data = referenzwerte, aes(x = Mittelwert, y = 1, color = Geschlecht, shape = "mean reference values"), size = 3) +
  geom_point(data = berechnete_mittelwerte, aes(x = Mittelwert, y = 1.5, color = Geschlecht, shape = "mean participants"), size = 4, stroke = 2) +
  labs(title = "Comparison of cortisol values with reference ranges",
       x = "Cortisol levels", y = "", 
       shape = "Legend") +
  scale_x_continuous(name = "Cortisol levels", limits = c(0, 25)) +
  scale_shape_manual(values = c("mean reference values" = 16, "mean participants" = 4)) +
  scale_color_manual(values = c("Male" = "blue", "Female" = "red")) +
  theme_minimal() +
  theme(legend.position = "right") +
  facet_wrap(~ Geschlecht + Alter_Gruppe, scales = "free_x")

# Anzeigen der Grafik

#Anpassungen bei der Grafik
library(ggplot2)

# Reference values for Cortisol
reference_values <- data.frame(
  Gender = rep(c("Female", "Male"), times = 2),
  Cortisol_Type = rep(c("Cortisol", "Cortisol"), each = 2),
  Mean = c(7.17,8.23),  # Mean values
  Percentile = c(5, 50, 95, 5, 50, 95),
  Value = c(1.34, 4.62, 22.14, 1.74, 5.44, 24.69),
  CI_Lower = c(6.97, 7.85),  # Lower limit of confidence interval
  CI_Upper = c(7.36, 8.60)  # Upper limit of confidence interval
)

# Calculated mean values for Cortisol1
calculated_means_cortisol1 <- data.frame(
  Gender = c("Female", "Male"),
  Cortisol_Type = "Cortisol1", 
  Mean = c(4.14, 3.60),  # Calculated means
  Percentile = c(5, 50, 95, 5, 50, 95),
  Value = c(1.05, 2.84, 10.60, 1.30, 3.0, 9.21),
  CI_Lower = c(3.26, 2.84),  # Lower limit of confidence interval
  CI_Upper = c(5.02, 4.34)   # Upper limit of confidence interval
)

# Calculated mean values for Cortisol2
calculated_means_cortisol2 <- data.frame(
  Gender = c("Female", "Male"), 
  Cortisol_Type = "Cortisol2",
  Mean = c(4.13, 3.40),  # Calculated means
  Percentile = c(5, 50, 95, 5, 50, 95),
  Value = c(0.67, 2.55, 10.40, 1.38, 2.81, 7.96),
  CI_Lower = c(2.87, 2.70),  # Lower limit of confidence interval
  CI_Upper = c(5.39, 4.10)   # Upper limit of confidence interval
)

# Plot for Cortisol1
p1 <- ggplot() +
  geom_point(data = reference_values[reference_values$Cortisol_Type == "Cortisol2", ],
             aes(x = Value, y = Percentile, color = Gender, shape = "mean reference values"), size = 3) +
  geom_errorbar(data = calculated_means_cortisol1,
                aes(x = Mean, ymin = CI_Lower, ymax = CI_Upper, color = Gender), width = 0.2) +
  geom_point(data = calculated_means_cortisol1,
             aes(x = Mean, y = 50, color = Gender, shape = "mean participants"), size = 4, stroke = 2) +
  labs(title = "Cortisol1: Comparison with Reference Values",
       x = "Cortisol Levels", y = "Percentile", 
       shape = "Legend") +
  scale_x_continuous(name = "Cortisol Levels", limits = c(0, 25)) +
  scale_y_continuous(name = "Percentile", breaks = c(5, 50, 95), labels = c("5th Percentile", "50th Percentile", "95th Percentile")) +
  scale_shape_manual(values = c("mean reference values" = 16, "mean participants" = 4)) +
  scale_color_manual(values = c("Male" = "blue", "Female" = "red")) +
  theme_minimal() +
  theme(legend.position = "right") +
  facet_wrap(~ Gender)

# Plot for Cortisol2
p2 <- ggplot() +
  geom_point(data = reference_values[reference_values$Cortisol_Type == "Cortisol2", ],
             aes(x = Value, y = Percentile, color = Gender, shape = "mean reference values"), size = 3) +
  geom_errorbar(data = calculated_means_cortisol2,
                aes(x = Mean, ymin = CI_Lower, ymax = CI_Upper, color = Gender), width = 0.2) +
  geom_point(data = calculated_means_cortisol2,
             aes(x = Mean, y = 50, color = Gender, shape = "mean participants"), size = 4, stroke = 2) +
  labs(title = "Cortisol2: Comparison with Reference Values",
       x = "Cortisol Levels", y = "Percentile", 
       shape = "Legend") +
  scale_x_continuous(name = "Cortisol Levels", limits = c(0, 25)) +
  scale_y_continuous(name = "Percentile", breaks = c(5, 50, 95), labels = c("5th Percentile", "50th Percentile", "95th Percentile")) +
  scale_shape_manual(values = c("mean reference values" = 16, "mean participants" = 4)) +
  scale_color_manual(values = c("Male" = "blue", "Female" = "red")) +
  theme_minimal() +
  theme(legend.position = "right") +
  facet_wrap(~ Gender)

# Display the plots
print(p1)
print(p2)




#funktioniert noch nicht:
#nochmals angepasster Code
library(ggplot2)

# Reference values for Cortisol
reference_values <- data.frame(
  Gender = rep(c("Female", "Male"), each = 3),
  Cortisol_Type = rep(c("Cortisol", "Cortisol", "Cortisol", "Cortisol"), each = 3),
  Percentile = rep(c(5, 50, 95), times = 4),
  Value = c(1.34, 4.62, 22.14, 1.74, 5.44, 24.69),
  Mean = rep(c(7.17, 8.23, 7.17, 8.23), each = 3),
  CI_Lower = c(6.97, 7.85, 6.97, 7.85, 6.97, 7.85, 2.87, 2.70, 2.87, 2.70, 2.87, 2.70),
  CI_Upper = c(7.36, 8.60, 7.36, 8.60, 7.36, 8.60, 5.39, 4.10, 5.39, 4.10, 5.39, 4.10)
)

# Calculated mean values for Cortisol1
calculated_means_cortisol1 <- data.frame(
  Gender = c("Female", "Male"),
  Cortisol_Type = "Cortisol1", 
  Mean = c(4.14, 3.60),  # Calculated means
  CI_Lower = c(3.26, 2.84),  # Lower limit of confidence interval
  CI_Upper = c(5.02, 4.34)   # Upper limit of confidence interval
)

# Calculated mean values for Cortisol2
calculated_means_cortisol2 <- data.frame(
  Gender = c("Female", "Male"), 
  Cortisol_Type = "Cortisol2",
  Mean = c(4.13, 3.40),  # Calculated means
  CI_Lower = c(2.87, 2.70),  # Lower limit of confidence interval
  CI_Upper = c(5.39, 4.10)   # Upper limit of confidence interval
)

# Plot for Cortisol1
p1 <- ggplot() +
  geom_point(data = reference_values[reference_values$Cortisol_Type == "Cortisol1", ],
             aes(x = Value, y = Percentile, color = Gender, shape = "Mean Reference Values"), size = 3) +
  geom_errorbar(data = calculated_means_cortisol1,
                aes(x = Mean, ymin = CI_Lower, ymax = CI_Upper, color = Gender), width = 0.2) +
  geom_point(data = calculated_means_cortisol1,
             aes(x = Mean, y = 50, color = Gender, shape = "Mean Participants"), size = 4, stroke = 2) +
  labs(title = "Cortisol1: Comparison with Reference Values",
       x = "Cortisol Levels", y = "Percentile", 
       shape = "Legend") +
  scale_x_continuous(name = "Cortisol Levels", limits = c(0, 25)) +
  scale_y_continuous(name = "Percentile", breaks = c(5, 50, 95), labels = c("5th Percentile", "50th Percentile", "95th Percentile")) +
  scale_shape_manual(values = c("Mean Reference Values" = 16, "Mean Participants" = 4)) +
  scale_color_manual(values = c("Male" = "blue", "Female" = "red")) +
  theme_minimal() +
  theme(legend.position = "right") +
  facet_wrap(~ Gender)

# Plot for Cortisol2
p2 <- ggplot() +
  geom_point(data = reference_values[reference_values$Cortisol_Type == "Cortisol2", ],
             aes(x = Value, y = Percentile, color = Gender, shape = "Mean Reference Values"), size = 3) +
  geom_errorbar(data = calculated_means_cortisol2,
                aes(x = Mean, ymin = CI_Lower, ymax = CI_Upper, color = Gender), width = 0.2) +
  geom_point(data = calculated_means_cortisol2,
             aes(x = Mean, y = 50, color = Gender, shape = "Mean Participants"), size = 4, stroke = 2) +
  labs(title = "Cortisol2: Comparison with Reference Values",
       x = "Cortisol Levels", y = "Percentile", 
       shape = "Legend") +
  scale_x_continuous(name = "Cortisol Levels", limits = c(0, 25)) +
  scale_y_continuous(name = "Percentile", breaks = c(5, 50, 95), labels = c("5th Percentile", "50th Percentile", "95th Percentile")) +
  scale_shape_manual(values = c("Mean Reference Values" = 16, "Mean Participants" = 4)) +
  scale_color_manual(values = c("Male" = "blue", "Female" = "red")) +
  theme_minimal() +
  theme(legend.position = "right") +
  facet_wrap(~ Gender)

# Display the plots
print(p1)
print(p2)

#zeigt nicht alles an,
#nächster Versuch:
library(ggplot2)

# Reference values for Cortisol
reference_values <- data.frame(
  Gender = rep(c("Female", "Male"), each = 3),
  Cortisol_Type = rep(c("Cortisol1", "Cortisol1", "Cortisol1", "Cortisol2", "Cortisol2", "Cortisol2"), each = 2),
  Percentile = rep(c(5, 50, 95), times = 4),
  Value = c(1.34, 4.62, 22.14, 1.74, 5.44, 24.69, 
            1.05, 2.84, 10.60, 1.30, 3.00, 9.21),
  Mean = rep(c(7.17, 8.23, 7.17, 8.23), each = 3),
  CI_Lower = c(6.97, 7.85, 6.97, 7.85, 6.97, 7.85, 
               2.87, 2.70, 2.87, 2.70, 2.87, 2.70),
  CI_Upper = c(7.36, 8.60, 7.36, 8.60, 7.36, 8.60,
               5.39, 4.10, 5.39, 4.10, 5.39, 4.10)
)

# Calculated mean values for Cortisol1
calculated_means_cortisol1 <- data.frame(
  Gender = c("Female", "Male"),
  Cortisol_Type = "Cortisol1", 
  Mean = c(4.14, 3.60),  # Calculated means
  CI_Lower = c(3.26, 2.84),  # Lower limit of confidence interval
  CI_Upper = c(5.02, 4.34)   # Upper limit of confidence interval
)

# Calculated mean values for Cortisol2
calculated_means_cortisol2 <- data.frame(
  Gender = c("Female", "Male"), 
  Cortisol_Type = "Cortisol2",
  Mean = c(4.13, 3.40),  # Calculated means
  CI_Lower = c(2.87, 2.70),  # Lower limit of confidence interval
  CI_Upper = c(5.39, 4.10)   # Upper limit of confidence interval
)

# Plot for Cortisol1
p1 <- ggplot() +
  geom_point(data = reference_values[reference_values$Cortisol_Type == "Cortisol1", ],
             aes(x = Value, y = Percentile, color = Gender, shape = "Mean Reference Values"), size = 3) +
  geom_errorbar(data = calculated_means_cortisol1,
                aes(x = Mean, ymin = CI_Lower, ymax = CI_Upper, color = Gender), width = 0.2) +
  geom_point(data = calculated_means_cortisol1,
             aes(x = Mean, y = 50, color = Gender, shape = "Mean Participants"), size = 4, stroke = 2) +
  labs(title = "Cortisol1: Comparison with Reference Values",
       x = "Cortisol Levels", y = "Percentile", 
       shape = "Legend") +
  scale_x_continuous(name = "Cortisol Levels", limits = c(0, 25)) +
  scale_y_continuous(name = "Percentile", breaks = c(5, 50, 95), labels = c("5th Percentile", "50th Percentile", "95th Percentile")) +
  scale_shape_manual(values = c("Mean Reference Values" = 16, "Mean Participants" = 17)) +
  scale_color_manual(values = c("Male" = "blue", "Female" = "red")) +
  theme_minimal() +
  theme(legend.position = "right") +
  facet_wrap(~ Gender)

# Plot for Cortisol2
p2 <- ggplot() +
  geom_point(data = reference_values[reference_values$Cortisol_Type == "Cortisol2", ],
             aes(x = Value, y = Percentile, color = Gender, shape = "Mean Reference Values"), size = 3) +
  geom_errorbar(data = calculated_means_cortisol2,
                aes(x = Mean, ymin = CI_Lower, ymax = CI_Upper, color = Gender), width = 0.2) +
  geom_point(data = calculated_means_cortisol2,
             aes(x = Mean, y = 50, color = Gender, shape = "Mean Participants"), size = 4, stroke = 2) +
  labs(title = "Cortisol2: Comparison with Reference Values",
       x = "Cortisol Levels", y = "Percentile", 
       shape = "Legend") +
  scale_x_continuous(name = "Cortisol Levels", limits = c(0, 25)) +
  scale_y_continuous(name = "Percentile", breaks = c(5, 50, 95), labels = c("5th Percentile", "50th Percentile", "95th Percentile")) +
  scale_shape_manual(values = c("Mean Reference Values" = 16, "Mean Participants" = 17)) +
  scale_color_manual(values = c("Male" = "blue", "Female" = "red")) +
  theme_minimal() +
  theme(legend.position = "right") +
  facet_wrap(~ Gender)

# Display the plots
print(p1)
print(p2)

#nächster Versuch:
library(ggplot2)

# Reference values for Cortisol
reference_values <- data.frame(
  Gender = rep(c("Female", "Male"), each = 3),
  Cortisol_Type = rep(c("Cortisol1", "Cortisol1", "Cortisol1", "Cortisol2", "Cortisol2", "Cortisol2"), each = 2),
  Percentile = rep(c(5, 50, 95), times = 4),
  Value = c(1.34, 4.62, 22.14, 1.74, 5.44, 24.69, 
            1.34, 4.62, 22.14, 1.74, 5.44, 24.69),
  Mean = rep(c(7.17, 8.23, 7.17, 8.23), each = 3),
  CI_Lower = c(6.97, 7.85, 6.97, 7.85, 6.97, 7.85, 
               2.87, 2.70, 2.87, 2.70, 2.87, 2.70),
  CI_Upper = c(7.36, 8.60, 7.36, 8.60, 7.36, 8.60,
               5.39, 4.10, 5.39, 4.10, 5.39, 4.10)
)

# Calculated mean values for Cortisol1
calculated_means_cortisol1 <- data.frame(
  Gender = c("Female", "Male"),
  Cortisol_Type = "Cortisol1", 
  Percentile = c(5,50,95),
  Value = c(1.05, 2.84, 10.60, 
            1.30,  3.0,  9.21),
  Mean = c(4.14, 3.60),  # Calculated means
  CI_Lower = c(3.26, 2.84),  # Lower limit of confidence interval
  CI_Upper = c(5.02, 4.34)   # Upper limit of confidence interval
)

# Calculated mean values for Cortisol2
calculated_means_cortisol2 <- data.frame(
  Gender = c("Female", "Male"), 
  Cortisol_Type = "Cortisol2",
  Percentile = c(5, 50, 95),
  Value = c(0.67, 2.55, 10.40, 
            1.38, 2.81, 7.96),
  Mean = c(4.13, 3.40),  # Calculated means
  CI_Lower = c(2.87, 2.70),  # Lower limit of confidence interval
  CI_Upper = c(5.39, 4.10)   # Upper limit of confidence interval
)

# Plot for Cortisol1
p1 <- ggplot() +
  geom_point(data = reference_values[reference_values$Cortisol_Type == "Cortisol1", ],
             aes(x = Value, y = Percentile, color = Gender, shape = "Mean Reference Values"), size = 3) +
  geom_errorbar(data = calculated_means_cortisol1,
                aes(x = Mean, ymin = CI_Lower, ymax = CI_Upper, color = Gender), width = 0.2) +
  geom_point(data = calculated_means_cortisol1,
             aes(x = Mean, y = Percentile, color = Gender, shape = "Mean Participants"), size = 4, stroke = 2) +
  labs(title = "Cortisol 1 month: Comparison with Reference Values",
       x = "Cortisol Levels", y = "Percentile", 
       shape = "Legend") +
  scale_x_continuous(name = "Cortisol Levels", limits = c(0, 25)) +
  scale_y_continuous(name = "Percentile", breaks = c(5, 50, 95), labels = c("5th Percentile", "50th Percentile", "95th Percentile")) +
  scale_shape_manual(values = c("Mean Reference Values" = 8, "Mean Participants" = 18)) +
  scale_color_manual(values = c("Male" = "blue", "Female" = "red")) +
  theme_minimal() +
  theme(legend.position = "right") +
  facet_wrap(~ Gender)

# Plot for Cortisol2
p2 <- ggplot() +
  geom_point(data = reference_values[reference_values$Cortisol_Type == "Cortisol2", ],
             aes(x = Value, y = Percentile, color = Gender, shape = "Mean Reference Values"), size = 3) +
  geom_errorbar(data = calculated_means_cortisol2,
                aes(x = Mean, ymin = CI_Lower, ymax = CI_Upper, color = Gender), width = 0.2) +
  geom_point(data = calculated_means_cortisol2,
             aes(x = Mean, y = Percentile, color = Gender, shape = "Mean Participants"), size = 4, stroke = 2) +
  labs(title = "Cortisol 3 months: Comparison with Reference Values",
       x = "Cortisol Levels", y = "Percentile", 
       shape = "Legend") +
  scale_x_continuous(name = "Cortisol Levels", limits = c(0, 25)) +
  scale_y_continuous(name = "Percentile", breaks = c(5, 50, 95), labels = c("5th Percentile", "50th Percentile", "95th Percentile")) +
  scale_shape_manual(values = c("Mean Reference Values" = 8, "Mean Participants" = 18)) +
  scale_color_manual(values = c("Male" = "blue", "Female" = "red")) +
  theme_minimal() +
  theme(legend.position = "right") +
  facet_wrap(~ Gender)

# Display the plots
print(p1)
print(p2)

