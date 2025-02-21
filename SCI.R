#SCI: 
# Entfernen der Spalte V2-V4 (brauche ich nicht) 
Demographics [2:4] <- list(NULL)

# Manuelles Zuweisen der Spaltennamen, falls bekannt
colnames(Demographics) <- c("study_id","sci_stress1_1",
                            "sci_stress1_2","sci_stress1_3","sci_stress1_4","sci_stress1_4",
                            "sci_stress1_5","sci_stress1_6","sci_stress1_7","sci_stress1_8","sci_stress2_1","
sci_stress2_2","sci_stress2_3","sci_stress2_4","sci_stress2_5","sci_stress2_6",
                            "sci_symtpom1","sci_symtpom2","sci_symtpom3","sci_symtpom4","sci_symtpom5",
                            "sci_symtpom6","sci_symtpom7","sci_symtpom8","sci_symtpom9","sci_symtpom10",
                            "sci_symtpom11","sci_symtpom12","sci_symtpom13","gesamtscore_symptom",
                            "gesamtscore_coping")


# Löschen der ersten Zeile
SCI <- SCI [-1, ]

# Sicherstellen, dass die Spalten numerisch sind
spalten <- paste0("V", 5:11)
for (spalte in spalten) {
  SCI[[spalte]] <- as.numeric(SCI[[spalte]])
}

# Funktion zur Umkodierung
umkodieren <- function(x) {
  Y <- (7 - 1) * (x - 0) / (7 - 0) + 1
  return(Y)}

# Neue Spalten mit umkodierten Werten erstellen
SCI$V5_new <- umkodieren(SCI$V5)
SCI$V6_new <- umkodieren(SCI$V6)
SCI$V7_new <- umkodieren(SCI$V7)
SCI$V8_new <- umkodieren(SCI$V8)
SCI$V9_new <- umkodieren(SCI$V9)
SCI$V10_new <- umkodieren(SCI$V10)
SCI$V11_new <- umkodieren(SCI$V11)

# Ausgabe des bearbeiteten Datenframes
print(SCI)


# Sicherstellen, dass die Spalten numerisch sind und nicht-numerische Werte identifizieren
#hier sind die Spaltennamen verschwunden: V12-V17 ist die Stress-Skala 2
spalten <- paste0("V", 12:17)
for (spalte in spalten) {
  SCI[[spalte]] <- as.numeric(SCI[[spalte]])
  if (any(is.na(SCI[[spalte]]))) {
    cat("Warnung: Nicht-numerische Werte in Spalte", spalte, "gefunden:\n")
    print(SCI[[spalte]][is.na(SCI[[spalte]])])
  }
}
# Funktion zur Umkodierung
umkodieren <- function(x) {
  Y <- (7 - 1) * (x - 0) / (7 - 0) + 1
  return(Y)
}
# Neue Spalten mit umkodierten Werten erstellen
for (spalte in spalten) {
  neue_spalte <- paste0(spalte, "_new")
  SCI[[neue_spalte]] <- umkodieren(SCI[[spalte]])
}
# Ausgabe des bearbeiteten DataFrames
print(SCI)

# Sicherstellen, dass die Spalten numerisch sind und nicht-numerische Werte zu NA umwandeln
spalten_gesamt <- paste0("V", 5:17)
for (spalte in spalten_gesamt) {
  SCI[[spalte]] <- as.numeric(SCI[[spalte]])
}

# Mittelwerte der einzelnen Spalten berechnen
mittelwerte_einzel <- sapply(SCI[, spalten_gesamt], mean, na.rm = TRUE)
print("Mittelwerte der einzelnen Spalten V5-V17:")
print(mittelwerte_einzel)

# Mittelwert über die Spalten V5-V11 berechnen
mittelwert_gruppe1 <- rowMeans(SCI[, paste0("V", 5:11)], na.rm = TRUE)
SCI$Mittelwert_V5_V11 <- mittelwert_gruppe1
mittelwert_gruppe1_gesamt <- mean(mittelwert_gruppe1, na.rm = TRUE)
print(paste("Mittelwert über die Spalten V5-V11:", mittelwert_gruppe1_gesamt))

# Mittelwert über die Spalten V12-V17 berechnen
mittelwert_gruppe2 <- rowMeans(SCI[, paste0("V", 12:17)], na.rm = TRUE)
SCI$Mittelwert_V12_V17 <- mittelwert_gruppe2
mittelwert_gruppe2_gesamt <- mean(mittelwert_gruppe2, na.rm = TRUE)
print(paste("Mittelwert über die Spalten V12-V17:", mittelwert_gruppe2_gesamt))

# Mittelwert der Spalte V31 berechnen
mittelwert_V31 <- mean(SCI$V31, na.rm = TRUE)

# Erstellen einer Tabelle mit allen berechneten Werten
tabelle <- data.frame(
  Spalte = c(spalten_gesamt, "Mittelwert_V5_V11", "Mittelwert_V12_V17", "Mittelwert_V31"),
  Mittelwert = c(mittelwerte_einzel, mittelwert_gruppe1_gesamt, mittelwert_gruppe2_gesamt, mittelwert_v31)
)

# Tabelle ausgeben
print("Tabelle der Mittelwerte:")
print(tabelle)

install.packages("xlsx")

# Tabelle als Excel-Datei speichern
write.xlsx(tabelle, file = "mittelwerte_tabelle.xlsx", rowNames = FALSE)

# Bestätigung ausgeben
print("Die Tabelle wurde als mittelwerte_tabelle.xlsx exportiert.")

# Laden der benötigten Bibliotheken, falls noch nicht geschehen
library(dplyr)  # Für Datenmanipulation

# Sicherstellen, dass die Spalten numerisch sind und nicht-numerische Werte zu NA umwandeln
spalten_gesamt <- paste0("V", 5:17)
for (spalte in spalten_gesamt) {
  SCI[[spalte]] <- as.numeric(SCI[[spalte]])
}

# Funktion zur Berechnung von Median und Alpha-Wert
compute_summary <- function(x) {
  median_value <- median(x, na.rm = TRUE)
  alpha_value <- alpha(x, na.rm = TRUE)$total
  return(list(Median = median_value, Alpha = alpha_value))
}

#Packages installieren
install.packages("dplyr")
install.packages("xlsx")
install.packages("psych")
install.packages("openxlsx")


# Laden der benötigten Bibliotheken, falls noch nicht geschehen
library(dplyr)  # Für Datenmanipulation
library(xlsx)   # Für das Schreiben von Excel-Dateien
library(psych)  # Für die Berechnung des Alpha-Werts
library(openxlsx)

# Berechnen des Gesamtscores durch Summierung der Spalten V5_new bis V11_new
SCI$Stress_Skala_1_Gesamtscore <- rowSums(SCI[, paste0("V", 5:11, "_new")], na.rm = TRUE)

# Überprüfen, ob die neue Spalte korrekt hinzugefügt wurde
head(SCI[, c("Stress_Skala_1_Gesamtscore")])

# Berechnen des Gesamtscores durch Summierung der Spalten V12_new bis V17_new
SCI$Stress_Skala_2_Gesamtscore <- rowSums(SCI[, paste0("V", 12:17, "_new")], na.rm = TRUE)

# Überprüfen, ob die neue Spalte korrekt hinzugefügt wurde
head(SCI[, c("Stress_Skala_2_Gesamtscore")])

# Sicherstellen, dass alle betroffenen Spalten numerisch sind
SCI[, paste0("V", 18:30)] <- lapply(SCI[, paste0("V", 18:30)], function(x) as.numeric(as.character(x)))

# Überprüfen, ob Konvertierungsprobleme vorliegen und NAs durch Umwandlungen entstehen
sum(is.na(SCI[, paste0("V", 18:30)]))

# Berechnen des Gesamtscores durch Summierung der Spalten V18 bis V30
SCI$Stress_Symptome_Skala_Gesamtscore <- rowSums(SCI[, paste0("V", 18:30)], na.rm = TRUE)

# Überprüfen, ob die neue Spalte korrekt hinzugefügt wurde
head(SCI[, "Stress_Symptome_Skala_Gesamtscore"])

# Berechnen des Gesamtscores durch Summierung der Spalten V18 bis V30
SCI$Stress_Symptome_Skala_Gesamtscore <- rowSums(SCI[, paste0("V", 18:30)], na.rm = TRUE)

# Überprüfen, ob die neue Spalte korrekt hinzugefügt wurde
head(SCI[, c("Stress_Symptome_Skala_Gesamtscore")])

# Statistische Berechnungen für Stress Skala 1 Gesamtscore
mean_stress1 <- mean(SCI$Stress_Skala_1_Gesamtscore, na.rm = TRUE)
sd_stress1 <- sd(SCI$Stress_Skala_1_Gesamtscore, na.rm = TRUE)
range_stress1 <- range(SCI$Stress_Skala_1_Gesamtscore, na.rm = TRUE)
n_stress1 <- sum(!is.na(SCI$Stress_Skala_1_Gesamtscore))
ci_width_stress1 <- qnorm(0.975) * (sd_stress1 / sqrt(n_stress1))
lower_ci_stress1 <- mean_stress1 - ci_width_stress1
upper_ci_stress1 <- mean_stress1 + ci_width_stress1

# Statistische Berechnungen für Stress Skala 2 Gesamtscore
mean_stress2 <- mean(SCI$Stress_Skala_2_Gesamtscore, na.rm = TRUE)
sd_stress2 <- sd(SCI$Stress_Skala_2_Gesamtscore, na.rm = TRUE)
range_stress2 <- range(SCI$Stress_Skala_2_Gesamtscore, na.rm = TRUE)
n_stress2 <- sum(!is.na(SCI$Stress_Skala_2_Gesamtscore))
ci_width_stress2 <- qnorm(0.975) * (sd_stress2 / sqrt(n_stress2))
lower_ci_stress2 <- mean_stress2 - ci_width_stress2
upper_ci_stress2 <- mean_stress2 + ci_width_stress2

# Statistische Berechnungen für Stress Symptome Skala Gesamtscore
mean_stress_symptoms <- mean(SCI$Stress_Symptome_Skala_Gesamtscore, na.rm = TRUE)
sd_stress_symptoms <- sd(SCI$Stress_Symptome_Skala_Gesamtscore, na.rm = TRUE)
range_stress_symptoms <- range(SCI$Stress_Symptome_Skala_Gesamtscore, na.rm = TRUE)
n_stress_symptoms <- sum(!is.na(SCI$Stress_Symptome_Skala_Gesamtscore))
ci_width_stress_symptoms <- qnorm(0.975) * (sd_stress_symptoms / sqrt(n_stress_symptoms))
lower_ci_stress_symptoms <- mean_stress_symptoms - ci_width_stress_symptoms
upper_ci_stress_symptoms <- mean_stress_symptoms + ci_width_stress_symptoms

# Ausgabe der Ergebnisse
cat("Stress Skala 1 - Mittelwert:", mean_stress1, "SD:", sd_stress1, "Range: [", range_stress1[1], ",", range_stress1[2], "] 95% CI: [", lower_ci_stress1, ",", upper_ci_stress1, "]\n")
cat("Stress Skala 2 - Mittelwert:", mean_stress2, "SD:", sd_stress2, "Range: [", range_stress2[1], ",", range_stress2[2], "] 95% CI: [", lower_ci_stress2, ",", upper_ci_stress2, "]\n")
cat("Stress Symptome - Mittelwert:", mean_stress_symptoms, "SD:", sd_stress_symptoms, "Range: [", range_stress_symptoms[1], ",", range_stress_symptoms[2], "] 95% CI: [", lower_ci_stress_symptoms, ",", upper_ci_stress_symptoms, "]\n")

# Laden des openxlsx Pakets, falls noch nicht installiert und geladen
if (!require(openxlsx)) {
  install.packages("openxlsx")
  library(openxlsx)
}

# Erstellung einer Tabelle mit allen berechneten Werten
stats_table <- data.frame(
  Metric = c("Mean", "SD", "Range Low", "Range High", "95% CI Low", "95% CI High"),
  Stress_Skala_1 = c(mean_stress1, sd_stress1, range_stress1[1], range_stress1[2], lower_ci_stress1, upper_ci_stress1),
  Stress_Skala_2 = c(mean_stress2, sd_stress2, range_stress2[1], range_stress2[2], lower_ci_stress2, upper_ci_stress2),
  Stress_Symptome = c(mean_stress_symptoms, sd_stress_symptoms, range_stress_symptoms[1], range_stress_symptoms[2], lower_ci_stress_symptoms, upper_ci_stress_symptoms)
)

# Erstellen einer neuen Excel-Datei
wb <- createWorkbook()
addWorksheet(wb, "SCI Statistical Measures")
writeData(wb, "SCI Statistical Measures", stats_table, startRow = 1, startCol = 1, colNames = TRUE)

# Spezifiziere den vollständigen Pfad
file_path <- "U:/PhD/LongNeck/Statistik/R Berechnungen/Akutdaten/Tables and Figures/SCI Statistical_Measures.xlsx"

# Speichere die Workbook-Datei an diesem Pfad
saveWorkbook(wb, file_path, overwrite = TRUE)

# Bestätigungsnachricht
cat("Die Excel-Datei wurde erfolgreich unter dem Pfad gespeichert:", file_path)

# Laden des Pakets dplyr, falls noch nicht installiert und geladen
if (!require(dplyr)) {
  install.packages("dplyr")
  library(dplyr)
}

# Funktion zur Berechnung der Statistiken
calculate_stats <- function(data, col) {
  mean_value <- mean(data[[col]], na.rm = TRUE)
  sd_value <- sd(data[[col]], na.rm = TRUE)
  median_value <- median(data[[col]], na.rm = TRUE)
  n <- sum(!is.na(data[[col]]))
  ci_width <- qnorm(0.975) * (sd_value / sqrt(n))
  lower_ci <- mean_value - ci_width
  upper_ci <- mean_value + ci_width
  return(c(Mean = mean_value, SD = sd_value, Median = median_value, `Lower CI` = lower_ci, `Upper CI` = upper_ci))
}

# Anwendung der Funktion auf jede Variable von V5_new bis V17_new und von V18 bis V30
results <- lapply(paste0("V", c(5:17, 18:30), ifelse(5:30 <= 17, "_new", "")), function(col) calculate_stats(SCI, col))

# Namen der Ergebnisliste setzen
names(results) <- paste0("V", c(5:17, 18:30), ifelse(5:30 <= 17, "_new", ""))

# Umwandlung der Liste in einen Dataframe für bessere Lesbarkeit
results_df <- do.call(rbind, results)

# Ergebnisse anzeigen
print(results_df)

# Laden des openxlsx Pakets, falls noch nicht installiert und geladen
if (!require(openxlsx)) {
  install.packages("openxlsx")
  library(openxlsx)
}

# Erstellen einer neuen Excel-Datei
wb <- createWorkbook()
addWorksheet(wb, "Variable Statistics")

# Schreiben der Daten in das Arbeitsblatt
writeData(wb, "Variable Statistics", results_df, startRow = 1, colNames = TRUE)

# Spezifizieren des vollständigen Pfades für die Excel-Datei
file_path <- "U:/PhD/LongNeck/Statistik/R Berechnungen/Akutdaten/Tables and Figures/Variable_Statistics.xlsx"

# Speichern der Excel-Datei an diesem Pfad
saveWorkbook(wb, file_path, overwrite = TRUE)

# Ausgabe einer Bestätigungsnachricht
cat("Die Excel-Datei wurde erfolgreich unter dem Pfad gespeichert:", file_path)



#density plot
library(ggplot2)

# Density Plot für Stress Skala 1 Gesamtscore
ggplot(SCI, aes(x = Stress_Skala_1_Gesamtscore)) +
  geom_density(fill = "blue", alpha = 0.5) +
  labs(title = "Density Plot of Stress Scale 1 Total score",
       x = "Stress Scale 1 Total score",
       y = "Density") +
  theme_minimal()

# Density Plot für Stress Skala 2 Gesamtscore
ggplot(SCI, aes(x = Stress_Skala_2_Gesamtscore)) +
  geom_density(fill = "red", alpha = 0.5) +
  labs(title = "Density Plot of Stress Scale 2 Total score",
       x = "Stress Scale 2 Total score",
       y = "Density") +
  theme_minimal()

# Density Plot für Stress Symptom Skala Gesamtscore
ggplot(SCI, aes(x = Stress_Symptome_Skala_Gesamtscore)) +
  geom_density(fill = "green", alpha = 0.5) +
  labs(title = "Density Plot of Stress Symptom Scale Total score",
       x = "Stress Symptom Scale Total score",
       y = "Density") +
  theme_minimal()
