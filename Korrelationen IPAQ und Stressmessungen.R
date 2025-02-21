#Korrelationen zwischen IPAQ und Stress Values
#20.01.2025#

#IPAQ
# Entfernen der Spalte V2-V4 (brauche ich nicht) 
Physical.Activity [2:4] <- list(NULL)

# Sicherstellen, dass die Werte numerisch sind
Physical.Activity$met_total <- as.numeric(Physical.Activity$met_total)

#SCI: 
library(dplyr)

# Entfernen der Spalten V2-V4 (brauche ich nicht)
AE.FS_SCI[2:4] <- list(NULL)

# Manuelles Zuweisen der Spaltennamen
colnames(AE.FS_SCI) <- c("study_id", "score_pps", "dms_score", "sci_stress1_1", "sci_stress1_2", 
                         "sci_stress1_3", "sci_stress1_4", "sci_stress1_5", "sci_stress1_6", 
                         "sci_stress1_7", "sci_stress2_1", "sci_stress2_2", "sci_stress2_3", 
                         "sci_stress2_4", "sci_stress2_5", "sci_stress2_6", "sci_symtpom1", 
                         "sci_symtpom2", "sci_symtpom3", "sci_symtpom4", "sci_symtpom5", 
                         "sci_symtpom6", "sci_symtpom7", "sci_symtpom8", "sci_symtpom9", 
                         "sci_symtpom10", "sci_symtpom11", "sci_symtpom12", "sci_symtpom13")

# Löschen der ersten Zeile
AE.FS_SCI <- AE.FS_SCI[-1, ]

# Sicherstellen, dass alle relevanten Spalten numerisch sind
stress_vars <- c("sci_stress1_1", "sci_stress1_2", "sci_stress1_3", "sci_stress1_4",
                 "sci_stress1_5", "sci_stress1_6", "sci_stress1_7", "sci_stress2_1", 
                 "sci_stress2_2", "sci_stress2_3", "sci_stress2_4", "sci_stress2_5", 
                 "sci_stress2_6", "sci_symtpom1", "sci_symtpom2", "sci_symtpom3", 
                 "sci_symtpom4", "sci_symtpom5", "sci_symtpom6", "sci_symtpom7", 
                 "sci_symtpom8", "sci_symtpom9", "sci_symtpom10", "sci_symtpom11", 
                 "sci_symtpom12", "sci_symtpom13")

AE.FS_SCI[stress_vars] <- lapply(AE.FS_SCI[stress_vars], as.numeric)

# Umkodierungsfunktion für fälschlicherweise codierte Fragen
umkodieren <- function(x, max_value) {
  return ((max_value - 1) * (x - 0) / (max_value - 0) + 1)
}

# Neue Spalten mit umkodierten Werten erstellen
for (i in 1:7) {
  AE.FS_SCI[[paste0("sci_stress1_", i, "_new")]] <- umkodieren(AE.FS_SCI[[paste0("sci_stress1_", i)]], 7)
}

for (i in 1:6) {
  AE.FS_SCI[[paste0("sci_stress2_", i, "_new")]] <- umkodieren(AE.FS_SCI[[paste0("sci_stress2_", i)]], 6)
}

for (i in 1:13) {
  AE.FS_SCI[[paste0("sci_symtpom", i, "_new")]] <- umkodieren(AE.FS_SCI[[paste0("sci_symtpom", i)]], 3)
}

# Entfernen nicht mehr benötigter Spalten
AE.FS_SCI[56:58] <- list(NULL)

# Erstelle einen Vektor mit den Spaltennamen zur Berechnung der Summen
spalten_zum_summieren <- paste0("sci_stress1_", 1:7, "_new")
spalten_zum_summieren_2 <- paste0("sci_stress2_", 1:6, "_new")
spalten_zum_summieren_3 <- paste0("sci_symtpom", 1:13, "_new")

# Berechne die Summen der einzelnen Stressskalen für jeden Probanden
AE.FS_SCI$stress_skala1_sum <- rowSums(AE.FS_SCI[, spalten_zum_summieren], na.rm = TRUE)
AE.FS_SCI$stress_skala2_sum <- rowSums(AE.FS_SCI[, spalten_zum_summieren_2], na.rm = TRUE)
AE.FS_SCI$stress_skala_symptome_sum <- rowSums(AE.FS_SCI[, spalten_zum_summieren_3], na.rm = TRUE)

# Anzeige der ersten paar Zeilen des Datensatzes, um die neuen Variablen zu überprüfen
head(AE.FS_SCI)


#HCC
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



##Korrelationen:
# Sicherstellen, dass die Pakete geladen sind
library(dplyr)

# Vorbereitung: Daten nach Study ID zusammenführen
# Sicherstellen, dass alle Study IDs übereinstimmen
Physical.Activity$study_id <- as.character(Physical.Activity$study_id)
AE.FS_SCI$study_id <- as.character(AE.FS_SCI$study_id)
HCC$study_id <- as.character(HCC$study_id)

# Datensätze zusammenführen
merged_data <- Physical.Activity %>%
  inner_join(AE.FS_SCI, by = "study_id") %>%
  inner_join(HCC, by = "study_id")

# Anzeige des kombinierten Datensatzes
head(merged_data)

# Sicherstellen, dass relevante Variablen numerisch sind
cor_vars <- c("met_total", "stress_skala1_sum", "stress_skala2_sum", "stress_skala_symptome_sum", "cortisol1", "cortisol2")
merged_data[cor_vars] <- lapply(merged_data[cor_vars], as.numeric)


# Korrelation zwischen IPAQ und subjektiver Stressmessung (SCI)
correlation_ipaq_sci <- cor(
  merged_data$met_total, 
  merged_data$stress_skala1_sum, 
  use = "pairwise.complete.obs", 
  method = "pearson"
)
print(paste("Korrelation zwischen IPAQ und SCI Stressskala 1: ", round(correlation_ipaq_sci, 2)))

correlation_ipaq_sci2 <- cor(
  merged_data$met_total, 
  merged_data$stress_skala2_sum, 
  use = "pairwise.complete.obs", 
  method = "pearson"
)
print(paste("Korrelation zwischen IPAQ und SCI Stressskala 2: ", round(correlation_ipaq_sci2, 2)))

correlation_ipaq_sci_symptoms <- cor(
  merged_data$met_total, 
  merged_data$stress_skala_symptome_sum, 
  use = "pairwise.complete.obs", 
  method = "pearson"
)
print(paste("Korrelation zwischen IPAQ und SCI Symptome: ", round(correlation_ipaq_sci_symptoms, 2)))

# Korrelation zwischen IPAQ und objektiver Stressmessung (HCC)
correlation_ipaq_hcc <- cor(
  merged_data$met_total, 
  merged_data$cortisol1, 
  use = "pairwise.complete.obs", 
  method = "pearson"
)
print(paste("Korrelation zwischen IPAQ und Cortisol 1: ", round(correlation_ipaq_hcc, 2)))

correlation_ipaq_hcc_cortisol2 <- cor(
  merged_data$met_total, 
  merged_data$cortisol2, 
  use = "pairwise.complete.obs", 
  method = "pearson"
)
print(paste("Korrelation zwischen IPAQ und Cortisol 2: ", round(correlation_ipaq_hcc_cortisol2, 2)))


# Korrelationen zusammenfassen
correlation_results <- data.frame(
  Variable1 = c("IPAQ", "IPAQ", "IPAQ", "IPAQ"),
  Variable2 = c("SCI Stressskala 1", "SCI Symptome", "Cortisol 1", "Cortisol 2"),
  Correlation = c(
    round(correlation_ipaq_sci, 2),
    round(correlation_ipaq_sci_symptoms, 2),
    round(correlation_ipaq_hcc, 2),
    round(correlation_ipaq_hcc_cortisol2, 2)
  )
)

# Ergebnisse anzeigen
print(correlation_results)
