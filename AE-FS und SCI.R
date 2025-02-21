# Laden der benötigten Pakete
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

# Sicherstellen, dass die Spalten numerisch sind
AE.FS_SCI$score_pps <- as.numeric(AE.FS_SCI$score_pps)
AE.FS_SCI$dms_score <- as.numeric(AE.FS_SCI$dms_score)

# Überprüfen, ob die Spalten erfolgreich umgewandelt wurden
str(AE.FS_SCI)

# Erstellen der Gruppenzuweisung
AE.FS_SCI <- AE.FS_SCI %>%
  mutate(activity_pattern = case_when(
    score_pps < 3 & dms_score == 2 ~ "FAR",
    score_pps >= 3 & dms_score == 2 ~ "DER",
    score_pps >= 3 & dms_score < 2 ~ "EER",
    score_pps < 3 & dms_score < 2 ~ "AR",
    TRUE ~ NA_character_
  ))

# Überprüfen, ob die Spalte hinzugefügt wurde und ob die Gruppenzuweisung korrekt ist
head(AE.FS_SCI)
table(AE.FS_SCI$activity_pattern)






################################Vorraussetzungen ANOVA prüfen#############################################################
# Benötigte Pakete laden (falls nicht bereits geladen)
library(ggplot2)
library(dplyr)
library(car)
library(nortest)

# Funktion zur Überprüfung der Normalverteilung der Residuen
check_normality <- function(data, response) {
  model <- lm(as.formula(paste(response, "~ activity_pattern")), data = data)
  residuals <- model$residuals
  
  # Q-Q Plot erstellen
  qqnorm(residuals)
  qqline(residuals, col = "red")
  title(main = paste("Q-Q Plot für Residuen von", response))  # Angepasster Titel
  
  # Shapiro-Wilk Test
  shapiro_test <- shapiro.test(residuals)
  print(paste("Shapiro-Wilk Test für", response, ": W =", round(shapiro_test$statistic, 4), 
              ", p-value =", round(shapiro_test$p.value, 4)))
}

# ANOVA Modell für jede Stress-Skala
for (scale in c('stress_skala1_sum', 'stress_skala2_sum', 'stress_skala_symptome_sum')) {
  check_normality(AE.FS_SCI, scale)  # Hier wird der Name der Skala übergeben
}

# 3. Homogenität der Varianzen mit Boxplot
par(mfrow=c(1,3))  # Layout für 3 Plots nebeneinander
for (scale in c('stress_skala1_sum', 'stress_skala2_sum', 'stress_skala_symptome_sum')) {
  boxplot(AE.FS_SCI[[scale]] ~ AE.FS_SCI$activity_pattern,
          main = paste("Boxplot für", scale),
          xlab = "Activity Pattern",
          ylab = scale)
}

# Levene-Test für Homogenität der Varianzen
for (scale in c('stress_skala1_sum', 'stress_skala2_sum', 'stress_skala_symptome_sum')) {
  levene_test <- leveneTest(as.formula(paste(scale, "~ activity_pattern")), data = AE.FS_SCI)
  
  # Ergebnisse des Levene-Tests ausgeben
  print(paste("Levene-Test für", scale, ": F =", round(levene_test$statistic, 4), 
              ", p-value =", round(levene_test$p.value, 4)))
}





# Benötigte Pakete laden (falls nicht bereits geladen)
library(ggplot2)
library(dplyr)
library(car)
library(nortest)

# Funktion zur Überprüfung der Normalverteilung der Residuen
check_normality <- function(data, response) {
  model <- lm(as.formula(paste(response, "~ activity_pattern")), data = data)
  residuals <- model$residuals
  
  # Q-Q Plot erstellen
  qqnorm(residuals)
  qqline(residuals, col = "red")
  title(main = paste("Q-Q Plot für Residuen von", response))  # Angepasster Titel
  
  # Shapiro-Wilk Test
  shapiro_test <- shapiro.test(residuals)
  print(paste("Shapiro-Wilk Test für", response, ": W =", round(shapiro_test$statistic, 4), 
              ", p-value =", round(shapiro_test$p.value, 4)))
}



# Sicherstellen, dass die activity_pattern-Spalte ein Faktor ist
AE.FS_SCI$activity_pattern <- as.factor(AE.FS_SCI$activity_pattern)

# Levene-Test für Homogenität der Varianzen
for (scale in c('stress_skala1_sum', 'stress_skala2_sum', 'stress_skala_symptome_sum')) {
  # Durchführen des Levene-Tests
  levene_test <- leveneTest(as.formula(paste(scale, "~ activity_pattern")), data = AE.FS_SCI)
  
  # Überprüfung der Struktur der Levene-Test-Ausgabe
  print(levene_test)  # Zeige die gesamte Ausgabe des Levene-Tests an
  
  # Ergebnisse des Levene-Tests ausgeben
  if (!is.null(levene_test$statistic)) {
    print(paste("Levene-Test für", scale, ": F =", round(levene_test$statistic, 4), 
                ", p-value =", round(levene_test$p.value, 4)))
  } else {
    print(paste("Levene-Test für", scale, "nicht erfolgreich."))
  }
}


################################Bootstrapping########################################
# Bootstrapping für jede Stress-Skala
for (scale in stress_scales) {
  # Bootstrapping durchführen
  boot_results <- boot(AE.FS_SCI[[scale]], statistic = mean_function, R = 1000)  # 1000 Bootstrap-Stichproben
  
  # Konfidenzintervall berechnen
  ci <- boot.ci(boot_results, type = "basic")
  
  # Ausgabe der Ergebnisse
  cat(paste("Konfidenzintervall für", scale, ":\n"))
  cat("Ursprünglicher Mittelwert:", mean(AE.FS_SCI[[scale]], na.rm = TRUE), "\n")
  cat("Konfidenzintervall (95%):", ci$basic[4], "-", ci$basic[5], "\n\n")  # Untere und obere Grenze
}



###########################Bootstrap Jürgen#######################################
# Beispiel-Daten erstellen (Falls nicht bereits vorhanden)
# set.seed(123)  # Für Reproduzierbarkeit
# AE.FS_SCI <- data.frame(stress_skala1_sum = rnorm(100, mean = 50, sd = 10))  # Beispielhafte Daten

# Bootstrapping-Funktion
bootstrap_mean <- function(data, num_resamples = 1000) {
  n <- length(data)  # Anzahl der Beobachtungen
  bootstrap_means <- numeric(num_resamples)  # Vektor zur Speicherung der Mittelwerte
  
  for (i in 1:num_resamples) {
    # Ziehen mit Zurücklegen
    sample_indices <- sample(1:n, n, replace = TRUE)
    bootstrap_sample <- data[sample_indices]
    bootstrap_means[i] <- mean(bootstrap_sample, na.rm = TRUE)  # Berechnung des Mittelwerts
  }
  
  return(bootstrap_means)
}

# Bootstrapping für die Stress-Skala 1
set.seed(123)  # Für Reproduzierbarkeit
boot_means <- bootstrap_mean(AE.FS_SCI$stress_skala1_sum, num_resamples = 1000)

# Empirische Quantile der Bootstrap-Mittelwerte
quantiles <- quantile(boot_means, probs = c(0.025, 0.5, 0.975))  # 2.5%, Median, 97.5%
print(quantiles)

# Ausgabe der Bootstrapped Mittelwerte für weitere Analyse (optional)
hist(boot_means, main = "Histogramm der Bootstrapped Mittelwerte", xlab = "Bootstrapped Mittelwerte", breaks = 30)
abline(v = mean(AE.FS_SCI$stress_skala1_sum, na.rm = TRUE), col = "red", lwd = 2)  # Ursprünglicher Mittelwert




#####################################Mittelwerte,Median usw.########################


# Berechnung des Mittelwerts, Medians und der Standardabweichung für jede Stress Skala
grouped_stress_skala1 <- AE.FS_SCI %>%
  group_by(activity_pattern) %>%
  summarize(mean_stress1 = mean(stress_skala1_sum, na.rm = TRUE),
            sd_stress1 = sd(stress_skala1_sum, na.rm = TRUE),
            median_stress1 = median(stress_skala1_sum, na.rm = TRUE))

grouped_stress_skala2 <- AE.FS_SCI %>%
  group_by(activity_pattern) %>%
  summarize(mean_stress2 = mean(stress_skala2_sum, na.rm = TRUE),
            sd_stress2 = sd(stress_skala2_sum, na.rm = TRUE),
            median_stress2 = median(stress_skala2_sum, na.rm = TRUE))

grouped_stress_skala_symptome_sum <- AE.FS_SCI %>%
  group_by(activity_pattern) %>%
  summarize(mean_stress_symptome = mean(stress_skala_symptome_sum, na.rm = TRUE),
            sd_stress_symptome = sd(stress_skala_symptome_sum, na.rm = TRUE),
            median_stress_symptome = median(stress_skala_symptome_sum, na.rm = TRUE))

# Ausgabe der Ergebnisse
print(grouped_stress_skala1)
print(grouped_stress_skala2)
print(grouped_stress_skala_symptome_sum)




###############################ANOVA:#############################################
install.packages("dplyr")
library(dplyr)

# Daten für ANOVA vorbereiten
stress_data <- AE.FS_SCI %>%
  select(activity_pattern, stress_skala1_sum, stress_skala2_sum, stress_skala_symptome_sum) %>%
  filter(!is.na(activity_pattern))

# ANOVA für jede Stress-Skala durchführen
anova_stress1 <- aov(stress_skala1_sum ~ activity_pattern, data = stress_data)
anova_stress2 <- aov(stress_skala2_sum ~ activity_pattern, data = stress_data)
anova_symptom <- aov(stress_skala_symptome_sum ~ activity_pattern, data = stress_data)

# Zusammenfassung der ANOVA-Ergebnisse
summary(anova_stress1)
summary(anova_stress2)
summary(anova_symptom)

# Post-Hoc-Tests durchführen, um spezifische Unterschiede zwischen den Gruppen zu identifizieren
TukeyHSD(anova_stress1)
TukeyHSD(anova_stress2)
TukeyHSD(anova_symptom)

# Post-Hoc-Tests durchführen, um spezifische Unterschiede zwischen den Gruppen zu identifizieren
tukey_stress1 <- TukeyHSD(anova_stress1)
tukey_stress2 <- TukeyHSD(anova_stress2)
tukey_symptom <- TukeyHSD(anova_symptom)

# Ergebnisse der Tukey-Tests in DataFrames umwandeln
tukey_stress1_df <- as.data.frame(tukey_stress1$activity_pattern)
tukey_stress2_df <- as.data.frame(tukey_stress2$activity_pattern)
tukey_symptom_df <- as.data.frame(tukey_symptom$activity_pattern)

# Ergebnisse anzeigen
print("Signifikanzwerte für Stress Skala 1:")
print(tukey_stress1_df)

print("Signifikanzwerte für Stress Skala 2:")
print(tukey_stress2_df)

print("Signifikanzwerte für Stress Symptome Skala:")
print(tukey_symptom_df)

# Installiere notwendige Pakete, falls noch nicht geschehen
install.packages("openxlsx")

# Lade das Paket
library(openxlsx)

# Funktion, um TukeyHSD-Ergebnisse in ein DataFrame zu konvertieren
tukey_to_df <- function(tukey_result) {
  result_list <- tukey_result$activity_pattern
  df <- as.data.frame(result_list)
  df$Comparison <- rownames(df)
  return(df)
}

# TukeyHSD-Ergebnisse in DataFrames umwandeln
tukey_stress1_df <- tukey_to_df(TukeyHSD(anova_stress1))
tukey_stress2_df <- tukey_to_df(TukeyHSD(anova_stress2))
tukey_symptom_df <- tukey_to_df(TukeyHSD(anova_symptom))

# Erstelle eine neue Excel-Datei
wb <- createWorkbook()

# Füge Blätter hinzu
addWorksheet(wb, "Stress Skala 1")
addWorksheet(wb, "Stress Skala 2")
addWorksheet(wb, "Symptome Skala")

# Schreibe Daten in die Excel-Blätter
writeData(wb, sheet = "Stress Skala 1", tukey_stress1_df)
writeData(wb, sheet = "Stress Skala 2", tukey_stress2_df)
writeData(wb, sheet = "Symptome Skala", tukey_symptom_df)

# Speichere die Excel-Datei
saveWorkbook(wb, "U:/PhD/LongNeck/Statistik/R/Akutdaten/TukeyHSD_Results.xlsx", overwrite = TRUE)





#effect sizes berechnen
# Installiere das Paket, falls noch nicht geschehen
install.packages("effectsize")

# Lade das Paket
library(effectsize)

# Berechne die Effektgrößen (Eta-Quadrat) für die ANOVA-Modelle
eta_squared_stress1 <- eta_squared(anova_stress1)
eta_squared_stress2 <- eta_squared(anova_stress2)
eta_squared_symptom <- eta_squared(anova_symptom)

# Ausgabe der Effektgrößen
print(eta_squared_stress1)
print(eta_squared_stress2)
print(eta_squared_symptom)

# Optional: Effektgrößen in DataFrames umwandeln
eta_squared_stress1_df <- as.data.frame(eta_squared_stress1)
eta_squared_stress2_df <- as.data.frame(eta_squared_stress2)
eta_squared_symptom_df <- as.data.frame(eta_squared_symptom)

# Hinzufügen der Effektgrößen zu den TukeyHSD-Ergebnissen und Excel-Export
tukey_stress1_df$eta_squared <- eta_squared_stress1_df[1, "Eta_Sq"]
tukey_stress2_df$eta_squared <- eta_squared_stress2_df[1, "Eta_Sq"]
tukey_symptom_df$eta_squared <- eta_squared_symptom_df[1, "Eta_Sq"]

# Erstelle eine neue Excel-Datei
wb <- createWorkbook()

# Füge Blätter hinzu
addWorksheet(wb, "Stress Skala 1")
addWorksheet(wb, "Stress Skala 2")
addWorksheet(wb, "Symptome Skala")

# Schreibe Daten in die Excel-Blätter
writeData(wb, sheet = "Stress Skala 1", tukey_stress1_df)
writeData(wb, sheet = "Stress Skala 2", tukey_stress2_df)
writeData(wb, sheet = "Symptome Skala", tukey_symptom_df)

# Speichere die Excel-Datei
saveWorkbook(wb, "U:/PhD/LongNeck/Statistik/R/Akutdaten/Tables/TukeyHSD_EffectSizes_Results.xlsx", overwrite = TRUE)

# Ausgabe der angepassten DataFrames
print(tukey_stress1_df)
print(tukey_stress2_df)
print(tukey_symptom_df)


##############################17.01.25 Effect sizes Neu###################################
# Effektgrößen berechnen
library(effectsize)

# Berechnung von Eta-Squared für die ANOVA-Modelle
eta_squared_stress1 <- eta_squared(anova_stress1)
eta_squared_stress2 <- eta_squared(anova_stress2)
eta_squared_symptom <- eta_squared(anova_symptom)

# Ausgabe der Effektgrößen
print(eta_squared_stress1)
print(eta_squared_stress2)
print(eta_squared_symptom)
# TukeyHSD für paarweise Vergleiche
tukey_stress1 <- TukeyHSD(anova_stress1)
tukey_stress2 <- TukeyHSD(anova_stress2)
tukey_symptom <- TukeyHSD(anova_symptom)

# Ausgabe der Tukey-Ergebnisse
print(tukey_stress1)
print(tukey_stress2)
print(tukey_symptom)
# Effektgröße als Zusatzspalte in die Tukey-Ergebnisse einfügen
tukey_stress1_df <- as.data.frame(tukey_stress1[[1]])
tukey_stress1_df$eta_squared <- eta_squared_stress1[1, "Eta_Sq"]

tukey_stress2_df <- as.data.frame(tukey_stress2[[1]])
tukey_stress2_df$eta_squared <- eta_squared_stress2[1, "Eta_Sq"]

tukey_symptom_df <- as.data.frame(tukey_symptom[[1]])
tukey_symptom_df$eta_squared <- eta_squared_symptom[1, "Eta_Sq"]
# Excel exportieren
library(openxlsx)

wb <- createWorkbook()
addWorksheet(wb, "Stress Skala 1")
addWorksheet(wb, "Stress Skala 2")
addWorksheet(wb, "Symptome Skala")

writeData(wb, sheet = "Stress Skala 1", tukey_stress1_df)
writeData(wb, sheet = "Stress Skala 2", tukey_stress2_df)
writeData(wb, sheet = "Symptome Skala", tukey_symptom_df)

saveWorkbook(wb,  "C:/Users/ritam/Desktop/TukeyHSD_EffectSizes_Results.xlsx", overwrite = TRUE)


#######################Effect sizes (Cohens f)################################
# Cohen's f berechnen
cohens_f_stress1 <- cohens_f(anova_stress1)
cohens_f_stress2 <- cohens_f(anova_stress2)
cohens_f_symptom <- cohens_f(anova_symptom)

# Ausgabe von Cohen's f
print(cohens_f_stress1)
print(cohens_f_stress2)
print(cohens_f_symptom)

# Zusammenfassung aller Effektgrößen in einem DataFrame
effect_sizes_df <- data.frame(
  Skala = c("Stress Skala 1", "Stress Skala 2", "Symptome Skala"),
  Eta_Squared = c(eta_squared_stress1[1, "Eta_Sq"], eta_squared_stress2[1, "Eta_Sq"], eta_squared_symptom[1, "Eta_Sq"]),
  Cohens_f = c(cohens_f_stress1[1, "Cohens_f"], cohens_f_stress2[1, "Cohens_f"], cohens_f_symptom[1, "Cohens_f"])
)

# Ausgabe der Effektgrößen-Zusammenfassung
print(effect_sizes_df)

# Visualisierung der Effektgrößen
library(ggplot2)

ggplot(effect_sizes_df, aes(x = Skala, y = Eta_Squared)) +
  geom_bar(stat = "identity") +
  labs(title = "Eta-Squared für verschiedene Skalen", x = "Skala", y = "Eta-Squared")

ggplot(effect_sizes_df, aes(x = Skala, y = Cohens_f)) +
  geom_bar(stat = "identity") +
  labs(title = "Cohen's f für verschiedene Skalen", x = "Skala", y = "Cohen's f")

# Export der Effektgrößen-Zusammenfassung
write.xlsx(effect_sizes_df, "C:/Users/ritam/Desktop/EffectSizes_Summary.xlsx", overwrite = TRUE)


####################Effect sizes (Cohens f) für Untergruppen der AP######################
# Pakete laden
library(dplyr)
library(effectsize)

# Überprüfe die Spalte activity_pattern
head(AE.FS_SCI)
table(AE.FS_SCI$activity_pattern)

# Prüfe, ob es fehlende Werte gibt
sum(is.na(AE.FS_SCI$activity_pattern))

# Erstellen der Gruppenzuweisung
AE.FS_SCI <- AE.FS_SCI %>%
  mutate(activity_pattern = case_when(
    score_pps < 3 & dms_score == 2 ~ "FAR",
    score_pps >= 3 & dms_score == 2 ~ "DER",
    score_pps >= 3 & dms_score < 2 ~ "EER",
    score_pps < 3 & dms_score < 2 ~ "AR",
    TRUE ~ NA_character_
  ))

# Filtere nur Zeilen mit gültigen activity_pattern-Werten
stress_data <- AE.FS_SCI %>%
  filter(!is.na(activity_pattern)) %>%
  select(activity_pattern, stress_skala1_sum, stress_skala2_sum, stress_skala_symptome_sum)

# Überprüfe die Struktur der Daten
str(stress_data)


# Überprüfen der Gruppenzuweisung
head(AE.FS_SCI)
table(AE.FS_SCI$activity_pattern)

# Daten für jede Gruppe filtern und ANOVA berechnen
grouped_anovas <- AE.FS_SCI %>%
  filter(!is.na(activity_pattern)) %>%
  group_by(activity_pattern) %>%
  group_map(~ {
    data_group <- .x
    
    # ANOVA-Modelle für jede Stress-Skala
    anova_stress1 <- aov(stress_skala1_sum ~ activity_pattern, data = data_group)
    anova_stress2 <- aov(stress_skala2_sum ~ activity_pattern, data = data_group)
    anova_symptom <- aov(stress_skala_symptome_sum ~ activity_pattern, data = data_group)
    
    # Cohen's f für jede Skala berechnen
    cohens_f_stress1 <- cohens_f(anova_stress1)
    cohens_f_stress2 <- cohens_f(anova_stress2)
    cohens_f_symptom <- cohens_f(anova_symptom)
    
    # Ergebnisse als DataFrame speichern
    data.frame(
      Group = unique(data_group$activity_pattern),
      Skala = c("Stress Skala 1", "Stress Skala 2", "Symptome Skala"),
      Cohens_f = c(cohens_f_stress1[1, "Cohens_f"], 
                   cohens_f_stress2[1, "Cohens_f"], 
                   cohens_f_symptom[1, "Cohens_f"])
    )
  })

# Ergebnisse kombinieren
cohens_f_results <- bind_rows(grouped_anovas)

# Ergebnisse anzeigen
print(cohens_f_results)

# Visualisierung der Cohen's f für die Untergruppen
library(ggplot2)

ggplot(cohens_f_results, aes(x = Skala, y = Cohens_f, fill = Group)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Cohen's f für verschiedene Gruppen und Skalen", 
       x = "Skala", 
       y = "Cohen's f", 
       fill = "Gruppe") +
  theme_minimal()

# Ergebnisse als Excel speichern
library(openxlsx)

wb <- createWorkbook()
addWorksheet(wb, "Cohen's f Results")

writeData(wb, sheet = "Cohen's f Results", cohens_f_results)

saveWorkbook(wb, "U:/PhD/LongNeck/Statistik/R Berechnungen/Akutdaten/Cohens_f_Group_Results.xlsx", overwrite = TRUE)




#-->funktioniert nicht der CODE!!!



library(effectsize)

# Paarweise Vergleiche (Cohen's d)
pairwise_effects <- data.frame(
  Comparison = c("FAR vs AR", "FAR vs DER", "FAR vs EER", "AR vs DER", "AR vs EER", "DER vs EER"),
  Cohens_d = c(
    cohens_d(stress_skala1_sum ~ activity_pattern, data = stress_data, subset = activity_pattern %in% c("FAR", "AR")),
    cohens_d(stress_skala1_sum ~ activity_pattern, data = stress_data, subset = activity_pattern %in% c("FAR", "DER")),
    cohens_d(stress_skala1_sum ~ activity_pattern, data = stress_data, subset = activity_pattern %in% c("FAR", "EER")),
    cohens_d(stress_skala1_sum ~ activity_pattern, data = stress_data, subset = activity_pattern %in% c("AR", "DER")),
    cohens_d(stress_skala1_sum ~ activity_pattern, data = stress_data, subset = activity_pattern %in% c("AR", "EER")),
    cohens_d(stress_skala1_sum ~ activity_pattern, data = stress_data, subset = activity_pattern %in% c("DER", "EER"))
  )
)

print(pairwise_effects)

# Export in eine Excel-Datei
wb <- createWorkbook()
addWorksheet(wb, "Cohen's d Results")
writeData(wb, sheet = "Cohen's d Results", pairwise_effects)

# Speichere die Excel-Datei
saveWorkbook(wb, "U:/PhD/LongNeck/Statistik/R Berechnungen/Akutdaten/Cohens_d_Pairwise_Results.xlsx", overwrite = TRUE)

table(stress_data$activity_pattern)


















#10.09.24: Boxplots erstellen
library(ggplot2)
library(dplyr)

# Daten für ANOVA vorbereiten
stress_data <- AE.FS_SCI %>%
  select(activity_pattern, stress_skala1_sum, stress_skala2_sum, stress_skala_symptome_sum) %>%
  filter(!is.na(activity_pattern))

# Boxplot für stress_skala1_sum erstellen
p1 <- ggplot(stress_data, aes(x = activity_pattern, y = stress_skala1_sum, fill = activity_pattern)) +
  geom_boxplot() +
  labs(title = "Boxplot of Stress Scale 1",
       x = "Activity Pattern", y = "Stress Scale 1 Sum") +
  theme_minimal() +
  theme(legend.position = "none")

# Boxplot für stress_skala2_sum erstellen
p2 <- ggplot(stress_data, aes(x = activity_pattern, y = stress_skala2_sum, fill = activity_pattern)) +
  geom_boxplot() +
  labs(title = "Boxplot of Stress Scale 2",
       x = "Activity Pattern", y = "Stress Scale 2 Sum") +
  theme_minimal() +
  theme(legend.position = "none")

# Boxplot für stress_skala_symptome_sum erstellen
p3 <- ggplot(stress_data, aes(x = activity_pattern, y = stress_skala_symptome_sum, fill = activity_pattern)) +
  geom_boxplot() +
  labs(title = "Boxplot of Stress Symptom Scale",
       x = "Activity Pattern", y = "Stress Symptom Scale Sum") +
  theme_minimal() +
  theme(legend.position = "none")

# Plots anzeigen
print(p1)
print(p2)
print(p3)




#######################################MANOVA:#######################################
#Voraussetzungen prüfen:
#Prüfung der Unabhängigkeit der Beobachtungen
#Prüfung der Normalverteilung der abhängigen Variablen:

# Shapiro-Wilk-Test für jede abhängige Variable
shapiro.test(AE.FS_SCI$stress_skala1_sum)
shapiro.test(AE.FS_SCI$stress_skala2_sum)
shapiro.test(AE.FS_SCI$stress_skala_symptome_sum)

# Q-Q Plots für jede abhängige Variable
qqnorm(AE.FS_SCI$stress_skala1_sum)
qqline(AE.FS_SCI$stress_skala1_sum, col = "red")

qqnorm(AE.FS_SCI$stress_skala2_sum)
qqline(AE.FS_SCI$stress_skala2_sum, col = "red")

qqnorm(AE.FS_SCI$stress_skala_symptome_sum)
qqline(AE.FS_SCI$stress_skala_symptome_sum, col = "red")



#Prüfung der Multivariaten Normalverteilung:

# Installiere und lade das MVN-Paket
if (!require(MVN)) install.packages("MVN")
library(MVN)

# Multivariate Normalität testen
mvn_results <- mvn(data = AE.FS_SCI[, c("stress_skala1_sum", "stress_skala2_sum", "stress_skala_symptome_sum")], 
                   mvnTest = "mardia")
print(mvn_results)

# Installiere und lade das "biotools"-Paket für den Box's M-Test
if (!require(biotools)) install.packages("biotools")
library(biotools)

# Box's M-Test für Homogenität der Kovarianzmatrizen
box_m_results <- boxM(AE.FS_SCI[, c("stress_skala1_sum", "stress_skala2_sum", "stress_skala_symptome_sum")], AE.FS_SCI$activity_pattern)
print(box_m_results)


#############Normalverteilung nicht gegeben-->Log-Transormation################
AE.FS_SCI$stress_skala1_sum_log <- log(AE.FS_SCI$stress_skala1_sum + 1)  # Log-Transformation
shapiro.test(AE.FS_SCI$stress_skala1_sum_log)  # Test nach der Transformation




# Installiere und lade das MASS-Paket, um die Box-Cox-Transformation anzuwenden
if (!require(MASS)) install.packages("MASS")
library(MASS)

# Box-Cox-Transformation für stress_skala1_sum
boxcox_result <- boxcox(stress_skala1_sum ~ 1, data = AE.FS_SCI)  # Beispiel ohne Gruppenvariable
lambda <- boxcox_result$x[which.max(boxcox_result$y)]  # Optimaler Lambda-Wert

# Transformation der Daten mit dem gefundenen Lambda
AE.FS_SCI$stress_skala1_sum_boxcox <- (AE.FS_SCI$stress_skala1_sum^lambda - 1) / lambda
shapiro.test(AE.FS_SCI$stress_skala1_sum_boxcox)  # Test nach der Transformation






# Beispiel-Daten (hier nehme ich die Stress-Skala 1 als Beispiel)
set.seed(123)  # Für Reproduzierbarkeit
data <- AE.FS_SCI$stress_skala1_sum  # Beispielhafte Variable aus deinem Datensatz

# Funktion zur Berechnung des Mittelwerts (du kannst auch andere Statistiken wie Median verwenden)
bootstrap_function <- function(data, num_resamples = 1000) {
  n <- length(data)  # Anzahl der Beobachtungen
  bootstrap_statistics <- numeric(num_resamples)  # Vektor zur Speicherung der 1000 Statistiken
  
  for (i in 1:num_resamples) {
    # Ziehen mit Zurücklegen
    sample_indices <- sample(1:n, n, replace = TRUE)  # Stichprobe mit Zurücklegen
    bootstrap_sample <- data[sample_indices]  # Erzeuge die Bootstrap-Stichprobe
    bootstrap_statistics[i] <- mean(bootstrap_sample, na.rm = TRUE)  # Berechne den Mittelwert der Stichprobe
  }
  
  return(bootstrap_statistics)  # Gib die 1000 Statistiken zurück
}

# Durchführung des Bootstrappings
num_resamples <- 1000
bootstrap_results <- bootstrap_function(data, num_resamples)

# Berechnung der empirischen Quantile (z.B. 2.5%, Median, 97.5%)
quantiles <- quantile(bootstrap_results, probs = c(0.025, 0.5, 0.975))

# Ausgabe der Quantile
print(quantiles)

# Optionale Visualisierung: Histogramm der Bootstrap-Stichprobenmittelwerte
hist(bootstrap_results, main = "Histogramm der Bootstrap-Mittelwerte", xlab = "Bootstrap Mittelwerte", breaks = 30)
abline(v = mean(data, na.rm = TRUE), col = "red", lwd = 2)  # Ursprünglicher Mittelwert als rote Linie






# Simulierte Daten erstellen (angenommen, die Daten sind in AE.FS_SCI$stress_skala1_sum gespeichert)
set.seed(123)  # Für Reproduzierbarkeit
real_data <- AE.FS_SCI$stress_skala1_sum  # Deine reale Stichprobe

# Berechnung von Mittelwert und Standardabweichung der echten Stichprobe
mean_real <- mean(real_data, na.rm = TRUE)
sd_real <- sd(real_data, na.rm = TRUE)

# Simulieren einer neuen Stichprobe aus einer Normalverteilung mit Mittelwert und SD der echten Daten
simulated_data <- rnorm(length(real_data), mean = mean_real, sd = sd_real)

# 1) QQ-Plot für die simulierten Daten
library(car)
qqPlot(simulated_data, main = "QQ-Plot der simulierten Daten aus normalverteilung")

# 2) QQ-Plot für eine Stichprobe aus einer echten Normalverteilung
real_normal_data <- rnorm(length(real_data))  # Ziehen einer Stichprobe aus der echten Normalverteilung
qqPlot(real_normal_data, main = "QQ-Plot aus echter Normalverteilung")


####################PERMANOVA########################################
# Installiere und lade das vegan-Paket, falls noch nicht installiert
if (!require(vegan)) {
  install.packages("vegan")
}
library(vegan)

# Datenmatrix der Stress-Skalen erstellen
stress_matrix <- AE.FS_SCI[, c("stress_skala1_sum", "stress_skala2_sum", "stress_skala_symptome_sum")]

# Gruppierungsvariable (activity_pattern) als Faktor definieren
activity_groups <- as.factor(AE.FS_SCI$activity_pattern)

# Durchführung der PERMANOVA
permanova_result <- adonis(stress_matrix ~ activity_groups, data = AE.FS_SCI, permutations = 999)

# Ergebnisse anzeigen
print(permanova_result)

# Zeige nur die p-Werte und R²-Werte der Hauptgruppe
print(permanova_result$aov.tab)  # Zeigt die ANOVA-Tabelle an

# Extrahiere die p-Werte und R²-Werte
p_values <- permanova_result$aov.tab[, "Pr(>F)"]
r2_values <- permanova_result$aov.tab[, "R2"]

print(p_values)
print(r2_values)

# Installiere ggplot2, falls noch nicht installiert
if (!require(ggplot2)) install.packages("ggplot2")
library(ggplot2)

# Erstelle einen Boxplot für jede Stress-Skala, unterteilt nach der Gruppierungsvariable activity_pattern
ggplot(AE.FS_SCI, aes(x = activity_pattern, y = stress_skala1_sum)) +
  geom_boxplot(aes(fill = activity_pattern)) +
  labs(title = "Boxplot für stress_skala1_sum nach Activity Pattern",
       x = "Activity Pattern", 
       y = "Stress Skala 1") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Violinplot für jede Stress-Skala, unterteilt nach der Gruppierungsvariable activity_pattern
ggplot(AE.FS_SCI, aes(x = activity_pattern, y = stress_skala1_sum)) +
  geom_violin(aes(fill = activity_pattern), trim = TRUE) +
  labs(title = "Violinplot für stress_skala1_sum nach Activity Pattern",
       x = "Activity Pattern", 
       y = "Stress Skala 1") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Transformiere die Daten in long format für ggplot
library(tidyr)
long_data <- AE.FS_SCI %>%
  gather(key = "Stress_Skala", value = "Wert", stress_skala1_sum, stress_skala2_sum, stress_skala_symptome_sum)

# Boxplot für alle Stress-Skalen
ggplot(long_data, aes(x = activity_pattern, y = Wert, fill = activity_pattern)) +
  geom_boxplot() +
  facet_wrap(~Stress_Skala) +  # Facet für jede Stress-Skala
  labs(title = "Boxplots der Stress-Skalen nach Activity Pattern",
       x = "Activity Pattern", 
       y = "Wert") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Violinplot für alle Stress-Skalen
ggplot(long_data, aes(x = activity_pattern, y = Wert, fill = activity_pattern)) +
  geom_violin(trim = TRUE) +
  facet_wrap(~Stress_Skala) +  # Facet für jede Stress-Skala
  labs(title = "Violinplots der Stress-Skalen nach Activity Pattern",
       x = "Activity Pattern", 
       y = "Wert") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Erforderliche Pakete laden
if (!require(car)) install.packages("car")  # Für qqPlot
library(car)

# Deine echte Datenvariable (z.B. stress_skala1_sum)
real_data <- AE.FS_SCI$stress_skala1_sum  # Beispielhafte Variable aus deinem Datensatz

# Berechnung von Mittelwert und Standardabweichung der echten Stichprobe
mean_real <- mean(real_data, na.rm = TRUE)
sd_real <- sd(real_data, na.rm = TRUE)

# Simulieren einer neuen Stichprobe aus einer Normalverteilung mit Mittelwert und SD der echten Daten
simulated_data <- rnorm(length(real_data), mean = mean_real, sd = sd_real)

# 1) QQ-Plot für die simulierten Daten
qqPlot(simulated_data, main = "QQ-Plot der simulierten Daten aus Normalverteilung")

# 2) QQ-Plot für eine Stichprobe aus einer echten Normalverteilung (mit rnorm)
real_normal_data <- rnorm(length(real_data))  # Ziehen einer Stichprobe aus der echten Normalverteilung
qqPlot(real_normal_data, main = "QQ-Plot der echten Normalverteilung")



#############################################MANOVA #####################################################################
# Gruppierungsvariable (activity_pattern) als Faktor definieren
AE.FS_SCI$activity_pattern <- as.factor(AE.FS_SCI$activity_pattern)

# Datenmatrix für die abhängigen Variablen erstellen
dependent_vars <- AE.FS_SCI[, c("stress_skala1_sum", "stress_skala2_sum", "stress_skala_symptome_sum")]

# MANOVA durchführen
manova_result <- manova(cbind(stress_skala1_sum, stress_skala2_sum, stress_skala_symptome_sum) ~ activity_pattern, data = AE.FS_SCI)

# Ergebnisse der MANOVA anzeigen
summary(manova_result)

# Einzeltests für jede abhängige Variable anzeigen
summary.aov(manova_result)







