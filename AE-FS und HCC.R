#Akutdaten AE-FS Gruppe mit höchstem objektivem Stress (HCC)#
#28.06.2024#

# Laden des Pakets
library(dplyr)

# Entfernen der Spalte V2-V4 (brauche ich nicht) 
HCC_AE.FS [2:4] <- list(NULL)

# Manuelles Zuweisen der Spaltennamen, falls bekannt
colnames(HCC_AE.FS) <- c("study_id","cortisol1","cortisol2","cortisone1","cortisone2","hairmass1","hairmass2","score_pps","dms_score")

# Löschen der ersten Zeile
HCC_AE.FS <- HCC_AE.FS [-1, ]

# Spalten definieren, in denen leere Stellen mit NA ersetzt werden sollen
columns_to_fill <- c("cortisol1", "cortisol2", "cortisone1", "cortisone2", "hairmass1", "hairmass2","score_pps","dms_score")

# Leere Stellen (repräsentiert als leere Strings oder explizite NAs) in diesen Spalten mit NA ersetzen
HCC_AE.FS[columns_to_fill] <- lapply(HCC_AE.FS[columns_to_fill], function(x) {
  x[x == "" | is.na(x)] <- NA
  return(x)
})


#FAR Gruppe bilden mit den if_else Vorgaben
HCC_AE.FS$FAR <- ifelse(HCC_AE.FS$score_pps < 3 & HCC_AE.FS$dms_score == 2, TRUE, FALSE)

#DER Gruppe bilden mit den if_else Vorgaben
HCC_AE.FS$DER <- ifelse(HCC_AE.FS$score_pps >= 3 & HCC_AE.FS$dms_score == 2, TRUE, FALSE)

#EER Gruppe bilden mit den if_else Vorgaben
HCC_AE.FS$EER <- ifelse(HCC_AE.FS$score_pps >= 3 & HCC_AE.FS$dms_score < 2, TRUE, FALSE)

#AR Gruppe bilden mit den if_else Vorgaben
HCC_AE.FS$AR <- ifelse(HCC_AE.FS$score_pps < 3 & HCC_AE.FS$dms_score < 2, TRUE, FALSE)

#neue Spalten mit den Summen bilden
HCC_AE.FS$Anzahl_FAR <- sum(HCC_AE.FS$FAR)
HCC_AE.FS$Anzahl_DER <- sum(HCC_AE.FS$DER)
HCC_AE.FS$Anzahl_EER <- sum(HCC_AE.FS$EER)
HCC_AE.FS$Anzahl_AR <- sum(HCC_AE.FS$AR)

# Beispiel-Daten
groups <- c("FAR", "DER", "EER", "AR")
summen <- c(HCC_AE.FS$Anzahl_FAR, HCC_AE.FS$Anzahl_DER, HCC_AE.FS$Anzahl_EER, HCC_AE.FS$Anzahl_AR)
summen_Zahlen <- c(7, 24, 64, 28)



# Sicherstellen, dass die Spalten numerisch sind
HCC_AE.FS$FAR <- as.numeric(HCC_AE.FS$FAR)
HCC_AE.FS$DER <- as.numeric(HCC_AE.FS$DER)
HCC_AE.FS$EER <- as.numeric(HCC_AE.FS$EER)
HCC_AE.FS$AR <- as.numeric(HCC_AE.FS$AR)
HCC_AE.FS$cortisol1 <- as.numeric(HCC_AE.FS$cortisol1)
HCC_AE.FS$cortisol2 <- as.numeric(HCC_AE.FS$cortisol2)
HCC_AE.FS$cortisone1 <- as.numeric(HCC_AE.FS$cortisone1)
HCC_AE.FS$cortisone2 <- as.numeric(HCC_AE.FS$cortisone2)


# Erstellen der Gruppenzuweisung
HCC_AE.FS <- HCC_AE.FS %>%
  mutate(activity_pattern = case_when(
    score_pps < 3 & dms_score == 2 ~ "FAR",
    score_pps >= 3 & dms_score == 2 ~ "DER",
    score_pps >= 3 & dms_score < 2 ~ "EER",
    score_pps < 3 & dms_score < 2 ~ "AR",
    TRUE ~ NA_character_
  ))
# Überprüfen, ob die Spalte hinzugefügt wurde und ob die Gruppenzuweisung korrekt ist
head(HCC_AE.FS)
table(HCC_AE.FS$activity_pattern)

# Berechnung des Mittelwerts, Medians und der Standardabweichung für jede HCC Spalte
grouped_cortisol_skala1 <- HCC_AE.FS %>%
  group_by(HCC_AE.FS$activity_pattern) %>%
  summarize(mean_cortisol1 = mean(HCC_AE.FS$cortisol1, na.rm = TRUE),
            sd_cortisol1 = sd(HCC_AE.FS$cortisol1, na.rm = TRUE),
            median_cortisol1 = median(HCC_AE.FS$cortisol1, na.rm = TRUE))

grouped_cortisol_skala2 <- HCC_AE.FS %>%
  group_by(HCC_AE.FS$activity_pattern) %>%
  summarize(mean_cortisol2 = mean(HCC_AE.FS$cortisol2, na.rm = TRUE),
            sd_cortisol2 = sd(HCC_AE.FS$cortisol2, na.rm = TRUE),
            median_cortisol2 = median(HCC_AE.FS$cortisol2, na.rm = TRUE))

grouped_cortisone_skala1 <- HCC_AE.FS %>%
  group_by(HCC_AE.FS$activity_pattern) %>%
  summarize(mean_cortisone1 = mean(HCC_AE.FS$cortisone1, na.rm = TRUE),
            sd_cortisone1 = sd(HCC_AE.FS$cortisone1, na.rm = TRUE),
            median_cortisone1 = median(HCC_AE.FS$cortisone1, na.rm = TRUE))

grouped_cortisone_skala2 <- HCC_AE.FS %>%
  group_by(HCC_AE.FS$activity_pattern) %>%
  summarize(mean_cortisone2 = mean(HCC_AE.FS$cortisone2, na.rm = TRUE),
            sd_cortisone2 = sd(HCC_AE.FS$cortisone2, na.rm = TRUE),
            median_cortisone2 = median(HCC_AE.FS$cortisone2, na.rm = TRUE))

library(dplyr)


# Berechnung des Mittelwerts, Medians und der Standardabweichung für jede Gruppe
grouped_stats <- HCC_AE.FS %>%
  group_by(activity_pattern) %>%
  summarize(
    mean_cortisol1 = mean(cortisol1, na.rm = TRUE),
    sd_cortisol1 = sd(cortisol1, na.rm = TRUE),
    median_cortisol1 = median(cortisol1, na.rm = TRUE),
    mean_cortisol2 = mean(cortisol2, na.rm = TRUE),
    sd_cortisol2 = sd(cortisol2, na.rm = TRUE),
    median_cortisol2 = median(cortisol2, na.rm = TRUE),
    mean_cortisone1 = mean(cortisone1, na.rm = TRUE),
    sd_cortisone1 = sd(cortisone1, na.rm = TRUE),
    median_cortisone1 = median(cortisone1, na.rm = TRUE),
    mean_cortisone2 = mean(cortisone2, na.rm = TRUE),
    sd_cortisone2 = sd(cortisone2, na.rm = TRUE),
    median_cortisone2 = median(cortisone2, na.rm = TRUE)
  )

# ANOVA durchführen
anova_results <- list(
  cortisol1 = aov(cortisol1 ~ activity_pattern, data = HCC_AE.FS),
  cortisol2 = aov(cortisol2 ~ activity_pattern, data = HCC_AE.FS),
  cortisone1 = aov(cortisone1 ~ activity_pattern, data = HCC_AE.FS),
  cortisone2 = aov(cortisone2 ~ activity_pattern, data = HCC_AE.FS)
)

# ANOVA Ergebnisse zusammenfassen
anova_summaries <- lapply(anova_results, summary)

# Überprüfung und weitere Aktionen, z.B. Ergebnisse anzeigen oder exportieren
print(grouped_stats)
print(anova_summaries)



library(dplyr)
library(openxlsx)
library(stats)  # Für aov() und TukeyHSD()

# Erstellen einer neuen Excel-Datei
wb <- createWorkbook()

# Definieren der Variablen und deren Beschreibungen
variables <- c("cortisol1", "cortisol2", "cortisone1", "cortisone2")
descriptions <- c("Cortisol1", "Cortisol2", "Cortisone1", "Cortisone2")

# Durchführung der ANOVA und Tukey's HSD für jede Variable
for (i in seq_along(variables)) {
  var <- variables[i]
  desc <- descriptions[i]
  
  # ANOVA durchführen
  anova_result <- aov(reformulate("activity_pattern", response = var), data = HCC_AE.FS)
  summary(anova_result)  # Optional: Ausgabe zur Überprüfung
  
  # Tukey's HSD Test
  tukey_result <- TukeyHSD(anova_result, "activity_pattern")
  tukey_df <- as.data.frame(tukey_result[["activity_pattern"]])
  
  # Arbeitsblatt für Tukey HSD Ergebnisse hinzufügen
  sheet_name <- paste("Tukey HSD", desc)
  addWorksheet(wb, sheet_name)
  writeData(wb, sheet_name, tukey_df)
}

# Speichern der Workbook
saveWorkbook(wb, "Tukey_HSD_Results_All_Variables.xlsx", overwrite = TRUE)

# Ausgabe des Speicherpfads
print("Die Tukey HSD Testergebnisse wurden gespeichert unter: Tukey_HSD_Results_All_Variables.xlsx")



library(dplyr)
library(openxlsx)
library(stats)

# Erstelle die Excel-Datei erneut
wb <- createWorkbook()

# Berechne ANOVA, Tukey HSD und Effektgrößen für jede Variable
for (var in c("cortisol1", "cortisol2", "cortisone1", "cortisone2")) {
  # Durchführen der ANOVA
  anova_result <- aov(reformulate("activity_pattern", response = var), data = HCC_AE.FS)
  
  # Tukey HSD Test
  tukey_result <- TukeyHSD(anova_result, "activity_pattern")
  tukey_df <- as.data.frame(tukey_result[["activity_pattern"]])
  
  # Berechnung der Effektgrößen
  sum_sq <- summary(anova_result)[[1]]$`Sum Sq`
  total_sum_sq <- sum(sum_sq)
  df <- summary(anova_result)[[1]]$Df
  ms_error <- sum_sq[2] / df[2]
  ss_between <- sum_sq[1]
  
  eta_squared <- ss_between / total_sum_sq
  omega_squared <- (ss_between - (df[1] * ms_error)) / (total_sum_sq + ms_error)
  
  # Effektgrößen in einem DataFrame speichern
  effect_size_df <- data.frame(
    Eta_Squared = eta_squared,
    Omega_Squared = omega_squared
  )
  
  # Arbeitsblätter in der Excel-Datei hinzufügen
  addWorksheet(wb, paste(var, "ANOVA"))
  writeData(wb, paste(var, "ANOVA"), summary(anova_result)[[1]])
  
  addWorksheet(wb, paste(var, "Tukey HSD"))
  writeData(wb, paste(var, "Tukey HSD"), tukey_df)
  
  addWorksheet(wb, paste(var, "Effect Sizes"))
  writeData(wb, paste(var, "Effect Sizes"), effect_size_df)
}

# Speichern der Workbook
saveWorkbook(wb, "Complete_Statistical_Analysis.xlsx", overwrite = TRUE)
print("Die Ergebnisse der statistischen Analyse wurden in 'Complete_Statistical_Analysis.xlsx' gespeichert.")


# Berechnung des Mittelwerts, Medians und der Standardabweichung für jede Gruppe
grouped_stats <- HCC_AE.FS %>%
  group_by(activity_pattern) %>%
  summarize(
    mean_cortisol1 = mean(cortisol1, na.rm = TRUE),
    sd_cortisol1 = sd(cortisol1, na.rm = TRUE),
    median_cortisol1 = median(cortisol1, na.rm = TRUE),
    mean_cortisol2 = mean(cortisol2, na.rm = TRUE),
    sd_cortisol2 = sd(cortisol2, na.rm = TRUE),
    median_cortisol2 = median(cortisol2, na.rm = TRUE),
    mean_cortisone1 = mean(cortisone1, na.rm = TRUE),
    sd_cortisone1 = sd(cortisone1, na.rm = TRUE),
    median_cortisone1 = median(cortisone1, na.rm = TRUE),
    mean_cortisone2 = mean(cortisone2, na.rm = TRUE),
    sd_cortisone2 = sd(cortisone2, na.rm = TRUE),
    median_cortisone2 = median(cortisone2, na.rm = TRUE)
  )
# Identifizieren der Gruppe mit den höchsten Durchschnittswerten
max_values <- grouped_stats %>%
  summarize(
    Highest_Mean_Cortisol1 = max(mean_cortisol1),
    Highest_Group_Cortisol1 = activity_pattern[which.max(mean_cortisol1)],
    Highest_Mean_Cortisol2 = max(mean_cortisol2),
    Highest_Group_Cortisol2 = activity_pattern[which.max(mean_cortisol2)],
    Highest_Mean_Cortisone1 = max(mean_cortisone1),
    Highest_Group_Cortisone1 = activity_pattern[which.max(mean_cortisone1)],
    Highest_Mean_Cortisone2 = max(mean_cortisone2),
    Highest_Group_Cortisone2 = activity_pattern[which.max(mean_cortisone2)]
  )
# Ausgabe der Ergebnisse
print(grouped_stats)
print(max_values)


library(openxlsx)

# Erstellen einer neuen Excel-Datei
wb <- createWorkbook()

# Hinzufügen eines Arbeitsblatts für die Gruppenstatistiken
addWorksheet(wb, "Grouped Stats")
writeData(wb, "Grouped Stats", grouped_stats)

# Hinzufügen eines Arbeitsblatts für die Maximalwerte
addWorksheet(wb, "Max Values")
writeData(wb, "Max Values", max_values)

# Speichern der Excel-Datei
file_path <- "HCC_AE_FS_Results.xlsx"
saveWorkbook(wb, file_path, overwrite = TRUE)

# Ausgabe des Speicherpfads
print(paste("Die Ergebnisse wurden gespeichert unter:", file_path))



library(ggplot2)

# Boxplot für Cortisol1
p1 <- ggplot(HCC_AE.FS, aes(x = activity_pattern, y = cortisol1, fill = activity_pattern)) +
  geom_boxplot() +
  labs(title = "Boxplot of Cortisol1 by Activity Pattern", x = "Activity Pattern", y = "Cortisol1 Levels") +
  theme_minimal()

# Boxplot für Cortisol2
p2 <- ggplot(HCC_AE.FS, aes(x = activity_pattern, y = cortisol2, fill = activity_pattern)) +
  geom_boxplot() +
  labs(title = "Boxplot of Cortisol2 by Activity Pattern", x = "Activity Pattern", y = "Cortisol2 Levels") +
  theme_minimal()

# Anzeigen der Plots
print(p1)
print(p2)

# Optional: Speichern der Plots als PNG-Dateien
ggsave("Boxplot_Cortisol1.png", plot = p1, width = 10, height = 8, dpi = 300)
ggsave("Boxplot_Cortisol2.png", plot = p2, width = 10, height = 8, dpi = 300)




# Boxplot mit beschränkten Y-Achsen-Limits wegen Ausreisser
ggplot(HCC_AE.FS, aes(x = activity_pattern, y = cortisol1, fill = activity_pattern)) +
  geom_boxplot() +
  ylim(0, 25) +  # Anpassen je nach Datenbereich
  labs(title = "Boxplot of Cortisol1 by Activity Pattern", x = "Activity Pattern", y = "Cortisol1 Levels") +
  theme_minimal()


#plot ohne die NA


# Daten vorbereiten, ausschließen von NAs in der Gruppenvariable
filtered_data <- HCC_AE.FS %>%
  filter(!is.na(activity_pattern)) %>%
  filter(activity_pattern != "NA")  # Falls "NA" als Zeichenkette existiert

# Boxplot für Cortisol1 ohne NA Gruppen und mit Y-Achsenlimit
p1 <- ggplot(filtered_data, aes(x = activity_pattern, y = cortisol1, fill = activity_pattern)) +
  geom_boxplot() +
  ylim(0, 25) +  # Y-Achsen-Limits setzen
  labs(title = "Boxplot of Cortisol1 by Activity Pattern", x = "Activity Pattern", y = "Cortisol1 Levels") +
  theme_minimal()

# Boxplot für Cortisol2 ohne NA Gruppen und mit Y-Achsenlimit
p2 <- ggplot(filtered_data, aes(x = activity_pattern, y = cortisol2, fill = activity_pattern)) +
  geom_boxplot() +
  ylim(0, 25) +  # Y-Achsen-Limits setzen
  labs(title = "Boxplot of Cortisol2 by Activity Pattern", x = "Activity Pattern", y = "Cortisol2 Levels") +
  theme_minimal()

# Anzeigen der Plots
print(p1)
print(p2)

# Optional: Speichern der Plots als PNG-Dateien
ggsave("Boxplot_Cortisol1_with_limits.png", plot = p1, width = 10, height = 8, dpi = 300)
ggsave("Boxplot_Cortisol2_with_limits.png", plot = p2, width = 10, height = 8, dpi = 300)






#Ausgabe der Daten der effect sizes mit Angabe der Gruppen AE-FS:
library(dplyr)
library(openxlsx)

# Durchführen der ANOVA für jede Variable und Speichern der Ergebnisse
variables <- c("cortisol1", "cortisol2", "cortisone1", "cortisone2")
anova_results <- list()
effect_sizes <- list()

for (var in variables) {
  # ANOVA durchführen
  anova_result <- aov(reformulate("activity_pattern", response = var), data = HCC_AE.FS)
  anova_summary <- summary(anova_result)
  
  # Effektgrößen berechnen
  sum_sq <- anova_summary[[1]]$`Sum Sq`
  total_sum_sq <- sum(sum_sq)
  df <- anova_summary[[1]]$Df
  ms_error <- sum_sq[2] / df[2]
  ss_between <- sum_sq[1]
  
  eta_squared <- ss_between / total_sum_sq
  omega_squared <- (ss_between - (df[1] * ms_error)) / (total_sum_sq + ms_error)
  
  # Effektgrößen und Gruppenbezeichnungen speichern
  effect_sizes[[var]] <- data.frame(
    Variable = var,
    Eta_Squared = eta_squared,
    Omega_Squared = omega_squared,
    Group = "All Groups"
  )
  
  # Ergebnisse anzeigen
  print(do.call(rbind, effect_sizes))
  
  # ANOVA Zusammenfassung speichern
  anova_results[[var]] <- anova_summary[[1]]
}

# Excel-Datei erstellen und Daten exportieren
wb <- createWorkbook()
addWorksheet(wb, "ANOVA Results and Effect Sizes")
row_counter <- 1

for (var in variables) {
  writeData(wb, "ANOVA Results and Effect Sizes", anova_results[[var]], startRow = row_counter, startCol = 1)
  row_counter <- row_counter + nrow(anova_results[[var]]) + 2  # Platz für die nächste Tabelle lassen
  writeData(wb, "ANOVA Results and Effect Sizes", effect_sizes[[var]], startRow = row_counter, startCol = 1)
  row_counter <- row_counter + nrow(effect_sizes[[var]]) + 2
}

# Excel-Datei speichern
saveWorkbook(wb, "Complete_ANOVA_and_Effect_Sizes.xlsx", overwrite = TRUE)
print("Die ANOVA-Ergebnisse und Effektgrößen wurden gespeichert unter: Complete_ANOVA_and_Effect_Sizes.xlsx")





#Tukey post hoc Ausgabe mit Angabe der AE-FS Gruppenzugehörigkeit:

library(openxlsx)
library(stats)
library(dplyr)

# Erstellen einer neuen Excel-Datei
wb <- createWorkbook()
addWorksheet(wb, "Tukey HSD Results")

# Variablen definieren, für die Tukey's HSD durchgeführt wird
variables <- c("cortisol1", "cortisol2")
start_row <- 1

# Tukey's HSD für jede Variable durchführen und Ergebnisse speichern
for (var in variables) {
  # ANOVA durchführen
  anova_result <- aov(reformulate("activity_pattern", response = var), data = HCC_AE.FS)
  
  # Tukey's HSD Test
  tukey_result <- TukeyHSD(anova_result, "activity_pattern")
  tukey_df <- as.data.frame(tukey_result[["activity_pattern"]], stringsAsFactors = FALSE)
  tukey_df$Comparison <- rownames(tukey_df)
  tukey_df <- tukey_df %>%
    mutate(Group1 = sub("-.*", "", Comparison),
           Group2 = sub(".*-", "", Comparison)) %>%
    select(Group1, Group2, diff, lwr, upr, `p adj`)
  
  # Ergebnisse in das Arbeitsblatt schreiben, ohne headerStyle zu setzen
  writeData(wb, "Tukey HSD Results", tukey_df, startRow = start_row, colNames = TRUE)
  start_row <- start_row + nrow(tukey_df) + 2  # Platz für nächste Ergebnisse
}
  


# Angeben eines expliziten Speicherpfads
file_path <- "U:/PhD/LongNeck/Statistik/R Berechnungen/Akutdaten/Akutdaten R Skripte/Tukey_HSD_Results.xlsx"
saveWorkbook(wb, file_path, overwrite = TRUE)
print(paste("Die Tukey HSD Testergebnisse wurden gespeichert unter:", file_path))

  
