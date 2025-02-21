#Akutdaten AE-FS Gruppenbildung#
#28.06.2024#

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


#Barplot mit y-Achse bis 70
# Ränder der Grafik anpassen, links größer
par(mar = c(5, 5, 4, 2))

# Barplot erstellen und x-Positionen der Balken speichern
bar_positions <- barplot(summen_Zahlen, names.arg = groups, col = "skyblue", main = "AE-FS-groups", ylab = "quantity", ylim = c(0, 70))

# Text mit den Summen mittig über den Balken platzieren
text(x = bar_positions, y = summen_Zahlen, labels = summen_Zahlen, pos = 3, col = "black", cex = 0.8, offset = 0.5)


#Prozente ausgeben für die Gruppen:
# Gesamtanzahl der Personen berechnen
total_count <- nrow(AE.FS)

# Prozentanteile der Gruppen berechnen
percent_FAR <- sum(AE.FS$FAR) / total_count * 100
percent_DER <- sum(AE.FS$DER) / total_count * 100
percent_EER <- sum(AE.FS$EER) / total_count * 100
percent_AR <- sum(AE.FS$AR) / total_count * 100

# Ergebnisse anzeigen
percent_FAR
percent_DER
percent_EER
percent_AR

# Erstellen eines DataFrames mit den Ergebnissen
groups <- c("FAR", "DER", "EER", "AR")
percentages <- c(percent_FAR, percent_DER, percent_EER, percent_AR)
results_df <- data.frame(Group = groups, Percentage = percentages)

# Anzeige der Ergebnisse
print(results_df)


# Barplot mit Prozentanteilen erstellen
par(mar = c(5, 5, 4, 2))
bar_positions <- barplot(percentages, names.arg = groups, col = "skyblue", main = "AE-FS-groups", ylab = "Percentage", ylim = c(0, 60))
text(x = bar_positions, y = percentages, labels = round(percentages, 1), pos = 3, col = "black", cex = 0.8, offset = 0.5)




#Farben gleich wie in anderem Plot anpassen:
# Ränder der Grafik anpassen, links größer
par(mar = c(5, 5, 4, 2))

# Farben für die Gruppen festlegen
group_colors <- c("grey", "skyblue", "plum", "olivedrab1")  # Farben für FAR, DER, EER, AR

# Barplot mit y-Achse bis 70 erstellen und x-Positionen der Balken speichern
bar_positions <- barplot(summen_Zahlen, 
                         names.arg = groups, 
                         col = group_colors,  # Verwende den Vektor mit den Farben
                         main = "AE-FS-groups", 
                         ylab = "quantity", 
                         ylim = c(0, 70))

# Text mit den Summen mittig über den Balken platzieren
text(x = bar_positions, y = summen_Zahlen, labels = summen_Zahlen, pos = 3, col = "black", cex = 0.8, offset = 0.5)


# Prozente ausgeben für die Gruppen:
# Gesamtanzahl der Personen berechnen
total_count <- nrow(AE.FS)

# Prozentanteile der Gruppen berechnen
percent_FAR <- sum(AE.FS$FAR) / total_count * 100
percent_DER <- sum(AE.FS$DER) / total_count * 100
percent_EER <- sum(AE.FS$EER) / total_count * 100
percent_AR <- sum(AE.FS$AR) / total_count * 100

# Ergebnisse anzeigen
percent_FAR
percent_DER
percent_EER
percent_AR

# Erstellen eines DataFrames mit den Ergebnissen
groups <- c("FAR", "DER", "EER", "AR")
percentages <- c(percent_FAR, percent_DER, percent_EER, percent_AR)
results_df <- data.frame(Group = groups, Percentage = percentages)

# Anzeige der Ergebnisse
print(results_df)

# Barplot mit Prozentanteilen und angepassten Farben erstellen
par(mar = c(5, 5, 4, 2))
bar_positions <- barplot(percentages, 
                         names.arg = groups, 
                         col = group_colors,  # Verwende den Vektor mit den Farben
                         main = "Frequencies of activity patterns", 
                         ylab = "Percentage", 
                         ylim = c(0, 60))

# Prozente mittig über den Balken platzieren
text(x = bar_positions, y = percentages, labels = round(percentages, 1), pos = 3, col = "black", cex = 0.8, offset = 0.5)









######################################################################################################################################
##########################Vergleich meiner activity patterns mit anderen Studien#################################################


# Beispiel-Datenrahmen mit Gruppen und Variablen
data <- data.frame(
  group = c("Fehrmann et al. 2017", "Hasenbring et al. 2012", "Plaas et al. 2014", "Titze et al. 2021", "Morf et al. (current study)"), 
  DER = c(34.00,19.20,18.40,20.10,19.50), # Prozentwerte für DER
  EER = c(17.00,16.40,12.20,47.00,52.00),   # Prozentwerte für EER
  FAR = c(24.00,9.60,8.20,9.80,5.70),   # Prozentwerte für FAR
  AR = c(25.00,54.80,61.20,23.10,22.80)     # Prozentwerte für AR
)

# Zusätzliche Infos für jede Studie, Morf mit "(current study)"
info <- c("Chronic LBP", "Subacute LBP", "LBP after disc surgery", "Athletes with LBP", "Acute NP")

# Kombiniere die Studiennamen mit den zusätzlichen Informationen
data$group <- paste(data$group, "\n", info)

# Reihenfolge der Gruppen festlegen (von Morf bis Fehrmann)
data$group <- factor(data$group, 
                     levels = c("Morf et al. (current study) \n Acute NP", 
                                "Titze et al. 2021 \n Athletes with LBP", 
                                "Plaas et al. 2014 \n LBP after disc surgery", 
                                "Hasenbring et al. 2012 \n Subacute LBP", 
                                "Fehrmann et al. 2017 \n Chronic LBP"))

# Daten in das richtige Format bringen (long format) mit tidyr
library(tidyr)
data_long <- data %>%
  pivot_longer(cols = c("DER", "EER", "FAR", "AR"), names_to = "variable", values_to = "value")

# Reihenfolge der Activity Patterns explizit festlegen
data_long$variable <- factor(data_long$variable, levels = c("AR", "FAR", "EER", "DER"))

# Gestapeltes Balkendiagramm erstellen
library(ggplot2)
ggplot(data_long, aes(x = group, y = value, fill = variable)) +
  geom_bar(stat = "identity") +
  labs(x = "",            # Bezeichnung der x-Achse
       y = "Frequency in %",   # Bezeichnung der y-Achse
       fill = "Activity Pattern") +  # Ändert den Legendentitel auf "Activity Pattern"
  scale_fill_manual(values = c("DER" = "skyblue", 
                               "EER" = "plum", 
                               "FAR" = "grey", 
                               "AR" = "olivedrab1"),
                    labels = c("AR" = "AR: Adaptive Response", 
                               "FAR" = "FAR: Fear Avoidance Response", 
                               "EER" = "EER: Eustress Endurance Response", 
                               "DER" = "DER: Distress Endurance Response")) +  # Angepasste Labels in der Legende
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text.y = element_text(size = 10, hjust = 0)) +  # Linksjustierung der Y-Achse
  coord_flip()  # Tausch der Achsen (horizontale Balken)







######################################################################################################################################
##########################Vergleich meiner activity patterns mit anderen Studien#################################################
#NEU: OHNE ABKÜRZUNGEN
#16.12.24


# Beispiel-Datenrahmen mit Gruppen und Variablen
data <- data.frame(
  group = c("Fehrmann et al. 2017", "Hasenbring et al. 2012", "Plaas et al. 2014", "Titze et al. 2021", "Morf et al. (current study)"), 
  DER = c(34.00,19.20,18.40,20.10,19.50), # Prozentwerte für DER
  EER = c(17.00,16.40,12.20,47.00,52.00),   # Prozentwerte für EER
  FAR = c(24.00,9.60,8.20,9.80,5.70),   # Prozentwerte für FAR
  AR = c(25.00,54.80,61.20,23.10,22.80)     # Prozentwerte für AR
)

# Zusätzliche Infos für jede Studie, Morf mit "(current study)"
info <- c("Chronic LBP", "Subacute LBP", "LBP after disc surgery", "Athletes with LBP", "Acute NP")

# Kombiniere die Studiennamen mit den zusätzlichen Informationen
data$group <- paste(data$group, "\n", info)

# Reihenfolge der Gruppen festlegen (von Morf bis Fehrmann)
data$group <- factor(data$group, 
                     levels = c("Morf et al. (current study) \n Acute NP", 
                                "Titze et al. 2021 \n Athletes with LBP", 
                                "Plaas et al. 2014 \n LBP after disc surgery", 
                                "Hasenbring et al. 2012 \n Subacute LBP", 
                                "Fehrmann et al. 2017 \n Chronic LBP"))

# Umbenennen der Variablen (DER, EER, FAR, AR) in die neuen Namen
names(data)[2:5] <- c("Distress Persistence", "Eustress Persistence", "Fear Avoidance", "Activity Pacing")

# Daten in das richtige Format bringen (long format) mit tidyr
library(tidyr)
data_long <- data %>%
  pivot_longer(cols = c("Distress Persistence", "Eustress Persistence", "Fear Avoidance", "Activity Pacing"), 
               names_to = "variable", values_to = "value")

# Reihenfolge der Activity Patterns explizit festlegen
data_long$variable <- factor(data_long$variable, levels = c("Activity Pacing", "Fear Avoidance", "Eustress Persistence", "Distress Persistence"))

# Gestapeltes Balkendiagramm erstellen
library(ggplot2)
ggplot(data_long, aes(x = group, y = value, fill = variable)) +
  geom_bar(stat = "identity") +
  labs(x = "",            # Bezeichnung der x-Achse
       y = "Frequency in %",   # Bezeichnung der y-Achse
       fill = "Activity Pattern") +  # Ändert den Legendentitel auf "Activity Pattern"
  scale_fill_manual(values = c("Distress Persistence" = "skyblue", 
                               "Eustress Persistence" = "plum", 
                               "Fear Avoidance" = "grey", 
                               "Activity Pacing" = "olivedrab1"),
                    labels = c("Activity Pacing", 
                               "Fear Avoidance", 
                               "Eustress Persistence", 
                               "Distress Persistence")) +  # Angepasste Labels in der Legende
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text.y = element_text(size = 10, hjust = 0)) +  # Linksjustierung der Y-Achse
  coord_flip()  # Tausch der Achsen (horizontale Balken)



