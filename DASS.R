str(DASS$V5)
head(DASS$V5)


# Überprüfen der Spalte V5
str(DASS$V5)

# Falls V5 nicht numerisch ist, in numerische Werte umwandeln
# Entfernen von potenziellen nicht-numerischen Zeichen und Umwandeln in numerisch
DASS$V5 <- as.numeric(gsub("[^0-9.-]", "", DASS$V5))

# Überprüfen, ob die Umwandlung erfolgreich war
if (any(is.na(DASS$V5))) {
  warning("Es gibt NAs in der Spalte V5 nach der Umwandlung. Bitte überprüfen Sie die Daten.")
}

# Filtern Sie den Datensatz, um alle Zeilen zu behalten, bei denen V6 nicht 0 ist
DASS <- DASS[DASS$V5 != 0, ]

# Überprüfen Sie die ersten paar Zeilen, um sicherzustellen, dass die Zeilen korrekt gelöscht wurden
head(DASS)

# Erstellen eines Boxplots für die Spalte DASS_Gesamtscore
boxplot(DASS$V5, 
        main = "Boxplot von DASS_Gesamtscore",
        ylab = "DASS Gesamtscore",
        col = "lightblue",
        border = "darkblue",
        notch = TRUE)

# Anzeigen der Boxplot-Statistiken
summary(DASS$V5)

boxplot(DASS$V5, 
        main = "Boxplot von DASS Gesamtscore",
        ylab = "DASS Gesamtscore",
        ylim = c(0, 20),  # y-Achse auf Bereich 0 bis 20 setzen
        col = "lightblue",
        border = "darkblue",
        notch = TRUE)
# Erstellen des Boxplots mit y-Achse von 0 bis 20 und Titel "DASS-Stress"
boxplot(DASS$DASS_Gesamtscore, 
        main = "DASS-Stress",
        ylab = "DASS Gesamtscore",
        ylim = c(0, 20),  # y-Achse auf Bereich 0 bis 20 setzen
        col = "lightblue",
        border = "darkblue",
        notch = TRUE)

# Berechnung des Mittelwerts
mean_score <- mean(DASS$DASS_Gesamtscore, na.rm = TRUE)

# Hinzufügen der Legende rechts oben
legend("topright", legend = paste("Mean:", round(mean_score, 2)), bty = "n")


boxplot(DASS$DASS_Gesamtscore, 
        main = "DASS-Stress",
        ylab = "DASS Gesamtscore",
        ylim = c(0, 20),  # y-Achse auf Bereich 0 bis 20 setzen
        col = "lightblue",
        border = "darkblue",
        notch = TRUE)

# Berechnung des Mittelwerts
mean_score <- mean(DASS$DASS_Gesamtscore, na.rm = TRUE)

# Hinzufügen der Legende oben links
legend("topleft", legend = paste("Mean:", round(mean_score, 2)), bty = "n")

boxplot(DASS$DASS_Gesamtscore, 
        main = "DASS-Stress",
        ylab = "DASS Gesamtscore",
        xlab = "Score",
        ylim = c(0, 20),  # y-Achse auf Bereich 0 bis 20 setzen
        col = "lightblue",
        border = "darkblue",
        notch = TRUE)

# Berechnung des Mittelwerts
mean_score <- mean(DASS$DASS_Gesamtscore, na.rm = TRUE)

# Hinzufügen der Legende oben links
legend("topleft", legend = paste("Mean:", round(mean_score, 2)), bty = "n")

boxplot(DASS$DASS_Gesamtscore, 
        main = "DASS-Stress",
        ylab = "Score",
        ylim = c(0, 20),  # y-Achse auf Bereich 0 bis 20 setzen
        col = "lightblue",
        border = "darkblue",
        notch = TRUE)

# Berechnung des Mittelwerts
mean_score <- mean(DASS$DASS_Gesamtscore, na.rm = TRUE)

# Hinzufügen der Legende oben links
legend("topleft", legend = paste("Mean:", round(mean_score, 2)), bty = "n")

# Filtern der Daten, um nur die Werte zu behalten, die über dem Cut-off von 10 liegen
DASS_above_cutoff <- DASS[DASS$DASS_Gesamtscore > 10, ]

# Anzeigen der ersten paar Zeilen der gefilterten Daten
head(DASS_above_cutoff)

# Zusammenfassung der gefilterten Daten
summary(DASS_above_cutoff$DASS_Gesamtscore)

# Anzahl der Werte, die über dem Cut-off liegen
num_above_cutoff <- nrow(DASS_above_cutoff)
cat("Anzahl der Werte über dem Cut-off von 10:", num_above_cutoff, "\n")

# Filtern der Daten, um nur die Werte zu behalten, die über dem Cut-off von 10 liegen
DASS_above_cutoff <- DASS[DASS$DASS_Gesamtscore > 10, ]

# Erstellen des Boxplots für die gefilterten Werte
boxplot(DASS_above_cutoff$DASS_Gesamtscore, 
        main = "DASS Gesamtscore über dem Cut-off von 10",
        ylab = "DASS Gesamtscore",
        ylim = c(10, 20),  # y-Achse auf Bereich 10 bis 20 setzen
        col = "lightblue",
        border = "darkblue",
        notch = TRUE)

# Berechnung des Mittelwerts der gefilterten Werte
mean_score_above_cutoff <- mean(DASS_above_cutoff$DASS_Gesamtscore, na.rm = TRUE)

# Hinzufügen der Legende oben links
legend("topleft", legend = paste("Mean:", round(mean_score_above_cutoff, 2)), bty = "n")

# Installieren und Laden des ggplot2-Pakets, falls noch nicht installiert
if (!require(ggplot2)) install.packages("ggplot2")
library(ggplot2)

# Erstellen eines Violinplots für die DASS_Gesamtscore
ggplot(DASS, aes(x = "", y = DASS_Gesamtscore)) + 
  geom_violin(fill = "lightblue", color = "darkblue") +
  geom_boxplot(width = 0.1, fill = "white", color = "darkblue") +
  geom_hline(yintercept = 10, linetype = "dashed", color = "red") +
  labs(title = "DASS-Stress",
       y = "DASS Gesamtscore") +
  theme_minimal() +
  theme(axis.title.x = element_blank(), 
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

# Berechnung des Mittelwerts
mean_score <- mean(DASS$DASS_Gesamtscore, na.rm = TRUE)

# Hinzufügen der Legende oben links
ggplot(DASS, aes(x = "", y = DASS_Gesamtscore)) + 
  geom_violin(fill = "lightblue", color = "darkblue") +
  geom_boxplot(width = 0.1, fill = "white", color = "darkblue") +
  geom_hline(yintercept = 10, linetype = "dashed", color = "red") +
  labs(title = "DASS-Stress",
       y = "DASS Gesamtscore") +
  annotate("text", x = 1, y = 19, label = paste("Mean:", round(mean_score, 2)), hjust = 1) +
  theme_minimal() +
  theme(axis.title.x = element_blank(), 
        
        # Erstellen eines Violinplots für die DASS_Gesamtscore
        ggplot(DASS, aes(x = "", y = DASS_Gesamtscore)) + 
          geom_violin(fill = "lightblue", color = "darkblue") +
          geom_boxplot(width = 0.1, fill = "white", color = "darkblue") +
          geom_hline(yintercept = 10, linetype = "dashed", color = "red") +
          labs(title = "DASS-Stress",
               y = "DASS Gesamtscore") +
          theme_minimal() +
          theme(axis.title.x = element_blank(), 
                axis.text.x = element_blank(),
                axis.ticks.x = element_blank())
        
        # Berechnung des Mittelwerts
        mean_score <- mean(DASS$DASS_Gesamtscore, na.rm = TRUE)
        
        # Hinzufügen der Legende oben links
        ggplot(DASS, aes(x = "", y = DASS_Gesamtscore)) + 
          geom_violin(fill = "lightblue", color = "darkblue") +
          geom_boxplot(width = 0.1, fill = "white", color = "darkblue") +
          geom_hline(yintercept = 10, linetype = "dashed", color = "red") +
          labs(title = "DASS-Stress",
               y = "Score") +
          annotate("text", x = 1, y = 19, label = paste("Mean:", round(mean_score, 2)), hjust = 1) +
          theme_minimal() +
          theme(axis.title.x = element_blank(), 
                
                # Installieren und Laden des ggplot2-Pakets, falls noch nicht installiert
                if (!require(ggplot2)) install.packages("ggplot2")
                library(ggplot2)
                
                # Erstellen eines Violinplots für die DASS_Gesamtscore
                ggplot(DASS, aes(x = "", y = DASS_Gesamtscore)) + 
                  geom_violin(fill = "lightblue", color = "darkblue") +
                  geom_boxplot(width = 0.1, fill = "white", color = "darkblue") +
                  geom_hline(yintercept = 10, linetype = "dashed", color = "red") +
                  labs(title = "DASS-Stress",
                       y = "Score") +
                  theme_minimal() +
                  theme(axis.title.x = element_blank(), 
                        axis.text.x = element_blank(),
                        axis.ticks.x = element_blank())
                
                # Berechnung des Mittelwerts
                mean_score <- mean(DASS$DASS_Gesamtscore, na.rm = TRUE)
                
                # Hinzufügen der Legende oben links
                ggplot(DASS, aes(x = "", y = DASS_Gesamtscore)) + 
                  geom_violin(fill = "lightblue", color = "darkblue") +
                  geom_boxplot(width = 0.1, fill = "white", color = "darkblue") +
                  geom_hline(yintercept = 10, linetype = "dashed", color = "red") +
                  labs(title = "DASS-Stress",
                       y = "Score") +
                  annotate("text", x = 1, y = 19, label = paste("Mean:", round(mean_score, 2)), hjust = 1) +
                  theme_minimal() +
                  theme(axis.title.x = element_blank(), 
                        axis.text.x = element_blank(),
                        axis.ticks.x = element_blank())
                
                
                # Erstellen eines Violinplots für die DASS_Gesamtscore mit Anmerkungen
                ggplot(DASS, aes(x = "", y = DASS_Gesamtscore)) + 
                  geom_violin(fill = "lightblue", color = "darkblue") +
                  geom_boxplot(width = 0.1, fill = "white", color = "darkblue") +
                  geom_hline(yintercept = 10, linetype = "dashed", color = "red") +
                  annotate("text", x = 0.9, y = 19.5, label = paste("Mean:", round(mean(DASS$DASS_Gesamtscore, na.rm = TRUE), 2)), hjust = 1) +
                  labs(title = "DASS-Stress",
                       y = "DASS Gesamtscore") +
                  theme_minimal() +
                  theme(axis.title.x = element_blank(), 
                        axis.text.x = element_blank(),
                        axis.ticks.x = element_blank()) +
                  annotate("text", x = 0.85, y = 10.5, label = "Cut-off bei 10", color = "red") +
                  annotate("text", x = 0.85, y = 10, label = "DASS-Score", color = "darkblue") +
                  guides(color = guide_legend(title = NULL))
                
                # Installieren und Laden des ggplot2-Pakets, falls noch nicht installiert
                if (!require(ggplot2)) install.packages("ggplot2")
                library(ggplot2)
                
                # Erstellen eines Violinplots für die DASS_Gesamtscore mit Anmerkungen
                ggplot(DASS, aes(x = "", y = DASS_Gesamtscore)) + 
                  geom_violin(fill = "lightblue", color = "darkblue") +
                  geom_boxplot(width = 0.1, fill = "white", color = "darkblue") +
                  geom_hline(yintercept = 10, linetype = "dashed", color = "red") +
                  annotate("text", x = 0.9, y = max(DASS$DASS_Gesamtscore) + 0.5, label = paste("Mean:", round(mean(DASS$DASS_Gesamtscore, na.rm = TRUE), 2)), hjust = 1) +
                  labs(title = "DASS-Stress",
                       y = "Score") +
                  theme_minimal() +
                  theme(axis.title.x = element_blank(), 
                        axis.text.x = element_blank(),
                        axis.ticks.x = element_blank(),
                        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0))) +
                  annotate("text", x = 1.1, y = 10, label = "Cut-off bei 10", color = "red", hjust = 0) +
                  guides(color = guide_legend(title = NULL))
                
                # Installieren und Laden des ggplot2-Pakets, falls noch nicht installiert
                if (!require(ggplot2)) install.packages("ggplot2")
                library(ggplot2)
                
                # Mittelwert berechnen
                mean_score <- mean(DASS$DASS_Gesamtscore, na.rm = TRUE)
                
                # Erstellen eines Violinplots für die DASS_Gesamtscore mit Anmerkungen
                ggplot(DASS, aes(x = "", y = DASS_Gesamtscore)) + 
                  geom_violin(fill = "lightblue", color = "darkblue") +
                  geom_boxplot(width = 0.1, fill = "white", color = "darkblue", show.legend = FALSE) +
                  geom_hline(yintercept = 10, linetype = "dashed", color = "red") +
                  annotate("text", x = 0.9, y = max(DASS$DASS_Gesamtscore) + 0.5, label = paste("Mean:", round(mean_score, 2)), hjust = 1) +
                  labs(title = "DASS-Stress",
                       y = "Score") +
                  theme_minimal() +
                  theme(axis.title.x = element_blank(), 
                        axis.text.x = element_blank(),
                        axis.ticks.x = element_blank(),
                        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0))) +
                  annotate("text", x = 1.1, y = 10.2, label = "Cut-off bei 10", color = "red", hjust = 0) +
                  guides(color = FALSE)
                
                # Installieren und Laden des ggplot2-Pakets, falls noch nicht installiert
                if (!require(ggplot2)) install.packages("ggplot2")
                library(ggplot2)
                
                # Mittelwert berechnen
                mean_score <- mean(DASS$DASS_Gesamtscore, na.rm = TRUE)
                
                # Erstellen eines Violinplots für die DASS_Gesamtscore mit Anmerkungen
                ggplot(DASS, aes(x = "", y = DASS_Gesamtscore)) + 
                  geom_violin(fill = "lightblue", color = "darkblue") +
                  geom_boxplot(width = 0.1, fill = "white", color = "darkblue", show.legend = FALSE) +
                  geom_hline(yintercept = 10, linetype = "dashed", color = "red") +
                  annotate("text", x = 1.4, y = 10.3, label = "Cut-off bei 10", color = "red", hjust = 0) +
                  annotate("text", x = 1.4, y = max(DASS$DASS_Gesamtscore) + 0.5, label = paste("Mean:", round(mean_score, 2)), hjust = 0) +
                  labs(title = "DASS-Stress",
                       y = "Score") +
                  theme_minimal() +
                  theme(axis.title.x = element_blank(), 
                        axis.text.x = element_blank(),
                        axis.ticks.x = element_blank(),
                        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)))
                
                # Erstellen eines Violinplots für die DASS_Gesamtscore mit Anmerkungen
                ggplot(DASS, aes(x = "", y = DASS_Gesamtscore)) + 
                  geom_violin(fill = "lightblue", color = "darkblue") +
                  geom_boxplot(width = 0.1, fill = "white", color = "darkblue", show.legend = FALSE) +
                  geom_hline(yintercept = 10, linetype = "dashed", color = "red") +
                  annotate("text", x = 1.4, y = 10.3, label = "Cut-off 10", color = "red", hjust = 0) +
                  annotate("text", x = 1.4, y = max(DASS$DASS_Gesamtscore) + 0.5, label = paste("Mean:", round(mean_score, 2)), hjust = 0) +
                  labs(title = "DASS-Stress",
                       y = "Score") +
                  theme_minimal() +
                  theme(axis.title.x = element_blank(), 
                        axis.text.x = element_blank(),
                        axis.ticks.x = element_blank(),
                        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)))
                
                # Installieren und Laden des ggplot2-Pakets, falls noch nicht installiert
                if (!require(ggplot2)) install.packages("ggplot2")
                library(ggplot2)
                
                # Mittelwert und Median berechnen
                mean_score <- mean(DASS$DASS_Gesamtscore, na.rm = TRUE)
                median_score <- median(DASS$DASS_Gesamtscore, na.rm = TRUE)
                
                # Erstellen eines Violinplots für die DASS_Gesamtscore mit Anmerkungen
                ggplot(DASS, aes(x = "", y = DASS_Gesamtscore)) + 
                  geom_violin(fill = "lightblue", color = "darkblue") +
                  geom_boxplot(width = 0.1, fill = "white", color = "darkblue", show.legend = FALSE) +
                  geom_hline(yintercept = 10, linetype = "dashed", color = "red") +
                  annotate("text", x = 0.9, y = max(DASS$DASS_Gesamtscore) + 0.5, label = paste("Mean:", round(mean_score, 2)), hjust = 1) +
                  annotate("text", x = 0.45, y = median_score, label = paste("Median:", median_score), hjust = 0, vjust = 1, color = "darkblue") +
                  labs(title = "DASS-Stress",
                       y = "Score") +
                  theme_minimal() +
                  theme(axis.title.x = element_blank(), 
                        axis.text.x = element_blank(),
                        axis.ticks.x = element_blank(),
                        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0))) +
                  annotate("text", x = 1.1, y = 10.2, label = "Cut-off bei 10", color = "red", hjust = 0) +
                  guides(color = FALSE)
                
                # Installieren und Laden des ggplot2-Pakets, falls noch nicht installiert
                if (!require(ggplot2)) install.packages("ggplot2")
                library(ggplot2)
                
                # Mittelwert und Median berechnen
                mean_score <- mean(DASS$DASS_Gesamtscore, na.rm = TRUE)
                median_score <- median(DASS$DASS_Gesamtscore, na.rm = TRUE)
                
                # Erstellen eines Violinplots für die DASS_Gesamtscore mit Anmerkungen
                ggplot(DASS, aes(x = "", y = DASS_Gesamtscore)) + 
                  geom_violin(fill = "lightblue", color = "darkblue") +
                  geom_boxplot(width = 0.1, fill = "white", color = "darkblue", show.legend = FALSE) +
                  geom_hline(yintercept = 10, linetype = "dashed", color = "red") +
                  annotate("text", x = 1.8, y = 10.5, label = "Cut-off 10", color = "red", hjust = 1) +
                  annotate("text", x = 0.5, y = median_score, label = paste("Median:", median_score), hjust = 0, vjust = 1, color = "darkblue") +
                  annotate("text", x = 1.3, y = max(DASS$DASS_Gesamtscore) + 0.5, label = paste("Mean:", round(mean_score, 2)), hjust = 0) +
                  labs(title = "DASS-Stress",
                       y = "Score") +
                  theme_minimal() +
                  theme(axis.title.x = element_blank(), 
                        axis.text.x = element_blank
                        
                        # Installieren und Laden des ggplot2-Pakets, falls noch nicht installiert
                        if (!require(ggplot2)) install.packages("ggplot2")
                        library(ggplot2)
                        
                        # Mittelwert und Median berechnen
                        mean_score <- mean(DASS$DASS_Gesamtscore, na.rm = TRUE)
                        median_score <- median(DASS$DASS_Gesamtscore, na.rm = TRUE)
                        
                        # Erstellen eines Violinplots für die DASS_Gesamtscore mit Anmerkungen
                        ggplot(DASS, aes(x = "", y = DASS_Gesamtscore)) + 
                          geom_violin(fill = "lightblue", color = "darkblue") +
                          geom_boxplot(width = 0.1, fill = "white", color = "darkblue", show.legend = FALSE) +
                          geom_hline(yintercept = 10, linetype = "dashed", color = "red") +
                          annotate("text", x = 1.5, y = 11, label = "Cut-off 10", color = "red", hjust = 1) +
                          annotate("text", x = 0.7, y = median_score, label = paste("Median:", median_score), hjust = 0, vjust = 1, color = "darkblue") +
                          annotate("text", x = 1.3, y = max(DASS$DASS_Gesamtscore) + 0.5, label = paste("Mean:", round(mean_score, 2)), hjust = 0) +
                          labs(title = "DASS-Stress",
                               y = "Score") +
                          theme_minimal() +
                          theme(axis.title.x = element_blank(), 
                                axis.text.x = element_blank(),
                                axis.ticks.x = element_blank(),
                                axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)))