#10.09.24
#plot für Visualisierung HCC Daten Probanden im Vergleich mit Referenzwerten
# Installiere ggplot2 und gridExtra, falls sie noch nicht installiert sind
if (!require(ggplot2)) install.packages("ggplot2")
if (!require(gridExtra)) install.packages("gridExtra")

# Lade ggplot2 und gridExtra
library(ggplot2)
library(gridExtra)

# Entfernen der Spalte V2-V4 (brauche ich nicht) 
HCC_ohne.Ausreisser [2:4] <- list(NULL)

# Manuelles Zuweisen der Spaltennamen, falls bekannt
colnames(HCC_ohne.Ausreisser) <- c("study_id","cortisol1","cortisol2","cortisone1","cortisone2","hairmass1","hairmass2","age","gender")

# Löschen der ersten Zeile
HCC_ohne.Ausreisser <- HCC_ohne.Ausreisser [-1, ]

# Spalten definieren, in denen leere Stellen mit NA ersetzt werden sollen
columns_to_fill <- c("cortisol1", "cortisol2", "cortisone1", "cortisone2", "hairmass1", "hairmass2")

# Leere Stellen (repräsentiert als leere Strings oder explizite NAs) in diesen Spalten mit NA ersetzen
HCC_ohne.Ausreisser[columns_to_fill] <- lapply(HCC_ohne.Ausreisser[columns_to_fill], function(x) {
  x[x == "" | is.na(x)] <- NA
  return(x)
})

# Sicherstellen, dass die Werte numerisch sind
HCC_ohne.Ausreisser$cortisol1 <- as.numeric(HCC_ohne.Ausreisser$cortisol1)
HCC_ohne.Ausreisser$cortisol2 <- as.numeric(HCC_ohne.Ausreisser$cortisol2)


library(ggplot2)

# Beispiel-Daten
probanden_data <- HCC_ohne.Ausreisser

library(ggplot2)

# Boxplot
ggplot(data = probanden_data, aes(x = as.factor(gender), y = cortisol1, fill = as.factor(gender))) +
  geom_boxplot(alpha = 0.5) +
  scale_x_discrete(labels = c("1" = "Male", "2" = "Female")) +
  scale_fill_manual(values = c("1" = "blue", "2" = "pink"), labels = c("Male", "Female")) +
  labs(title = "Boxplot of Cortisol Levels",
       x = "Gender",
       y = "Cortisol Levels",
       fill = "Gender") +
  theme_minimal()

# Violinplot
ggplot(data = probanden_data, aes(x = as.factor(gender), y = cortisol1, fill = as.factor(gender))) +
  geom_violin(alpha = 0.5) +
  scale_x_discrete(labels = c("1" = "Male", "2" = "Female")) +
  scale_fill_manual(values = c("1" = "blue", "2" = "pink"), labels = c("Male", "Female")) +
  labs(title = "Violin Plot of Cortisol Levels",
       x = "Gender",
       y = "Cortisol Levels",
       fill = "Gender") +
  theme_minimal()

# Density Plot
ggplot(data = probanden_data, aes(x = cortisol1, fill = as.factor(gender))) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("1" = "blue", "2" = "pink"), labels = c("Male", "Female")) +
  labs(title = "Density Plot of Cortisol Levels",
       x = "Cortisol Levels",
       y = "Density",
       fill = "Gender") +
  scale_x_continuous(limits = c(min(probanden_data$cortisol1, na.rm = TRUE), max(probanden_data$cortisol1, na.rm = TRUE))) +
  theme_minimal()



#16.09.24: Anpassungen Boxplots nach Tipps von Jürgen, Punkte einzeichnen lassen:
# Boxplot with points and proper labels
ggplot(data = HCC_ohne.Ausreisser, aes(x = as.factor(gender), y = cortisol1, fill = as.factor(gender))) +
  geom_boxplot(alpha = 0.5) +
  geom_jitter(aes(color = as.factor(gender)), 
              width = 0.2,  # Adjust width to spread out points horizontally
              size = 2,     # Adjust size of points
              alpha = 0.6) + # Adjust transparency of points
  scale_x_discrete(labels = c("1" = "Male", "2" = "Female")) +
  scale_fill_manual(values = c("1" = "purple", "2" = "red"), labels = c("Male", "Female")) +
  scale_color_manual(values = c("1" = "purple", "2" = "red"), labels = c("Male", "Female")) +
  labs(title = "Cortisol Levels: Participants",
       x = "Gender",
       y = "Cortisol Levels",
       fill = "Gender",
       color = "Gender") +
  xlab("") +  # Entfernt das X-Achsenlabel
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))  # Zentriert den Titel




#16.09.24: Anpassungen Violinplots nach Tipps von Jürgen, Punkte einzeichnen lassen:
# Violinplot with points and proper labels
# Violinplot with jittered points
ggplot(data = HCC_ohne.Ausreisser, aes(x = as.factor(gender), y = cortisol1, fill = as.factor(gender))) +
  geom_violin(alpha = 0.5) +
  geom_jitter(aes(color = as.factor(gender)), width = 0.2, size = 2, alpha = 0.7) +
  scale_x_discrete(labels = c("1" = "Male", "2" = "Female")) +
  scale_fill_manual(values = c("1" = "purple", "2" = "red"), labels = c("Male", "Female")) +
  scale_color_manual(values = c("1" = "purple", "2" = "red"), labels = c("Male", "Female")) +
  labs(title = "Cortisol Levels: Participants",
       x = "Gender",   # X-Achsenlabel bleibt hier zunächst gesetzt
       y = "Cortisol Levels",
       fill = "Gender",
       color = "Gender") +
  xlab("") +  # Entfernt das X-Achsenlabel
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))  # Zentriert den Titel



# Density Plot with Jittered Points
ggplot(data = HCC_ohne.Ausreisser, aes(x = cortisol1, fill = as.factor(gender))) +
  geom_density(alpha = 0.5) +
  geom_jitter(aes(y = 0, color = as.factor(gender)), width = 0.3, height = 0, alpha = 0.7, size = 2) +
  scale_fill_manual(values = c("1" = "purple", "2" = "red"), labels = c("Male", "Female")) +
  scale_color_manual(values = c("1" = "purple", "2" = "red"), labels = c("Male", "Female")) +
  labs(title = "Cortisol Levels: Participants",
       x = "Cortisol Levels",
       y = "Density",
       fill = "Gender",
       color = "Gender") +
  scale_x_continuous(limits = c(min(HCC_ohne.Ausreisser$cortisol1, na.rm = TRUE), max(HCC_ohne.Ausreisser$cortisol1, na.rm = TRUE))) +
  xlab("") +  # Entfernt das X-Achsenlabel
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))  # Zentriert den Titel



#Gegenüberstellung der Daten, mit Klarheit, dass Referenzdaten nur Mittelwerte sind und keine Rohdaten:

# Erstellen eines Datensatzes für die Referenzdaten
referenz_data <- data.frame(
  gender = factor(c("Female", "Male"), levels = c("Female", "Male")),
  N = c(4945, 1666),
  Mean = c(7.17, 8.23),
  SE = c(0.10, 0.19),
  CI_lower = c(6.97, 7.85),
  CI_upper = c(7.36, 8.60),
  P5 = c(1.34, 1.74),
  P50 = c(4.62, 5.44),
  P95 = c(22.14, 24.69)
)

# Plot der Referenzdaten
ggplot(data = referenz_data, aes(x = gender)) +
  
  # Referenzdaten als Punkte (Mittelwerte)
  geom_point(aes(y = Mean), color = "black", size = 4) +
  
  # Fehlerbalken für die Konfidenzintervalle
  geom_errorbar(aes(x = gender, ymin = CI_lower, ymax = CI_upper), width = 0.2, color = "black") +
  
  # Linien für die 5., 50. und 95. Perzentile
  geom_hline(aes(yintercept = P5), linetype = "dashed", color = "blue") +
  geom_hline(aes(yintercept = P50), linetype = "solid", color = "blue") +
  geom_hline(aes(yintercept = P95), linetype = "dashed", color = "blue") +
  
  # Anpassung der Labels
  labs(title = "Reference Data for Cortisol Levels",
       x = "Gender",
       y = "Cortisol Levels",
       fill = "Gender") +
  
  # Anpassung des Themes
  theme_minimal()


# Erstellen eines Datensatzes für die Referenzdaten
referenz_data <- data.frame(
  gender = factor(c("Female", "Male"), levels = c("Female", "Male")),
  N = c(4945, 1666),
  Mean = c(7.17, 8.23),
  SE = c(0.10, 0.19),
  CI_lower = c(6.97, 7.85),
  CI_upper = c(7.36, 8.60),
  P5 = c(1.34, 1.74),
  P50 = c(4.62, 5.44),
  P95 = c(22.14, 24.69)
)

# Plot der Referenzdaten mit Legende
ggplot(data = referenz_data, aes(x = gender)) +
  
  # Referenzdaten als Punkte (Mittelwerte)
  geom_point(aes(y = Mean), color = "black", size = 4) +
  
  # Fehlerbalken für die Konfidenzintervalle
  geom_errorbar(aes(x = gender, ymin = CI_lower, ymax = CI_upper), width = 0.2, color = "black") +
  
  # Linien für die 5., 50. und 95. Perzentile mit einer Legende
  geom_hline(aes(yintercept = P5, linetype = "5th Percentile"), color = "blue") +
  geom_hline(aes(yintercept = P50, linetype = "50th Percentile (Median)"), color = "blue") +
  geom_hline(aes(yintercept = P95, linetype = "95th Percentile"), color = "blue") +
  
  # Anpassung der Labels
  labs(title = "Reference Data for Cortisol Levels",
       x = "Gender",
       y = "Cortisol Levels",
       fill = "Gender",
       linetype = "Percentiles") +  # Hier wird die Legende definiert
  
  # Anpassung des Themes
  theme_minimal() +
  
  # Anpassung der Legende
  scale_linetype_manual(values = c("5th Percentile" = "dashed", "50th Percentile (Median)" = "solid", "95th Percentile" = "dashed"))



# Erstellen eines Datensatzes für die Referenzdaten
referenz_data <- data.frame(
  gender = factor(c("Female", "Male"), levels = c("Female", "Male")),
  N = c(4945, 1666),
  Mean = c(7.17, 8.23),
  SE = c(0.10, 0.19),
  CI_lower = c(6.97, 7.85),
  CI_upper = c(7.36, 8.60),
  P5 = c(1.34, 1.74),
  P50 = c(4.62, 5.44),
  P95 = c(22.14, 24.69)
)

# Plot der Referenzdaten ohne Obertitel für die Legende
ggplot(data = referenz_data, aes(x = gender)) +
  
  # Mittelwerte als Punkte
  geom_point(aes(y = Mean, shape = "Mean"), color = "black", size = 4) +
  
  # Fehlerbalken für die Konfidenzintervalle
  geom_errorbar(aes(x = gender, ymin = CI_lower, ymax = CI_upper, linetype = "95% Confidence Interval"), 
                width = 0.2, color = "black") +
  
  # Linien für die 5., 50. und 95. Perzentile mit einer Legende
  geom_hline(aes(yintercept = P5, linetype = "5th Percentile"), color = "blue") +
  geom_hline(aes(yintercept = P50, linetype = "50th Percentile (Median)"), color = "blue") +
  geom_hline(aes(yintercept = P95, linetype = "95th Percentile"), color = "blue") +
  
  # Anpassung der Labels (ohne Obertitel für die Legende)
  labs(title = "Reference Data for Cortisol Levels",
       x = "Gender",
       y = "Cortisol Levels",
       linetype = NULL,  # Entfernen des Obertitels für die Linien-Legende
       shape = NULL) +   # Entfernen des Obertitels für die Punkte-Legende
  
  # Anpassung des Themes
  theme_minimal() +
  
  # Anpassung der Legende für Perzentile, Mittelwert und Konfidenzintervalle
  scale_linetype_manual(values = c("5th Percentile" = "dashed", 
                                   "50th Percentile (Median)" = "solid", 
                                   "95th Percentile" = "dashed",
                                   "95% Confidence Interval" = "solid")) +
  
  # Legende für den Mittelwert als Punkt
  scale_shape_manual(values = c("Mean" = 16)) +
  
  # Kombinierte Legende für Linien und Punkte (ohne Obertitel)
  guides(linetype = guide_legend(title = NULL), 
         shape = guide_legend(title = NULL))


#Bootstrap Konfidenzintervalle für die Referenzdaten berechnen
set.seed(123)  # Für Reproduzierbarkeit

# Simulierte Daten für Männer und Frauen basierend auf Mittelwert und SE
simulate_data <- function(mean, se, n) {
  rnorm(n, mean = mean, sd = se * sqrt(n))  # Simuliert Daten basierend auf Mittelwert und SE
}

# Simulierte Daten basierend auf den Referenzwerten
data_female <- simulate_data(mean = 7.17, se = 0.10, n = 4945)
data_male <- simulate_data(mean = 8.23, se = 0.19, n = 1666)

# Funktion zum Bootstrappen und Berechnen von Konfidenzintervallen
bootstrap_ci <- function(data, n_bootstrap = 1000, conf_level = 0.95) {
  # Bootstrap-Sampling
  bootstrap_means <- replicate(n_bootstrap, mean(sample(data, replace = TRUE)))
  
  # Berechne das Konfidenzintervall
  ci <- quantile(bootstrap_means, probs = c((1 - conf_level) / 2, 1 - (1 - conf_level) / 2))
  return(ci)
}

# Berechne Bootstrap-Konfidenzintervalle für beide Gruppen
ci_female <- bootstrap_ci(data_female)
ci_male <- bootstrap_ci(data_male)

# Füge die berechneten CIs zum Referenzdatensatz hinzu
referenz_data$Bootstrap_CI_lower <- c(ci_female[1], ci_male[1])
referenz_data$Bootstrap_CI_upper <- c(ci_female[2], ci_male[2])

# Schaue die Bootstrap-Konfidenzintervalle an
referenz_data


# Plot der Referenzdaten mit Bootstrap-Konfidenzintervallen
ggplot(data = referenz_data, aes(x = gender)) +
  
  # Mittelwerte als Punkte
  geom_point(aes(y = Mean, shape = "Mean"), color = "black", size = 4) +
  
  # Fehlerbalken für die ursprünglichen Konfidenzintervalle
  geom_errorbar(aes(x = gender, ymin = CI_lower, ymax = CI_upper, linetype = "95% Confidence Interval"), 
                width = 0.2, color = "black") +
  
  # Bootstrap-Konfidenzintervalle hinzufügen
  geom_errorbar(aes(x = gender, ymin = Bootstrap_CI_lower, ymax = Bootstrap_CI_upper, linetype = "Bootstrap CI"), 
                width = 0.2, color = "red") +  # Bootstrap CI in Rot
  
  # Linien für die 5., 50. und 95. Perzentile mit einer Legende
  geom_hline(aes(yintercept = P5, linetype = "5th Percentile"), color = "blue") +
  geom_hline(aes(yintercept = P50, linetype = "50th Percentile (Median)"), color = "blue") +
  geom_hline(aes(yintercept = P95, linetype = "95th Percentile"), color = "blue") +
  
  # Anpassung der Labels
  labs(title = "Reference Data for Cortisol Levels with Bootstrap CI",
       x = "Gender",
       y = "Cortisol Levels",
       linetype = NULL,
       shape = NULL) +
  
  # Anpassung des Themes
  theme_minimal() +
  
  # Anpassung der Legende für Perzentile, Mittelwert und Konfidenzintervalle
  scale_linetype_manual(values = c("5th Percentile" = "dashed", 
                                   "50th Percentile (Median)" = "solid", 
                                   "95th Percentile" = "dashed",
                                   "95% Confidence Interval" = "solid",
                                   "Bootstrap CI" = "solid")) +  # Bootstrap CI in der Legende
  scale_shape_manual(values = c("Mean" = 16)) +
  
  # Kombinierte Legende für Linien und Punkte
  guides(linetype = guide_legend(title = NULL), 
         shape = guide_legend(title = NULL))








#Kombination von Referenzdaten-Darstellung und Boxplot der Probandendaten:

install.packages("cowplot")

# Lade das cowplot Paket
library(cowplot)



# Erstellen eines Datensatzes für die Referenzdaten
referenz_data <- data.frame(
  gender = factor(c("Female", "Male"), levels = c("Female", "Male")),
  N = c(4945, 1666),
  Mean = c(7.17, 8.23),
  SE = c(0.10, 0.19),
  CI_lower = c(6.97, 7.85),
  CI_upper = c(7.36, 8.60),
  P5 = c(1.34, 1.74),
  P50 = c(4.62, 5.44),
  P95 = c(22.14, 24.69)
)

# Boxplot für deine Probandendaten
boxplot_probanden <- ggplot(data = HCC_ohne.Ausreisser, aes(x = as.factor(gender), y = cortisol1, fill = as.factor(gender))) +
  geom_boxplot(alpha = 0.5) +
  geom_jitter(aes(color = as.factor(gender)), 
              width = 0.2,  
              size = 2,     
              alpha = 0.6) + 
  scale_x_discrete(labels = c("1" = "Male", "2" = "Female")) +
  scale_fill_manual(values = c("1" = "purple", "2" = "red"), labels = c("Male", "Female")) +
  scale_color_manual(values = c("1" = "purple", "2" = "red"), labels = c("Male", "Female")) +
  labs(title = "Cortisol Levels: Participants",
       x = "Gender",
       y = "Cortisol Levels",
       fill = "Gender",
       color = "Gender") +
  scale_y_continuous(limits = c(0, 25), breaks = seq(0, 25, 5)) +  # Beschränkung der Y-Achse von 0 bis 25
  xlab("") +  # Entfernt das X-Achsenlabel
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))  # Zentriert den Titel

# Plot für die Referenzdaten (mit getrennten Perzentillinien für Male und Female)
referenz_plot <- ggplot(data = referenz_data, aes(x = gender)) +
  
  # Mittelwerte als Punkte
  geom_point(aes(y = Mean, shape = "Mean"), color = "black", size = 4) +
  
  # Fehlerbalken für die Konfidenzintervalle (Mittelwert)
  geom_errorbar(aes(x = gender, ymin = CI_lower, ymax = CI_upper, linetype = "Mean Confidence Interval"), 
                width = 0.2, color = "black") +
  
  # Linien für die Perzentile nach Gruppen
  geom_hline(data = subset(referenz_data, gender == "Female"), aes(yintercept = P5, linetype = "5th Percentile (Female)"), color = "blue") +
  geom_hline(data = subset(referenz_data, gender == "Female"), aes(yintercept = P50, linetype = "50th Percentile (Median, Female)"), color = "blue") +
  geom_hline(data = subset(referenz_data, gender == "Female"), aes(yintercept = P95, linetype = "95th Percentile (Female)"), color = "blue") +
  
  geom_hline(data = subset(referenz_data, gender == "Male"), aes(yintercept = P5, linetype = "5th Percentile (Male)"), color = "red") +
  geom_hline(data = subset(referenz_data, gender == "Male"), aes(yintercept = P50, linetype = "50th Percentile (Median, Male)"), color = "red") +
  geom_hline(data = subset(referenz_data, gender == "Male"), aes(yintercept = P95, linetype = "95th Percentile (Male)"), color = "red") +
  
  labs(title = "Reference Data",
       x = "Gender",
       y = "Cortisol Levels",
       linetype = NULL,
       shape = NULL) +
  
  scale_y_continuous(limits = c(0, 25), breaks = seq(0, 25, 5)) +  # Beschränkung der Y-Achse von 0 bis 25
  xlab("") +  # Entfernt das X-Achsenlabel
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) + # Zentriert den Titel
  
  # Anpassung der Legende für die getrennten Perzentile und das Konfidenzintervall
  scale_linetype_manual(values = c("5th Percentile (Female)" = "dashed", 
                                   "50th Percentile (Median, Female)" = "solid", 
                                   "95th Percentile (Female)" = "dashed",
                                   "5th Percentile (Male)" = "dashed", 
                                   "50th Percentile (Median, Male)" = "solid", 
                                   "95th Percentile (Male)" = "dashed",
                                   "Mean Confidence Interval" = "solid")) +
  
  scale_shape_manual(values = c("Mean" = 16)) +
  
  guides(linetype = guide_legend(title = NULL), 
         shape = guide_legend(title = NULL))

# Kombiniere beide Plots nebeneinander mit cowplot
combined_plot <- plot_grid(boxplot_probanden, referenz_plot, labels = c("A", "B"))

# Zeige den kombinierten Plot an
print(combined_plot)





#Referenzdaten müssen anders dargestellt werden, da die Linien nicht stimmen:
# Lade das cowplot Paket


# Lade das cowplot Paket
library(cowplot)

# Erstellen eines Datensatzes für die Referenzdaten
referenz_data <- data.frame(
  gender = factor(c("Female", "Male"), levels = c("Female", "Male")),
  N = c(4945, 1666),
  Mean = c(7.17, 8.23),
  SE = c(0.10, 0.19),
  CI_lower = c(6.97, 7.85),
  CI_upper = c(7.36, 8.60),
  P5 = c(1.34, 1.74),
  P50 = c(4.62, 5.44),
  P95 = c(22.14, 24.69)
)

# Einheitliche Definition für die Linientypen
linetypes <- c("5th Percentile" = "dashed", 
               "50th Percentile (Median)" = "solid", 
               "95th Percentile" = "dashed",
               "Mean Confidence Interval" = "dotted")

# Plot für Female Referenzdaten (ohne Legende)
referenz_plot_female <- ggplot(data = subset(referenz_data, gender == "Female"), aes(x = gender)) +
  
  # Mittelwerte als Punkte
  geom_point(aes(y = Mean), color = "black", size = 4) +
  
  # Fehlerbalken für die Konfidenzintervalle (Mittelwert)
  geom_errorbar(aes(ymin = CI_lower, ymax = CI_upper, linetype = "Mean Confidence Interval"), width = 0.2, color = "black") +
  
  # Linien für die Perzentile (Female)
  geom_hline(aes(yintercept = P5, linetype = "5th Percentile"), color = "black") +
  geom_hline(aes(yintercept = P50, linetype = "50th Percentile (Median)"), color = "black") +
  geom_hline(aes(yintercept = P95, linetype = "95th Percentile"), color = "black") +
  
  labs(title = "Reference Data: Female",
       x = "",
       y = "Cortisol Levels") +
  
  scale_linetype_manual(values = linetypes) +  # Anwenden der einheitlichen Linientypen
  
  scale_y_continuous(limits = c(0, 25), breaks = seq(0, 25, 5)) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none")  # Entferne die Legende für Female

# Plot für Male Referenzdaten (mit Legende)
referenz_plot_male <- ggplot(data = subset(referenz_data, gender == "Male"), aes(x = gender)) +
  
  # Mittelwerte als Punkte
  geom_point(aes(y = Mean), color = "black", size = 4) +
  
  # Fehlerbalken für die Konfidenzintervalle (Mittelwert)
  geom_errorbar(aes(ymin = CI_lower, ymax = CI_upper, linetype = "Mean Confidence Interval"), width = 0.2, color = "black") +
  
  # Linien für die Perzentile (Male)
  geom_hline(aes(yintercept = P5, linetype = "5th Percentile"), color = "black") +
  geom_hline(aes(yintercept = P50, linetype = "50th Percentile (Median)"), color = "black") +
  geom_hline(aes(yintercept = P95, linetype = "95th Percentile"), color = "black") +
  
  labs(title = "Reference Data: Male",
       x = "",
       y = "Cortisol Levels") +
  
  scale_linetype_manual(name = "Legend", values = linetypes) +  # Ein einheitliches Mapping für Linientypen
  
  scale_y_continuous(limits = c(0, 25), breaks = seq(0, 25, 5)) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +  # Zentriert den Titel
  
  guides(
    linetype = guide_legend(
      title = "Legend",
      override.aes = list(color = "black")  # Alle Linien in Schwarz
    )
  )

# Kombiniere beide Referenzplots nebeneinander mit cowplot, gleiche Breite
combined_referenz_plot <- plot_grid(referenz_plot_female, referenz_plot_male, labels = c("A", "B"), rel_widths = c(1, 1))

# Zeige den kombinierten Plot an
print(combined_referenz_plot)




#Kombination von Referenzdaten-Darstellung und Boxplot der Probandendaten:
install.packages("ggplot2")
install.packages("gridExtra")
install.packages("cowplot")
# Lade Pakete
library(cowplot)
library(ggplot2)
library(gridExtra)
# Erstellen eines Datensatzes für die Referenzdaten
referenz_data <- data.frame(
  gender = factor(c("Female", "Male"), levels = c("Female", "Male")),
  N = c(4945, 1666),
  Mean = c(7.17, 8.23),
  SE = c(0.10, 0.19),
  CI_lower = c(6.97, 7.85),
  CI_upper = c(7.36, 8.60),
  P5 = c(1.34, 1.74),
  P50 = c(4.62, 5.44),
  P95 = c(22.14, 24.69)
)

# Boxplot für deine Probandendaten mit geänderter Legendenbeschriftung
boxplot_probanden <- ggplot(data = HCC_ohne.Ausreisser, aes(x = as.factor(gender), y = cortisol1, fill = as.factor(gender))) +
  geom_boxplot(alpha = 0.5) +
  geom_jitter(aes(color = as.factor(gender)), 
              width = 0.2,  
              size = 2,     
              alpha = 0.6) + 
  scale_x_discrete(labels = c("1" = "Male", "2" = "Female")) +
  scale_fill_manual(values = c("1" = "purple", "2" = "red"), labels = c("Male", "Female")) +
  scale_color_manual(values = c("1" = "purple", "2" = "red"), labels = c("Male", "Female")) +
  labs(title = "Cortisol Levels: Participants",
       x = "Gender",
       y = "Cortisol Levels") +  
  scale_y_continuous(limits = c(0, 25), breaks = seq(0, 25, 5)) +  # Beschränkung der Y-Achse von 0 bis 25
  xlab("") +  # Entfernt das X-Achsenlabel
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none")   # Zentriert den Titel & # Entferne die Legende 

# Funktion zur Erstellung der Referenzplots (Female und Male)
plot_referenz_data <- function(data, gender_label) {
  ggplot(data = data, aes(x = gender)) +
    # Mittelwerte als Punkte
    geom_point(aes(y = Mean), color = "black", size = 4) +
    # Fehlerbalken für die Konfidenzintervalle (Mittelwert)
    geom_errorbar(aes(ymin = CI_lower, ymax = CI_upper, linetype = "Mean Confidence Interval"), width = 0.2, color = "black") +
    # Linien für die Perzentile
    geom_hline(aes(yintercept = P5, linetype = "5th Percentile"), color = "black") +
    geom_hline(aes(yintercept = P50, linetype = "50th Percentile (Median)"), color = "black") +
    geom_hline(aes(yintercept = P95, linetype = "95th Percentile"), color = "black") +
    labs(title = "",  # Der individuelle Titel für jeden Plot
         x = "",
         y = "Cortisol Levels") +
    scale_y_continuous(limits = c(0, 25), breaks = seq(0, 25, 5)) +
    scale_linetype_manual(values = c("5th Percentile" = "dashed", 
                                     "50th Percentile (Median)" = "solid", 
                                     "95th Percentile" = "dashed",
                                     "Mean Confidence Interval" = "dotted")) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5), legend.position = "none")  # Zentriert den Titel und entfernt die Legende bei Female
}

# Erstellung der Referenzplots für Female und Male
referenz_plot_female <- plot_referenz_data(subset(referenz_data, gender == "Female"), "Female")
referenz_plot_male <- plot_referenz_data(subset(referenz_data, gender == "Male"), "Male") +
  # Gemeinsame Legende nur im Male Plot
  guides(
    linetype = guide_legend(
      title = "Legend",
      override.aes = list(color = "black")
    )
  )

# Kombinierte Referenzdaten-Plots (mit gemeinsamer Breite und ohne Labels A und B)
combined_referenz_plot <- plot_grid(referenz_plot_female, referenz_plot_male, rel_widths = c(1, 1))

# Erstelle den gemeinsamen Titel "Reference Data"
combined_referenz_plot_with_title <- plot_grid(
  ggdraw() + draw_label("Reference Data", fontface = "plain", hjust = 0.5, size = 13),
  combined_referenz_plot,
  ncol = 1,
  rel_heights = c(0.1, 1)
)

# Kombiniere Boxplot und Referenzplots nebeneinander
final_combined_plot <- plot_grid(boxplot_probanden, combined_referenz_plot_with_title, rel_widths = c(1, 1.5))

# Zeige den kombinierten Plot an
print(final_combined_plot)




#
#
#
##############################################################################################################################
######Definitive Darstellung############Cortisol 1#########################################################################

#17.10.24 definitive Darstellung des Vergleichs Cortisol 1 month mit reference values:

#Kombination von Referenzdaten-Darstellung und Boxplot der Probandendaten:

#packages installieren
install.packages("ggplot2")
install.packages("gridExtra")
install.packages("cowplot")

# Lade Pakete
library(cowplot)
library(ggplot2)
library(gridExtra)


# Entfernen der Spalte V2-V4 (brauche ich nicht) 
HCC_ohne.Ausreisser [2:4] <- list(NULL)

# Manuelles Zuweisen der Spaltennamen, falls bekannt
colnames(HCC_ohne.Ausreisser) <- c("study_id","cortisol1","cortisol2","cortisone1","cortisone2","hairmass1","hairmass2","age","gender")

# Löschen der ersten Zeile
HCC_ohne.Ausreisser <- HCC_ohne.Ausreisser [-1, ]

# Spalten definieren, in denen leere Stellen mit NA ersetzt werden sollen
columns_to_fill <- c("cortisol1", "cortisol2", "cortisone1", "cortisone2", "hairmass1", "hairmass2")

# Leere Stellen (repräsentiert als leere Strings oder explizite NAs) in diesen Spalten mit NA ersetzen
HCC_ohne.Ausreisser[columns_to_fill] <- lapply(HCC_ohne.Ausreisser[columns_to_fill], function(x) {
  x[x == "" | is.na(x)] <- NA
  return(x)
})

# Sicherstellen, dass die Werte numerisch sind
HCC_ohne.Ausreisser$cortisol1 <- as.numeric(HCC_ohne.Ausreisser$cortisol1)
HCC_ohne.Ausreisser$cortisol2 <- as.numeric(HCC_ohne.Ausreisser$cortisol2)


# Erstellen eines Datensatzes für die Referenzdaten
referenz_data <- data.frame(
  gender = factor(c("Female", "Male"), levels = c("Female", "Male")),
  N = c(4945, 1666),
  Mean = c(7.17, 8.23),
  SE = c(0.10, 0.19),
  CI_lower = c(6.97, 7.85),
  CI_upper = c(7.36, 8.60),
  P5 = c(1.34, 1.74),
  P50 = c(4.62, 5.44),
  P95 = c(22.14, 24.69)
)



# Boxplot für deine Probandendaten mit geänderter Reihenfolge der Kategorien
boxplot_probanden <- ggplot(data = HCC_ohne.Ausreisser, aes(x = factor(gender, levels = c("2", "1")), y = cortisol1, fill = as.factor(gender))) +
  geom_boxplot(alpha = 0.5) +
  geom_jitter(aes(color = as.factor(gender)), 
              width = 0.2,  
              size = 2,     
              alpha = 0.6) + 
  scale_x_discrete(labels = c("1" = "Male", "2" = "Female")) +
  scale_fill_manual(values = c("1" = "blue", "2" = "red"), labels = c("Male", "Female")) +
  scale_color_manual(values = c("1" = "blue", "2" = "red"), labels = c("Male", "Female")) +
  labs(title = "Cortisol 1 month: Participants",
       x = "Gender",
       y = "Cortisol Levels") +
  scale_y_continuous(limits = c(0, 25), breaks = seq(0, 25, 5)) +  # Beschränkung der Y-Achse von 0 bis 25
  xlab("") +  # Entfernt das X-Achsenlabel
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +  # Zentriert den Titel
  guides(fill = "none", color = "none")  # Entfernt die Legende


# Plot für die Referenzdaten (mit getrennten Perzentillinien für Male und Female)
referenz_plot <- ggplot(data = referenz_data, aes(x = gender)) +
  
  # Mittelwerte als Punkte
  geom_point(aes(y = Mean, shape = "Mean Reference Values"), color = "black", size = 2) +
  
  # Fehlerbalken für die Konfidenzintervalle (Mittelwert)
  geom_errorbar(aes(x = gender, ymin = CI_lower, ymax = CI_upper, linetype = "Mean Confidence Interval"), 
                width = 0.2, color = "black") +
  
  # Linien für die Perzentile nach Gruppen
  geom_hline(data = subset(referenz_data, gender == "Female"), aes(yintercept = P5, linetype = "5th Percentile (Female)"), color = "red") +
  geom_hline(data = subset(referenz_data, gender == "Female"), aes(yintercept = P50, linetype = "50th Percentile (Median, Female)"), color = "red") +
  geom_hline(data = subset(referenz_data, gender == "Female"), aes(yintercept = P95, linetype = "95th Percentile (Female)"), color = "red") +
  
  geom_hline(data = subset(referenz_data, gender == "Male"), aes(yintercept = P5, linetype = "5th Percentile (Male)"), color = "blue") +
  geom_hline(data = subset(referenz_data, gender == "Male"), aes(yintercept = P50, linetype = "50th Percentile (Median, Male)"), color = "blue") +
  geom_hline(data = subset(referenz_data, gender == "Male"), aes(yintercept = P95, linetype = "95th Percentile (Male)"), color = "blue") +
  
  labs(title = "Reference Data",
       x = "Gender",
       y = "Cortisol Levels",
       linetype = NULL,
       shape = NULL) +
  
  scale_y_continuous(limits = c(0, 25), breaks = seq(0, 25, 5)) +  # Beschränkung der Y-Achse von 0 bis 25
  xlab("") +  # Entfernt das X-Achsenlabel
  ylab("") + # Entfernt das Y-Achsenlabel
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5), # Zentriert den Titel
  legend.spacing.y = unit(0.1, "cm")) +    # Reduziert den vertikalen Abstand zwischen den Legenden   
  # Anpassung der Legende für die getrennten Perzentile und das Konfidenzintervall
  scale_linetype_manual(values = c("5th Percentile (Female)" = "dashed", 
                                   "50th Percentile (Median, Female)" = "solid", 
                                   "95th Percentile (Female)" = "dashed",
                                   "5th Percentile (Male)" = "dashed", 
                                   "50th Percentile (Median, Male)" = "solid", 
                                   "95th Percentile (Male)" = "dashed",
                                   "Mean Confidence Interval" = "dotted")) +
  
  scale_shape_manual(values = c("Mean Reference Values" = 16)) +
  
  guides(
    linetype = guide_legend(title = NULL, 
                            order = 2, # Perzentile werden nach dem Mean angezeigt
                            override.aes = list(
                              color = c("red", "blue", "red", "blue", "red", "blue", "black"))), 
    shape = guide_legend(title = NULL, 
                         order = 1)  # Mean wird zuerst angezeigt
  )

# Kombiniere beide Plots nebeneinander mit cowplot
combined_plot <- plot_grid(boxplot_probanden, referenz_plot, labels = c("A", "B"))

# Zeige den kombinierten Plot an
print(combined_plot)

#
#
#
##############################################################################################################################
######Definitive Darstellung############Cortisol 3#########################################################################
# definitive Darstellung des Vergleichs Cortisol 3 months mit reference values:

# Erstellen eines Datensatzes für die Referenzdaten
referenz_data <- data.frame(
  gender = factor(c("Female", "Male"), levels = c("Female", "Male")),
  N = c(4945, 1666),
  Mean = c(7.17, 8.23),
  SE = c(0.10, 0.19),
  CI_lower = c(6.97, 7.85),
  CI_upper = c(7.36, 8.60),
  P5 = c(1.34, 1.74),
  P50 = c(4.62, 5.44),
  P95 = c(22.14, 24.69)
)



# Boxplot für deine Probandendaten mit geänderter Reihenfolge der Kategorien
boxplot_probanden <- ggplot(data = HCC_ohne.Ausreisser, aes(x = factor(gender, levels = c("2", "1")), y = cortisol2, fill = as.factor(gender))) +
  geom_boxplot(alpha = 0.5) +
  geom_jitter(aes(color = as.factor(gender)), 
              width = 0.2,  
              size = 2,     
              alpha = 0.6) + 
  scale_x_discrete(labels = c("1" = "Male", "2" = "Female")) +
  scale_fill_manual(values = c("1" = "blue", "2" = "red"), labels = c("Male", "Female")) +
  scale_color_manual(values = c("1" = "blue", "2" = "red"), labels = c("Male", "Female")) +
  labs(title = "Cortisol 3 months: Participants",
       x = "Gender",
       y = "Cortisol Levels") +
  scale_y_continuous(limits = c(0, 25), breaks = seq(0, 25, 5)) +  # Beschränkung der Y-Achse von 0 bis 25
  xlab("") +  # Entfernt das X-Achsenlabel
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +  # Zentriert den Titel
  guides(fill = "none", color = "none")  # Entfernt die Legende


# Plot für die Referenzdaten (mit getrennten Perzentillinien für Male und Female)
referenz_plot <- ggplot(data = referenz_data, aes(x = gender)) +
  
  # Mittelwerte als Punkte
  geom_point(aes(y = Mean, shape = "Mean Reference Values"), color = "black", size = 2) +
  
  # Fehlerbalken für die Konfidenzintervalle (Mittelwert)
  geom_errorbar(aes(x = gender, ymin = CI_lower, ymax = CI_upper, linetype = "Mean Confidence Interval"), 
                width = 0.2, color = "black") +
  
  # Linien für die Perzentile nach Gruppen
  geom_hline(data = subset(referenz_data, gender == "Female"), aes(yintercept = P5, linetype = "5th Percentile (Female)"), color = "red") +
  geom_hline(data = subset(referenz_data, gender == "Female"), aes(yintercept = P50, linetype = "50th Percentile (Median, Female)"), color = "red") +
  geom_hline(data = subset(referenz_data, gender == "Female"), aes(yintercept = P95, linetype = "95th Percentile (Female)"), color = "red") +
  
  geom_hline(data = subset(referenz_data, gender == "Male"), aes(yintercept = P5, linetype = "5th Percentile (Male)"), color = "blue") +
  geom_hline(data = subset(referenz_data, gender == "Male"), aes(yintercept = P50, linetype = "50th Percentile (Median, Male)"), color = "blue") +
  geom_hline(data = subset(referenz_data, gender == "Male"), aes(yintercept = P95, linetype = "95th Percentile (Male)"), color = "blue") +
  
  labs(title = "Reference Data",
       x = "Gender",
       y = "Cortisol Levels",
       linetype = NULL,
       shape = NULL) +
  
  scale_y_continuous(limits = c(0, 25), breaks = seq(0, 25, 5)) +  # Beschränkung der Y-Achse von 0 bis 25
  xlab("") +  # Entfernt das X-Achsenlabel
  ylab("") + # Entfernt das Y-Achsenlabel
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),   # Zentriert den Titel
  legend.spacing.y = unit(0.1, "cm")) +    # Reduziert den vertikalen Abstand zwischen den Legenden   
  
  # Anpassung der Legende für die getrennten Perzentile und das Konfidenzintervall
  scale_linetype_manual(values = c("5th Percentile (Female)" = "dashed", 
                                   "50th Percentile (Median, Female)" = "solid", 
                                   "95th Percentile (Female)" = "dashed",
                                   "5th Percentile (Male)" = "dashed", 
                                   "50th Percentile (Median, Male)" = "solid", 
                                   "95th Percentile (Male)" = "dashed",
                                   "Mean Confidence Interval" = "dotted")) +
  
  scale_shape_manual(values = c("Mean Reference Values" = 16)) +
  
  # Anpassung der Legendenreihenfolge
  guides(
    linetype = guide_legend(title = NULL, 
                            order = 2, # Perzentile werden nach dem Mean angezeigt
                            override.aes = list(
                              color = c("red", "blue", "red", "blue", "red", "blue", "black"))), 
    shape = guide_legend(title = NULL, 
                         order = 1)  # Mean wird zuerst angezeigt
  )


# Kombiniere beide Plots nebeneinander mit cowplot
combined_plot <- plot_grid(boxplot_probanden, referenz_plot, labels = c("A", "B"))

# Zeige den kombinierten Plot an
print(combined_plot)
