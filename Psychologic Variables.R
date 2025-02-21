#Akutdaten-Psychologische Variablen#
#26.06.2024#

# Entfernen der Spalte V2-V4 (brauche ich nicht) 
Psychology [2:4] <- list(NULL)

# Manuelles Zuweisen der Spaltennamen, falls bekannt
colnames(Psychology) <- c("study_id","dass21_depression","dass21_anxiety","dass21_stress","stais_score","pvaq_score_pain","pvaq_score_painchanges","pvaq_score_total")

# Löschen der ersten Zeile
Psychology <- Psychology [-1, ]


#DASS

# Sicherstellen, dass die Werte numerisch sind
Psychology$dass21_depression <- as.numeric(Psychology$dass21_depression)
Psychology$dass21_stress <- as.numeric(Psychology$dass21_stress)

# Berechnung des Mittelwerts
mean_dass21_depression <- mean(Psychology$dass21_depression, na.rm = TRUE)
mean_dass21_stress <- mean(Psychology$dass21_stress, na.rm = TRUE)

# Berechnung des Medians
median_dass21_depression <- median(Psychology$dass21_depression, na.rm = TRUE)
median_dass21_stress <- median(Psychology$dass21_stress, na.rm = TRUE)

# Berechnung der Standardabweichung
sd_dass21_depression <- sd(Psychology$dass21_depression, na.rm = TRUE)
sd_dass21_stress <- sd(Psychology$dass21_stress, na.rm = TRUE)

# Ausgabe der Ergebnisse
mean_dass21_depression
median_dass21_depression
sd_dass21_depression

# Anzahl der gültigen Beobachtungen (nicht NA)
n_depression <- sum(!is.na(Psychology$dass21_depression))
n_stress <- sum(!is.na(Psychology$dass21_stress))

# Z-Wert für 95% Konfidenzintervall (zweischwänzig)
z_value <- qnorm(0.975)  # qnorm(0.975) entspricht etwa 1.96

# Berechnung der Konfidenzintervalle für Depression
ci_width_depression <- z_value * (sd_dass21_depression / sqrt(n_depression))
lower_ci_depression <- mean_dass21_depression - ci_width_depression
upper_ci_depression <- mean_dass21_depression + ci_width_depression

# Berechnung der Konfidenzintervalle für Stress
ci_width_stress <- z_value * (sd_dass21_stress / sqrt(n_stress))
lower_ci_stress <- mean_dass21_stress - ci_width_stress
upper_ci_stress <- mean_dass21_stress + ci_width_stress

# Berechnung des Wertebereichs für Depression
range_depression <- range(Psychology$dass21_depression, na.rm = TRUE)
# Berechnung des Wertebereichs für Stress
range_stress <- range(Psychology$dass21_stress, na.rm = TRUE)

# Ausgabe der Ergebnisse für Depression
cat("Mittelwert von DASS21 Depression:", mean_dass21_depression, "\n")
cat("Median von DASS21 Depression:", median_dass21_depression, "\n")
cat("Standardabweichung von DASS21 Depression:", sd_dass21_depression, "\n")
cat("95% Konfidenzintervall für DASS21 Depression: [", lower_ci_depression, ",", upper_ci_depression, "]\n")
cat("Wertebereich von DASS21 Depression: [", range_depression[1], ",", range_depression[2], "]\n")

# Ausgabe der Ergebnisse für Stress
cat("Mittelwert von DASS21 Stress:", mean_dass21_stress, "\n")
cat("Median von DASS21 Stress:", median_dass21_stress, "\n")
cat("Standardabweichung von DASS21 Stress:", sd_dass21_stress, "\n")
cat("95% Konfidenzintervall für DASS21 Stress: [", lower_ci_stress, ",", upper_ci_stress, "]\n")
cat("Wertebereich von DASS21 Stress: [", range_stress[1], ",", range_stress[2], "]\n")


#STAI

# Sicherstellen, dass die Werte numerisch sind
Psychology$stais_score <- as.numeric(Psychology$stais_score)

# Berechnung des Mittelwerts
mean_stais_score <- mean(Psychology$stais_score, na.rm = TRUE)

# Berechnung des Medians
median_stais_score <- median(Psychology$stais_score, na.rm = TRUE)

# Berechnung der Standardabweichung
sd_stais_score <- sd(Psychology$stais_score, na.rm = TRUE)

# Ausgabe der Ergebnisse
mean_stais_score
median_stais_score
sd_stais_score

# Anzahl der gültigen Beobachtungen (nicht NA)
n_stais <- sum(!is.na(Psychology$stais_score))

# Z-Wert für 95% Konfidenzintervall (zweischwänzig)
z_value_stais <- qnorm(0.975)  # qnorm(0.975) entspricht etwa 1.96

# Berechnung des Konfidenzintervalls
ci_width_stais <- z_value_stais * (sd_stais_score / sqrt(n_stais))
lower_ci_stais <- mean_stais_score - ci_width_stais
upper_ci_stais <- mean_stais_score + ci_width_stais

# Berechnung des Wertebereichs
range_stais <- range(Psychology$stais_score, na.rm = TRUE)

# Ausgabe der Ergebnisse
cat("Mittelwert von STAI Score:", mean_stais_score, "\n")
cat("Median von STAI Score:", median_stais_score, "\n")
cat("Standardabweichung von STAI Score:", sd_stais_score, "\n")
cat("95% Konfidenzintervall für STAI Score: [", lower_ci_stais, ",", upper_ci_stais, "]\n")
cat("Wertebereich von STAI Score: [", range_stais[1], ",", range_stais[2], "]\n")


#PVAQ

# Sicherstellen, dass die Werte numerisch sind
Psychology$pvaq_score_total <- as.numeric(Psychology$pvaq_score_total)

# Berechnung des Mittelwerts
mean_pvaq <- mean(Psychology$pvaq_score_total, na.rm = TRUE)

# Berechnung des Medians
median_pvaq <- median(Psychology$pvaq_score_total, na.rm = TRUE)

# Berechnung der Standardabweichung
sd_pvaq <- sd(Psychology$pvaq_score_total, na.rm = TRUE)

# Ausgabe der Ergebnisse
mean_pvaq
median_pvaq
sd_pvaq

# Anzahl der gültigen Beobachtungen (nicht NA)
n_pvaq <- sum(!is.na(Psychology$pvaq_score_total))

# Z-Wert für 95% Konfidenzintervall (zweischwänzig)
z_value_pvaq <- qnorm(0.975)  # qnorm(0.975) entspricht etwa 1.96

# Berechnung des Konfidenzintervalls
ci_width_pvaq <- z_value_pvaq * (sd_pvaq / sqrt(n_pvaq))
lower_ci_pvaq <- mean_pvaq - ci_width_pvaq
upper_ci_pvaq <- mean_pvaq + ci_width_pvaq

# Berechnung des Wertebereichs
range_pvaq <- range(Psychology$pvaq_score_total, na.rm = TRUE)

# Ausgabe der Ergebnisse
cat("Mittelwert von PVAQ Score:", mean_pvaq, "\n")
cat("Median von PVAQ Score:", median_pvaq, "\n")
cat("Standardabweichung von PVAQ Score:", sd_pvaq, "\n")
cat("95% Konfidenzintervall für PVAQ Score: [", lower_ci_pvaq, ",", upper_ci_pvaq, "]\n")
cat("Wertebereich von PVAQ Score: [", range_pvaq[1], ",", range_pvaq[2], "]\n")

