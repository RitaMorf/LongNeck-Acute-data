#AE-FS: mean, SD, range, Konfidenzintervall
# Sicherstellen, dass die Werte numerisch sind
AE.FS$score_pps <- as.numeric(AE.FS$score_pps)
AE.FS$dms_score <- as.numeric(AE.FS$dms_score)

# Berechnung des Mittelwerts
mean_score_pps <- mean(AE.FS$score_pps, na.rm = TRUE)
mean_dms_score <- mean(AE.FS$dms_score, na.rm = TRUE)

# Berechnung des Medians
median_score_pps <- median(AE.FS$score_pps, na.rm = TRUE)
median_dms_score <- median(AE.FS$dms_score, na.rm = TRUE)

# Berechnung der Standardabweichung
sd_score_pps <- sd(AE.FS$score_pps, na.rm = TRUE)
sd_dms_score <- sd(AE.FS$dms_score, na.rm = TRUE)

# Berechnung des Wertebereichs
range_score_pps <- range(AE.FS$score_pps, na.rm = TRUE)
range_dms_score <- range(AE.FS$dms_score, na.rm = TRUE)

# Anzahl der gültigen Beobachtungen (nicht NA)
n_score_pps <- sum(!is.na(AE.FS$score_pps))
n_dms_score <- sum(!is.na(AE.FS$dms_score))

# Z-Wert für 95% Konfidenzintervall (zweischwänzig)
z_value <- qnorm(0.975)  # qnorm(0.975) entspricht etwa 1.96

# Berechnung des Konfidenzintervalls
ci_width_score_pps <- z_value * (sd_score_pps / sqrt(n_score_pps))
lower_ci_score_pps <- mean_score_pps - ci_width_score_pps
upper_ci_score_pps <- mean_score_pps + ci_width_score_pps

ci_width_dms_score <- z_value * (sd_dms_score / sqrt(n_dms_score))
lower_ci_dms_score <- mean_dms_score - ci_width_dms_score
upper_ci_dms_score <- mean_dms_score + ci_width_dms_score

# Ausgabe der Ergebnisse
cat("Mittelwert von score_pps:", mean_score_pps, "\n")
cat("Median von score_pps:", median_score_pps, "\n")
cat("Standardabweichung von score_pps:", sd_score_pps, "\n")
cat("95% Konfidenzintervall für score_pps: [", lower_ci_score_pps, ",", upper_ci_score_pps, "]\n")
cat("Wertebereich von score_pps: [", range_score_pps[1], ",", range_score_pps[2], "]\n\n")

cat("Mittelwert von dms_score:", mean_dms_score, "\n")
cat("Median von dms_score:", median_dms_score, "\n")
cat("Standardabweichung von dms_score:", sd_dms_score, "\n")
cat("95% Konfidenzintervall für dms_score: [", lower_ci_dms_score, ",", upper_ci_dms_score, "]\n")
cat("Wertebereich von dms_score: [", range_dms_score[1], ",", range_dms_score[2], "]\n")

