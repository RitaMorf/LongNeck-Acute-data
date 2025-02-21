#Akutdaten-Physical Activity#
#26.06.2024#

# Entfernen der Spalte V2-V4 (brauche ich nicht) 
Physical.Activity [2:4] <- list(NULL)


#Physical Activity: IPAQ-SF: activity

# Sicherstellen, dass die Werte numerisch sind
Physical.Activity$met_total <- as.numeric(Physical.Activity$met_total)

# Berechnung des Mittelwerts
mean_met_total <- mean(Physical.Activity$met_total, na.rm = TRUE)

# Berechnung des Medians
median_met_total <- median(Physical.Activity$met_total, na.rm = TRUE)

# Berechnung der Standardabweichung
sd_met_total <- sd(Physical.Activity$met_total, na.rm = TRUE)

# Ausgabe der Ergebnisse
mean_met_total
median_met_total
sd_met_total

# Anzahl der gültigen Beobachtungen (nicht NA)
n_met_total <- sum(!is.na(Physical.Activity$met_total))

# Z-Wert für 95% Konfidenzintervall (zweischwänzig)
z_value_met_total <- qnorm(0.975)  # qnorm(0.975) entspricht etwa 1.96

# Berechnung des Konfidenzintervalls
ci_width_met_total <- z_value_met_total * (sd_met_total / sqrt(n_met_total))
lower_ci_met_total <- mean_met_total - ci_width_met_total
upper_ci_met_total <- mean_met_total + ci_width_met_total

# Berechnung des Wertebereichs
range_met_total <- range(Physical.Activity$met_total, na.rm = TRUE)

# Ausgabe der Ergebnisse
cat("Mittelwert von MET Total:", mean_met_total, "\n")
cat("Median von MET Total:", median_met_total, "\n")
cat("Standardabweichung von MET Total:", sd_met_total, "\n")
cat("95% Konfidenzintervall für MET Total: [", lower_ci_met_total, ",", upper_ci_met_total, "]\n")
cat("Wertebereich von MET Total: [", range_met_total[1], ",", range_met_total[2], "]\n")


#Physical Activity: IPAQ-SF: sitting
# Sicherstellen, dass die Werte numerisch sind
Physical.Activity$total_sitting_mins <- as.numeric(Physical.Activity$total_sitting_mins)

# Berechnung des Mittelwerts
mean_total_sitting_mins <- mean(Physical.Activity$total_sitting_mins, na.rm = TRUE)

# Berechnung des Medians
median_total_sitting_mins <- median(Physical.Activity$total_sitting_mins, na.rm = TRUE)

# Berechnung der Standardabweichung
sd_total_sitting_mins <- sd(Physical.Activity$total_sitting_mins, na.rm = TRUE)

# Ausgabe der Ergebnisse
mean_total_sitting_mins
median_total_sitting_mins
sd_total_sitting_mins

# Anzahl der gültigen Beobachtungen (nicht NA)
n_total_sitting_mins <- sum(!is.na(Physical.Activity$total_sitting_mins))

# Z-Wert für 95% Konfidenzintervall (zweischwänzig)
z_value_total_sitting_mins <- qnorm(0.975)  # qnorm(0.975) entspricht etwa 1.96

# Berechnung des Konfidenzintervalls
ci_width_total_sitting_mins <- z_value_total_sitting_mins * (sd_total_sitting_mins / sqrt(n_total_sitting_mins))
lower_ci_total_sitting_mins <- mean_total_sitting_mins - ci_width_total_sitting_mins
upper_ci_total_sitting_mins <- mean_total_sitting_mins + ci_width_total_sitting_mins

# Berechnung des Wertebereichs
range_total_sitting_mins <- range(Physical.Activity$total_sitting_mins, na.rm = TRUE)

# Ausgabe der Ergebnisse
cat("Mittelwert von Total Sitting Minutes:", mean_total_sitting_mins, "\n")
cat("Median von Total Sitting Minutes:", median_total_sitting_mins, "\n")
cat("Standardabweichung von Total Sitting Minutes:", sd_total_sitting_mins, "\n")
cat("95% Konfidenzintervall für Total Sitting Minutes: [", lower_ci_total_sitting_mins, ",", upper_ci_total_sitting_mins, "]\n")
cat("Wertebereich von Total Sitting Minutes: [", range_total_sitting_mins[1], ",", range_total_sitting_mins[2], "]\n")



