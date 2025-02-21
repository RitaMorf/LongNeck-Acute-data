#Akutdaten-Schmerz-Schlaf-Disability#
#26.06.2024#


# Entfernen der Spalte V2-V4 (brauche ich nicht) 
Baseline [2:4] <- list(NULL)

# Manuelles Zuweisen der Spaltennamen, falls bekannt
colnames(Baseline) <- c("study_id","sleepquality","paindetect1","paindetect2","paindetect3","paindetect_mean","ndi")

# Löschen der ersten Zeile
Baseline <- Baseline [-1, ]


#PainDetect

# Sicherstellen, dass die Werte numerisch sind
Baseline$paindetect_mean <- as.numeric(Baseline$paindetect_mean)
Baseline$ndi <- as.numeric(Baseline$ndi)

# Auf null Kommastellen runden
Baseline$paindetect_mean <- round(Baseline$paindetect_mean, digits = 0)
Baseline$ndi <- round(Baseline$ndi, digits = 0)

# Entfernen der Spalte paindetect_mean da nicht alle Daten vorhanden
Baseline [6] <- list(NULL)

# Sicherstellen, dass die Werte numerisch sind
Baseline$paindetect1 <- as.numeric(Baseline$paindetect1)
Baseline$paindetect2 <- as.numeric(Baseline$paindetect2)
Baseline$paindetect3 <- as.numeric(Baseline$paindetect3)


# Berechnen des Mittelwerts der Spalten "paindetect1", "paindetect2" und "paindetect3"
Baseline$paindetect_mean <- rowMeans(Baseline[, c("paindetect1", "paindetect2", "paindetect3")], na.rm = TRUE)

# Überprüfen, ob die neue Spalte hinzugefügt wurde
head(Baseline)

# Auf zwei Kommastellen runden
Baseline$paindetect_mean <- round(Baseline$paindetect_mean, digits = 2)

# Berechnung des Mittelwerts
mean_paindetect_mean <- mean(Baseline$paindetect_mean, na.rm = TRUE)

# Berechnung des Medians
median_paindetect_mean <- median(Baseline$paindetect_mean, na.rm = TRUE)

# Berechnung der Standardabweichung
sd_paindetect_mean <- sd(Baseline$paindetect_mean, na.rm = TRUE)

# Ausgabe der Ergebnisse
mean_paindetect_mean
median_paindetect_mean
sd_paindetect_mean

# Anzahl der gültigen Beobachtungen (nicht NA)
n <- sum(!is.na(Baseline$paindetect_mean))

# Z-Wert für 95% Konfidenzintervall (zweischwänzig)
z <- qnorm(0.975)  # qnorm(0.975) entspricht etwa 1.96

# Berechnung des Konfidenzintervalls
ci_width <- z * (sd_paindetect_mean / sqrt(n))
lower_ci <- mean_paindetect_mean - ci_width
upper_ci <- mean_paindetect_mean + ci_width

# Ausgabe der Ergebnisse
cat("Mittelwert von PainDetect Mean:", mean_paindetect_mean, "\n")
cat("Median von PainDetect Mean:", median_paindetect_mean, "\n")
cat("Standardabweichung von PainDetect Mean:", sd_paindetect_mean, "\n")
cat("95% Konfidenzintervall für PainDetect Mean: [", lower_ci, ",", upper_ci, "]\n")



#NDI

# Berechnung des Mittelwerts
mean_NDI <- mean(Baseline$ndi, na.rm = TRUE)

# Berechnung des Medians
median_NDI <- median(Baseline$ndi, na.rm = TRUE)

# Berechnung der Standardabweichung
sd_NDI <- sd(Baseline$ndi, na.rm = TRUE)

# Ausgabe der Ergebnisse
mean_NDI
median_NDI
sd_NDI

# Anzahl der gültigen Beobachtungen (nicht NA)
n_NDI <- sum(!is.na(Baseline$ndi))

# Z-Wert für 95% Konfidenzintervall (zweischwänzig)
z_NDI <- qnorm(0.975)  # qnorm(0.975) entspricht etwa 1.96

# Berechnung des Konfidenzintervalls
ci_width_NDI <- z_NDI * (sd_NDI / sqrt(n_NDI))
lower_ci_NDI <- mean_NDI - ci_width_NDI
upper_ci_NDI <- mean_NDI + ci_width_NDI

# Ausgabe der Ergebnisse
cat("Mittelwert von NDI:", mean_NDI, "\n")
cat("Median von NDI:", median_NDI, "\n")
cat("Standardabweichung von NDI:", sd_NDI, "\n")
cat("95% Konfidenzintervall für NDI: [", lower_ci_NDI, ",", upper_ci_NDI, "]\n")
    

#Sleep 

# Sicherstellen, dass die Werte numerisch sind
Baseline$sleepquality <- as.numeric(Baseline$sleepquality)

# Berechnung des Mittelwerts
mean_sleepquality <- mean(Baseline$sleepquality, na.rm = TRUE)

# Berechnung des Medians
median_sleepquality <- median(Baseline$sleepquality, na.rm = TRUE)

# Berechnung der Standardabweichung
sd_sleepquality <- sd(Baseline$sleepquality, na.rm = TRUE)

# Ausgabe der Ergebnisse
mean_sleepquality
median_sleepquality
sd_sleepquality


# Ermitteln des niedrigsten Wertes in der Spalte sleepquality
min_sleep_quality <- min(Baseline$sleepquality, na.rm = TRUE)

# Ermitteln des höchsten Wertes in der Spalte sleepquality
max_sleep_quality <- max(Baseline$sleepquality, na.rm = TRUE)

# Ausgabe der Werte
print(paste("Niedrigster Wert der Schlafqualität:", min_sleep_quality))
print(paste("Höchster Wert der Schlafqualität:", max_sleep_quality))

# Anzahl der gültigen Beobachtungen (nicht NA)
n_sleepquality <- sum(!is.na(Baseline$sleepquality))

# Z-Wert für 95% Konfidenzintervall (zweischwänzig)
z_sleepquality <- qnorm(0.975)  # qnorm(0.975) entspricht etwa 1.96

# Berechnung des Konfidenzintervalls
ci_width_sleepquality <- z_sleepquality * (sd_sleepquality / sqrt(n_sleepquality))
lower_ci_sleepquality <- mean_sleepquality - ci_width_sleepquality
upper_ci_sleepquality <- mean_sleepquality + ci_width_sleepquality

# Ausgabe der Ergebnisse
cat("Mittelwert von Sleep Quality:", mean_sleepquality, "\n")
cat("Median von Sleep Quality:", median_sleepquality, "\n")
cat("Standardabweichung von Sleep Quality:", sd_sleepquality, "\n")
cat("95% Konfidenzintervall für Sleep Quality: [", lower_ci_sleepquality, ",", upper_ci_sleepquality, "]\n")



# Ermitteln des niedrigsten Wertes in der Spalte ndi
min_ndi <- min(Baseline$ndi, na.rm = TRUE)

# Ermitteln des höchsten Wertes in der Spalte ndi
max_ndi <- max(Baseline$ndi, na.rm = TRUE)

# Ausgabe der Werte
print(paste("Niedrigster Wert der NDI (Neck Disability Index):", min_ndi))
print(paste("Höchster Wert der NDI (Neck Disability Index):", max_ndi))



# Ermitteln des niedrigsten Wertes in der Spalte paindetect_mean
min_paindetect_mean <- min(Baseline$paindetect_mean, na.rm = TRUE)

# Ermitteln des höchsten Wertes in der Spalte paindetect_mean
max_paindetect_mean <- max(Baseline$paindetect_mean, na.rm = TRUE)

# Ausgabe der Werte
print(paste("Niedrigster Wert von Pain Detect Mean:", min_paindetect_mean))
print(paste("Höchster Wert von Pain Detect Mean:", max_paindetect_mean))
