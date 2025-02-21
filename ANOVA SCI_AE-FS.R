#Akutdaten-AE-FS Groups und Stress (SCI)#
#Um zu bestimmen, welche Gruppe in den Aktivitätsmustern (Fear Avoidance, Persistence, Pacing) die höchsten Werte in den subjektiven und objektiven Stressparametern hat, können Sie eine Varianzanalyse (ANOVA) verwenden, gefolgt von einem Post-Hoc-Test, um die Unterschiede zwischen den Gruppen zu identifizieren.

Hier ist der Schritt-für-Schritt-Code in R:
  
  Vorbereitung des Datensatzes:
  Stellen Sie sicher, dass Ihre Daten in einem geeigneten Format vorliegen und dass die Aktivitätsmuster in einer faktorisierten Spalte vorliegen.

Varianzanalyse (ANOVA):
  Führen Sie die ANOVA durch, um zu sehen, ob es signifikante Unterschiede zwischen den Gruppen gibt.

Post-Hoc-Test:
  Wenn die ANOVA signifikant ist, verwenden Sie einen Post-Hoc-Test, um zu identifizieren, welche Gruppen sich signifikant unterscheiden.

# Beispiel-Datensatz vorbereiten
# Angenommen, der Datensatz heißt 'Baseline' und enthält die Spalten 'activity_pattern', 'subjektiver_Stress' und 'Kortisol'

# Aktivitätsmuster als Faktor setzen
Baseline$activity_pattern <- factor(Baseline$activity_pattern, levels = c("Fear Avoidance", "Persistence", "Pacing"))

# ANOVA für subjektiven Stress
anova_subj_stress <- aov(subjektiver_Stress ~ activity_pattern, data = Baseline)
summary(anova_subj_stress)

# ANOVA für objektiven Stress
anova_obj_stress <- aov(Kortisol ~ activity_pattern, data = Baseline)
summary(anova_obj_stress)

# Post-Hoc-Test, wenn die ANOVA signifikant ist
# Hier verwenden wir den Tukey HSD Test
if (summary(anova_subj_stress)[[1]][["Pr(>F)"]][1] < 0.05) {
  posthoc_subj_stress <- TukeyHSD(anova_subj_stress)
  print(posthoc_subj_stress)
}

if (summary(anova_obj_stress)[[1]][["Pr(>F)"]][1] < 0.05) {
  posthoc_obj_stress <- TukeyHSD(anova_obj_stress)
  print(posthoc_obj_stress)
}
