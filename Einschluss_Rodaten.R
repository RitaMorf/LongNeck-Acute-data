# Anzahl der 1-Werte (YES) in der Spalte "inclusion" zählen = Einschluss
anzahl_Einschluss <- sum(Einschluss_Rohdaten$inclusion == 1, na.rm = TRUE)

# Ausgabe der Anzahl
anzahl_Einschluss

# Anzahl der 0-Werte (NO) in der Spalte "inclusion" zählen = Ausschluss
anzahl_Ausschluss <- sum(Einschluss_Rohdaten$inclusion == 0, na.rm = TRUE)

# Ausgabe der Anzahl
anzahl_Ausschluss
