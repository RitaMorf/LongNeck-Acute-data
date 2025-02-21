#missing values ermitteln#

#HCC Datensatz:

# Entfernen der Spalte V2-V4 (brauche ich nicht) 
HCC [2:4] <- list(NULL)

# Manuelles Zuweisen der Spaltennamen, falls bekannt
colnames(HCC) <- c("study_id","cortisol1","cortisol2","cortisone1","cortisone2","hairmass1","hairmass2","age","gender")

# Löschen der ersten Zeile
HCC <- HCC [-1, ]

# Spalten definieren, in denen leere Stellen mit NA ersetzt werden sollen
columns_to_fill <- c("cortisol1", "cortisol2", "cortisone1", "cortisone2", "hairmass1", "hairmass2")

# Leere Stellen (repräsentiert als leere Strings oder explizite NAs) in diesen Spalten mit NA ersetzen
HCC[columns_to_fill] <- lapply(HCC[columns_to_fill], function(x) {
  x[x == "" | is.na(x)] <- NA
  return(x)
})

# Zählen der NA's in den Spalten "Cortisol 1" und "Cortisol 2"
na_count_cortisol1 <- sum(is.na(HCC$`cortisol1`))
na_count_cortisol2 <- sum(is.na(HCC$`cortisol2`))

# Ausgabe der NA-Zählergebnisse
print(paste("Anzahl der NA's in cortisol1:", na_count_cortisol1))
print(paste("Anzahl der NA's in cortisol2:", na_count_cortisol2))



#SCI Datensatz:






