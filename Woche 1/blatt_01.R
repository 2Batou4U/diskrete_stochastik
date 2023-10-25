my_median <- function(data_vektor) {
  # Input der Funktion ist ein Vektor data_vektor
  # Output ist der Median der Vektorkomponenten

  # Finde die Länge von data_vektor
  laenge <- length(data_vektor)

  # Wir können nur den Median berechnen, wenn data_vektor eine Länge größer als 0 hat
  if (laenge == 0) {
    stop("Input hat Länge 0")
  }

  # Benutze die Funktion sort um die Komponente der Variable data_vektor zu sortieren
  data_vektor_steigend <- sort(data_vektor, decreasing = FALSE)

  # Benutze die Länge von data_vektor um einen Median zu berechnen.
  # Die Funktion soll funktionieren für eine gerade und eine ungerade Länge.

  # Finde den Index bei der Hälfte der Gesamtelemente. Falls Kommazahl, dann runden.
  halfway_idx <- ceiling(laenge / 2)

  # Hat der Vektor eine gerade Länge, dann gebe zwei mittlerste Elemente wieder,
  # sonst nur das mittlerste Element.
  if (laenge %% 2 == 0) {
    resultat <- data_vektor_steigend[c(floor(halfway_idx), floor(halfway_idx) + 1)]
  } else {
    resultat <- data_vektor_steigend[halfway_idx]
  }

  return(resultat)
}

mittelwert <- function(data_vektor) {
  # Input der Funktion ist ein Vektor data_vektor
  # Output ist der Mittelwert der Vektor

  # Finde die Länge von data_vektor
  laenge <- length(data_vektor)

  # Wir können nur den Mittelwert berechnen, wenn data_vektor eine Länge größer als 0 hat
  if (laenge == 0) {
    stop("Input hat Länge 0")
  }

  # Benutze eine For-Schleife um die Werte in data_vektor zu Summieren
  summe <- 0 # Anfangswert
  for (i in 1:laenge) {
    summe <- summe + data_vektor[i]
  }

  # Teile die Summe durch die Länge
  resultat <- summe / laenge

  return(resultat)
}
