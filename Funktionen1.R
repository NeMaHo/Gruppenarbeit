### Funkionen Skript 1 ###

# benoetigte Pakete
#install.packages("Kendall")
library(Kendall)

# Working Directory muss korrekt gesetzt sein!
source("Funktionen2.R")

# a) deskriptive Statistiken - metrisch

# Funktion - metric:
# -> Analyse einer metrischen Variable durch geeignete deskriptive Statistiken

# Input: numerischer Vektor

# Output: benannte Liste aufgeteilt in Lage- und Streuungsmasse

metric <- function(x) {
  
  # Lagemasse
  mittel <- mean(x)
  med <- median(x)
  modalwert <- which.max(table(x))
  oberesQ <- unname(quantile(x, 0.75))
  unteresQ <- unname(quantile(x, 0.25))
  minimum <- min(x)
  maximum <- max(x)
  
  # Streuungsmasse
  spannweite <- maximum - minimum
  varianz <- var(x)
  stand <- sd(x)
  quart <- oberesQ - unteresQ
  
  return(list(
    Lage = list(Mittelwert = mittel,
                Modalwert = modalwert,
                Minimum = minimum,
                Unteres_Quartil = unteresQ,
                Median = med,
                Oberes_Quartil = oberesQ,
                Maximum = maximum
                ),
    Streuung = list(Spannweite = spannweite,
                    Varianz = varianz,
                    Standardabweichung = stand,
                    Quartilsdifferenz = quart
    )
  ))
}


# b) deskriptive Statistiken - kategorisch

# Funktion - categorical:
# -> Analyse einer kategorischen Variable durch geeignete deskriptive Statistiken

# Input: kategroischer Vektor

# Output: benannte Liste aufgeteilt in Lage- und Streuungsmasse

categorical <- function(x) {
  
  # Lagemasse
  med <- median(x)
  modalwert <- which.max(table(x))
  oberesQ <- unname(quantile(x, 0.75))
  unteresQ <- unname(quantile(x, 0.25))
  minimum <- min(x)
  maximum <- max(x)
  
  # Streuungsmasse
  spannweite <- maximum - minimum
  varianz <- var(x)
  stand <- sd(x)
  quart <- oberesQ - unteresQ
  
  return(list(
    Lage = list(Modalwert = modalwert,
                Minimum = minimum,
                Unteres_Quartil = unteresQ,
                Median = med,
                Oberes_Quartil = oberesQ,
                Maximum = maximum
    ),
    Streuung = list(Spannweite = spannweite,
                    Quartilsdifferenz = quart
    )
  ))
}



# c) deskriptive bivariate Zusammenhangs-Statistiken - kategorisch

# Funktion - categoricalRelation:
# -> Berechnung des Rankkorrelationskoeffizientens nach Spearman und Kendall

# Input: 
# - x: kategorischer Vektor
# - y: kategorischer Vektor

# Output: benannte Liste mit zwei Rankkorrelationskoeffizienten von x und y

categoricalRelation <- function(x, y) {
  
  x_r <- rank(x)
  y_r <- rank(y)
  
  kend <- Kendall(x_r, y_r)$tau[[1]]
  spear <- cov(x_r, y_r) / (sd(x_r) * sd(y_r))
  
  return(list(Spearman = spear,
              Kendall = kend))
}



# d) deskriptive bivariate Zusammenhangs-Statistiken - kategorisch/metrisch

# Funktion - metricCategoricalRelation:
# -> Berechnung des Rankkorrelationskoeffizientens nach Spearman und Kendall,
#    wobei das metrische Merkmal auf Raenge heruntergebrochen wird

# Input: 
# - x: numerischer Vektor
# - y: kategorischer Vektor

# Output: benannte Liste mit zwei Rankkorrelationskoeffizienten von x und y

metricCategoricalRelation <- function(x, y) {
  
  x_r <- rank(x)
  y_r <- rank(y)
  
  kend <- Kendall(x_r, y_r)$tau[[1]]
  spear <- cov(x_r, y_r) / (sd(x_r) * sd(y_r))
  
  return(list(Spearman = spear,
              Kendall = kend))
}



# e) Kategorisierung einer mindestens ordinalen Variable

# Funktion - categorize:
# -> Kategorisierung einer mindestens ordinalen Variable anhand der Quantile in
#    die Bereiche: "niedrig", "mittel", "hoch"

# Input: 
# - x: numerischer/kategorischer Vektor

# Output: kategorisierter Vektor der Stufen: "niedrig", "mittel", "hoch" 

categorize <- function(x) {
  
  x_cat <- vector("character", length(x))
  for (i in seq_along(x)) {
    x_cat[i] <- category(x, x[i]) 
  }
  
  x_cat <- as.factor(x_cat)
  return(x_cat)
}



# f) Visualisierung von kategorialen Variablen

# Funktion - plotCategorical:
# -> Erstellen eines gemeinsamen Boxplots der Inputvektoren

# Input: 
# - ...: Einzelne kategoriale Vektoren oder eine Matrix kategorialer Vektoren

# Output: Boxplot der kategorialen Variablen

plotCategorical <- function(...) {
  boxplot(...)
}

## Testbereich
x1 <- rnorm(100)
x2 <- sample(1:7, 100, replace = TRUE, 
             prob = c(0.2, 0.25, 0.2, 0.15, 0.1, 0.09, 0.01))
x3 <- sample(1:7, 100, replace = TRUE, 
             prob = c(0.01, 0.01, 0.01, 0.02, 0.05, 0.4, 0.5))

metric(x1)

categorical(x2)

categoricalRelation(x2, x3)

metricCategoricalRelation(x1, x2)

categorize(x1)
categorize(x2)

plotCategorical(x1,x2)
