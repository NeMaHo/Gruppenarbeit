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
  stopifnot(is.numeric(x))
  return(list(
    Lage = list(Mittelwert = mean(x),
                Modalwert = which.max(table(x)),
                Minimum = min(x),
                Unteres_Quartil = unname(quantile(x, 0.25)),
                Median = median(x),
                Oberes_Quartil = unname(quantile(x, 0.75)),
                Maximum = max(x)),
    
    Streuung = list(Spannweite = diff(range(x)),
                    Varianz = var(x),
                    Standardabweichung = sd(x),
                    Quartilsdifferenz = IQR(x))
  ))
}


# b) deskriptive Statistiken - kategorial

# Funktion - categorical:
# -> Analyse einer kategorialen Variable durch geeignete deskriptive Statistiken

# Input: kategorialer Vektor

# Output: benannte Liste aufgeteilt in Lage- und Streuungsmasse

categorical <- function(x) {
  
  return(list(
    Lage = list(Modalwert = which.max(table(x)),
                Minimum = min(x),
                Unteres_Quartil = unname(quantile(x, 0.25)),
                Median = median(x),
                Oberes_Quartil = unname(quantile(x, 0.75)),
                Maximum = max(x)),
    
    Streuung = list(Spannweite = diff(range(x)),
                    Interquartilsabstand = IQR(x))
  ))
}



# c) deskriptive bivariate Zusammenhangs-Statistiken - kategorial

# Funktion - categoricalRelation:
# -> Berechnung des Rankkorrelationskoeffizientens nach Spearman und Kendall

# Input: 
# - x: kategorialer Vektor
# - y: kategorialer Vektor

# Output: benannte Liste mit zwei Rankkorrelationskoeffizienten von x und y

categoricalRelation <- function(x, y) {
  
  x_r <- rank(x)
  y_r <- rank(y)
  
  return(list(Spearman = cov(x_r, y_r) / (sd(x_r) * sd(y_r)),
              Kendall = Kendall(x_r, y_r)$tau[[1]]))
}



# d) deskriptive bivariate Zusammenhangs-Statistiken - kategorial/metrisch

# Funktion - metricCategoricalRelation:
# -> Berechnung des Rankkorrelationskoeffizientens nach Spearman und Kendall,
#    wobei das metrische Merkmal auf Raenge heruntergebrochen wird

# Input: 
# - x: numerischer Vektor
# - y: kategorialer Vektor

# Output: benannte Liste mit zwei Rankkorrelationskoeffizienten von x und y

metricCategoricalRelation <- function(x, y) {
  stopifnot(is.numeric(x))
  x_r <- rank(x)
  y_r <- rank(y)
  
  return(list(Spearman = cov(x_r, y_r) / (sd(x_r) * sd(y_r)),
              Kendall = Kendall(x_r, y_r)$tau[[1]]))
}



# e) Kategorisierung einer mindestens ordinalen Variable

# Funktion - categorize:
# -> Kategorisierung einer mindestens ordinalen Variable anhand der Quantile in
#    die Bereiche: "niedrig", "mittel", "hoch"

# Input: 
# - x: numerischer/kategorialer Vektor

# Output: kategorisierter Vektor der Stufen: "niedrig", "mittel", "hoch" 

categorize <- function(x) {
  
  x_cat <- vector("character", length(x))
  for (i in seq_along(x)) {
    x_cat[i] <- category(x, x[i]) 
  }
  
  x_cat <- as.factor(x_cat)
  return(x_cat)
}

## Hier ein Alternativvorschlag. Waere effizienter, aber so benoetigten wir
## die Funktion category nicht. Bin mir nicht sicher, ob das nicht eher schlecht
## waere hinsichtlich des Erstellens eines zweiten Skripts :D
categorize2 <- function(x)
{
  stopifnot(is.numeric(x))
  unten <- quantile(x, 1/3)
  oben <- quantile(x, 2/3)
  y <- x
  
  y[x <= unten] <- "niedrig"
  y[(x > unten) & (x <= oben)] <- "mittel"
  y[x > oben] <- "hoch"
  
  return(as.factor(y))
}


# f) Visualisierung von kategorialen Variablen

# Funktion - plotCategorical:
# -> Erstellen von drei oder vier Plots der Inputvariablen.

# Input: 
# - x: Ein Dataframe mit drei oder vier kategorialen Variablen.

# Output: Boxplot fuer ordinale Variablen, Barplot fuer nominale Variablen.

plotCategorical = function(x)
{
  stopifnot(is.data.frame(x))
  #Aufteilung des Fenster abhaengig von Anzahl der Variablen
  if(length(x) == 3)
  {
    layout(matrix(c(1, 2, 3, 3), nr=2, byrow=T))
  }
  else
  {
    par(mfrow = c(2, 2))
  }
  
  for(i in 1:length(x))
  {
    if(is.numeric(x[,i]))
    {
      boxplot(x[,i])
    }
    else
    {
      barplot(nominalH(x[, i]))  
    }
  }  
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

plotCategorical(as.data.frame(matrix(rep(x2,4),100)))

