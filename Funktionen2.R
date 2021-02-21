### Funkionen Skript 2 ###

# Hilfsfunktion zu categorize: category
# -> Bestimmt die richtige Kategorie des Datenpunkts in den Stufen: 
#    "niedrig", "mittel", "hoch" anhand des 1/3 und 2/3 Quantils

# Input: 
# - x: numerischer/kategorischer Vektor
# - value: konkreter numerischer/kategorischer Wert, welcher einzuordnen ist.

# Output: Kategorie des Datenpunkts in Stufen: "niedrig", "mittel", "hoch" 


category <- function(x, value) {
  
  unten <- quantile(x, 1/3)
  oben <- quantile(x, 2/3)
  
  if (value <= unten) {
    return("niedrig")
  } else if(value > unten & value <= oben) {
    return("mittel")
  } else {
    return("hoch")
  }
  
}


# Hilfsfunktion zu plotCategorical: nominalH
# -> Bestimmt Tabellen mit absoluten Haeufigkeiten fuer nominalskalierte Variablen

# Input:
# - x: Vektor

# Output: Tabelle mit absoluten Haeufigkeiten, wenn Vektor nominal, sonst NULL

nominalH <- function(x)
{
  if(is.character(x))
  {
    table(factor(x, levels = levels(as.factor(x))))
  }
}
