### Funkionen Skript 2 ###

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
