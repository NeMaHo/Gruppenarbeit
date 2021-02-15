#### Wissenschaftliches Arbeiten ####

## Simulation des Datensatzes:
## Erstellt durch Tim Schmale und Stefan Maier

## Zunaechst einzelne Spalten erstellen:

"ID" <- c(1:100)

"Alter" <- numeric(100)
set.seed(23362)
"Alter" <- rnorm(100, 25, 2)

"Studienfach" <- numeric(100)
set.seed(6474)
"Studienfach" <- sample(c("Statistik", "Data Science", "Informatik",
                          "Mathematik"), size = 100, 
                        prob = c(0.35, 0.35, 0.25, 0.05),
                        replace = TRUE)


"Interesse an Mathematik" <- numeric(100)
set.seed(84638)
for(i in 1:100){
  if(Studienfach[i] == "Statistik"){
    `Interesse an Mathematik`[i] <- sample(c(1:7), 1, prob = c(0.05, 0.1, 0.1, 0.15, 0.2, 0.3, 0.1))
  } else if(Studienfach[i] == "Data Science"){
    `Interesse an Mathematik`[i] <- sample(c(1:7), 1, prob = c(0.1, 0.15, 0.15, 0.2, 0.2, 0.1, 0.1))
  } else if(Studienfach[i] == "Informatik"){
    `Interesse an Mathematik`[i] <- sample(c(1:7), 1, prob = c(0.2, 0.25, 0.2, 0.15, 0.1, 0.09, 0.01))
  } else if(Studienfach[i] == "Mathematik"){
    `Interesse an Mathematik`[i] <- sample(c(1:7), 1, prob = c(0.01, 0.01, 0.01, 0.02, 0.05, 0.4, 0.5))
  }
}

"Interesse an Programmieren" <- numeric(100)
set.seed(84638)
for(i in 1:100){
  if(Studienfach[i] == "Statistik"){
    `Interesse an Programmieren`[i] <- sample(c(1:7), 1, prob = c(0.05, 0.1, 0.1, 0.25, 0.2, 0.2, 0.1))
  } else if(Studienfach[i] == "Data Science"){
    `Interesse an Programmieren`[i] <- sample(c(1:7), 1, prob = c(0.1, 0.1, 0.1, 0.2, 0.2, 0.2, 0.1))
  } else if(Studienfach[i] == "Informatik"){
    `Interesse an Programmieren`[i] <- sample(c(1:7), 1, prob = c(0.01, 0.01, 0.01, 0.01, 0.01, 0.05, 0.9))
  } else if(Studienfach[i] == "Mathematik"){
    `Interesse an Programmieren`[i] <- sample(c(1:7), 1, prob = c(0.4, 0.03, 0.2, 0.025, 0.025, 0.025, 0.025))
  }
}

"Mathematik-LK(ja/nein)" <- integer(100)
set.seed(467836)
for(i in 1:100){
  if(Studienfach[i] == "Statistik"){
    `Mathematik-LK(ja/nein)`[i] <- sample(c(TRUE, FALSE), 1, prob = c(0.7, 0.3))
  } else if(Studienfach[i] == "Data Science"){
    `Mathematik-LK(ja/nein)`[i] <- sample(c(TRUE, FALSE), 1, prob = c(0.4, 0.6))
  } else if(Studienfach[i] == "Informatik"){
    `Mathematik-LK(ja/nein)`[i] <- sample(c(TRUE, FALSE), 1, prob = c(0.2, 0.8))
  } else if(Studienfach[i] == "Mathematik"){
    `Mathematik-LK(ja/nein)`[i] <- sample(c(TRUE, FALSE), 1, prob = c(0.9, 0.1))
  }
}


# Zusammenfassen der Spalten in einem Datensatz
Datensatz <- data.frame(`ID`, `Alter`, `Studienfach`, `Interesse an Mathematik`,
           `Interesse an Programmieren`, `Mathematik-LK(ja/nein)`)

# Schreiben des Datensatzes in CSV-Datei
# write.csv(Datensatz, "data-wissenschaftliches-Arbeiten.csv")
