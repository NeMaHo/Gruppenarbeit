#### Wissenschaftliches Arbeiten ####

## Simulation des Datensatzes:
## Zunaechst einzelne Spalten:

"ID" <- c(1:100)
"Alter" <- rnorm(100, 25, 2)
"Studienfach" <- sample(c("Statistik", "Data Science", "Informatik",
                          "Mathe"), size = 100, 
                        prob = c(0.35, 0.35, 0.25, 0.05),
                        replace = TRUE)

"Interesse an Mathematik" <- c(1:100)
for(i in 1:100){
  if(isTRUE(Studienfach[i] == "Statistik")){
    `Interesse an Mathematik`[i] <- sample(c(1:7), 1, prob = c(0.05, 0.1, 0.1, 0.15, 0.2, 0.3, 0.1))
  } else if(isTRUE(Studienfach[i] == "Data Science")){
    `Interesse an Mathematik`[i] <- sample(c(1:7), 1, prob = c(0.1, 0.15, 0.15, 0.2, 0.2, 0.1, 0.1))
  } else if(isTRUE(Studienfach[i] == "Informatik")){
    `Interesse an Mathematik`[i] <- sample(c(1:7), 1, prob = c(0.2, 0.25, 0.2, 0.15, 0.1, 0.09, 0.01))
  } else if(isTRUE(Studienfach[i] == "Mathe")){
    `Interesse an Mathematik`[i] <- sample(c(1:7), 1, prob = c(0.01, 0.01, 0.01, 0.02, 0.05, 0.4, 0.5))
  }
}


"Interesse an Programmieren" <- c(1:100)
for(i in 1:100){
  if(isTRUE(Studienfach[i] == "Statistik")){
    `Interesse an Programmieren`[i] <- sample(c(1:7), 1, prob = c(0.05, 0.1, 0.1, 0.25, 0.2, 0.2, 0.1))
  } else if(isTRUE(Studienfach[i] == "Data Science")){
    `Interesse an Programmieren`[i] <- sample(c(1:7), 1, prob = c(0.1, 0.1, 0.1, 0.2, 0.2, 0.2, 0.1))
  } else if(isTRUE(Studienfach[i] == "Informatik")){
    `Interesse an Programmieren`[i] <- sample(c(1:7), 1, prob = c(0.01, 0.01, 0.01, 0.01, 0.01, 0.05, 0.9))
  } else if(isTRUE(Studienfach[i] == "Mathe")){
    `Interesse an Programmieren`[i] <- sample(c(1:7), 1, prob = c(0.4, 0.03, 0.2, 0.025, 0.025, 0.025, 0.025))
  }
}

"Mathe-LK(ja/nein)" <- c(1:100)
for(i in 1:100){
  if(isTRUE(Studienfach[i] == "Statistik")){
    `Mathe-LK(ja/nein)`[i] <- sample(c(TRUE, FALSE), 1, prob = c(0.7, 0.3))
  } else if(isTRUE(Studienfach[i] == "Data Science")){
    `Mathe-LK(ja/nein)`[i] <- sample(c(TRUE, FALSE), 1, prob = c(0.4, 0.6))
  } else if(isTRUE(Studienfach[i] == "Informatik")){
    `Mathe-LK(ja/nein)`[i] <- sample(c(TRUE, FALSE), 1, prob = c(0.2, 0.8))
  } else if(isTRUE(Studienfach[i] == "Mathe")){
    `Mathe-LK(ja/nein)`[i] <- sample(c(TRUE, FALSE), 1, prob = c(0.9, 0.1))
  }
}

Datensatz <- data.frame(`ID`, `Alter`, `Studienfach`, `Interesse an Mathematik`,
           `Interesse an Programmieren`, `Mathe-LK(ja/nein)`)



