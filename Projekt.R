#### Projket z Wnioskowania w warunkach niepweności ####

#### Instalacja odpowiednich pakietów ####
install.packages('bnlearn')
install.packages('Rgraphviz')
install.packages('gRain')
install.packages(Rtools)
install.packages("BiocManager")
BiocManager::install("Rgraphviz")
library(lattice)
library(Rgraphviz)
library(bnlearn)
library(gRain)

#### Wczytanie  Ramki danych ####
heart_data <- read.csv("C:/Users/mateu/Desktop/Studia/6 semestr/Wnioskowanie w warunkach niepewności/Prediction/heart_pred.csv")
str(heart_data)
colnames(heart_data)
#### Czyszczenie danych: usuniecie niektorych kolumn, zmiana wartosci na liczbowe, zmiana typu danych  ####
#Edycja danych
# sprawdzenie czy nie ma pustych wartosci
for (i in 1:length(heart_data)){
  cat('kolumna', i, ': ', any(is.null(heart_data)),'\n')
}
#### Zmniejszenie libczy kolumn #### 
heart_data <- heart_data[,-12]
heart_data <- heart_data[,-11]
heart_data <- heart_data[,-9]
heart_data <- heart_data[,-9]
heart_data <- heart_data[,-7]

# konwersja danych na factor
for (i in 1:length(heart_data)){
  heart_data[,i] <- as.factor(heart_data[,i])
  cat('Klasa kolumny', i,':', class(heart_data[,i]), '\n')
}

#### Grupowanie Danych #####

generate_groups <- function(data, column_name) {
  min_genetics <- min(data[[column_name]])
  max_genetics <- max(data[[column_name]])
  
  diff_genetics <- max_genetics - min_genetics
  
  groups <- cut(data[[column_name]], 
                breaks = c(min_genetics - 1, 
                           min_genetics + 0.333 * diff_genetics, 
                           min_genetics + 0.667 * diff_genetics, 
                           max_genetics + 1), 
                labels = c(sprintf("(%.2f, %.2f]", min_genetics - 1, 
                                   min_genetics + 0.333 * diff_genetics), 
                           sprintf("(%.2f, %.2f]", min_genetics + 0.333 * diff_genetics, 
                                   min_genetics + 0.667 * diff_genetics), 
                           sprintf("(%.2f, %.2f]", min_genetics + 0.667 * diff_genetics, 
                                   max_genetics + 1))) 
  
  return(groups)
}

#### siec hc ####
siechc <- hc(heart_data)
graphviz.plot(siechc)

sieciamb <- iamb(heart_data)
graphviz.plot(sieciamb)
sieciamb <- set.arc(sieciamb, "chol", "cp")
sieciamb <- set.arc(sieciamb, "age", "sex")
sieciamb <- set.arc(sieciamb, "trpbps", "tchalachh")
sieciamb <- set.arc(sieciamb, "oldpeak", 'Class.Duration')
graphviz.plot(sieciamb)
