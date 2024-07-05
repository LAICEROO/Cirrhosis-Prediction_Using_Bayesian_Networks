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
# ustawienie folderu roboczego
setwd('C:/Users/mateu/Desktop/Studia/6 semestr/Wnioskowanie w warunkach niepewności/')
getwd()

# Import danych
dane <- read.csv('students_adaptability_level_online_education.csv')

#### Czyszczenie danych: usuniecie niektorych kolumn, zmiana wartosci na liczbowe, zmiana typu danych  ####
#Edycja danych
# sprawdzenie czy nie ma pustych wartosci
for (i in 1:length(dane)){
  cat('kolumna', i, ': ', any(is.null(dane[,i])),'\n')
}
#usuniecie kolumn malo waznych takich jak, typ instytucji (rzadowa czy prywatna), rodzaj internetu (wifi czy z telefonu),
#lokacje (miasto czy nie), loadsheeding (obicazenie sieci energetycznej), status finansowy, it.student, wiek 1-10
dane$Location <- NULL
dane$Internet.Type <- NULL
dane$Load.shedding <- NULL
dane$Institution.Type <- NULL
dane$Financial.Condition <- NULL
dane$IT.Student <- NULL
dane <- subset(dane, Age != '1-5')

# Wybieranie pierwszych 300 wierszy
dane <- dane[1:300, ]

# konwersja danych na factor
for (i in 1:length(dane)){
  dane[,i] <- as.factor(dane[,i])
  cat('Klasa kolumny', i,':', class(dane[,i]), '\n')
}
pairwise_ci_test <- function(df) {
  n <- ncol(df)
  results <- data.frame(
    Comparison = character(),
    p_value = numeric(),
    Independence = character(),
    stringsAsFactors = FALSE
  )
  
  for (i in 1:(n-1)) {
    for (j in (i+1):n) {
      col1 <- df[[i]]
      col2 <- df[[j]]
      
      if (!is.factor(col1)) col1 <- as.factor(col1)
      if (!is.factor(col2)) col2 <- as.factor(col2)
      
      test_result <- ci.test(col1, col2, test = "x2")
      comparison_name <- paste(names(df)[i], "vs", names(df)[j])
      independence <- ifelse(test_result$p.value < 0.05, "dependent", "independent")
      results <- rbind(results, data.frame(
        Comparison = comparison_name,
        p_value = test_result$p.value,
        Independence = independence
      ))
    }
  }
  
  return(results)
}

wyniki <- pairwise_ci_test(data)
#### Sprawdzanie zależności i tworzenie tabeli zależności ####

# Tworzenie macierdane# Tworzenie macierzy nxn wypełnionej zerami
nazwy <- colnames(dane)
tabela <- matrix(0, nrow = length(dane), ncol = length(dane), dimnames = list(nazwy, nazwy))

# Utworzenie kombinacji
pary_zmiennych <- combn(colnames(dane), 2)

# Przeprowadzenie testu chi-kwadrat dla każdej pary zmiennych
(wyniki <- apply(pary_zmiennych, 2, function(x) {
  test <- ci.test(dane[, x], test = 'x2')
  return(list(zmienne = x, wynik_testu = test))
}))

# Wypełnianie przekątnej wartościami NA
diag(tabela) <- NA

# Iteracja przez kolumny i wypełnianie wartościami z listy wyników

start_idx <- 1
start2 <- 2
for (col_idx in 1:ncol(tabela)) {
  # Wypełnianie wartościami dla kolumny (z pominięciem NA)
  for (j in start2:length(dane)){
    tabela[j, col_idx] <- wyniki[[start_idx]][[2]][[2]]
    cat('start_idx' ,start_idx)
    cat('start2', j,'\n')
    start_idx <- start_idx + 1
    }
  start2 <- start2 + 1
  if (start2 == length(dane)+1)
  {break}
}

#tabela_symetryczna <- matrix(NA, nrow = nrow(tabela), ncol = ncol(tabela))
tabela_symetryczna <- matrix(NA, nrow = length(nazwy), ncol = length(nazwy), 
                             dimnames = list(nazwy, nazwy))

# Przepisanie wartości pod diagonalem symetrycznie nad nią 
for (i in 1:(nrow(tabela) - 1)) {
  for (j in (i + 1):ncol(tabela)) {
    tabela_symetryczna[i, j] <- tabela[j, i]
    tabela_symetryczna[j, i] <- tabela[j, i]
  }
}

print(tabela_symetryczna)

#### siec hc ####
siechc <- hc(dane)
graphviz.plot(siechc)


#### siec iamb ####
sieciamb <- iamb(dane)
graphviz.plot(sieciamb)
sieciamb <- set.arc(sieciamb, "Class.Duration", "Adaptivity.Level")
sieciamb <- set.arc(sieciamb, "Age", "Education.Level")
sieciamb <- set.arc(sieciamb, "Gender", "Age")
sieciamb <- set.arc(sieciamb, "Education.Level", "Device")
sieciamb <- set.arc(sieciamb, "Device", 'Class.Duration')
graphviz.plot(sieciamb)

#### siec gs ####
siecgs <- gs(dane)
graphviz.plot(siecgs)
siecgs<- set.arc(siecgs, "Age", "Education.Level")
siecgs <- set.arc(siecgs, "Network.Type", 'Self.Lms')
siecgs <- set.arc(siecgs, "Education.Level", 'Device')
siecgs <- set.arc(siecgs, "Device", 'Class.Duration')
siecgs <- set.arc(siecgs, 'Class.Duration', 'Adaptivity.Level')
graphviz.plot(siecgs)

#### wyniki (score) ####
## siecgs  ##
cat('Wynik siecgs: ', score(siecgs, data = dane, type = 'bic'))
## siechc  ##
cat('Wynik siechc: ', score(siechc, data = dane, type = 'bic'))
## sieciamb  ##
cat('Wynik sieciamb: ', score(sieciamb, data = dane, type = 'bic'))

#### estymacja ####
(bn <- bn.fit(siechc, data=dane))

#### BN$GENDER ####
bn.fit.barchart(bn$Gender)

#### BN$AGE ####
bn.fit.barchart(bn$Age)

#### BN$EDUCATION.LEVEL ####
bn.fit.barchart(bn$Education.Level)

#### BN$NETWORK.TYPE ####
bn.fit.barchart(bn$Network.Type)

#### BN$DURATION ####
bn.fit.barchart(bn$Class.Duration)

#### BN$SELF.LMS ####
bn.fit.barchart(bn$Self.Lms)

#### BN$DEVICE ####
bn.fit.barchart(bn$Device)

#### BN$ADAPTIVITY.LEVEL ####
bn.fit.barchart(bn$Adaptivity.Level)

#### PRAWDOPODOBIENSTWO ADAPTACYJNOSCI NA POZIOMIE HIGH POD WARUNKIEM WIEKU 16-20 ####
bn_grain <- as.grain(bn)
TEST_adplvl <- setEvidence(bn_grain, nodes = c('Age'), states = c('16-20'))
TEST_adplvl_wynik <- querygrain(TEST_adplvl, nodes = c('Adaptivity.Level'), result = 'array')$Adaptivity.Level
TEST_adplvl_wynik

#### PRAWDOPODOBIENSTWO ADAPTACYJNOSCI NA POZIOMIE MODERATE POD WARUNKIEM ZE ILOSC ZAJEC 1-3 ####
bn_grain_2 <- as.grain(bn)
TEST2_adplvl <- setEvidence(bn_grain_2, nodes = c('Class.Duration'), states = c('1-3'))
TEST2_adplvl_wynik <- querygrain(TEST2_adplvl, nodes = c('Adaptivity.Level'), result = 'array')$Adaptivity.Level
TEST2_adplvl_wynik

#### PRAWDOPODOBIENSTWO, ZE KOBIETA UZYWA TABLETA  ####
bn_grain_3 <- as.grain(bn)
TEST3_adplvl <- setEvidence(bn_grain_3, nodes = c('Device'), states = c("Tab"))
TEST3_adplvl_wynik <- querygrain(TEST3_adplvl, nodes = c('Gender'), result = 'array')$Gender
TEST3_adplvl_wynik
