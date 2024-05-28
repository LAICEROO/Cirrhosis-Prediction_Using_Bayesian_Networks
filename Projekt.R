#### Instalacja odpowiednich pakietów ####
install.packages('bnlearn')
install.packages('Rgraphviz')
install.packages('gRain')
if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install("Rgraphviz")

library(lattice)
library(Rgraphviz)
library(bnlearn)
library(gRain)

# ustawienie folderu roboczego, jeśli potrzebne
#setwd('C:/Users/olsze/Desktop/wnioskowanie_w_warunkach_niepewnosci_projekt')
#getwd()

# Import danych
dane <- read.csv('/mnt/data/heart_pred.csv')

#### Czyszczenie danych: usuniecie niektorych kolumn, zmiana wartosci na liczbowe, zmiana typu danych  ####
# Sprawdzenie czy nie ma pustych wartości
for (i in 1:length(dane)){
  cat('kolumna', i, ': ', any(is.na(dane[,i])),'\n')
}

# Wybieranie pierwszych 300 wierszy
dane <- dane[1:300, ]

# Konwersja danych na factor
for (i in 1:length(dane)){
  dane[,i] <- as.factor(dane[,i])
  cat('Klasa kolumny', i, ':', class(dane[,i]), '\n')
}

#### Sprawdzanie zależności i tworzenie tabeli zależności ####

# Tworzenie macierzy nxn wypełnionej zerami
nazwy <- colnames(dane)
tabela <- matrix(0, nrow = length(dane), ncol = length(dane), dimnames = list(nazwy, nazwy))

# Utworzenie kombinacji
pary_zmiennych <- combn(colnames(dane), 2)

# Przeprowadzenie testu chi-kwadrat dla każdej pary zmiennych
wyniki <- apply(pary_zmiennych, 2, function(x) {
  test <- ci.test(dane[, x], test = 'x2')
  return(list(zmienne = x, wynik_testu = test))
})

# Wypełnianie przekątnej wartościami NA
diag(tabela) <- NA

# Wypełnianie tabeli wynikami testów chi-kwadrat
for (i in 1:length(wyniki)) {
  zmienne <- wyniki[[i]]$zmienne
  p_value <- wyniki[[i]]$wynik_testu$p.value
  tabela[zmienne[1], zmienne[2]] <- p_value
  tabela[zmienne[2], zmienne[1]] <- p_value
}

# Ustalanie łuków sieci
siecgs <- empty.graph(nazwy)

# Tworzenie sieci
siechc <- hc(dane)
sieciamb <- iamb(dane)

# Rysowanie sieci
graphviz.plot(siechc)
graphviz.plot(siecgs)

#### Wyniki (score) ####
cat('Wynik siecgs: ', score(siecgs, data = dane, type = 'bic'), '\n')
cat('Wynik siechc: ', score(siechc, data = dane, type = 'bic'), '\n')
cat('Wynik sieciamb: ', score(sieciamb, data = dane, type = 'bic'), '\n')

#### Estymacja ####
bn <- bn.fit(siechc, data=dane)

#### Wizualizacja ####
bn.fit.barchart(bn$sex)
bn.fit.barchart(bn$age)
bn.fit.barchart(bn$chest_pain_type)
bn.fit.barchart(bn$resting_bp_s)
bn.fit.barchart(bn$cholesterol)
bn.fit.barchart(bn$fasting_blood_sugar)
bn.fit.barchart(bn$resting_ecg)
bn.fit.barchart(bn$max_heart_rate)
bn.fit.barchart(bn$exercise_angina)
bn.fit.barchart(bn$oldpeak)
bn.fit.barchart(bn$ST_slope)
bn.fit.barchart(bn$target)

#### Prawdopodobieństwa warunkowe ####
bn_grain <- as.grain(bn)

# Przykład: prawdopodobieństwo 'target' pod warunkiem 'age' == '40'
TEST_target <- setEvidence(bn_grain, nodes = c('age'), states = c('40'))
TEST_target_wynik <- querygrain(TEST_target, nodes = c('target'), result = 'array')$target
print(TEST_target_wynik)

# Przykład: prawdopodobieństwo 'target' pod warunkiem 'chest_pain_type' == '2'
TEST2_target <- setEvidence(bn_grain, nodes = c('chest_pain_type'), states = c('2'))
TEST2_target_wynik <- querygrain(TEST2_target, nodes = c('target'), result = 'array')$target
print(TEST2_target_wynik)

# Przykład: prawdopodobieństwo, że 'sex' pod warunkiem 'exercise_angina' == '0'
TEST3_sex <- setEvidence(bn_grain, nodes = c('exercise_angina'), states = c('0'))
TEST3_sex_wynik <- querygrain(TEST3_sex, nodes = c('sex'), result = 'array')$sex
print(TEST3_sex_wynik)


#### Z bota 1 
#### Instalacja odpowiednich pakietów ####
install.packages('bnlearn')
install.packages('Rgraphviz')
install.packages('gRain')
if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install("Rgraphviz")

library(lattice)
library(Rgraphviz)
library(bnlearn)
library(gRain)

# ustawienie folderu roboczego, jeśli potrzebne
#setwd('C:/Users/olsze/Desktop/wnioskowanie_w_warunkach_niepewnosci_projekt')
#getwd()

# Import danych
dane <- read.csv('/mnt/data/heart_pred.csv')

#### Czyszczenie danych: usuniecie niektorych kolumn, zmiana wartosci na liczbowe, zmiana typu danych  ####
# Sprawdzenie czy nie ma pustych wartości
for (i in 1:length(dane)){
  cat('kolumna', i, ': ', any(is.null(dane[,i])),'\n')
}

# Przykład usunięcia kolumn - dostosuj do swojego zbioru danych
# dane$Location <- NULL
# dane$Internet.Type <- NULL
# dane$Load.shedding <- NULL
# dane$Institution.Type <- NULL
# dane$Financial.Condition <- NULL
# dane$IT.Student <- NULL
# dane <- subset(dane, Age != '1-5')

# Wybieranie pierwszych 300 wierszy
dane <- dane[1:300, ]

# Konwersja danych na factor
for (i in 1:length(dane)){
  dane[,i] <- as.factor(dane[,i])
  cat('Klasa kolumny', i, ':', class(dane[,i]), '\n')
}

#### Sprawdzanie zależności i tworzenie tabeli zależności ####

# Tworzenie macierzy nxn wypełnionej zerami
nazwy <- colnames(dane)
tabela <- matrix(0, nrow = length(dane), ncol = length(dane), dimnames = list(nazwy, nazwy))

# Utworzenie kombinacji
pary_zmiennych <- combn(colnames(dane), 2)

# Przeprowadzenie testu chi-kwadrat dla każdej pary zmiennych
wyniki <- apply(pary_zmiennych, 2, function(x) {
  test <- ci.test(dane[, x], test = 'x2')
  return(list(zmienne = x, wynik_testu = test))
})

# Wypełnianie przekątnej wartościami NA
diag(tabela) <- NA

# Wypełnianie tabeli wynikami testów chi-kwadrat
for (i in 1:length(wyniki)) {
  zmienne <- wyniki[[i]]$zmienne
  p_value <- wyniki[[i]]$wynik_testu$p.value
  tabela[zmienne[1], zmienne[2]] <- p_value
  tabela[zmienne[2], zmienne[1]] <- p_value
}

# Ustalanie łuków sieci
siecgs <- empty.graph(nazwy)
siecgs <- set.arc(siecgs, "A", "B")  # Przykład, dostosuj do swoich danych

# Tworzenie sieci
siechc <- hc(dane)
sieciamb <- iamb(dane)

# Rysowanie sieci
graphviz.plot(siechc)
graphviz.plot(siecgs)

#### Wyniki (score) ####
cat('Wynik siecgs: ', score(siecgs, data = dane, type = 'bic'))
cat('Wynik siechc: ', score(siechc, data = dane, type = 'bic'))
cat('Wynik sieciamb: ', score(sieciamb, data = dane, type = 'bic'))

#### Estymacja ####
bn <- bn.fit(siechc, data=dane)

#### Wizualizacja ####
bn.fit.barchart(bn$Gender)
bn.fit.barchart(bn$Age)
bn.fit.barchart(bn$Education.Level)
bn.fit.barchart(bn$Network.Type)
bn.fit.barchart(bn$Class.Duration)
bn.fit.barchart(bn$Self.Lms)
bn.fit.barchart(bn$Device)
bn.fit.barchart(bn$Adaptivity.Level)

#### Prawdopodobieństwa warunkowe ####
bn_grain <- as.grain(bn)

# Przykład: prawdopodobieństwo adaptacyjności na poziomie wysokim dla wieku 16-20
TEST_adplvl <- setEvidence(bn_grain, nodes = c('Age'), states = c('16-20'))
TEST_adplvl_wynik <- querygrain(TEST_adplvl, nodes = c('Adaptivity.Level'), result = 'array')$Adaptivity.Level
print(TEST_adplvl_wynik)

# Przykład: prawdopodobieństwo adaptacyjności na poziomie umiarkowanym dla klasy trwającej 1-3
TEST2_adplvl <- setEvidence(bn_grain, nodes = c('Class.Duration'), states = c('1-3'))
TEST2_adplvl_wynik <- querygrain(TEST2_adplvl, nodes = c('Adaptivity.Level'), result = 'array')$Adaptivity.Level
print(TEST2_adplvl_wynik)

# Przykład: prawdopodobieństwo, że kobieta używa tabletu
TEST3_adplvl <- setEvidence(bn_grain, nodes = c('Device'), states = c("Tab"))
TEST3_adplvl_wynik <- querygrain(TEST3_adplvl, nodes = c('Gender'), result = 'array')$Gender
print(TEST3_adplvl_wynik)



#### Z bota 2
#### Instalacja odpowiednich pakietów ####
install.packages('bnlearn')
install.packages('Rgraphviz')
install.packages('gRain')
if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install("Rgraphviz")

library(lattice)
library(Rgraphviz)
library(bnlearn)
library(gRain)

# ustawienie folderu roboczego, jeśli potrzebne
#setwd('C:/Users/olsze/Desktop/wnioskowanie_w_warunkach_niepewnosci_projekt')
#getwd()

# Import danych
dane <- read.csv('/mnt/data/heart_pred.csv')

#### Czyszczenie danych: usuniecie niektorych kolumn, zmiana wartosci na liczbowe, zmiana typu danych  ####
# Sprawdzenie czy nie ma pustych wartości
for (i in 1:length(dane)){
  cat('kolumna', i, ': ', any(is.na(dane[,i])),'\n')
}

# Wybieranie pierwszych 300 wierszy
dane <- dane[1:300, ]

# Konwersja danych na factor
for (i in 1:length(dane)){
  dane[,i] <- as.factor(dane[,i])
  cat('Klasa kolumny', i, ':', class(dane[,i]), '\n')
}

#### Sprawdzanie zależności i tworzenie tabeli zależności ####

# Tworzenie macierzy nxn wypełnionej zerami
nazwy <- colnames(dane)
tabela <- matrix(0, nrow = length(dane), ncol = length(dane), dimnames = list(nazwy, nazwy))

# Utworzenie kombinacji
pary_zmiennych <- combn(colnames(dane), 2)

# Przeprowadzenie testu chi-kwadrat dla każdej pary zmiennych
wyniki <- apply(pary_zmiennych, 2, function(x) {
  test <- ci.test(dane[, x], test = 'x2')
  return(list(zmienne = x, wynik_testu = test))
})

# Wypełnianie przekątnej wartościami NA
diag(tabela) <- NA

# Wypełnianie tabeli wynikami testów chi-kwadrat
for (i in 1:length(wyniki)) {
  zmienne <- wyniki[[i]]$zmienne
  p_value <- wyniki[[i]]$wynik_testu$p.value
  tabela[zmienne[1], zmienne[2]] <- p_value
  tabela[zmienne[2], zmienne[1]] <- p_value
}

# Ustalanie łuków sieci
siecgs <- empty.graph(nazwy)

# Tworzenie sieci
siechc <- hc(dane)
sieciamb <- iamb(dane)

# Rysowanie sieci
graphviz.plot(siechc)
graphviz.plot(siecgs)

#### Wyniki (score) ####
cat('Wynik siecgs: ', score(siecgs, data = dane, type = 'bic'), '\n')
cat('Wynik siechc: ', score(siechc, data = dane, type = 'bic'), '\n')
cat('Wynik sieciamb: ', score(sieciamb, data = dane, type = 'bic'), '\n')

#### Estymacja ####
bn <- bn.fit(siechc, data=dane)

#### Wizualizacja ####
bn.fit.barchart(bn$sex)
bn.fit.barchart(bn$age)
bn.fit.barchart(bn$chest_pain_type)
bn.fit.barchart(bn$resting_bp_s)
bn.fit.barchart(bn$cholesterol)
bn.fit.barchart(bn$fasting_blood_sugar)
bn.fit.barchart(bn$resting_ecg)
bn.fit.barchart(bn$max_heart_rate)
bn.fit.barchart(bn$exercise_angina)
bn.fit.barchart(bn$oldpeak)
bn.fit.barchart(bn$ST_slope)
bn.fit.barchart(bn$target)

#### Prawdopodobieństwa warunkowe ####
bn_grain <- as.grain(bn)

# Przykład: prawdopodobieństwo 'target' pod warunkiem 'age' == '40'
TEST_target <- setEvidence(bn_grain, nodes = c('age'), states = c('40'))
TEST_target_wynik <- querygrain(TEST_target, nodes = c('target'), result = 'array')$target
print(TEST_target_wynik)

# Przykład: prawdopodobieństwo 'target' pod warunkiem 'chest_pain_type' == '2'
TEST2_target <- setEvidence(bn_grain, nodes = c('chest_pain_type'), states = c('2'))
TEST2_target_wynik <- querygrain(TEST2_target, nodes = c('target'), result = 'array')$target
print(TEST2_target_wynik)

# Przykład: prawdopodobieństwo, że 'sex' pod warunkiem 'exercise_angina' == '0'
TEST3_sex <- setEvidence(bn_grain, nodes = c('exercise_angina'), states = c('0'))
TEST3_sex_wynik <- querygrain(TEST3_sex, nodes = c('sex'), result = 'array')$sex
print(TEST3_sex_wynik)
