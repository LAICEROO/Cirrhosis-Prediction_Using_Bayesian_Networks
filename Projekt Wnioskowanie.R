#### Instalacja odpowiednich pakietów ####
install.packages('bnlearn')
install.packages('Rgraphviz')
install.packages('gRain')
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
data <- read.csv('cirrhosis.csv')
data <- head(data, 300)

# sprawdzenie czy nie ma pustych wartosci
for (i in 1:length(data)){
  cat('kolumna', i, ': ', any(is.null(data[,i])),'\n')
}

# Wybor kolumn ktore nas interesuja
data = data[, c("Stage", "Bilirubin", "Albumin", "Prothrombin", "Ascites", "Hepatomegaly", "Spiders", "Edema")]


# Funkcja grupujaca
generate_groups <- function(data, column_name) {
  min_val <- min(data[[column_name]], na.rm = TRUE)
  max_val <- max(data[[column_name]], na.rm = TRUE)
  diff_val <- max_val - min_val
  
  groups <- cut(data[[column_name]], 
                breaks = c(min_val - 1, 
                           min_val + 0.333 * diff_val, 
                           min_val + 0.667 * diff_val, 
                           max_val + 1), 
                labels = c(sprintf("(%.2f, %.2f]", min_val - 1, 
                                   min_val + 0.333 * diff_val), 
                           sprintf("(%.2f, %.2f]", min_val + 0.333 * diff_val, 
                                   min_val + 0.667 * diff_val), 
                           sprintf("(%.2f, %.2f]", min_val + 0.667 * diff_val, 
                                   max_val + 1))) 
  
  return(groups)
}

# Grupowanie zmiennych
data$Bilirubin <- generate_groups(data, 'Bilirubin')
data$Albumin <- generate_groups(data, 'Albumin')
data$Prothrombin <- generate_groups(data, 'Prothrombin')

# Konwersja wszystkich kolumn na współczynniki
data[] <- lapply(data, factor)

# Funkcja testu niezależności
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
      independence <- ifelse(test_result$p.value < 0.05, "zalezne", "niezalezne")
      results <- rbind(results, data.frame(
        Comparison = comparison_name,
        p_value = test_result$p.value,
        Independence = independence
      ))
    }
  }
  
  return(results)
}
results_df <- pairwise_ci_test(data)

# Tworzenie sieci 
# Hill-Climbing (HC) 
siec_hc <- hc(data)      
graphviz.plot(siec_hc)

# Incremental Association Markov Blanket (IAMB) 
siec_iamb <- iamb(data)
siec_iamb <- set.arc(siec_iamb, "Albumin", "Ascites")
siec_iamb <- set.arc(siec_iamb, "Ascites", "Stage")
siec_iamb <- set.arc(siec_iamb, "Stage", "Bilirubin")
siec_iamb <- set.arc(siec_iamb, "Ascites", "Edema")
siec_iamb <- set.arc(siec_iamb, "Stage", "Hepatomegaly")
siec_iamb <- set.arc(siec_iamb, "Hepatomegaly", "Spiders")
siec_iamb <- set.arc(siec_iamb, "Edema", "Prothrombin")
graphviz.plot(siec_iamb)


# Fast Incremental Association Markov Blanket IAMB 
siec_fast_iamb <- fast.iamb(data)
siec_fast_iamb <- set.arc(siec_fast_iamb, "Bilirubin", "Albumin")
siec_fast_iamb <- set.arc(siec_fast_iamb, "Stage", "Ascites")
siec_fast_iamb <- set.arc(siec_fast_iamb, "Stage", "Bilirubin")
siec_fast_iamb <- set.arc(siec_fast_iamb, "Ascites", "Edema")
siec_fast_iamb <- set.arc(siec_fast_iamb, "Stage", "Hepatomegaly")
siec_fast_iamb <- set.arc(siec_fast_iamb, "Hepatomegaly", "Spiders")
siec_fast_iamb <- set.arc(siec_fast_iamb, "Edema", "Prothrombin")
graphviz.plot(siec_fast_iamb)


# PC-Stable network
siec_pc_stable <- pc.stable(data)
siec_pc_stable <- set.arc(siec_pc_stable, "Albumin", "Ascites")
siec_pc_stable <- set.arc(siec_pc_stable, "Stage", "Ascites")
siec_pc_stable <- set.arc(siec_pc_stable, "Stage", "Bilirubin")
siec_pc_stable <- set.arc(siec_pc_stable, "Ascites", "Edema")
siec_pc_stable <- set.arc(siec_pc_stable, "Stage", "Hepatomegaly")
siec_pc_stable <- set.arc(siec_pc_stable, "Hepatomegaly", "Spiders")
siec_pc_stable <- set.arc(siec_pc_stable, "Edema", "Prothrombin")
graphviz.plot(siec_pc_stable)

# GS network
siec_gs <- tabu(data)
graphviz.plot(siec_gs)



# Ocena sieci
score(siec_hc, data = data, type = "bic")
score(siec_iamb, data = data, type = "bic")
score(siec_fast_iamb, data = data, type = "bic")
score(siec_pc_stable, data = data, type = "bic")
score(siec_gs, data = data, type = "bic")

# wybranie najlpszej sieci
best_network <- siec_hc 

# Wyznaczenie rozkładów prawdopodobieństwa
(bn <- bn.fit(siec_hc, data = data))

bn.fit.barchart(bn$Stage)
bn.fit.barchart(bn$Bilirubin)
bn.fit.barchart(bn$Albumin)
bn.fit.barchart(bn$Prothrombin)
bn.fit.barchart(bn$Ascites)
bn.fit.barchart(bn$Hepatomegaly)
bn.fit.barchart(bn$Spiders)
bn.fit.barchart(bn$Edema)

bn_grain <- as.grain(bn)

# Prawdopodobienstwa

### PR1 
TEST_adplvl <- setEvidence(bn_grain, nodes = c('Age'), states = c('16-20'))
TEST_adplvl_wynik <- querygrain(TEST_adplvl, nodes = c('Ascites'), result = 'array')$Ascites
print(TEST_adplvl_wynik)

### PR2 
TEST2_adplvl <- setEvidence(bn_grain, nodes = c('Prothrombin'), states = c('<12'))
TEST2_adplvl_wynik <- querygrain(TEST2_adplvl, nodes = c('Spiders'), result = 'array')$Spiders
print(TEST2_adplvl_wynik)

### PR3
TEST3_adplvl <- setEvidence(bn_grain, nodes = c('Stage'), states = c('4'))
TEST3_adplvl_wynik <- querygrain(TEST3_adplvl, nodes = c('Hepatomegaly'), result = 'array')$Hepatomegaly
print(TEST3_adplvl_wynik)