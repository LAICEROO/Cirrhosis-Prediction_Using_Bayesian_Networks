# Cirrhosis Prediction Using Bayesian Networks

This project focuses on analyzing and predicting cirrhosis stages using Bayesian Networks. It utilizes the Cirrhosis Prediction Dataset to build and evaluate various network structures for inference under uncertainty.

## Dataset
The dataset used in this project is the Cirrhosis Prediction Dataset, which contains information about patients with liver cirrhosis. It includes various biomarkers and clinical symptoms used to analyze and predict the stage of the disease.

### Key variables include:
- Stage
- Bilirubin
- Albumin
- Prothrombin
- Ascites
- Hepatomegaly
- Spiders
- Edema

## Libraries Used
- Rgraphviz: For graph visualization
- gRain: For probabilistic modeling
- bnlearn: For learning Bayesian network structures and probabilistic inference
- lattice: For advanced trellis plots

## Key Features
- Implements various Bayesian network structure learning algorithms (HC, IAMB, Fast-IAMB, PC-stable, GS)
- Compares network structures using BIC scores
- Visualizes learned network structures
- Calculates and visualizes probability distributions
- Provides examples of probabilistic queries on the learned network

## Results
The project compares different network structures and selects the best one based on the BIC score. It then uses this network to calculate probability distributions and perform inference tasks.
