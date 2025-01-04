# Adaptive constrained weighted estimation for incorporating multiple external datasets

## Description
This repository contains the R code to perform all analyses described in the paper: "Adaptive constrained weighted estimation for incorporating multiple external datasets."  
The version corresponding to the published results is tagged as v1.0.

- `function`: A file that contains all required functions.
- `numerical_example`: An execution file for the numerical examples.
- `simulation`: An execution file for the Monte Carlo simulations.
- `application`: An execution file for the application.

## Usage
Paste the file path of `functions` into the `source()` function at the beginning of each execution file, and each file will then be ready for execution.

## Note
- To avoid conflicts, it is recommended to execute the code in an environment where no packages are installed except for those specified below.
- Since Monte Carlo simulations can be time-consuming, save the results for each method as needed.

## Requirements
- Software
  - R version 4.4.2

- R packages
  
Name | Version
--- | --- 
dplyr | 1.1.4
tidyr | 1.3.1
purrr | 1.0.2
ggplot2 | 3.5.1
MASS | 7.3-63
parallel | 4.4.2
nloptr | 2.1.1
conflicted | 1.2.0
ggpubr | 0.6.0
RBesT | 1.7-4
