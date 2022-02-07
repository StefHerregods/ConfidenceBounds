# Internship project 2021-2022
# Script estimates DDM parameters of DDM with confidence bounds
# Input: decision reaction times, decisions, confidence rating reaction times, binary confidence rating
# Based on https://github.com/kdesende/dynamic_influences_on_static_measures/blob/main/3A_experiment1_sato
  # Desender, K., Vermeylen, L., Verguts, T. (2021)


rm(list=ls())
setwd('C:\\Users\\herre\\OneDrive\\Documenten\\GitHub\\ConfidenceBounds\\Analyses\\Exp1_analyses')

# Load packages

library(Rcpp)  # To source, compile and run C++ functions
library(DEoptim)  # Optimization algorithm
library(RcppZiggurat)  # Random number generator (normal distribution) 

# Give R access to the DDM simulation function in C++

sourceCpp("DDM_confidence_bounds.cpp") 

# Variable settings

z = 0.5  # Starting point (accuracy-coded dataset -> 0.5)
ntrials = 1000  # Number of decision-making simulations per observation
sigma = 1  # Within-trial noise
dt = 0.001  # Precision
# v, a, ter, a2, postdriftmod

# ?

chi_square_optim <- function(params, observations, returnFit){  
  
  # (1) Generate predictions:
  
  names(params) <- c('v', 'a', 'ter', 'z', 'ntrials', 'sigma', 'dt', 'a2', 'postdriftmod')
  predictions <- data.frame(DDM_confidence_bounds(v = params['v'], a = params['a'], ter = params['ter'], z = params['z'], ntrials = dim(observations)[1] * ntrials, s = params['sigma'], dt = params['dt'], a2 = params['a2'], postdriftmod = params['post_drift_mod']))
  names(predictions) <- c('rt', 'resp', 'cor', 'raw_evidence2', 'rt2', 'cj')
  predictions$evidence2 <- predictions$raw_evidence2
  
}







