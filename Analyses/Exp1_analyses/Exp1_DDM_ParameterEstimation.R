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

# for observations: use 'metacognition', not cj

# ?

chi_square_optim <- function(params, observations, returnFit){  
  
  # (1) Generate predictions
  
  names(params) <- c('v', 'a', 'ter', 'z', 'ntrials', 'sigma', 'dt', 'a2', 'postdriftmod')
  predictions <- data.frame(DDM_confidence_bounds(v = params['v'], a = params['a'], ter = params['ter'], z = params['z'], ntrials = dim(observations)[1] * ntrials, s = params['sigma'], dt = params['dt'], a2 = params['a2'], postdriftmod = params['post_drift_mod']))
  names(predictions) <- c('rt', 'resp', 'cor', 'raw_evidence2', 'rtfull', 'confidence', 'rtconf', 'cj')
  #predictions$evidence2 <- predictions$raw_evidence2 #???
  
  # (2) Linear scaling of confidence 
  
  predictions$cj <- (predictions$cj + params['add_mean']) / params['add_sd']
  predictions$cj <- ifelse(predictions$cj > 0, 1, 0)  # Transform to binary variable (0-1)
  
  # (3) Separate predictions according to the response
  
  c_predicted <- predictions[predictions$cor == 1,]
  e_predicted <- predictions[predictions$cor == 0,]
  
  # (4) Sort prediction rt's 
  
  c_predicted_rt <- sort(c_predicted$rt)
  e_predicted_rt <- sort(e_predicted$rt)
  
  # (5.1) If we're only simulating data: return the predictions
  
  if(returnFit==0){ 
    return(predictions[,c('rt','cor','cj')])
    
  # (5.2) If we are fitting the model, compare these predictions to the observations 
  
  }else{ 
  
    # First, separate the data in correct and error trials
    c_observed <- observations[observations$cor == 1,]
    e_observed <- observations[observations$cor == 0,]
    
    # Now, get the quantile RTs on the "observed data" for correct and error distributions separately (for quantiles .1, .3, .5, .7, .9)
    c_quantiles <- quantile(c_observed$rt, probs = c(.1,.3,.5,.7,.9), names = FALSE)
    e_quantiles <- quantile(e_observed$rt, probs = c(.1,.3,.5,.7,.9), names = FALSE)
    
    # to combine correct and incorrect we scale the expected interquantile probability by the proportion of correct and incorect respectively
    prop_obs_c <- dim(c_observed)[1] / dim(observations)[1]
    prop_obs_e <- dim(e_observed)[1] / dim(observations)[1]
    
    c_obs_proportion = prop_obs_c * c(.1, .2, .2, .2, .2, .1)
    e_obs_proportion = prop_obs_e * c(.1, .2, .2, .2, .2, .1)
    obs_props <- c(c_obs_proportion,e_obs_proportion)
    
    # now, get the proportion of responses that fall between the observed quantiles when applied to the predicted data (scale by N?)
    c_pred_proportion <- c(
      sum(c_predicted_rt <= c_quantiles[1]),
      sum(c_predicted_rt <= c_quantiles[2]) - sum(c_predicted_rt <= c_quantiles[1]),
      sum(c_predicted_rt <= c_quantiles[3]) - sum(c_predicted_rt <= c_quantiles[2]),
      sum(c_predicted_rt <= c_quantiles[4]) - sum(c_predicted_rt <= c_quantiles[3]),
      sum(c_predicted_rt <= c_quantiles[5]) - sum(c_predicted_rt <= c_quantiles[4]),
      sum(c_predicted_rt > c_quantiles[5])
    ) / dim(predictions)[1]
    
    e_pred_proportion <- c(
      sum(e_predicted_rt <= e_quantiles[1]),
      sum(e_predicted_rt <= e_quantiles[2]) - sum(e_predicted_rt <= e_quantiles[1]),
      sum(e_predicted_rt <= e_quantiles[3]) - sum(e_predicted_rt <= e_quantiles[2]),
      sum(e_predicted_rt <= e_quantiles[4]) - sum(e_predicted_rt <= e_quantiles[3]),
      sum(e_predicted_rt <= e_quantiles[5]) - sum(e_predicted_rt <= e_quantiles[4]),
      sum(e_predicted_rt > e_quantiles[5])
    ) / dim(predictions)[1]
    pred_props <- c(c_pred_proportion,e_pred_proportion)
    
    # avoid zeros in the the data (because of division by predictions for chi square statistic) -> set to small number
    pred_props[pred_props==0] <- .0000001
    
    # Now, do the same for confidence
    obs_props_cj <- c(sum(c_observed$cj==1),
                      sum(c_observed$cj==2),
                      sum(c_observed$cj==3),
                      sum(c_observed$cj==4),
                      sum(c_observed$cj==5),
                      sum(c_observed$cj==6),
                      sum(e_observed$cj==1),
                      sum(e_observed$cj==2),
                      sum(e_observed$cj==3),
                      sum(e_observed$cj==4),
                      sum(e_observed$cj==5),
                      sum(e_observed$cj==6)
    )/length(observations$cj)
    
    # to make the next step easier, lets sort the predictions for correct and errors
    c_predicted_cj <- sort(c_predicted$cj)
    e_predicted_cj <- sort(e_predicted$cj)
    
    # now, get the proportion of responses that fall between the observed quantiles when applied to the predicted data (scale by N?)
    pred_props_cj <- c(
      sum(c_predicted_cj == 1),
      sum(c_predicted_cj == 2),
      sum(c_predicted_cj == 3),
      sum(c_predicted_cj == 4),
      sum(c_predicted_cj == 5),
      sum(c_predicted_cj == 6),
      sum(e_predicted_cj == 1),
      sum(e_predicted_cj == 2),
      sum(e_predicted_cj == 3),
      sum(e_predicted_cj == 4),
      sum(e_predicted_cj == 5),
      sum(e_predicted_cj == 6)
    ) / dim(predictions)[1]    
    
    # avoid zeros in the the data (because of division by predictions for chi square statistic) -> set to small number
    pred_props_cj[pred_props_cj==0] <- .0000001
    
    # Combine the quantiles for rts and cj
    obs_props <- c(obs_props,obs_props_cj)
    pred_props <- c(pred_props,pred_props_cj)
    # calculate chi square
    chiSquare = sum( ( (obs_props - pred_props) ^ 2) / pred_props )
    
    #Return chiSquare
    return(chiSquare)
    
  }
}







