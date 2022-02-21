# Internship project 2021-2022
# Script estimates DDM parameters of DDM with confidence bounds
# Input: decision reaction times, decisions, confidence rating reaction times, binary confidence rating (high vs. low confidence)
# Based on https://github.com/kdesende/dynamic_influences_on_static_measures/blob/main/3A_experiment1_sato
  # Desender, K., Vermeylen, L., Verguts, T. (2021)


# TO DO
# - simulaties based on estimated parameters
# - comparison of different cost functions
# - change precision/iterations
# - also separate postdriftmod???

# Issues:
# - manipulation order?
# - v2, v3

# Test file:
# - change directory!!!


rm(list=ls())

# Setting working directory

setwd('C:\\Users\\herre\\Desktop\\Internship\\Results\\Exp1_Results')

# Load packages

library(Rcpp)  # To source, compile and run C++ functions
library(DEoptim)  # Optimization algorithm
library(RcppZiggurat)  # Random number generator (normal distribution) 
library(dplyr)

# Give R access to the DDM simulation function in C++

sourceCpp("C:\\Users\\herre\\OneDrive\\Documenten\\GitHub\\ConfidenceBounds\\Analyses\\Exp1_analyses\\DDM_confidence_bounds.cpp") 

# Variable settings

overwrite <- F  # Overwrite already existing files?

z <- 0.5  # Starting point (accuracy-coded dataset -> 0.5)
ntrials <- 1000  # Number of decision-making simulations per observation
sigma <- 1  # Within-trial noise
dt <- 0.1  # Precision

itermax <- 500  # Number of DeOptim iterations

# Variable vectors

v_vector <- c('v1', 'v2', 'v3')
coherence_vector <- c(0.1, 0.2, 0.4)


### Calculate Chi-square (expected versus observed values)


chi_square_optim <- function(params, all_observations, returnFit){  
  
  # Reset chi-square
  
  chiSquare <- 0
  
  # Name parameters
  
  names(params) <- c('v1', 'v2', 'v3', 'a', 'ter', 'a2', 'postdriftmod', 'a2_slope')
  
  # Calculate separate chi-square for each level of coherence
  
  for (i in 1:3){
    observations <- all_observations %>% filter(coherence == coherence_vector[i])
    
    # Generate predictions 
    
    predictions <<- data.frame(DDM_confidence_bounds(v = params[v_vector[i]], a = params['a'], ter = params['ter'], z = z, ntrials = ntrials, s = sigma, dt = dt, a2 = params['a2'], postdriftmod = params['postdriftmod'], a2_slope = params['a2_slope']))
    names(predictions) <- c('rt', 'resp', 'cor', 'conf_evidence', 'rtfull', 'rtconf', 'cj')
    
    # Separate predictions according to the response
    
    c_predicted <- predictions[predictions$cor == 1,]
    e_predicted <- predictions[predictions$cor == 0,]
    
    # Separate predictions according to the cj
    
    high_conf_predicted <- predictions[predictions$cj == 1,]
    low_conf_predicted <- predictions[predictions$cj == 0,]
    
    # RT data frame
    
    c_predicted_rt <- c_predicted$rt
    e_predicted_rt <- e_predicted$rt
    
    # RTconf data frame
    
    high_conf_predicted_rtconf <- high_conf_predicted$rtconf
    low_conf_predicted_rtconf <- low_conf_predicted$rtconf
    
    # If we are only simulating data: return the predictions
    
    if(returnFit==0){ 
      return(predictions[,c('rt', 'cor', 'cj', 'rtconf')])
      
    # If we are fitting the model: compare these predictions to the observations 
      
    }else{ 
      
      
      ### 1 - Decision RT comparison ###
      
      
      # Separate observations into correct and error trials
      
      c_observed <- observations[observations$cor == 1,]
      e_observed <- observations[observations$cor == 0,]
      
      # Get the quantile RTs on the "observed data" for correct and error distributions separately (for quantiles .1, .3, .5, .7, .9)
      
      c_quantiles <- quantile(c_observed$rt, probs = c(.1,.3,.5,.7,.9), names = FALSE)
      e_quantiles <- quantile(e_observed$rt, probs = c(.1,.3,.5,.7,.9), names = FALSE)
      
      # To combine correct and incorrect trials, we scale the expected interquantile probability by the proportion of correct and incorrect respectively
      
      prop_obs_c <- dim(c_observed)[1] / dim(observations)[1]
      prop_obs_e <- dim(e_observed)[1] / dim(observations)[1]
      
      c_obs_proportion = prop_obs_c * c(.1, .2, .2, .2, .2, .1)
      e_obs_proportion = prop_obs_e * c(.1, .2, .2, .2, .2, .1)
      obs_props <- c(c_obs_proportion,e_obs_proportion)
      
      # Calculate proportion of responses that fall between the observed quantiles when applied to the predicted data 
      
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
      
      pred_props_rt <- c(c_pred_proportion, e_pred_proportion)
      
      # Avoid zeros in the the data (because of division by predictions for chi square statistic) -> set to small number
      
      pred_props_rt[pred_props_rt == 0] <- .0000001
      
      
      ### 2 - Confidence rating RT comparison ###
      
      
      # Separate observations into high and low confidence
      
      high_conf_observed <- observations[observations$cj == 1,]
      low_conf_observed <- observations[observations$cj == 0,]
      
      # Get the quantile confidence RTs on the "observed data" for correct and error distributions separately (for quantiles .1, .3, .5, .7, .9)
      
      high_conf_quantiles <- quantile(high_conf_observed$rtconf, probs = c(.1,.3,.5,.7,.9), names = FALSE)
      low_conf_quantiles <- quantile(low_conf_observed$rtconf, probs = c(.1,.3,.5,.7,.9), names = FALSE)
      
      # To combine correct and incorrect trials, we scale the expected interquantile probability by the proportion of correct and incorrect respectively
      
      prop_obs_high_conf <- dim(high_conf_observed)[1] / dim(observations)[1]
      prop_obs_low_conf <- dim(low_conf_observed)[1] / dim(observations)[1]
      
      high_conf_obs_proportion = prop_obs_c * c(.1, .2, .2, .2, .2, .1)
      low_conf_obs_proportion = prop_obs_e * c(.1, .2, .2, .2, .2, .1)
      conf_obs_props <- c(c_obs_proportion,e_obs_proportion)
      
      # Calculate proportion of responses that fall between the observed quantiles when applied to the predicted data 
      
      high_conf_pred_proportion <- c(
        sum(high_conf_predicted_rtconf <= high_conf_quantiles[1]),
        sum(high_conf_predicted_rtconf <= high_conf_quantiles[2]) - sum(high_conf_predicted_rtconf <= high_conf_quantiles[1]),
        sum(high_conf_predicted_rtconf <= high_conf_quantiles[3]) - sum(high_conf_predicted_rtconf <= high_conf_quantiles[2]),
        sum(high_conf_predicted_rtconf <= high_conf_quantiles[4]) - sum(high_conf_predicted_rtconf <= high_conf_quantiles[3]),
        sum(high_conf_predicted_rtconf <= high_conf_quantiles[5]) - sum(high_conf_predicted_rtconf <= high_conf_quantiles[4]),
        sum(high_conf_predicted_rtconf > high_conf_quantiles[5])
      ) / dim(predictions)[1]
      
      low_conf_pred_proportion <- c(
        sum(low_conf_predicted_rtconf <= low_conf_quantiles[1]),
        sum(low_conf_predicted_rtconf <= low_conf_quantiles[2]) - sum(low_conf_predicted_rtconf <= low_conf_quantiles[1]),
        sum(low_conf_predicted_rtconf <= low_conf_quantiles[3]) - sum(low_conf_predicted_rtconf <= low_conf_quantiles[2]),
        sum(low_conf_predicted_rtconf <= low_conf_quantiles[4]) - sum(low_conf_predicted_rtconf <= low_conf_quantiles[3]),
        sum(low_conf_predicted_rtconf <= low_conf_quantiles[5]) - sum(low_conf_predicted_rtconf <= low_conf_quantiles[4]),
        sum(low_conf_predicted_rtconf > low_conf_quantiles[5])
      ) / dim(predictions)[1]
      
      pred_props_rtconf <- c(high_conf_pred_proportion, low_conf_pred_proportion)
      
      # Avoid zeros in the the data (because of division by predictions for chi square statistic) -> set to small number
      
      pred_props_rtconf[pred_props_rtconf == 0] <- .0000001
      
      
      ### 3 - Calculating chi-square
      
      # Combine the quantiles for RTs and confidence RT's
      
      obs_props <- c(obs_props, conf_obs_props) 
      pred_props <- c(pred_props_rt, pred_props_rtconf) 
      
      # Calculate chi-square
      
      chiSquare_temp = sum( ( (obs_props - pred_props)^ 2) / pred_props)
      
      # Add chi-squares 
      
      chiSquare <- chiSquare + chiSquare_temp
    
  }
  
  # Return chiSquare
    
  return(chiSquare)
    
  }
}

# Load data

df <- read.csv(file = "Exp1_data_viable.csv")
subs <- unique(df$sub)
N<-length(subs)

# Optimize (extended) DDM parameters 

for(i in 1:N){  # For each participant separately
   
  print(paste('Running participant', subs[i], 'from', N))
  condLab <- unique(df$manipulation)  
  tempAll <- subset(df, sub == subs[i])
  
  for(c in 1:4){  # For each condition separately 
    
    tempDat <- subset(tempAll, manipulation == condLab[c])
    tempDat <- tempDat[,c('rt', 'cor', 'resp', 'cj', 'manipulation', 'rtconf', 'coherence')]
    
    # Load existing individual results if these already exist
    
    file_name <- paste0('Parameter_estimation\\results_sub_', i, '_', condLab[c], '.Rdata')
    if (overwrite == F & file.exists(file_name)){

      load(file_name)
      
    # Else, estimate parameters
      
    } else {
      
      # Optimization function
      
      optimal_params <- DEoptim(chi_square_optim,  # Function to optimize
                                # Possible values for v (for each level of coherence: 0.1, 0.2 and 0.4), a, ter, a2, postdriftmod 
                                lower = c(0, 0, 0, .5,   0, 0,   0,  0),  
                                upper = c(3, 3, 3,  4, 1.5, 5.5, 15, 0),
                                all_observations = tempDat, returnFit = 1, control = c(itermax = itermax))
      
      results <- summary(optimal_params)
      
      # Save individual results
      
      save(results, file = file_name)
      
    }
  }
}

