# Internship project 2021-2022
# Script estimates DDM parameters of DDM with confidence bounds
# Input: decision reaction times, decisions, confidence rating reaction times, binary confidence rating (high vs. low confidence)
# Based on https://github.com/kdesende/dynamic_influences_on_static_measures/blob/main/3A_experiment1_sato
  # Desender, K., Vermeylen, L., Verguts, T. (2021)


# Load packages

library(Rcpp)  # To source, compile and run C++ functions
library(DEoptim)  # Optimization algorithm
library(dplyr)
library(optparse)  # Necessary for parallel computing
library(MASS)  # log-normal distribution

# Give R access to the DDM simulation function in C++

sourceCpp("DDM_confidence_bounds_vratio2.cpp") 

# Variable settings

overwrite <- T  # Overwrite already existing files?

z <- 0.5  # Starting point (accuracy-coded dataset -> 0.5)
ntrials <- 1000  # Number of decision-making simulations per observation
sigma <- 1  # Within-trial noise
dt <- 0.01  # Precision

itermax <- 500  # Number of DeOptim iterations

# Variable vectors

v_vector <- c('v1', 'v2', 'v3')
coherence_vector <- c(0.1, 0.2, 0.4)


### Calculate cj based on log-normal distributions

cj_calculation_c_h <- function(var){
  cj_est <- sample(x = c(4, 5, 6), size = 1,
                   prob = c(dlnorm(var, meanlog = fit4_c[[1]][1], sdlog = fit4_c[[1]][2]) * prop_table[4,2] + 0.00000001,
                            dlnorm(var, meanlog = fit5_c[[1]][1], sdlog = fit5_c[[1]][2]) * prop_table[5,2] + 0.00000001,
                            dlnorm(var, meanlog = fit6_c[[1]][1], sdlog = fit6_c[[1]][2]) * prop_table[6,2] + 0.00000001))
  return(cj_est)
}
cj_calculation_e_h <- function(var){
  cj_est <- sample(x = c(4, 5, 6), size = 1,
                   prob = c(dlnorm(var, meanlog = fit4_e[[1]][1], sdlog = fit4_e[[1]][2]) * prop_table[4,1] + 0.00000001,
                            dlnorm(var, meanlog = fit5_e[[1]][1], sdlog = fit5_e[[1]][2]) * prop_table[5,1] + 0.00000001,
                            dlnorm(var, meanlog = fit6_e[[1]][1], sdlog = fit6_e[[1]][2]) * prop_table[6,1] + 0.00000001))
  return(cj_est)
}
cj_calculation_c_l <- function(var){
  cj_est <- sample(x = c(1, 2, 3), size = 1,
                   prob = c(dlnorm(var, meanlog = fit1_c[[1]][1], sdlog = fit1_c[[1]][2]) * prop_table[1,2] + 0.00000001,
                            dlnorm(var, meanlog = fit2_c[[1]][1], sdlog = fit2_c[[1]][2]) * prop_table[2,2] + 0.00000001,
                            dlnorm(var, meanlog = fit3_c[[1]][1], sdlog = fit3_c[[1]][2]) * prop_table[3,2] + 0.00000001))
  return(cj_est)
}
cj_calculation_e_l <- function(var){
  cj_est <- sample(x = c(1, 2, 3), size = 1,
                   prob = c(dlnorm(var, meanlog = fit1_e[[1]][1], sdlog = fit1_e[[1]][2]) * prop_table[1,1] + 0.00000001,
                            dlnorm(var, meanlog = fit2_e[[1]][1], sdlog = fit2_e[[1]][2]) * prop_table[2,1] + 0.00000001,
                            dlnorm(var, meanlog = fit3_e[[1]][1], sdlog = fit3_e[[1]][2]) * prop_table[3,1] + 0.00000001))
  return(cj_est)
}


### Calculate Chi-square (expected versus observed values)


chi_square_optim <- function(params, all_observations, returnFit){  
  
  # Reset chi-square
  
  chiSquare <- 0
  
  # Name parameters
  
  names(params) <- c('v1', 'v2', 'v3', 'a', 'ter', 'a2', 'postdriftmod_1', 'postdriftmod_2', 'a2_slope', 'ter2')
  
  # Calculate separate chi-square for each level of coherence
  
  for (i in 1:3){
    
    observations <- all_observations %>% filter(coherence == coherence_vector[i])
    
    # Generate predictions 
    
    predictions <- data.frame(DDM_confidence_bounds(v = params[v_vector[i]], a = params['a'], ter = params['ter'], z = z, ntrials = ntrials, s = sigma, dt = dt, a2 = params['a2'], postdriftmod_1 = params['postdriftmod_1'], postdriftmod_2 = params['postdriftmod_2'], a2_slope = params['a2_slope'], ter2 = params['ter2']))
    names(predictions) <- c('rt', 'resp', 'cor', 'evidence_2', 'rtfull', 'rtconf', 'cj')
    
    # Transform predicted conf_evidence into cj
    
    fit1_c <<- ifelse(length(observations$rtconf[observations$cj == 1 & observations$cor == 1]) > 1,
                      as.data.frame(fitdistr(observations$rtconf[observations$cj == 1 & observations$cor == 1], densfun = "log-normal")[[1]]),
                      data.frame(c(0,1)))
    fit2_c <<- ifelse(length(observations$rtconf[observations$cj == 2 & observations$cor == 1]) > 1,
                      as.data.frame(fitdistr(observations$rtconf[observations$cj == 2 & observations$cor == 1], densfun = "log-normal")[[1]]),
                      data.frame(c(0,1)))
    fit3_c <<- ifelse(length(observations$rtconf[observations$cj == 3 & observations$cor == 1]) > 1,
                      as.data.frame(fitdistr(observations$rtconf[observations$cj == 3 & observations$cor == 1], densfun = "log-normal")[[1]]),
                      data.frame(c(0,1)))
    fit4_c <<- ifelse(length(observations$rtconf[observations$cj == 4 & observations$cor == 1]) > 1,
                      as.data.frame(fitdistr(observations$rtconf[observations$cj == 4 & observations$cor == 1], densfun = "log-normal")[[1]]),
                      data.frame(c(0,1)))
    fit5_c <<- ifelse(length(observations$rtconf[observations$cj == 5 & observations$cor == 1]) > 1,
                      as.data.frame(fitdistr(observations$rtconf[observations$cj == 5 & observations$cor == 1], densfun = "log-normal")[[1]]),
                      data.frame(c(0,1)))
    fit6_c <<- ifelse(length(observations$rtconf[observations$cj == 6 & observations$cor == 1]) > 1,
                      as.data.frame(fitdistr(observations$rtconf[observations$cj == 6 & observations$cor == 1], densfun = "log-normal")[[1]]),
                      data.frame(c(0,1)))
    
    fit1_e <<- ifelse(length(observations$rtconf[observations$cj == 1 & observations$cor == 0]) > 1,
                      as.data.frame(fitdistr(observations$rtconf[observations$cj == 1 & observations$cor == 0], densfun = "log-normal")[[1]]),
                      data.frame(c(0,1)))    
    fit2_e <<- ifelse(length(observations$rtconf[observations$cj == 2 & observations$cor == 0]) > 1,
                      as.data.frame(fitdistr(observations$rtconf[observations$cj == 2 & observations$cor == 0], densfun = "log-normal")[[1]]),
                      data.frame(c(0,1)))  
    fit3_e <<- ifelse(length(observations$rtconf[observations$cj == 3 & observations$cor == 0]) > 1,
                      as.data.frame(fitdistr(observations$rtconf[observations$cj == 3 & observations$cor == 0], densfun = "log-normal")[[1]]),
                      data.frame(c(0,1)))  
    fit4_e <<- ifelse(length(observations$rtconf[observations$cj == 4 & observations$cor == 0]) > 1,
                      as.data.frame(fitdistr(observations$rtconf[observations$cj == 4 & observations$cor == 0], densfun = "log-normal")[[1]]),
                      data.frame(c(0,1)))  
    fit5_e <<- ifelse(length(observations$rtconf[observations$cj == 5 & observations$cor == 0]) > 1,
                      as.data.frame(fitdistr(observations$rtconf[observations$cj == 5 & observations$cor == 0], densfun = "log-normal")[[1]]),
                      data.frame(c(0,1)))  
    fit6_e <<- ifelse(length(observations$rtconf[observations$cj == 6 & observations$cor == 0]) > 1,
                      as.data.frame(fitdistr(observations$rtconf[observations$cj == 6 & observations$cor == 0], densfun = "log-normal")[[1]]),
                      data.frame(c(0,1)))  
    
    prop_table <<- prop.table(table(factor(observations$cj, levels = 1:6), factor(observations$cor, levels = 0:1)))
    
    predictions_c_h <- predictions[predictions$cor == 1 & predictions$cj == 1,]
    predictions_e_h <- predictions[predictions$cor == 0 & predictions$cj == 1,]
    predictions_c_l <- predictions[predictions$cor == 1 & predictions$cj == 0,]
    predictions_e_l <- predictions[predictions$cor == 0 & predictions$cj == 0,]
    
    predictions_c_h$cj_6 <- lapply(predictions_c_h$rtconf, cj_calculation_c_h)
    predictions_e_h$cj_6 <- lapply(predictions_e_h$rtconf, cj_calculation_e_h)
    predictions_c_l$cj_6 <- lapply(predictions_c_l$rtconf, cj_calculation_c_l)
    predictions_e_l$cj_6 <- lapply(predictions_e_l$rtconf, cj_calculation_e_l)
    
    predictions <- rbind(predictions_c_h, predictions_c_l, predictions_e_h, predictions_e_l)
    
    # Separate predictions according to the response
    
    c_predicted <- predictions[predictions$cor == 1,]
    e_predicted <- predictions[predictions$cor == 0,]
    
    # Separate predictions according to the cj
    
    conf_predicted_1 <- predictions[predictions$cj_6 == 1,]
    conf_predicted_2 <- predictions[predictions$cj_6 == 2,]
    conf_predicted_3 <- predictions[predictions$cj_6 == 3,]
    conf_predicted_4 <- predictions[predictions$cj_6 == 4,]
    conf_predicted_5 <- predictions[predictions$cj_6 == 5,]
    conf_predicted_6 <- predictions[predictions$cj_6 == 6,]
    
    # RT data frame
    
    c_predicted_rt <- c_predicted$rt
    e_predicted_rt <- e_predicted$rt
    
    # RTconf data frame (1)
    
    conf_predicted_1_rtconf <- conf_predicted_1$rtconf
    conf_predicted_2_rtconf <- conf_predicted_2$rtconf
    conf_predicted_3_rtconf <- conf_predicted_3$rtconf
    conf_predicted_4_rtconf <- conf_predicted_4$rtconf
    conf_predicted_5_rtconf <- conf_predicted_5$rtconf
    conf_predicted_6_rtconf <- conf_predicted_6$rtconf
    
    # RTconf data frame (2)
    
    c_conf_predicted_rtconf <- c_predicted$rtconf
    e_conf_predicted_rtconf <- e_predicted$rtconf
    
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
      
      if (any(is.na(c_quantiles))) {
        c_quantiles <- rep(0,5)
      }
      if (any(is.na(e_quantiles))) {
        e_quantiles <- rep(0,5)
      }
      
      # To combine correct and incorrect trials, we scale the expected interquantile probability by the proportion of correct and incorrect respectively
      
      prop_obs_c <- dim(c_observed)[1] / dim(observations)[1]
      prop_obs_e <- dim(e_observed)[1] / dim(observations)[1]
      
      c_obs_proportion = prop_obs_c * c(.1, .2, .2, .2, .2, .1)
      e_obs_proportion = prop_obs_e * c(.1, .2, .2, .2, .2, .1)
      obs_props <- c(c_obs_proportion, e_obs_proportion)
      
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
      
      
      ### 2 - Confidence rating RT comparison (1) ###
      
      
      # Separate observations into confidence ratings
      
      conf_observed_1 <- observations[observations$cj == 1,]
      conf_observed_2 <- observations[observations$cj == 2,]
      conf_observed_3 <- observations[observations$cj == 3,]
      conf_observed_4 <- observations[observations$cj == 4,]
      conf_observed_5 <- observations[observations$cj == 5,]
      conf_observed_6 <- observations[observations$cj == 6,]
      
      # Get the quantile confidence RT's on the "observed data" for each distributions separately (for quantiles .1, .3, .5, .7, .9)
      
      conf_quantiles_1 <- quantile(conf_observed_1$rtconf, probs = c(.1,.3,.5,.7,.9), names = FALSE)
      conf_quantiles_2 <- quantile(conf_observed_2$rtconf, probs = c(.1,.3,.5,.7,.9), names = FALSE)
      conf_quantiles_3 <- quantile(conf_observed_3$rtconf, probs = c(.1,.3,.5,.7,.9), names = FALSE)
      conf_quantiles_4 <- quantile(conf_observed_4$rtconf, probs = c(.1,.3,.5,.7,.9), names = FALSE)
      conf_quantiles_5 <- quantile(conf_observed_5$rtconf, probs = c(.1,.3,.5,.7,.9), names = FALSE)
      conf_quantiles_6 <- quantile(conf_observed_6$rtconf, probs = c(.1,.3,.5,.7,.9), names = FALSE)
      
      if (any(is.na(conf_quantiles_1))) {
        conf_quantiles_1 <- rep(0,5)
      }
      if (any(is.na(conf_quantiles_2))) {
        conf_quantiles_2 <- rep(0,5)
      }
      if (any(is.na(conf_quantiles_3))) {
        conf_quantiles_3 <- rep(0,5)
      }
      if (any(is.na(conf_quantiles_4))) {
        conf_quantiles_4 <- rep(0,5)
      }
      if (any(is.na(conf_quantiles_5))) {
        conf_quantiles_5 <- rep(0,5)
      }
      if (any(is.na(conf_quantiles_6))) {
        conf_quantiles_6 <- rep(0,5)
      }
      
      # To combine correct and incorrect trials, we scale the expected interquantile probability by the proportion of correct and incorrect respectively
      
      prop_obs_conf_1 <- dim(conf_observed_1)[1] / dim(observations)[1]
      prop_obs_conf_2 <- dim(conf_observed_2)[1] / dim(observations)[1]
      prop_obs_conf_3 <- dim(conf_observed_3)[1] / dim(observations)[1]
      prop_obs_conf_4 <- dim(conf_observed_4)[1] / dim(observations)[1]
      prop_obs_conf_5 <- dim(conf_observed_5)[1] / dim(observations)[1]
      prop_obs_conf_6 <- dim(conf_observed_6)[1] / dim(observations)[1]
      
      conf_obs_1_proportion = prop_obs_conf_1 * c(.1, .2, .2, .2, .2, .1)
      conf_obs_2_proportion = prop_obs_conf_2 * c(.1, .2, .2, .2, .2, .1)
      conf_obs_3_proportion = prop_obs_conf_3 * c(.1, .2, .2, .2, .2, .1)
      conf_obs_4_proportion = prop_obs_conf_4 * c(.1, .2, .2, .2, .2, .1)
      conf_obs_5_proportion = prop_obs_conf_5 * c(.1, .2, .2, .2, .2, .1)
      conf_obs_6_proportion = prop_obs_conf_6 * c(.1, .2, .2, .2, .2, .1)
      conf_obs_props_1 <- c(conf_obs_1_proportion, conf_obs_2_proportion, conf_obs_3_proportion, conf_obs_4_proportion, conf_obs_5_proportion, conf_obs_6_proportion)
      
      # Calculate proportion of responses that fall between the observed quantiles when applied to the predicted data 
      
      conf_1_pred_proportion <- c(
        sum(conf_predicted_1_rtconf <= conf_quantiles_1[1]),
        sum(conf_predicted_1_rtconf <= conf_quantiles_1[2]) - sum(conf_predicted_1_rtconf <= conf_quantiles_1[1]),
        sum(conf_predicted_1_rtconf <= conf_quantiles_1[3]) - sum(conf_predicted_1_rtconf <= conf_quantiles_1[2]),
        sum(conf_predicted_1_rtconf <= conf_quantiles_1[4]) - sum(conf_predicted_1_rtconf <= conf_quantiles_1[3]),
        sum(conf_predicted_1_rtconf <= conf_quantiles_1[5]) - sum(conf_predicted_1_rtconf <= conf_quantiles_1[4]),
        sum(conf_predicted_1_rtconf > conf_quantiles_1[5])
      ) / dim(predictions)[1]
      
      conf_2_pred_proportion <- c(
        sum(conf_predicted_2_rtconf <= conf_quantiles_2[1]),
        sum(conf_predicted_2_rtconf <= conf_quantiles_2[2]) - sum(conf_predicted_2_rtconf <= conf_quantiles_2[1]),
        sum(conf_predicted_2_rtconf <= conf_quantiles_2[3]) - sum(conf_predicted_2_rtconf <= conf_quantiles_2[2]),
        sum(conf_predicted_2_rtconf <= conf_quantiles_2[4]) - sum(conf_predicted_2_rtconf <= conf_quantiles_2[3]),
        sum(conf_predicted_2_rtconf <= conf_quantiles_2[5]) - sum(conf_predicted_2_rtconf <= conf_quantiles_2[4]),
        sum(conf_predicted_2_rtconf > conf_quantiles_2[5])
      ) / dim(predictions)[1]
      
      conf_3_pred_proportion <- c(
        sum(conf_predicted_3_rtconf <= conf_quantiles_3[1]),
        sum(conf_predicted_3_rtconf <= conf_quantiles_3[2]) - sum(conf_predicted_3_rtconf <= conf_quantiles_3[1]),
        sum(conf_predicted_3_rtconf <= conf_quantiles_3[3]) - sum(conf_predicted_3_rtconf <= conf_quantiles_3[2]),
        sum(conf_predicted_3_rtconf <= conf_quantiles_3[4]) - sum(conf_predicted_3_rtconf <= conf_quantiles_3[3]),
        sum(conf_predicted_3_rtconf <= conf_quantiles_3[5]) - sum(conf_predicted_3_rtconf <= conf_quantiles_3[4]),
        sum(conf_predicted_3_rtconf > conf_quantiles_3[5])
      ) / dim(predictions)[1]
      
      conf_4_pred_proportion <- c(
        sum(conf_predicted_4_rtconf <= conf_quantiles_4[1]),
        sum(conf_predicted_4_rtconf <= conf_quantiles_4[2]) - sum(conf_predicted_4_rtconf <= conf_quantiles_4[1]),
        sum(conf_predicted_4_rtconf <= conf_quantiles_4[3]) - sum(conf_predicted_4_rtconf <= conf_quantiles_4[2]),
        sum(conf_predicted_4_rtconf <= conf_quantiles_4[4]) - sum(conf_predicted_4_rtconf <= conf_quantiles_4[3]),
        sum(conf_predicted_4_rtconf <= conf_quantiles_4[5]) - sum(conf_predicted_4_rtconf <= conf_quantiles_4[4]),
        sum(conf_predicted_4_rtconf > conf_quantiles_4[5])
      ) / dim(predictions)[1]
      
      conf_5_pred_proportion <- c(
        sum(conf_predicted_5_rtconf <= conf_quantiles_5[1]),
        sum(conf_predicted_5_rtconf <= conf_quantiles_5[2]) - sum(conf_predicted_5_rtconf <= conf_quantiles_5[1]),
        sum(conf_predicted_5_rtconf <= conf_quantiles_5[3]) - sum(conf_predicted_5_rtconf <= conf_quantiles_5[2]),
        sum(conf_predicted_5_rtconf <= conf_quantiles_5[4]) - sum(conf_predicted_5_rtconf <= conf_quantiles_5[3]),
        sum(conf_predicted_5_rtconf <= conf_quantiles_5[5]) - sum(conf_predicted_5_rtconf <= conf_quantiles_5[4]),
        sum(conf_predicted_5_rtconf > conf_quantiles_5[5])
      ) / dim(predictions)[1]
      
      conf_6_pred_proportion <- c(
        sum(conf_predicted_6_rtconf <= conf_quantiles_6[1]),
        sum(conf_predicted_6_rtconf <= conf_quantiles_6[2]) - sum(conf_predicted_6_rtconf <= conf_quantiles_6[1]),
        sum(conf_predicted_6_rtconf <= conf_quantiles_6[3]) - sum(conf_predicted_6_rtconf <= conf_quantiles_6[2]),
        sum(conf_predicted_6_rtconf <= conf_quantiles_6[4]) - sum(conf_predicted_6_rtconf <= conf_quantiles_6[3]),
        sum(conf_predicted_6_rtconf <= conf_quantiles_6[5]) - sum(conf_predicted_6_rtconf <= conf_quantiles_6[4]),
        sum(conf_predicted_6_rtconf > conf_quantiles_6[5])
      ) / dim(predictions)[1]
      
      pred_props_rtconf_1 <- c(conf_1_pred_proportion, conf_2_pred_proportion, conf_3_pred_proportion, conf_4_pred_proportion, conf_5_pred_proportion, conf_6_pred_proportion)
      
      # Avoid zeros in the the data (because of division by predictions for chi square statistic) -> set to small number
      
      pred_props_rtconf_1[pred_props_rtconf_1 == 0] <- .0000001
      
      
      ### 3 - Confidence rating RT comparison (2) ###
      
      
      # Separate observations into correct and wrong trials
      
      c_conf_observed <- observations[observations$cor == 1,]
      e_conf_observed <- observations[observations$cor == 0,]
      
      # Get the quantile confidence RTs on the "observed data" for correct and error distributions separately (for quantiles .1, .3, .5, .7, .9)
      
      c_conf_quantiles <- quantile(c_conf_observed$rtconf, probs = c(.1,.3,.5,.7,.9), names = FALSE)
      e_conf_quantiles <- quantile(e_conf_observed$rtconf, probs = c(.1,.3,.5,.7,.9), names = FALSE)
      
      if (any(is.na(c_conf_quantiles))) {
        c_conf_quantiles <- rep(0,5)
      }
      if (any(is.na(e_conf_quantiles))) {
        e_conf_quantiles <- rep(0,5)
      }
      
      # To combine correct and incorrect trials, we scale the expected interquantile probability by the proportion of correct and incorrect respectively
      
      prop_obs_c_conf <- dim(c_conf_observed)[1] / dim(observations)[1]
      prop_obs_e_conf <- dim(e_conf_observed)[1] / dim(observations)[1]
      
      c_conf_obs_proportion = prop_obs_c_conf * c(.1, .2, .2, .2, .2, .1)
      e_conf_obs_proportion = prop_obs_e_conf * c(.1, .2, .2, .2, .2, .1)
      conf_obs_props_2 <- c(c_conf_obs_proportion, e_conf_obs_proportion)
      
      # Calculate proportion of responses that fall between the observed quantiles when applied to the predicted data 
      
      c_conf_pred_proportion <- c(
        sum(c_conf_predicted_rtconf <= c_conf_quantiles[1]),
        sum(c_conf_predicted_rtconf <= c_conf_quantiles[2]) - sum(c_conf_predicted_rtconf <= c_conf_quantiles[1]),
        sum(c_conf_predicted_rtconf <= c_conf_quantiles[3]) - sum(c_conf_predicted_rtconf <= c_conf_quantiles[2]),
        sum(c_conf_predicted_rtconf <= c_conf_quantiles[4]) - sum(c_conf_predicted_rtconf <= c_conf_quantiles[3]),
        sum(c_conf_predicted_rtconf <= c_conf_quantiles[5]) - sum(c_conf_predicted_rtconf <= c_conf_quantiles[4]),
        sum(c_conf_predicted_rtconf > c_conf_quantiles[5])
      ) / dim(predictions)[1]
      
      e_conf_pred_proportion <- c(
        sum(e_conf_predicted_rtconf <= e_conf_quantiles[1]),
        sum(e_conf_predicted_rtconf <= e_conf_quantiles[2]) - sum(e_conf_predicted_rtconf <= e_conf_quantiles[1]),
        sum(e_conf_predicted_rtconf <= e_conf_quantiles[3]) - sum(e_conf_predicted_rtconf <= e_conf_quantiles[2]),
        sum(e_conf_predicted_rtconf <= e_conf_quantiles[4]) - sum(e_conf_predicted_rtconf <= e_conf_quantiles[3]),
        sum(e_conf_predicted_rtconf <= e_conf_quantiles[5]) - sum(e_conf_predicted_rtconf <= e_conf_quantiles[4]),
        sum(e_conf_predicted_rtconf > e_conf_quantiles[5])
      ) / dim(predictions)[1]
      
      pred_props_rtconf_2 <- c(c_conf_pred_proportion, e_conf_pred_proportion)
      
      # Avoid zeros in the the data (because of division by predictions for chi square statistic) -> set to small number
      
      pred_props_rtconf_2[pred_props_rtconf_2 == 0] <- .0000001
      
      
      ### 4 - Calculating chi-square
      
      # Combine the quantiles for RT's and confidence RT's
      
      obs_props <- c(obs_props, obs_props, conf_obs_props_1, conf_obs_props_2)
      pred_props <- c(pred_props_rt, pred_props_rt, pred_props_rtconf_1, pred_props_rtconf_2)
      
      # Calculate chi-square
      
      chiSquare_temp = sum( ( (obs_props - pred_props) ^ 2) )
      
      # Add chi-squares 
      
      chiSquare <- chiSquare + chiSquare_temp
      
    }
    
  }
  
  # Return chiSquare
  
  return(chiSquare)
  
}

# Load data

df <- read.csv(file = "Exp2_data_viable.csv")
subs <- unique(df$sub)
N<-length(subs)

# Optimize (extended) DDM parameters (parallel)

option_list = list(
  make_option(c("-s","--subject"), type = "character",default=NULL,metavar="character")
)
opt_parser = OptionParser(option_list = option_list)
opt = parse_args(opt_parser)
i = as.numeric(opt$subject)

# For each participant separately
   
print(paste('Running participant', subs[i], 'from', N))
condLab <- unique(df$manipulation)  
tempAll <- subset(df, sub == subs[i])

for(c in 1:4){  # For each condition separately 
  
  tempDat <- subset(tempAll, manipulation == condLab[c])
  tempDat <- tempDat[,c('rt', 'cor', 'resp', 'cj', 'manipulation', 'rtconf', 'coherence')]
  
  # Load existing individual results if these already exist
  
  file_name <- paste0('exp2_vratio2_prob_results_sub_', i, '_', condLab[c], '.Rdata')
  if (overwrite == F & file.exists(file_name)){

    load(file_name)
    
  # Else, estimate parameters
    
  } else {
    
    # Optimization function
    
    optimal_params <- DEoptim(chi_square_optim,  # Function to optimize
                              # Possible values for v (for each level of coherence: 0.1, 0.2 and 0.4), a, ter, a2, postdriftmod_1, postdriftmod_2, a2_slope, ter2
                              lower = c(0, 0, 0, .3, 0, 0.0001, 0,  0,  0, -2),  
                              upper = c(3, 3, 3, 4, 1.5,    10, 15, 15, 10, 2),
                              all_observations = tempDat, returnFit = 1, control = c(itermax = itermax))
    
    results <- summary(optimal_params)
    
    # Save individual results
    
    save(results, file = file_name)
    
  }
}


