
rm(list = ls())

library(Rcpp)  # To source, compile and run C++ functions
library(DEoptim)  # Evolutionary algorithm

# Parameters

EXPERIMENT <- 2
CLUSTER <- TRUE
subject_ID <- 1
ITERMAX <- 4000  # Higher because 4x the parames


# Load data

if (EXPERIMENT == 1){
  file_name <- 'Data/Experiment_1/exp1_data_viable.csv'
} else if (EXPERIMENT == 2){
  file_name <- 'Data/Experiment_2/exp2_data_viable.csv'
} 
data <- read.csv(file_name)


# Subset data per participant

if (CLUSTER == TRUE){
  
  args = commandArgs(trailingOnly=TRUE)  
  subject_ID = as.numeric(args[1])
  data_sub <- data[data$sub == subject_ID,]

} else if (CLUSTER == FALSE) {
  
  data_sub <- data[data$sub == subject_ID,]
  
}


# Compute data fit


# Compute parameter set cost ---------------------------------------------------


compute_chi2_commonV <- function(params, df, model_type, experiment, cluster = FALSE, n_trials){
  
  library(Rcpp)

  # Initialize variables
  chi2 <- 0
  
  # Name parameters
  param_names <- switch(as.character(model_type),
                        '1' = c('a_1', 'a_2', 'a_3', 'a_4', 'v1', 'v2', 'v3', 'ter', 'a2_1', 'a2_2', 'a2_3', 'a2_4', 'a2_slope_1', 'a2_slope_2', 'a2_slope_3', 'a2_slope_4', 'vratio', 'ter2'),
                        '4' = c('a_1', 'a_2', 'a_3', 'a_4', 'v1', 'v2', 'v3', 'ter', 'a2_1', 'a2_2', 'a2_3', 'a2_4', 'a2_slope_upper_1', 'a2_slope_upper_2', 'a2_slope_upper_3', 'a2_slope_upper_4', 'a2_slope_lower_1', 'a2_slope_lower_2', 'a2_slope_lower_3', 'a2_slope_lower_4', 'starting_point_confidence_1', 'starting_point_confidence_2', 'starting_point_confidence_3', 'starting_point_confidence_4', 'vratio', 'ter2'),
  )
  
  # Check the value of param_names
  param_names
  names(params) <- param_names
  
  # Load c++ script on each node
  if (cluster == TRUE){
    if (experiment == 1){
      sourceCpp('Code/C++_generators/DDM_confidence_bounds_binary.cpp')
    } else if (experiment == 2){
      sourceCpp('Code/C++_generators/DDM_confidence_bounds_scale6.cpp')
    }
  }

  # Loop through each manipulation
  for (manipulation_level in 1:4){
    
    params['a'] <- switch(manipulation_level,
                          params['a_1'], 
                          params['a_2'], 
                          params['a_3'], 
                          params['a_4'])
    params['a2'] <- switch(manipulation_level,
                          params['a2_1'], 
                          params['a2_2'], 
                          params['a2_3'], 
                          params['a2_4'])
    if (experiment == 1){
      params['a2_slope'] <- switch(manipulation_level,
                             params['a2_slope_1'], 
                             params['a2_slope_2'], 
                             params['a2_slope_3'], 
                             params['a2_slope_4'])
    } else if (experiment == 2){
      params['a2_slope_upper'] <- switch(manipulation_level,
                                   params['a2_slope_upper_1'], 
                                   params['a2_slope_upper_2'], 
                                   params['a2_slope_upper_3'], 
                                   params['a2_slope_upper_4'])
      params['a2_slope_lower'] <- switch(manipulation_level,
                                   params['a2_slope_lower_1'], 
                                   params['a2_slope_lower_2'], 
                                   params['a2_slope_lower_3'], 
                                   params['a2_slope_lower_4'])
      params['starting_point_confidence'] <- switch(manipulation_level,
                                         params['starting_point_confidence_1'], 
                                         params['starting_point_confidence_2'], 
                                         params['starting_point_confidence_3'], 
                                         params['starting_point_confidence_4'])
    }
    
    # Loop through each level of confidence
    for (coherence_level in 1:3){
      
      params['v'] <- params[c('v1', 'v2', 'v3')[coherence_level]]
      
      df_obs <- df[df$coherence == c(0.1, 0.2, 0.4)[coherence_level] & df$manipulation == c('AccAcc', 'AccFast', 'FastFast', 'FastAcc')[manipulation_level],]
      
      
      # Make predictions  --------------------------------------------------------

      
      if (experiment == 1){
        df_pred <- data.frame(DDM_confidence_bounds_binary(v = params['v'], a = params['a'], a_slope = 0, ter = params['ter'], n_trials = n_trials, a2 = params['a2'], vratio = params['vratio'], a2_slope_upper = params['a2_slope'],       a2_slope_lower = params['a2_slope'],       ter2 = params['ter2'], starting_point_confidence = 0.5))
      } else {
        df_pred <- data.frame(DDM_confidence_bounds_scale6(v = params['v'], a = params['a'], a_slope = 0, ter = params['ter'], n_trials = n_trials, a2 = params['a2'], vratio = params['vratio'], a2_slope_upper = params['a2_slope_upper'], a2_slope_lower = params['a2_slope_lower'], ter2 = params['ter2'], starting_point_confidence = params['starting_point_confidence']))
      }
      
      names(df_pred) <- c('rt', 'cor', 'rtconf', 'cj')
      
      
      # Compute chi2 -------------------------------------------------------------
      
      
      # (CONFIDENCE) RT QUANTILES
      obs_props <- NULL
      pred_props <- NULL
      
      for (accuracy in c(0,1)){
        
        obs_temp <- df_obs[df_obs$cor == accuracy, ]
        pred_temp <- df_pred[df_pred$cor == accuracy, ]
        
        # (1) - RT quantiles per accuracy level
        quantiles <- quantile(obs_temp$rt, probs = c(.1,.3,.5,.7,.9), names = FALSE)
        if (any(is.na(quantiles))){quantiles <- rep(0,5)}
        obs_proportion <- nrow(obs_temp)/nrow(df_obs)
        obs_proportions <- obs_proportion * c(.1, .2, .2, .2, .2, .1)
        pred_proportions <- c(
          sum(pred_temp$rt <= quantiles[1]),
          sum(pred_temp$rt <= quantiles[2]) - sum(pred_temp$rt <= quantiles[1]),
          sum(pred_temp$rt <= quantiles[3]) - sum(pred_temp$rt <= quantiles[2]),
          sum(pred_temp$rt <= quantiles[4]) - sum(pred_temp$rt <= quantiles[3]),
          sum(pred_temp$rt <= quantiles[5]) - sum(pred_temp$rt <= quantiles[4]),
          sum(pred_temp$rt > quantiles[5])
        ) / nrow(df_pred)
        obs_props <- c(obs_props, obs_proportions)
        pred_props <- c(pred_props, pred_proportions)
        
        # (2) - RTconf quantiles per accuracy level
        quantiles <- quantile(obs_temp$rtconf, probs = c(.1,.3,.5,.7,.9), names = FALSE)
        if (any(is.na(quantiles))){quantiles <- rep(0,5)}
        obs_proportion <- nrow(obs_temp)/nrow(df_obs)
        obs_proportions <- obs_proportion * c(.1, .2, .2, .2, .2, .1)
        pred_proportions <- c(
          sum(pred_temp$rtconf <= quantiles[1]),
          sum(pred_temp$rtconf <= quantiles[2]) - sum(pred_temp$rtconf <= quantiles[1]),
          sum(pred_temp$rtconf <= quantiles[3]) - sum(pred_temp$rtconf <= quantiles[2]),
          sum(pred_temp$rtconf <= quantiles[4]) - sum(pred_temp$rtconf <= quantiles[3]),
          sum(pred_temp$rtconf <= quantiles[5]) - sum(pred_temp$rtconf <= quantiles[4]),
          sum(pred_temp$rtconf > quantiles[5])
        ) / nrow(df_pred)
        obs_props <- c(obs_props, obs_proportions)
        pred_props <- c(pred_props, pred_proportions)
        
      }
      
      obs_props[obs_props==0] <- 0.0000001  
      pred_props[pred_props==0] <- 0.0000001  # avoid zeros in the numerator of chi2  
      rt_chi2 <- sum( ( (obs_props - pred_props) ^ 2) / pred_props)
      
      # CJ PROPORTIONS
      if (experiment == 1){
        obs_props <- as.vector(prop.table(table(factor(df_obs$cj, 0:1), factor(df_obs$cor, 0:1))))
        pred_props <- as.vector(prop.table(table(factor(df_pred$cj, 0:1), factor(df_pred$cor, 0:1))))
      } else if (experiment == 2){
        obs_props <- as.vector(prop.table(table(factor(df_obs$cj, 1:6), factor(df_obs$cor, 0:1))))
        pred_props <- as.vector(prop.table(table(factor(df_pred$cj, 1:6), factor(df_pred$cor, 0:1))))
      }
     
      obs_props[obs_props==0] <- 0.0000001  
      pred_props[pred_props==0] <- 0.0000001  # avoid zeros in the numerator of chi2
      confidence_chi2 <- sum( ( (obs_props - pred_props) ^ 2) / pred_props)
      
      
      # B - COMBINE CHI2's
      chi2 <- chi2 + rt_chi2 + confidence_chi2
      
    }
    
  }

  return(chi2)
  
}

# Optimization function

if (EXPERIMENT == 1){
  sourceCpp('Code/C++_generators/DDM_confidence_bounds_binary.cpp')
  optimal_params <- DEoptim(fn = compute_chi2_commonV, 
                            # a_1234, v1, v2, v3, ter, a2_1234, a2_slope_1234, vratio, ter2
                            lower = c(0.3, 0.3, 0.3, 0.3, 0, 0, 0,   0,  0,  0,  0,  0,  0,  0,  0,  0, 0.2, -1.5),  
                            upper = c(  4,   4,   4,   4, 3, 3, 3, 1.5, 20, 20, 20, 20,  5,  5,  5,  5,   5,  1.5),
                            df = data_sub, model_type = 1, experiment = 1, n_trials = 2000, cluster = CLUSTER, control = list(itermax = ITERMAX, parallelType = 'parallel', packages = c('Rcpp')))  
} else if (EXPERIMENT == 2){
  sourceCpp('Code/C++_generators/DDM_confidence_bounds_scale6.cpp')
  optimal_params <- DEoptim(fn = compute_chi2_commonV, 
                            # a_1234, v1, v2, v3, ter, a2_1234, a2_slope_upper_1234, a2_slope_lower_1234, starting_point_confidence_1234, vratio, ter2
                            lower = c(0.3, 0.3, 0.3, 0.3, 0, 0, 0,   0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, 0, 0, 0, 0, 0.2, -1.5),  
                            upper = c(  4,   4,   4,   4, 3, 3, 3, 1.5, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 1, 1, 1, 1,   5,  1.5),
                            df = data_sub, model_type = 4, experiment = 2, n_trials = 2000, cluster = CLUSTER, control = list(itermax = ITERMAX, parallelType = 'parallel', packages = c('Rcpp')))
}

if (EXPERIMENT == 1){
  file_name <- paste0('Data/Experiment_', EXPERIMENT,'/Model_parameters/exp', EXPERIMENT, '_model', 1, '_sub', subject_ID, '_commonV.Rdata')
} else if (EXPERIMENT == 2){
  file_name <- paste0('Data/Experiment_', EXPERIMENT,'/Model_parameters/exp', EXPERIMENT, '_model', 4, '_sub', subject_ID, '_commonV.Rdata')
}

results <- summary(optimal_params)
save(results, file = file_name)
print('Parameter optimization finished.')




# ----------------------


model_type = 1
experiment = 1
  
params <- NULL


for (sub in 1:40){
  
  
  file_name <- paste0('Data/Experiment_', experiment,'/Model_parameters/exp', experiment, '_model', model_type, '_sub', sub, '_commonV.Rdata')
  load(file_name)
  
  # Add parameter set
  if (experiment == 1){
    params_set <- c(sub, results$optim$bestmem[1], results$optim$bestmem[2], results$optim$bestmem[3], results$optim$bestmem[4], results$optim$bestmem[5], results$optim$bestmem[6], results$optim$bestmem[7], results$optim$bestmem[8], results$optim$bestmem[9], results$optim$bestmem[10], results$optim$bestmem[11], results$optim$bestmem[12], results$optim$bestmem[13], results$optim$bestmem[14], results$optim$bestmem[15], results$optim$bestmem[16], results$optim$bestmem[17], results$optim$bestmem[18])
    params <- rbind(params, params_set)
    colnames(params) <- c('sub', 'a_1', 'a_2', 'a_3', 'a_4', 'v1', 'v2', 'v3', 'ter', 'a2_1', 'a2_2', 'a2_3', 'a2_4', 'a2_slope_1', 'a2_slope_2', 'a2_slope_3', 'a2_slope_4', 'vratio', 'ter2')
  } else if (experiment == 2){
    params_set <- c(sub, results$optim$bestmem[1], results$optim$bestmem[2], results$optim$bestmem[3], results$optim$bestmem[4], results$optim$bestmem[5], results$optim$bestmem[6], results$optim$bestmem[7], results$optim$bestmem[8], results$optim$bestmem[9], results$optim$bestmem[10], results$optim$bestmem[11], results$optim$bestmem[12], results$optim$bestmem[13], results$optim$bestmem[14], results$optim$bestmem[15], results$optim$bestmem[16], results$optim$bestmem[17], results$optim$bestmem[18], results$optim$bestmem[19], results$optim$bestmem[20], results$optim$bestmem[21], results$optim$bestmem[22], results$optim$bestmem[23], results$optim$bestmem[24], results$optim$bestmem[25], results$optim$bestmem[26])
    params <- rbind(params, params_set)
    colnames(params) <- c('sub', 'a_1', 'a_2', 'a_3', 'a_4', 'v1', 'v2', 'v3', 'ter', 'a2_1', 'a2_2', 'a2_3', 'a2_4', 'a2_slope_upper_1', 'a2_slope_upper_2', 'a2_slope_upper_3', 'a2_slope_upper_4', 'a2_slope_lower_1', 'a2_slope_lower_2', 'a2_slope_lower_3', 'a2_slope_lower_4', 'starting_point_confidence_1', 'starting_point_confidence_2', 'starting_point_confidence_3', 'starting_point_confidence_4', 'vratio', 'ter2')
  }
  
  
}
    


  
file_name <- paste0('Data/Experiment_', experiment,'/Model_parameters/exp', experiment, '_model', model_type, '_commonV.csv')
write.csv(params, file = file_name, row.names = FALSE)
  


# --------------------------------


if (EXPERIMENT == 1){
  file_name <- 'Data/Experiment_1/Model_parameters/exp1_model1_parameters.csv'
} else if (EXPERIMENT == 2){
  file_name <- 'Data/Experiment_2/exp2_data_viable.csv'
} 
df_params <- read.csv(file_name)
df_params_commonV <- data.frame(params)


par(mfrow = c(2, 2))


cor(df_params$a[df_params$manipulation == 'AccAcc'], df_params_commonV$a_1)
cor(df_params$a[df_params$manipulation == 'AccFast'], df_params_commonV$a_2)
cor(df_params$a[df_params$manipulation == 'FastFast'], df_params_commonV$a_3)
cor(df_params$a[df_params$manipulation == 'FastAcc'], df_params_commonV$a_4)
a <- plot(df_params$a[df_params$manipulation == 'AccAcc'], df_params_commonV$a_1)
b <- plot(df_params$a[df_params$manipulation == 'AccFast'], df_params_commonV$a_2)
c <- plot(df_params$a[df_params$manipulation == 'FastFast'], df_params_commonV$a_3)
d <- plot(df_params$a[df_params$manipulation == 'FastAcc'], df_params_commonV$a_4)

cor(df_params$a2[df_params$manipulation == 'AccAcc'], df_params_commonV$a2_1)
cor(df_params$a2[df_params$manipulation == 'AccFast'], df_params_commonV$a2_2)
cor(df_params$a2[df_params$manipulation == 'FastFast'], df_params_commonV$a2_3)
cor(df_params$a2[df_params$manipulation == 'FastAcc'], df_params_commonV$a2_4)
a <- plot(df_params$a2[df_params$manipulation == 'AccAcc'], df_params_commonV$a2_1)
b <- plot(df_params$a2[df_params$manipulation == 'AccFast'], df_params_commonV$a2_2)
c <- plot(df_params$a2[df_params$manipulation == 'FastFast'], df_params_commonV$a2_3)
d <- plot(df_params$a2[df_params$manipulation == 'FastAcc'], df_params_commonV$a2_4)

cor(df_params$a2_slope[df_params$manipulation == 'AccAcc'], df_params_commonV$a2_slope_1)
cor(df_params$a2_slope[df_params$manipulation == 'AccFast'], df_params_commonV$a2_slope_2)
cor(df_params$a2_slope[df_params$manipulation == 'FastFast'], df_params_commonV$a2_slope_3)
cor(df_params$a2_slope[df_params$manipulation == 'FastAcc'], df_params_commonV$a2_slope_4)
a <- plot(df_params$a2_slope[df_params$manipulation == 'AccAcc'], df_params_commonV$a2_slope_1)
b <- plot(df_params$a2_slope[df_params$manipulation == 'AccFast'], df_params_commonV$a2_slope_2)
c <- plot(df_params$a2_slope[df_params$manipulation == 'FastFast'], df_params_commonV$a2_slope_3)
d <- plot(df_params$a2_slope[df_params$manipulation == 'FastAcc'], df_params_commonV$a2_slope_4)

cor(df_params$ter[df_params$manipulation == 'AccAcc'], df_params_commonV$ter_1)
cor(df_params$ter[df_params$manipulation == 'AccFast'], df_params_commonV$ter_2)
cor(df_params$ter[df_params$manipulation == 'FastFast'], df_params_commonV$ter_3)
cor(df_params$ter[df_params$manipulation == 'FastAcc'], df_params_commonV$ter_4)
a <- plot(df_params$ter[df_params$manipulation == 'AccAcc'], df_params_commonV$ter_1)
b <- plot(df_params$ter[df_params$manipulation == 'AccFast'], df_params_commonV$ter_2)
c <- plot(df_params$ter[df_params$manipulation == 'FastFast'], df_params_commonV$ter_3)
d <- plot(df_params$ter[df_params$manipulation == 'FastAcc'], df_params_commonV$ter_4)

cor(df_params$ter2[df_params$manipulation == 'AccAcc'], df_params_commonV$ter2_1)
cor(df_params$ter2[df_params$manipulation == 'AccFast'], df_params_commonV$ter2_2)
cor(df_params$ter2[df_params$manipulation == 'FastFast'], df_params_commonV$ter2_3)
cor(df_params$ter2[df_params$manipulation == 'FastAcc'], df_params_commonV$ter2_4)
a <- plot(df_params$ter2[df_params$manipulation == 'AccAcc'], df_params_commonV$ter2_1)
b <- plot(df_params$ter2[df_params$manipulation == 'AccFast'], df_params_commonV$ter2_2)
c <- plot(df_params$ter2[df_params$manipulation == 'FastFast'], df_params_commonV$ter2_3)
d <- plot(df_params$ter2[df_params$manipulation == 'FastAcc'], df_params_commonV$ter2_4)

















