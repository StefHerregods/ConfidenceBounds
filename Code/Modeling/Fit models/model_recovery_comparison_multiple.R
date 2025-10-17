################################################################################
# (1) Make predictions for estimated model parameters                     
# (2) Estimate model parameters on predictions
# (3) Make model predictions from new parameters
# (4) Compute BIC for each model combination
################################################################################


rm(list = ls())
source('Code/model_functions.R')

CLUSTER <- FALSE
EXPERIMENT <- 2
MODEL_PREDICTIONS <- 9  # Model predictions to fit on
MODEL_ESTIMATION <- 9  # Model to estimate parameters from
COMPUTE_BIC_MATRIX <- TRUE


# (1) Make predictions for estimated model parameters --------------------------


if (CLUSTER == FALSE){  # Do this before using the cluster
  
  parameter_location <- paste0('Data/Experiment_', EXPERIMENT, '/Model_parameters/exp', EXPERIMENT, '_model', MODEL_PREDICTIONS, '_parameters.csv')
  predictions_location  <- paste0('Data/Experiment_', EXPERIMENT, '/Model_predictions/exp', EXPERIMENT, '_model', MODEL_PREDICTIONS, '_predictions_180.csv')
  
  if (file.exists(predictions_location)){
    
    print('Predictions already exist')
    all_predictions <- read.csv(predictions_location)
    
  } else {
    print(parameter_location)
    parameters <- read.csv(file = parameter_location)
    print(parameters)
    all_predictions <- NULL
    
    for (row in 1:nrow(parameters)){  # Make predictions for every combination of sub/manipulation
      parameters_sub <- parameters[row,]
      predictions <- make_predictions(model_type = MODEL_PREDICTIONS, experiment = EXPERIMENT, params = parameters_sub, n_trials = 180)  # n_trials = trials in experiment
      predictions$sub <- parameters_sub$sub
      predictions$manipulation <- parameters_sub$manipulation
      all_predictions <- rbind(all_predictions, predictions)
    }

    write.csv(x = all_predictions, file = predictions_location, row.names = F)
    print('Predictions created')
    
  }

} else {
  
  predictions_location  <- paste0('Data/Experiment_', EXPERIMENT, '/Model_predictions/exp', EXPERIMENT, '_model', MODEL_PREDICTIONS, '_predictions_180.csv')
  all_predictions <- read.csv(predictions_location)
  
}


# (2) Estimate model parameters on predictions ---------------------------------


if (CLUSTER == FALSE){
  
  # Loop through subjects
  for (subject_ID in unique(all_predictions$sub)){
    data_sub <- all_predictions[all_predictions$sub == subject_ID,]
    estimate_params(model_type = MODEL_ESTIMATION, model_type_prediction = MODEL_PREDICTIONS, experiment = EXPERIMENT, cluster = CLUSTER, df_sub = data_sub, subject = subject_ID, model_recovery = TRUE)
  }
  
} else if (CLUSTER == TRUE){

  # Estimate in parallel
  args = commandArgs(trailingOnly = TRUE)  
  subject_ID = as.numeric(args[1])
  data_sub <- all_predictions[all_predictions$sub == subject_ID,]
  for (MODEL_ESTIMATION in 1:9){  
    estimate_params(model_type = MODEL_ESTIMATION, model_type_prediction = MODEL_PREDICTIONS, experiment = EXPERIMENT, cluster = CLUSTER, df_sub = data_sub, subject = subject_ID, model_recovery = TRUE)
  }
}

# Grouping all estimated parameters if not on cluster

if (CLUSTER == FALSE){
  group_params(model_type = MODEL_ESTIMATION, experiment = EXPERIMENT, model_recovery = TRUE, model_type_predictions = MODEL_PREDICTIONS)
}


# (3) Make model predictions from new parameters & compute BIC -----------------

if (EXPERIMENT == 1){
  bic_models <- c(1, 6, 7, 8)
} else {
  bic_models <- c(4, 6, 7, 8, 9)
}
n_models <- length(bic_models)

if (CLUSTER == FALSE & COMPUTE_BIC_MATRIX == TRUE){  
  
  if (EXPERIMENT == 1){
    sourceCpp('Code/C++_generators/DDM_confidence_bounds_binary.cpp')
    sourceCpp('Code/C++_generators/DDM_2DSD_binary.cpp')
    sourceCpp('Code/C++_generators/DDM_CCB_binary.cpp')
    sourceCpp('Code/C++_generators/DDM_race_binary.cpp')
    sourceCpp('Code/C++_generators/DDM_2DSD_critVar_binary.cpp')
  } else if (EXPERIMENT == 2){
    sourceCpp('Code/C++_generators/DDM_confidence_bounds_scale6.cpp')
    sourceCpp('Code/C++_generators/DDM_2DSD_scale6.cpp')
    sourceCpp('Code/C++_generators/DDM_CCB_scale6.cpp')
    sourceCpp('Code/C++_generators/DDM_race_scale6.cpp')
    sourceCpp('Code/C++_generators/DDM_2DSD_critVar_scale6.cpp')
    sourceCpp('Code/C++_generators/DDM_2DSD_OptionalStopping_scale6.cpp')
  }
  
  df_BIC <- as.data.frame(matrix(0, nrow = n_models, ncol = n_models))
  df_BIC_sub <- NULL
  
  for (i in 1:n_models){
    
    # A - Load original model predictions
    
    predictions_location  <- paste0('Data/Experiment_', EXPERIMENT, '/Model_predictions/exp', EXPERIMENT, '_model', bic_models[i], '_predictions_180.csv')
    predictions_original <- read.csv(predictions_location)
    
    for (j in 1:n_models){
      
      print(paste(bic_models[i], bic_models[j]))
      
      # B - Make predictions from new model fit
      
      parameters_location <- paste0('Data/Experiment_', EXPERIMENT,'/Model_parameters/exp', EXPERIMENT, '_ModelRecovery_predM', bic_models[i], '_fitM', bic_models[j], '_parameters.csv')
      
      if (file.exists(parameters_location)){
        
        parameters <- read.csv(file = parameters_location)
        predictions_new <- NULL
        
        for (row in 1:nrow(parameters)){  # Make predictions for every combination of sub/manipulation
          parameters_sub <- parameters[row,]
          predictions <- make_predictions_recovery(model_type = bic_models[j], experiment = EXPERIMENT, params = parameters_sub, n_trials = 1000)  # n_trials = trials in experiment per coherence level per manipulation
          predictions$sub <- parameters_sub$sub
          predictions$manipulation <- parameters_sub$manipulation
          predictions_new <- rbind(predictions_new, predictions)
        }
        
        # C - Compute BIC 
        
        BIC <- list()
        for (sub in unique(predictions_original$sub)){
          for (manipulation in unique(predictions_original$manipulation)){
            
            predictions_original_sub <- predictions_original[predictions_original$sub == sub & predictions_original$manipulation == manipulation,]
            predictions_new_sub <- predictions_new[predictions_new$sub == sub & predictions_new$manipulation == manipulation,]
            BIC_list <- compute_BIC(df_observed = predictions_original_sub, df_predicted = predictions_new_sub, model_type = bic_models[j], experiment = EXPERIMENT)
            BIC <- append(BIC, BIC_list$BIC)
            df_BIC_sub <- rbind(df_BIC_sub, c(sub, manipulation, BIC_list$BIC, bic_models[i], bic_models[j]))
            
          }
        } 
        
        df_BIC[i,j] <- mean(unlist(BIC))
        
      } else {
        print('no file')
      }
      
    }
    
  }
  
  if (EXPERIMENT == 1){
    rownames(df_BIC) <- c('original fit - FCB1', 'original fit - CCB', 'original fit - Van Zandt', 'original fit - 2DSD + variation')
    colnames(df_BIC) <- c('new fit - FCB1', 'new fit - CCB', 'new fit - Van Zandt', 'new fit - 2DSD + variation')
    write.csv(round(df_BIC,2), 'BIC_modelRecovery_exp1.csv')
  } else if (EXPERIMENT == 2){
    rownames(df_BIC) <- c('original fit - FCB4', 'original fit - CCB', 'original fit - Van Zandt', 'original fit - 2DSD + variation', 'original fit - 2DSD optional stopping')
    colnames(df_BIC) <- c('new fit - FCB4', 'new fit - CCB', 'new fit - Van Zandt', 'new fit - 2DSD + variation', 'new fit - optional stopping')
    write.csv(round(df_BIC,2), 'BIC_modelRecovery_exp2.csv')
  }
  print(df_BIC)

  
  df_BIC_sub <- data.frame(df_BIC_sub)
  names(df_BIC_sub) <- c('sub', 'manipulation', 'BIC', 'original_model', 'new_model')
  
}


df_BIC_count <- as.data.frame(matrix(0, nrow = n_models, ncol = n_models))
for (participant_ID in 1:40){
  for (manipulation in c('AccAcc', 'AccFast', 'FastFast', 'FastAcc')){
    for (i in 1:n_models){
      df_temp <- df_BIC_sub[df_BIC_sub$sub == participant_ID & df_BIC_sub$manipulation == manipulation & df_BIC_sub$original_model == bic_models[i], ]
      j <- which(as.numeric(df_temp[which.min(df_temp$BIC), ]$new_model) == bic_models)
      
      df_BIC_count[i, j] <- df_BIC_count[i, j] + 1
      
      
    }
  }
}

df_BIC_count <- data.frame(df_BIC_count)

if (EXPERIMENT == 1){
  rownames(df_BIC_count) <- c('original fit - FCB1', 'original fit - CCB', 'original fit - Van Zandt', 'original fit - 2DSD + variation')
  colnames(df_BIC_count) <- c('new fit - FCB1', 'new fit - CCB', 'new fit - Van Zandt', 'new fit - 2DSD + variation')
  write.csv(round(df_BIC_count), 'counts_modelRecovery_exp1.csv')
} else if (EXPERIMENT == 2){
  rownames(df_BIC_count) <- c('original fit - FCB4', 'original fit - CCB', 'original fit - Van Zandt', 'original fit - 2DSD + variation', 'original fit - 2DSD optional stopping')
  colnames(df_BIC_count) <- c('new fit - FCB4', 'new fit - CCB', 'new fit - Van Zandt', 'new fit - 2DSD + variation', 'new fit - optional stopping')
  write.csv(round(df_BIC_count,2), 'counts_modelRecovery_exp2.csv')
}


df_BIC_delta <- t(apply(df_BIC, 1, function(x) x - min(x)))
if (EXPERIMENT == 1){
  write.csv(round(df_BIC_delta,2), 'deltaBIC_modelRecovery_exp1.csv')
} else if (EXPERIMENT == 2){
  write.csv(round(df_BIC_delta,2), 'deltaBIC_modelRecovery_exp2.csv')
}



