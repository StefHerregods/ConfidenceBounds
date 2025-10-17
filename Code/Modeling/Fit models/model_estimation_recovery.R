################################################################################
# Parameter recovery                                                           #
# (1) Choose parameters at random                                              #
# (2) Make parameter predictions                                               #
# (3) Recovar parameters                                                       #
################################################################################


# Set-up -----------------------------------------------------------------------


rm(list = ls())

# Load functions
source('Code/model_functions.R')


# Constants --------------------------------------------------------------------


# Experiment number (1 = binary cj, 2 = 6-point scale cj)
EXPERIMENT <- 1
MODEL_TYPE <- ifelse(EXPERIMENT == 1, 1, 4)


# n_trials version (180 - 30,000)
VERSION <- 180


# Use cluster computer
CLUSTER <- FALSE


# Generate or load parameters & predictions --------------------------------------------


if (CLUSTER == FALSE){
  
  if (EXPERIMENT == 1){
    
    # Generate parameters
    file_name_parameters <- 'Data/Experiment_1/Model_parameters/exp1_model1_parameters_beforeRecovery.csv'
    if (file.exists(file_name_parameters)){
      params <- read.csv(file_name_parameters)
      print('Parameters already randomly generated')
    } else {
      a <- runif(n = 100, min = 1, max = 3)
      v1 <- runif(n = 100, min = 0, max = 2)
      v2 <- runif(n = 100, min = 0, max = 2)
      v3 <- runif(n = 100, min = 0, max = 2)
      ter <- runif(n = 100, min = 0, max = 1)
      a2 <- runif(n = 100, min = 0, max = 4)
      a2_slope <- runif(n = 100, min = 0, max = 1)
      vratio <- runif(n = 100, min = 0, max = 2)
      ter2 <- runif(n = 100, min = -0.5, max = 0.5)
      sub <- 1:100
      manipulation <- 'recovery'
      params <- data.frame(a, v1, v2, v3, ter, a2, a2_slope, vratio, ter2, sub, manipulation)
      write.csv(x = params, file = file_name_parameters, row.names = FALSE)
      print('New parameters generated')
    }
    
    # Generate model predictions
    if (VERSION == 180){
      file_name_predictions <- 'Data/Experiment_1/Model_predictions/exp1_model1_predictions_recovery_180.csv'
      if (file.exists(file_name_predictions)){
        data <- read.csv(file_name_predictions)
        print('Predictions already generated')
      } else {
        data <- NULL
        for (subject_ID in unique(params$sub)){
          for (model_condition in unique(params$manipulation)){
            params_subset <- params[params$sub == subject_ID & params$manipulation == model_condition,]
            predictions_subset <- make_predictions(model_type = 1, experiment = 1, params = params_subset, n_trials = 60)
            predictions_subset$manipulation <- model_condition
            predictions_subset$sub <- subject_ID
            data <- rbind(data, predictions_subset)
          }
        }
        write.csv(data, file = file_name_predictions, row.names = FALSE)
        print('New predictions generated')
      }  
    } else if (VERSION == 30000){
      file_name_predictions <- 'Data/Experiment_1/Model_predictions/exp1_model1_predictions_recovery_30000.csv'
      if (file.exists(file_name_predictions)){
        data <- read.csv(file_name_predictions)
        print('Predictions already generated')
      } else {
        data <- NULL
        for (subject_ID in unique(params$sub)){
          for (model_condition in unique(params$manipulation)){
            params_subset <- params[params$sub == subject_ID & params$manipulation == model_condition,]
            predictions_subset <- make_predictions(model_type = 1, experiment = 1, params = params_subset, n_trials = 10000)
            predictions_subset$manipulation <- model_condition
            predictions_subset$sub <- subject_ID
            data <- rbind(data, predictions_subset)
          }
        }
        write.csv(data, file = file_name_predictions, row.names = FALSE)
        print('New predictions generated')
      }  
    }

  } else if (EXPERIMENT == 2){
    
    # Generate parameters
    file_name_parameters <- 'Data/Experiment_2/Model_parameters/exp2_model4_parameters_beforeRecovery.csv'
    if (file.exists(file_name_parameters)){
      params <- read.csv(file_name_parameters)
      print('Parameters already randomly generated')
    } else {
      a <- runif(n = 100, min = 1, max = 3)
      v1 <- runif(n = 100, min = 0, max = 2)
      v2 <- runif(n = 100, min = 0, max = 2)
      v3 <- runif(n = 100, min = 0, max = 2)
      ter <- runif(n = 100, min = 0, max = 1)
      a2 <- runif(n = 100, min = 2, max = 7)
      a2_slope_upper <- runif(n = 100, min = 0, max = 3)
      a2_slope_lower <- runif(n = 100, min = 0, max = 1.5)
      starting_point_confidence <- runif(n = 100, min = 0.5, max = 0.8)
      vratio <- runif(n = 100, min = 0, max = 1)
      ter2 <- runif(n = 100, min = -0.5, max = 0.5)
      sub <- 1:100
      manipulation <- 'recovery'
      params <- data.frame(a, v1, v2, v3, ter, a2, a2_slope_upper, a2_slope_lower, starting_point_confidence, vratio, ter2, sub, manipulation)
      write.csv(x = params, file = file_name_parameters, row.names = FALSE)
      print('New parameters generated')
    }

    # Generate model predictions 
    if (VERSION == 180){
      file_name_predictions <- 'Data/Experiment_2/Model_predictions/exp2_model4_predictions_recovery_180.csv'
      if (file.exists(file_name_predictions)){
        data <- read.csv(file_name_predictions)
        print('Predictions already generated')
      } else {
        data <- NULL
        for (subject_ID in unique(params$sub)){
          for (model_condition in unique(params$manipulation)){
            params_subset <- params[params$sub == subject_ID & params$manipulation == model_condition,]
            predictions_subset <- make_predictions(model_type = 4, experiment = 2, params = params_subset, n_trials = 60)
            predictions_subset$manipulation <- model_condition
            predictions_subset$sub <- subject_ID
            data <- rbind(data, predictions_subset)
          }
        }
        write.csv(data, file = file_name_predictions, row.names = FALSE)
        print('New predictions generated')
      }  
    } else if (VERSION == 30000){
      file_name_predictions <- 'Data/Experiment_2/Model_predictions/exp2_model4_predictions_recovery_30000.csv'
      if (file.exists(file_name_predictions)){
        data <- read.csv(file_name_predictions)
        print('Predictions already generated')
      } else {
        data <- NULL
        for (subject_ID in unique(params$sub)){
          for (model_condition in unique(params$manipulation)){
            params_subset <- params[params$sub == subject_ID & params$manipulation == model_condition,]
            predictions_subset <- make_predictions(model_type = 4, experiment = 2, params = params_subset, n_trials = 10000)
            predictions_subset$manipulation <- model_condition
            predictions_subset$sub <- subject_ID
            data <- rbind(data, predictions_subset)
          }
        }
        write.csv(data, file = file_name_predictions, row.names = FALSE)
        print('New predictions generated')
      }  
    }
    
  }
} else if (CLUSTER == TRUE){
  file_name_predictions <- paste0('Data/Experiment_', EXPERIMENT, '/Model_predictions/exp', EXPERIMENT, '_model', MODEL_TYPE, '_predictions_recovery_', VERSION, '.csv')
  data <- read.csv(file_name_predictions)
}


# Estimate parameters ----------------------------------------------------------


if (CLUSTER == TRUE){
  
  # Estimate in parallel
  args = commandArgs(trailingOnly=TRUE)  
  subject_ID = as.numeric(args[1])
  data_sub <- data[data$sub == subject_ID,]
  estimate_params(model_type = MODEL_TYPE, experiment = EXPERIMENT, cluster = CLUSTER, df_sub = data_sub, subject = subject_ID, param_recovery = TRUE, recovery_version = VERSION)
  
} else if (CLUSTER == FALSE) {

  # Loop through subjects
  for (subject_ID in unique(data$sub)){
    data_sub <- data[data$sub == subject_ID,]
    estimate_params(model_type = MODEL_TYPE, experiment = EXPERIMENT, cluster = CLUSTER, df_sub = data_sub, subject = subject_ID, param_recovery = TRUE, recovery_version = VERSION)
  }
  
}


# Group parameters -------------------------------------------------------------


if (CLUSTER == FALSE){
  group_params(model_type = MODEL_TYPE, experiment = EXPERIMENT, param_recovery = TRUE, recovery_version = VERSION)
}

