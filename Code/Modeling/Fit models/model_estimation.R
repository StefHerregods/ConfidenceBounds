################################################################################
# - Estimate evidence accumulation model parameters                            #
# - Make model predictions                                                     #
################################################################################


# Set-up -----------------------------------------------------------------------


rm(list = ls())

# Load functions
source('Code/model_functions.R')


# Constants --------------------------------------------------------------------


# Model type
# [1] Herregods et al. - equal confidence urgencies, no confidence bias
# [2] Herregods et al. - unequal confidence urgencies, no confidence bias
# [3] Herregods et al. - equal confidence urgencies, with confidence bias
# [4] Herregods et al. - unequal confidence urgencies, with confidence bias
# [5] 2DSD 
# [6] CCB
# [7] Zandt
# [8] 2DSD + variation for tau
# [9] 2DSD optional stopping
MODEL_TYPE <- 9

# Experiment number (1 = binary cj, 2 = 6-point scale cj)
EXPERIMENT <- 1

# Number of trials to predict per set of parameters
PREDICTION_TRIALS <- 5000

# Use cluster computer
CLUSTER <- FALSE


# Load data --------------------------------------------------------------------

  
if (EXPERIMENT == 1){
  file_name <- 'Data/Experiment_1/exp1_data_viable.csv'
} else if (EXPERIMENT == 2){
  file_name <- 'Data/Experiment_2/exp2_data_viable.csv'
} 
data <- read.csv(file_name)


# Estimate parameters ----------------------------------------------------------


if (CLUSTER == TRUE){
  
  # Estimate in parallel
  args = commandArgs(trailingOnly=TRUE)  
  subject_ID = as.numeric(args[1])
  data_sub <- data[data$sub == subject_ID,]
  estimate_params(model_type = MODEL_TYPE, experiment = EXPERIMENT, cluster = CLUSTER, df_sub = data_sub, subject = subject_ID)
  
} else if (CLUSTER == FALSE) {

  # Loop through subjects
  for (subject_ID in unique(data$sub)){
    data_sub <- data[data$sub == subject_ID,]
    estimate_params(model_type = MODEL_TYPE, experiment = EXPERIMENT, cluster = CLUSTER, df_sub = data_sub, subject = subject_ID)
  }
  
}


# Group parameters -------------------------------------------------------------


if (CLUSTER == FALSE){
  group_params(model_type = MODEL_TYPE, experiment = EXPERIMENT)
}


# Make predictions -------------------------------------------------------------


if (CLUSTER == FALSE){
  predictions <- NULL
  file_name_params <- paste0('Data/Experiment_', EXPERIMENT,'/Model_parameters/exp', EXPERIMENT, '_model', MODEL_TYPE, '_parameters.csv')
  params <- read.csv(file = file_name_params, stringsAsFactors = FALSE, check.names = FALSE)
  for (subject_ID in unique(params$sub)){
    for (model_condition in unique(params$manipulation)){
      params_subset <- params[params$sub == subject_ID & params$manipulation == model_condition,]
      predictions_subset <- make_predictions(model_type = MODEL_TYPE, experiment = EXPERIMENT, params = params_subset, n_trials = PREDICTION_TRIALS)
      predictions_subset$manipulation <- model_condition
      predictions_subset$sub <- subject_ID
      predictions <- rbind(predictions, predictions_subset)
    }
  }
  file_name_predictions <- paste0('Data/Experiment_', EXPERIMENT,'/Model_predictions/exp', EXPERIMENT, '_model', MODEL_TYPE, '_predictions.csv')
  write.csv(predictions, file = file_name_predictions, row.names = FALSE)
}

