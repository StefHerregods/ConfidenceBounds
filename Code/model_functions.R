################################################################################
# FUNCTIONS:                                                                   #
# * estimate_params()                                                          #
# * compute_chi2()                                                             #
# * group_params()                                                             #
# * make_predictions()                                                         #
# * compute_BIC()                                                              #
################################################################################


# Load functions ---------------------------------------------------------------


library(Rcpp)  # To source, compile and run C++ functions
library(DEoptim)  # Evolutionary algorithm


# Estimate parameters ----------------------------------------------------------


estimate_params <- function(model_type, experiment, cluster, df_sub, subject, param_recovery = FALSE, recovery_version = 0, itermax = 500, n_trials = 2000, model_recovery = FALSE, model_type_prediction = 0){ 
  
  print(paste0('Parameter estimation - EXPERIMENT ', experiment, ', MODEL ', model_type))
  
  # Loop through each experiment condition
  for (manipulation in unique(df_sub$manipulation)){
    
    print(paste0('Start parameter estimation - SUB ', subject, ', CONDITION ', manipulation))
    
    if (param_recovery == TRUE){
      file_name <- paste0('Data/Experiment_', experiment,'/Model_parameters/exp', experiment, '_model', model_type, '_sub', subject, '_manipulation', manipulation, '_recovery', recovery_version, '.Rdata')
    } else if (param_recovery == FALSE){
      file_name <- paste0('Data/Experiment_', experiment,'/Model_parameters/exp', experiment, '_model', model_type, '_sub', subject, '_manipulation', manipulation, '.Rdata')
    }
    
    if (model_recovery == TRUE){
      file_name <- paste0('Data/Experiment_', experiment,'/Model_parameters/exp', experiment, '_sub', subject, '_manipulation', manipulation, '_ModelRecovery_predM', model_type_prediction, '_fitM', model_type, '.Rdata')
    }

    if (file.exists(file_name)){
      
      print('Parameters already estimated')
      
    } else {
      
      df_manipulation <- df_sub[df_sub$manipulation == manipulation,]
      
      
      # EXPERIMENT 1 -----------------------------------------------------------
      
      
      # MODEL 1  -  collapsing confidence boundaries - equal confidence urgencies, no confidence bias
      
      if (model_type == 1 & experiment == 1){
        sourceCpp('Code/C++_generators/DDM_confidence_bounds_binary.cpp')
        optimal_params <- DEoptim(fn = compute_chi2, 
                                  # a, v1, v2, v3, ter, a2, a2_slope, vratio, ter2 
                                  lower = c(0.3, 0, 0, 0,   0,  0,  0, 0.2,  -1.5),  
                                  upper = c(  4, 3, 3, 3, 1.5, 20,  5,   5,   1.5),
                                  df = df_manipulation, model_type = model_type, experiment = experiment, n_trials = n_trials, cluster = cluster, control = list(itermax = itermax, parallelType = 'parallel', packages = c('Rcpp')))}

      # MODEL 2  -  collapsing confidence boundaries - unequal confidence urgencies, no confidence bias
      
      if (model_type == 2 & experiment == 1){
        sourceCpp('Code/C++_generators/DDM_confidence_bounds_binary.cpp')
        optimal_params <- DEoptim(fn = compute_chi2, 
                                  # a, v1, v2, v3, ter, a2, a2_slope_upper, a2_slope_lower, vratio, ter2 
                                  lower = c(0.3, 0, 0, 0,   0,  0,  0,  0, 0.2,  -1.5),  
                                  upper = c(  4, 3, 3, 3, 1.5, 20, 20, 20,   5,   1.5),
                                  df = df_manipulation, model_type = model_type, experiment = experiment, n_trials = n_trials, cluster = cluster, control = list(itermax = itermax, parallelType = 'parallel', packages = c('Rcpp')))}
      
      # MODEL 3  -  collapsing confidence boundaries - equal confidence urgencies, with confidence bias
      
      if (model_type == 3 & experiment == 1){
        sourceCpp('Code/C++_generators/DDM_confidence_bounds_binary.cpp')
        optimal_params <- DEoptim(fn = compute_chi2, 
                                  # a, v1, v2, v3, ter, a2, a2_slope, starting_point_confidence, vratio, ter2 
                                  lower = c(0.3, 0, 0, 0,   0,  0,  0, 0, 0.2,  -1.5),  
                                  upper = c(  4, 3, 3, 3, 1.5, 20,  5, 1,   5,   1.5),
                                  df = df_manipulation, model_type = model_type, experiment = experiment, n_trials = n_trials, cluster = cluster, control = list(itermax = itermax, parallelType = 'parallel', packages = c('Rcpp')))}
      
      # MODEL 4  -  collapsing confidence boundaries - unequal confidence urgencies, with confidence bias
      
      if (model_type == 4 & experiment == 1){
        sourceCpp('Code/C++_generators/DDM_confidence_bounds_binary.cpp')
        optimal_params <- DEoptim(fn = compute_chi2, 
                                  # a, v1, v2, v3, ter, a2, a2_slope_upper, a2_slope_lower, starting_point_confidence, vratio, ter2 
                                  lower = c(0.3, 0, 0, 0,   0,  0,  0,  0, 0, 0.2,  -1.5),  
                                  upper = c(  4, 3, 3, 3, 1.5, 20, 20, 20, 1,   5,   1.5),
                                  df = df_manipulation, model_type = model_type, experiment = experiment, n_trials = n_trials, cluster = cluster, control = list(itermax = itermax, parallelType = 'parallel', packages = c('Rcpp')))}
      
      # MODEL 5  -  2DSD
      
      if (model_type == 5 & experiment == 1){
        sourceCpp('Code/C++_generators/DDM_2DSD_binary.cpp')
        optimal_params <- DEoptim(fn = compute_chi2, 
                                  # a, v1, v2, v3, ter, ter2, tau, vratio, c1
                                  lower = c(0.3, 0, 0, 0,   0, -1.5, 0, 0.2,  -1.5),  
                                  upper = c(  4, 3, 3, 3, 1.5,  1.5, 5,   5,   1.5),
                                  df = df_manipulation, model_type = model_type, experiment = experiment, n_trials = n_trials, cluster = cluster, control = list(itermax = itermax, parallelType = 'parallel', packages = c('Rcpp')))}
      
      # MODEL 6  -  CCB
      
      if (model_type == 6 & experiment == 1){
        sourceCpp('Code/C++_generators/DDM_CCB_binary.cpp') 
        optimal_params <- DEoptim(fn = compute_chi2, 
                                  # a, v1, v2, v3, ter, ter2, vratio, absolute_height, collapse_timer
                                  lower = c(0.3, 0, 0, 0,   0, -1.5, 0.2,  0, 0),  
                                  upper = c(  4, 3, 3, 3, 1.5,  1.5,   5, 20, 4),
                                  df = df_manipulation, model_type = model_type, experiment = experiment, n_trials = n_trials, cluster = cluster, control = list(itermax = itermax, parallelType = 'parallel', packages = c('Rcpp')))}
      
      # MODEL 7  -  van Zandt
      
      if (model_type == 7 & experiment == 1){
        sourceCpp('Code/C++_generators/DDM_race_binary.cpp')
        optimal_params <- DEoptim(fn = compute_chi2, 
                                  # v_correct_1, v_correct_2, v_correct_3, v_error_1, v_error_2, v_error_3, a, a2, ter, ter2
                                  lower = c(0, 0, 0, 0, 0, 0, 0,  0, 0,  -1.5),  
                                  upper = c(3, 3, 3, 3, 3, 3, 5, 10, 1.5, 1.5),
                                  df = df_manipulation, model_type = model_type, experiment = experiment, n_trials = n_trials, cluster = cluster, control = list(itermax = itermax, parallelType = 'parallel', packages = c('Rcpp')))}

      # MODEL 8  -  2DSD with stopping criterium variation

      if (model_type == 8 & experiment == 1){
        sourceCpp('Code/C++_generators/DDM_2DSD_critVar_binary.cpp')
        optimal_params <- DEoptim(fn = compute_chi2, 
                                  # v_correct_1, v_correct_2, v_correct_3, v_error_1, v_error_2, v_error_3, a, a2, ter, ter2, a2_var
                                  lower = c(0, 0, 0, 0, 0, 0, 0,  0, 0,  -1.5, 0),  
                                  upper = c(3, 3, 3, 3, 3, 3, 5, 10, 1.5, 1.5, 3),
                                  df = df_manipulation, model_type = model_type, experiment = experiment, n_trials = n_trials, cluster = cluster, control = list(itermax = itermax, parallelType = 'parallel', packages = c('Rcpp')))}
      
      # MODEL 9  - optional stopping (2DSD)
      
      if (model_type == 9 & experiment == 1){
        sourceCpp('Code/C++_generators/DDM_confidence_bounds_binary.cpp')
        optimal_params <- DEoptim(fn = compute_chi2, 
                                  # a, v1, v2, v3, ter, a2, a2_slope, vratio, ter2 
                                  lower = c(0.3, 0, 0, 0,   0,  0,  0, 0.2,  -1.5),  
                                  upper = c(  4, 3, 3, 3, 1.5, 20,  0,   5,   1.5),
                                  df = df_manipulation, model_type = model_type, experiment = experiment, n_trials = n_trials, cluster = cluster, control = list(itermax = itermax, parallelType = 'parallel', packages = c('Rcpp')))}
      
      
      # EXPERIMENT 2 -----------------------------------------------------------
      
      
      # MODEL 1  -  collapsing confidence boundaries - equal confidence urgencies, no confidence bias
      
      if (model_type == 1 & experiment == 2){
        sourceCpp('Code/C++_generators/DDM_confidence_bounds_scale6.cpp')
        optimal_params <- DEoptim(fn = compute_chi2, 
                                  # a, v1, v2, v3, ter, a2, a2_slope, vratio, ter2 
                                  lower = c(0.3, 0, 0, 0,   0,  0,  0, 0.2,  -1.5),  
                                  upper = c(  4, 3, 3, 3, 1.5, 20, 20,   5,   1.5),
                                  df = df_manipulation, model_type = model_type, experiment = experiment, n_trials = n_trials, cluster = cluster, control = list(itermax = itermax, parallelType = 'parallel', packages = c('Rcpp')))}
      
      # MODEL 2  -  collapsing confidence boundaries - unequal confidence urgencies, no confidence bias
      
      if (model_type == 2 & experiment == 2){
        sourceCpp('Code/C++_generators/DDM_confidence_bounds_scale6.cpp')
        optimal_params <- DEoptim(fn = compute_chi2, 
                                  # a, v1, v2, v3, ter, a2, a2_slope_upper, a2_slope_lower, vratio, ter2 
                                  lower = c(0.3, 0, 0, 0,   0,  0,  0,  0, 0.2,  -1.5),  
                                  upper = c(  4, 3, 3, 3, 1.5, 20, 20, 20,   5,   1.5),
                                  df = df_manipulation, model_type = model_type, experiment = experiment, n_trials = n_trials, cluster = cluster, control = list(itermax = itermax, parallelType = 'parallel', packages = c('Rcpp')))}
      
      # MODEL 3  -  collapsing confidence boundaries - equal confidence urgencies, with confidence bias
      
      if (model_type == 3 & experiment == 2){
        sourceCpp('Code/C++_generators/DDM_confidence_bounds_scale6.cpp')
        optimal_params <- DEoptim(fn = compute_chi2, 
                                  # a, v1, v2, v3, ter, a2, a2_slope, starting_point_confidence, vratio, ter2 
                                  lower = c(0.3, 0, 0, 0,   0,  0,  0, 0, 0.2,  -1.5),  
                                  upper = c(  4, 3, 3, 3, 1.5, 20, 20, 1,   5,   1.5),
                                  df = df_manipulation, model_type = model_type, experiment = experiment, n_trials = n_trials, cluster = cluster, control = list(itermax = itermax, parallelType = 'parallel', packages = c('Rcpp')))}
      
      # MODEL 4  -  collapsing confidence boundaries - unequal confidence urgencies, with confidence bias
      
      if (model_type == 4 & experiment == 2){
        sourceCpp('Code/C++_generators/DDM_confidence_bounds_scale6.cpp')
        optimal_params <- DEoptim(fn = compute_chi2, 
                                  # a, v1, v2, v3, ter, a2, a2_slope_upper, a2_slope_lower, starting_point_confidence, vratio, ter2 
                                  lower = c(0.3, 0, 0, 0,   0,  0,  0,  0, 0, 0.2,  -1.5),  
                                  upper = c(  4, 3, 3, 3, 1.5, 20, 20, 20, 1,   5,   1.5),
                                  df = df_manipulation, model_type = model_type, experiment = experiment, n_trials = n_trials, cluster = cluster, control = list(itermax = itermax, parallelType = 'parallel', packages = c('Rcpp')))}
      
      # MODEL 5  -  2DSD
      
      if (model_type == 5 & experiment == 2){
        sourceCpp('Code/C++_generators/DDM_2DSD_scale6.cpp')
        optimal_params <- DEoptim(fn = compute_chi2, 
                                  # a, v1, v2, v3, ter, ter2, tau, vratio, c1, c2, c3, c4, c5 
                                  lower = c(0.3, 0, 0, 0,   0, -1.5, 0, 0.2,  -10, 0, 0, 0, 0),  
                                  upper = c(  4, 3, 3, 3, 1.5,  1.5, 5,   5,   10, 5, 5, 5, 5),
                                  df = df_manipulation, model_type = model_type, experiment = experiment, n_trials = n_trials, cluster = cluster, control = list(itermax = itermax, parallelType = 'parallel', packages = c('Rcpp')))}
      
      # MODEL 6  -  CCB
      
      if (model_type == 6 & experiment == 2){
        sourceCpp('Code/C++_generators/DDM_CCB_scale6.cpp')
        optimal_params <- DEoptim(fn = compute_chi2, 
                                  # a, v1, v2, v3, ter, a2, ter2, vratio, absolute_height, collapse_height, collapse_timer
                                  lower = c(0.3, 0, 0, 0,   0, -1.5, 0.2,  0,  0, 0),  
                                  upper = c(  4, 3, 3, 3, 1.5,  1.5,   5, 20, 20, 4),
                                  df = df_manipulation, model_type = model_type, experiment = experiment, n_trials = n_trials, cluster = cluster, control = list(itermax = itermax, parallelType = 'parallel', packages = c('Rcpp')))}
      
      # MODEL 7  -  van Zandt
      
      if (model_type == 7 & experiment == 2){
        sourceCpp('Code/C++_generators/DDM_race_scale6.cpp')
        optimal_params <- DEoptim(fn = compute_chi2, 
                                  # v_correct_1, v_correct_2, v_correct_3, v_error_1, v_error_2, v_error_3, a, a2, ter, ter2
                                  lower = c(0, 0, 0, 0, 0, 0, 0,  0, 0,  -1.5),  
                                  upper = c(3, 3, 3, 3, 3, 3, 5, 10, 1.5, 1.5),
                                  df = df_manipulation, model_type = model_type, experiment = experiment, n_trials = n_trials, cluster = cluster, control = list(itermax = itermax, parallelType = 'parallel', packages = c('Rcpp')))}

      # MODEL 8  -  2DSD + additional confidence criterium variation
      
      if (model_type == 8 & experiment == 2){
        sourceCpp('Code/C++_generators/DDM_2DSD_critVar_scale6.cpp')
        optimal_params <- DEoptim(fn = compute_chi2, 
                                  # a, v1, v2, v3, ter, ter2, tau, vratio, c1, c2, c3, c4, c5, a2_var 
                                  lower = c(0.3, 0, 0, 0,   0, -1.5, 0, 0.2,  -10, 0, 0, 0, 0, 0),  
                                  upper = c(  4, 3, 3, 3, 1.5,  1.5, 5,   5,   10, 5, 5, 5, 5, 3),
                                  df = df_manipulation, model_type = model_type, experiment = experiment, n_trials = n_trials, cluster = cluster, control = list(itermax = itermax, parallelType = 'parallel', packages = c('Rcpp')))}

      # Model 9 - optional stopping (2DSD)
      
      if (model_type == 9 & experiment == 2){
        sourceCpp('Code/C++_generators/DDM_2DSD_OptionalStopping_scale6.cpp')
        optimal_params <- DEoptim(fn = compute_chi2, 
                                  # a, v1, v2, v3, ter, ter2, vratio, cj1, cj2, cj3, cj4, cj5, cj6, cj2_p, cj3_p, cj4_p, cj5_p 
                                  lower = c(0.3, 0, 0, 0,   0, -1.5, 0.2, -10, 0, 0, 0, 0, 0, 0, 0, 0, 0),  
                                  upper = c(  4, 3, 3, 3, 1.5,  1.5,   5,   0, 10, 10, 10, 10, 10, 1, 1, 1, 1),
                                  df = df_manipulation, model_type = model_type, experiment = experiment, n_trials = n_trials, cluster = cluster, control = list(itermax = itermax, parallelType = 'parallel', packages = c('Rcpp')))}
      
      results <- summary(optimal_params)
      save(results, file = file_name)
      
    }
    
  }
  
  print('Parameter optimization finished.')
  
}


# Compute parameter set cost ---------------------------------------------------


compute_chi2 <- function(params, df, model_type, experiment, cluster = FALSE, n_trials){
  
  library(Rcpp)
  
  # Initialize variables
  chi2 <- 0

  # Name parameters
  param_names <- switch(model_type,
                        '1' = c('a', 'v1', 'v2', 'v3', 'ter', 'a2', 'a2_slope', 'vratio', 'ter2'),
                        '2' = c('a', 'v1', 'v2', 'v3', 'ter', 'a2', 'a2_slope_upper', 'a2_slope_lower', 'vratio', 'ter2'),
                        '3' = c('a', 'v1', 'v2', 'v3', 'ter', 'a2', 'a2_slope', 'starting_point_confidence', 'vratio', 'ter2'),
                        '4' = c('a', 'v1', 'v2', 'v3', 'ter', 'a2', 'a2_slope_upper', 'a2_slope_lower', 'starting_point_confidence', 'vratio', 'ter2'),
                        '5' = {
                          if (experiment == 1) {
                            c('a', 'v1', 'v2', 'v3', 'ter', 'ter2', 'tau', 'vratio', 'c1')
                          } else if (experiment == 2){
                            c('a', 'v1', 'v2', 'v3', 'ter', 'ter2', 'tau', 'vratio', 'c1', 'c2', 'c3', 'c4', 'c5')}},
                        '6' = {
                          if (experiment == 1) {
                            c('a', 'v1', 'v2', 'v3', 'ter', 'ter2', 'vratio', 'absolute_height', 'collapse_timer')
                          } else if (experiment == 2){
                            c('a', 'v1', 'v2', 'v3', 'ter', 'ter2', 'vratio', 'absolute_height', 'collapse_height', 'collapse_timer')}},
                        '7' = c('v_correct_1', 'v_correct_2', 'v_correct_3', 'v_error_1', 'v_error_2', 'v_error_3', 'a', 'a2', 'ter', 'ter2'),
                        '8' = {
                          if (experiment == 1) {
                            c('a', 'v1', 'v2', 'v3', 'ter', 'ter2', 'tau', 'vratio', 'c1', 'a2_var')
                          } else if (experiment == 2){
                            c('a', 'v1', 'v2', 'v3', 'ter', 'ter2', 'tau', 'vratio', 'c1', 'c2', 'c3', 'c4', 'c5', 'a2_var')}},
                        '9' = {
                          if (experiment == 1) {
                            c('a', 'v1', 'v2', 'v3', 'ter', 'a2', 'a2_slope', 'vratio', 'ter2')
                          } else if (experiment == 2){
                            c('a', 'v1', 'v2', 'v3', 'ter', 'ter2', 'vratio', 'cj1', 'cj2', 'cj3', 'cj4', 'cj5', 'cj6', 'cj2_p', 'cj3_p', 'cj4_p', 'cj5_p')}}
  )
  
  # Check the value of param_names
  param_names
  names(params) <- param_names
  
  # Load c++ script on each node
  if (cluster == TRUE){
    if (experiment == 1){
      switch(model_type,
             '1' = sourceCpp('Code/C++_generators/DDM_confidence_bounds_binary.cpp'),
             '2' = sourceCpp('Code/C++_generators/DDM_confidence_bounds_binary.cpp'),
             '3' = sourceCpp('Code/C++_generators/DDM_confidence_bounds_binary.cpp'),
             '4' = sourceCpp('Code/C++_generators/DDM_confidence_bounds_binary.cpp'),
             '5' = sourceCpp('Code/C++_generators/DDM_2DSD_binary.cpp'),
             '6' = sourceCpp('Code/C++_generators/DDM_CCB_binary.cpp'),
             '7' = sourceCpp('Code/C++_generators/DDM_race_binary.cpp'),
             '8' = sourceCpp('Code/C++_generators/DDM_2DSD_critVar_binary.cpp'),
             '1' = sourceCpp('Code/C++_generators/DDM_confidence_bounds_binary.cpp')
      )
    } else if (experiment == 2){
      switch(model_type,
             '1' = sourceCpp('Code/C++_generators/DDM_confidence_bounds_scale6.cpp'),
             '2' = sourceCpp('Code/C++_generators/DDM_confidence_bounds_scale6.cpp'),
             '3' = sourceCpp('Code/C++_generators/DDM_confidence_bounds_scale6.cpp'),
             '4' = sourceCpp('Code/C++_generators/DDM_confidence_bounds_scale6.cpp'),
             '5' = sourceCpp('Code/C++_generators/DDM_2DSD_scale6.cpp'),
             '6' = sourceCpp('Code/C++_generators/DDM_CCB_scale6.cpp'),
             '7' = sourceCpp('Code/C++_generators/DDM_race_scale6.cpp'),
             '8' = sourceCpp('Code/C++_generators/DDM_2DSD_critVar_scale6.cpp'),
             '9' = sourceCpp('Code/C++_generators/DDM_2DSD_OptionalStopping_scale6.cpp'))
    }
  }
  
  # Loop through each level of confidence
  for (coherence_level in 1:3){
    
    df_obs <- df[df$coherence == c(0.1, 0.2, 0.4)[coherence_level],]
    if (model_type == 7){
      params['v_correct'] <- params[c('v_correct_1', 'v_correct_2', 'v_correct_3')[coherence_level]]
      params['v_error'] <- params[c('v_error_1', 'v_error_2', 'v_error_3')[coherence_level]]
    } else {
      params['v'] <- params[c('v1', 'v2', 'v3')[coherence_level]]
    }    
    
    
    # Make predictions  --------------------------------------------------------

    
    if (experiment == 1){
      df_pred <- switch(model_type, 
                            '1' = data.frame(DDM_confidence_bounds_binary(v = params['v'], a = params['a'], a_slope = 0, ter = params['ter'], n_trials = n_trials, a2 = params['a2'], vratio = params['vratio'], a2_slope_upper = params['a2_slope'],       a2_slope_lower = params['a2_slope'],       ter2 = params['ter2'], starting_point_confidence = 0.5)),
                            '2' = data.frame(DDM_confidence_bounds_binary(v = params['v'], a = params['a'], a_slope = 0, ter = params['ter'], n_trials = n_trials, a2 = params['a2'], vratio = params['vratio'], a2_slope_upper = params['a2_slope_upper'], a2_slope_lower = params['a2_slope_lower'], ter2 = params['ter2'], starting_point_confidence = 0.5)),
                            '3' = data.frame(DDM_confidence_bounds_binary(v = params['v'], a = params['a'], a_slope = 0, ter = params['ter'], n_trials = n_trials, a2 = params['a2'], vratio = params['vratio'], a2_slope_upper = params['a2_slope'],       a2_slope_lower = params['a2_slope'],       ter2 = params['ter2'], starting_point_confidence = params['starting_point_confidence'])),
                            '4' = data.frame(DDM_confidence_bounds_binary(v = params['v'], a = params['a'], a_slope = 0, ter = params['ter'], n_trials = n_trials, a2 = params['a2'], vratio = params['vratio'], a2_slope_upper = params['a2_slope_upper'], a2_slope_lower = params['a2_slope_lower'], ter2 = params['ter2'], starting_point_confidence = params['starting_point_confidence'])),
                            '5' = data.frame(DDM_2DSD_binary(v = params['v'], a = params['a'], a_slope = 0, ter = params['ter'], n_trials = n_trials, ter2 = params['ter2'], tau = params['tau'], vratio = params['vratio'], eta = 0, sz = 0, c1 = params['c1'])),
                            '6' = data.frame(DDM_CCB_binary(v = params['v'], a = params['a'], a_slope = 0, ter = params['ter'], n_trials = n_trials, vratio = params['vratio'], ter2 = params['ter2'], absolute_height = params['absolute_height'], collapse_timer = params['collapse_timer'])),
                            '7' = data.frame(DDM_race_binary(v_correct = params['v_correct'], v_error = params['v_error'], a = params['a'], ter = params['ter'], n_trials = n_trials, a2 = params['a2'], ter2 = params['ter2'])),
                            '8' = data.frame(DDM_2DSD_critVar_binary(v = params['v'], a = params['a'], a_slope = 0, ter = params['ter'], n_trials = n_trials, ter2 = params['ter2'], tau = params['tau'], vratio = params['vratio'], eta = 0, sz = 0, c1 = params['c1'], a2_var = params['a2_var'])),
                            '9' = data.frame(DDM_confidence_bounds_binary(v = params['v'], a = params['a'], a_slope = 0, ter = params['ter'], n_trials = n_trials, a2 = params['a2'], vratio = params['vratio'], a2_slope_upper = params['a2_slope'],       a2_slope_lower = params['a2_slope'],       ter2 = params['ter2'], starting_point_confidence = 0.5)))
    } else {
      df_pred <- switch(model_type, 
                            '1' = data.frame(DDM_confidence_bounds_scale6(v = params['v'], a = params['a'], a_slope = 0, ter = params['ter'], n_trials = n_trials, a2 = params['a2'], vratio = params['vratio'], a2_slope_upper = params['a2_slope'],       a2_slope_lower = params['a2_slope'],       ter2 = params['ter2'], starting_point_confidence = 0.5)),
                            '2' = data.frame(DDM_confidence_bounds_scale6(v = params['v'], a = params['a'], a_slope = 0, ter = params['ter'], n_trials = n_trials, a2 = params['a2'], vratio = params['vratio'], a2_slope_upper = params['a2_slope_upper'], a2_slope_lower = params['a2_slope_lower'], ter2 = params['ter2'], starting_point_confidence = 0.5)),
                            '3' = data.frame(DDM_confidence_bounds_scale6(v = params['v'], a = params['a'], a_slope = 0, ter = params['ter'], n_trials = n_trials, a2 = params['a2'], vratio = params['vratio'], a2_slope_upper = params['a2_slope'],       a2_slope_lower = params['a2_slope'],       ter2 = params['ter2'], starting_point_confidence = params['starting_point_confidence'])),
                            '4' = data.frame(DDM_confidence_bounds_scale6(v = params['v'], a = params['a'], a_slope = 0, ter = params['ter'], n_trials = n_trials, a2 = params['a2'], vratio = params['vratio'], a2_slope_upper = params['a2_slope_upper'], a2_slope_lower = params['a2_slope_lower'], ter2 = params['ter2'], starting_point_confidence = params['starting_point_confidence'])),
                            '5' = data.frame(DDM_2DSD_scale6(v = params['v'], a = params['a'], a_slope = 0, ter = params['ter'], n_trials = n_trials, ter2 = params['ter2'], tau = params['tau'], vratio = params['vratio'], eta = 0, sz = 0, c1 = params['c1'], c2 = params['c2'], c3 = params['c3'], c4 = params['c4'], c5 = params['c5'])),
                            '6' = data.frame(DDM_CCB_scale6(v = params['v'], a = params['a'], a_slope = 0, ter = params['ter'], n_trials = n_trials, vratio = params['vratio'], ter2 = params['ter2'], absolute_height = params['absolute_height'], collapse_timer = params['collapse_timer'], collapse_height = params['collapse_height'])),
                            '7' = data.frame(DDM_race_scale6(v_correct = params['v_correct'], v_error = params['v_error'], a = params['a'], ter = params['ter'], n_trials = n_trials, a2 = params['a2'], ter2 = params['ter2'])),
                            '8' = data.frame(DDM_2DSD_critVar_scale6(v = params['v'], a = params['a'], a_slope = 0, ter = params['ter'], n_trials = n_trials, ter2 = params['ter2'], tau = params['tau'], vratio = params['vratio'], eta = 0, sz = 0, c1 = params['c1'], c2 = params['c2'], c3 = params['c3'], c4 = params['c4'], c5 = params['c5'], a2_var = params['a2_var'])),
                            '9' = data.frame(DDM_2DSD_OptionalStopping_scale6(v = params['v'], a = params['a'], a_slope = 0, ter = params['ter'], ter2 = params['ter2'], n_trials = n_trials, vratio = params['vratio'], cj1 = params['cj1'], cj2 = params['cj2'], cj3 = params['cj3'], cj4 = params['cj4'], cj5 = params['cj5'], cj6 = params['cj6'], cj2_p = params['cj2_p'], cj3_p = params['cj3_p'], cj4_p = params['cj4_p'], cj5_p = params['cj5_p'])))
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

  return(chi2)
   
}


# Combine Rdata parameter sets to one file -------------------------------------


group_params <- function(model_type, experiment, param_recovery = FALSE, recovery_version = 0, model_recovery = FALSE, model_type_predictions = 0){

  params <- NULL
  
  if (param_recovery == FALSE){

    for (subject in 1:40){
        
      for (manipulation in c('AccAcc', 'AccFast', 'FastFast', 'FastAcc')){
        
        if (model_recovery == FALSE){
          file_name <- paste0('Data/Experiment_', experiment,'/Model_parameters/exp', experiment, '_model', model_type, '_sub', subject, '_manipulation', manipulation, '.Rdata')
        } else {
          file_name <- paste0('Data/Experiment_', experiment,'/Model_parameters/exp', experiment, '_sub', subject, '_manipulation', manipulation, '_ModelRecovery_predM', model_type_predictions, '_fitM', model_type, '.Rdata')
        }
        load(file_name)  
        
        # Add parameter set
        if (model_type == 1){
          params_set <- c(subject, manipulation, results$optim$bestmem[1], results$optim$bestmem[2], results$optim$bestmem[3], results$optim$bestmem[4], results$optim$bestmem[5], results$optim$bestmem[6], results$optim$bestmem[7], results$optim$bestmem[8], results$optim$bestmem[9])
          params <- rbind(params, params_set)
          colnames(params) <- c('sub', 'manipulation', 'a', 'v1', 'v2', 'v3', 'ter', 'a2', 'a2_slope', 'vratio', 'ter2')
        } else if (model_type == 2){
          params_set <- c(subject, manipulation, results$optim$bestmem[1], results$optim$bestmem[2], results$optim$bestmem[3], results$optim$bestmem[4], results$optim$bestmem[5], results$optim$bestmem[6], results$optim$bestmem[7], results$optim$bestmem[8], results$optim$bestmem[9], results$optim$bestmem[10])
          params <- rbind(params, params_set)
          colnames(params) <- c('sub', 'manipulation', 'a', 'v1', 'v2', 'v3', 'ter', 'a2', 'a2_slope_upper', 'a2_slope_lower', 'vratio', 'ter2')
        } else if (model_type == 3){
          params_set <- c(subject, manipulation, results$optim$bestmem[1], results$optim$bestmem[2], results$optim$bestmem[3], results$optim$bestmem[4], results$optim$bestmem[5], results$optim$bestmem[6], results$optim$bestmem[7], results$optim$bestmem[8], results$optim$bestmem[9], results$optim$bestmem[10])
          params <- rbind(params, params_set)
          colnames(params) <- c('sub', 'manipulation', 'a', 'v1', 'v2', 'v3', 'ter', 'a2', 'a2_slope', 'starting_point_confidence', 'vratio', 'ter2')
        } else if (model_type == 4){
          params_set <- c(subject, manipulation, results$optim$bestmem[1], results$optim$bestmem[2], results$optim$bestmem[3], results$optim$bestmem[4], results$optim$bestmem[5], results$optim$bestmem[6], results$optim$bestmem[7], results$optim$bestmem[8], results$optim$bestmem[9], results$optim$bestmem[10], results$optim$bestmem[11])
          params <- rbind(params, params_set)
          colnames(params) <- c('sub', 'manipulation', 'a', 'v1', 'v2', 'v3', 'ter', 'a2', 'a2_slope_upper', 'a2_slope_lower', 'starting_point_confidence', 'vratio', 'ter2')
        } else if (model_type == 5){
          if (experiment == 1){
            params_set <- c(subject, manipulation, results$optim$bestmem[1], results$optim$bestmem[2], results$optim$bestmem[3], results$optim$bestmem[4], results$optim$bestmem[5], results$optim$bestmem[6], results$optim$bestmem[7], results$optim$bestmem[8], results$optim$bestmem[9])
            params <- rbind(params, params_set)
            colnames(params) <- c('sub', 'manipulation', 'a', 'v1', 'v2', 'v3', 'ter', 'ter2', 'tau', 'vratio', 'c1')
          } else if (experiment == 2){
            params_set <- c(subject, manipulation, results$optim$bestmem[1], results$optim$bestmem[2], results$optim$bestmem[3], results$optim$bestmem[4], results$optim$bestmem[5], results$optim$bestmem[6], results$optim$bestmem[7], results$optim$bestmem[8], results$optim$bestmem[9], results$optim$bestmem[10], results$optim$bestmem[11], results$optim$bestmem[12], results$optim$bestmem[13])
            params <- rbind(params, params_set)
            colnames(params) <- c('sub', 'manipulation', 'a', 'v1', 'v2', 'v3', 'ter', 'ter2', 'tau', 'vratio', 'c1', 'c2', 'c3', 'c4', 'c5')}
        } else if (model_type == 6){
          if (experiment == 1){
            params_set <- c(subject, manipulation, results$optim$bestmem[1], results$optim$bestmem[2], results$optim$bestmem[3], results$optim$bestmem[4], results$optim$bestmem[5], results$optim$bestmem[6], results$optim$bestmem[7], results$optim$bestmem[8], results$optim$bestmem[9])
            params <- rbind(params, params_set)
            colnames(params) <- c('sub', 'manipulation', 'a', 'v1', 'v2', 'v3', 'ter', 'ter2', 'vratio', 'absolute_height', 'collapse_timer')
          } else if (experiment == 2){
            params_set <- c(subject, manipulation, results$optim$bestmem[1], results$optim$bestmem[2], results$optim$bestmem[3], results$optim$bestmem[4], results$optim$bestmem[5], results$optim$bestmem[6], results$optim$bestmem[7], results$optim$bestmem[8], results$optim$bestmem[9], results$optim$bestmem[10])
            params <- rbind(params, params_set)
            colnames(params) <- c('sub', 'manipulation', 'a', 'v1', 'v2', 'v3', 'ter', 'ter2', 'vratio', 'absolute_height', 'collapse_height', 'collapse_timer')}
        } else if (model_type == 7){
          params_set <- c(subject, manipulation, results$optim$bestmem[1], results$optim$bestmem[2], results$optim$bestmem[3], results$optim$bestmem[4], results$optim$bestmem[5], results$optim$bestmem[6], results$optim$bestmem[7], results$optim$bestmem[8], results$optim$bestmem[9], results$optim$bestmem[10])
          params <- rbind(params, params_set)
          colnames(params) <- c('sub', 'manipulation', 'v_correct_1', 'v_correct_2', 'v_correct_3', 'v_error_1', 'v_error_2', 'v_error_3', 'a', 'a2', 'ter', 'ter2')
        } else if (model_type == 8){
          if (experiment == 1){
            params_set <- c(subject, manipulation, results$optim$bestmem[1], results$optim$bestmem[2], results$optim$bestmem[3], results$optim$bestmem[4], results$optim$bestmem[5], results$optim$bestmem[6], results$optim$bestmem[7], results$optim$bestmem[8], results$optim$bestmem[9], results$optim$bestmem[10])
            params <- rbind(params, params_set)
            colnames(params) <- c('sub', 'manipulation', 'a', 'v1', 'v2', 'v3', 'ter', 'ter2', 'tau', 'vratio', 'c1', 'a2_var')
          } else if (experiment == 2){
            params_set <- c(subject, manipulation, results$optim$bestmem[1], results$optim$bestmem[2], results$optim$bestmem[3], results$optim$bestmem[4], results$optim$bestmem[5], results$optim$bestmem[6], results$optim$bestmem[7], results$optim$bestmem[8], results$optim$bestmem[9], results$optim$bestmem[10], results$optim$bestmem[11], results$optim$bestmem[12], results$optim$bestmem[13], results$optim$bestmem[14])
            params <- rbind(params, params_set)
            colnames(params) <- c('sub', 'manipulation', 'a', 'v1', 'v2', 'v3', 'ter', 'ter2', 'tau', 'vratio', 'c1', 'c2', 'c3', 'c4', 'c5', 'a2_var')}
        } else if (model_type == 9){
          if (experiment == 1){
            params_set <- c(subject, manipulation, results$optim$bestmem[1], results$optim$bestmem[2], results$optim$bestmem[3], results$optim$bestmem[4], results$optim$bestmem[5], results$optim$bestmem[6], results$optim$bestmem[7], results$optim$bestmem[8], results$optim$bestmem[9])
            params <- rbind(params, params_set)
            colnames(params) <- c('sub', 'manipulation', 'a', 'v1', 'v2', 'v3', 'ter', 'a2', 'a2_slope', 'vratio', 'ter2')
          } else if (experiment == 2){
            params_set <- c(subject, manipulation, results$optim$bestmem[1], results$optim$bestmem[2], results$optim$bestmem[3], results$optim$bestmem[4], results$optim$bestmem[5], results$optim$bestmem[6], results$optim$bestmem[7], results$optim$bestmem[8], results$optim$bestmem[9], results$optim$bestmem[10], results$optim$bestmem[11], results$optim$bestmem[12], results$optim$bestmem[13], results$optim$bestmem[14], results$optim$bestmem[15], results$optim$bestmem[16], results$optim$bestmem[17])
            params <- rbind(params, params_set)
            colnames(params) <- c('sub', 'manipulation', 'a', 'v1', 'v2', 'v3', 'ter', 'ter2', 'vratio', 'cj1', 'cj2', 'cj3', 'cj4', 'cj5', 'cj6', 'cj2_p', 'cj3_p', 'cj4_p', 'cj5_p')
          }
        }
      }
    }
    
    file_name <- paste0('Data/Experiment_', experiment,'/Model_parameters/exp', experiment, '_model', model_type, '_parameters.csv')
    if (model_recovery == TRUE){
      file_name <- paste0('Data/Experiment_', experiment,'/Model_parameters/exp', experiment, '_ModelRecovery_predM', model_type_predictions, '_fitM', model_type, '_parameters.csv')
    }
    write.csv(params, file = file_name, row.names = FALSE)
    
  } else if (param_recovery == TRUE){

    for (subject in 1:100){

      file_name <- paste0('Data/Experiment_', experiment,'/Model_parameters/exp', experiment, '_model', model_type, '_sub', subject, '_manipulationrecovery_recovery', recovery_version, '.Rdata')
      load(file_name)
      
      # Add parameter set
      if (experiment == 1){
        params_set <- c(subject, 'recovery', results$optim$bestmem[1], results$optim$bestmem[2], results$optim$bestmem[3], results$optim$bestmem[4], results$optim$bestmem[5], results$optim$bestmem[6], results$optim$bestmem[7], results$optim$bestmem[8], results$optim$bestmem[9])
        params <- rbind(params, params_set)
        colnames(params) <- c('sub', 'manipulation', 'a', 'v1', 'v2', 'v3', 'ter', 'a2', 'a2_slope', 'vratio', 'ter2')
      } else if (experiment == 2){
        params_set <- c(subject, 'recovery', results$optim$bestmem[1], results$optim$bestmem[2], results$optim$bestmem[3], results$optim$bestmem[4], results$optim$bestmem[5], results$optim$bestmem[6], results$optim$bestmem[7], results$optim$bestmem[8], results$optim$bestmem[9], results$optim$bestmem[10], results$optim$bestmem[11])
        params <- rbind(params, params_set)
        colnames(params) <- c('sub', 'manipulation', 'a', 'v1', 'v2', 'v3', 'ter', 'a2', 'a2_slope_upper', 'a2_slope_lower', 'starting_point_confidence', 'vratio', 'ter2')
      }
    }
    
    file_name <- paste0('Data/Experiment_', experiment,'/Model_parameters/exp', experiment, '_model', model_type, '_parameters_afterRecovery_', recovery_version, '.csv')
    write.csv(params, file = file_name, row.names = FALSE)
    
  }
}


# Make model predictions -------------------------------------------------------


make_predictions <- function(model_type, experiment, params, n_trials){
  
  if (!is.data.frame(params)){return('Params should be a data.frame()')}
    
  if (experiment == 1){
    
    if (model_type == 1){
      sourceCpp('Code/C++_generators/DDM_confidence_bounds_binary.cpp')
      predictions1 <- data.frame(coherence = 0.1, DDM_confidence_bounds_binary(v = params$v1, a = params$a, a_slope = 0, ter = params$ter, n_trials = n_trials, a2 = params$a2, vratio = params$vratio, a2_slope_upper = params$a2_slope, a2_slope_lower = params$a2_slope, ter2 = params$ter2, starting_point_confidence = 0.5))
      predictions2 <- data.frame(coherence = 0.2, DDM_confidence_bounds_binary(v = params$v2, a = params$a, a_slope = 0, ter = params$ter, n_trials = n_trials, a2 = params$a2, vratio = params$vratio, a2_slope_upper = params$a2_slope, a2_slope_lower = params$a2_slope, ter2 = params$ter2, starting_point_confidence = 0.5))
      predictions3 <- data.frame(coherence = 0.4, DDM_confidence_bounds_binary(v = params$v3, a = params$a, a_slope = 0, ter = params$ter, n_trials = n_trials, a2 = params$a2, vratio = params$vratio, a2_slope_upper = params$a2_slope, a2_slope_lower = params$a2_slope, ter2 = params$ter2, starting_point_confidence = 0.5))
    } else if (model_type == 2){
      sourceCpp('Code/C++_generators/DDM_confidence_bounds_binary.cpp')
      predictions1 <- data.frame(coherence = 0.1, DDM_confidence_bounds_binary(v = params$v1, a = params$a, a_slope = 0, ter = params$ter, n_trials = n_trials, a2 = params$a2, vratio = params$vratio, a2_slope_upper = params$a2_slope_upper, a2_slope_lower = params$a2_slope_lower, ter2 = params$ter2, starting_point_confidence = 0.5))
      predictions2 <- data.frame(coherence = 0.2, DDM_confidence_bounds_binary(v = params$v2, a = params$a, a_slope = 0, ter = params$ter, n_trials = n_trials, a2 = params$a2, vratio = params$vratio, a2_slope_upper = params$a2_slope_upper, a2_slope_lower = params$a2_slope_lower, ter2 = params$ter2, starting_point_confidence = 0.5))
      predictions3 <- data.frame(coherence = 0.4, DDM_confidence_bounds_binary(v = params$v3, a = params$a, a_slope = 0, ter = params$ter, n_trials = n_trials, a2 = params$a2, vratio = params$vratio, a2_slope_upper = params$a2_slope_upper, a2_slope_lower = params$a2_slope_lower, ter2 = params$ter2, starting_point_confidence = 0.5))
    } else if (model_type == 3){
      sourceCpp('Code/C++_generators/DDM_confidence_bounds_binary.cpp')
      predictions1 <- data.frame(coherence = 0.1, DDM_confidence_bounds_binary(v = params$v1, a = params$a, a_slope = 0, ter = params$ter, n_trials = n_trials, a2 = params$a2, vratio = params$vratio, a2_slope_upper = params$a2_slope, a2_slope_lower = params$a2_slope, ter2 = params$ter2, starting_point_confidence = params$starting_point_confidence))
      predictions2 <- data.frame(coherence = 0.2, DDM_confidence_bounds_binary(v = params$v2, a = params$a, a_slope = 0, ter = params$ter, n_trials = n_trials, a2 = params$a2, vratio = params$vratio, a2_slope_upper = params$a2_slope, a2_slope_lower = params$a2_slope, ter2 = params$ter2, starting_point_confidence = params$starting_point_confidence))
      predictions3 <- data.frame(coherence = 0.4, DDM_confidence_bounds_binary(v = params$v3, a = params$a, a_slope = 0, ter = params$ter, n_trials = n_trials, a2 = params$a2, vratio = params$vratio, a2_slope_upper = params$a2_slope, a2_slope_lower = params$a2_slope, ter2 = params$ter2, starting_point_confidence = params$starting_point_confidence))
    } else if (model_type == 4){
      sourceCpp('Code/C++_generators/DDM_confidence_bounds_binary.cpp')
      predictions1 <- data.frame(coherence = 0.1, DDM_confidence_bounds_binary(v = params$v1, a = params$a, a_slope = 0, ter = params$ter, n_trials = n_trials, a2 = params$a2, vratio = params$vratio, a2_slope_upper = params$a2_slope_upper, a2_slope_lower = params$a2_slope_lower, ter2 = params$ter2, starting_point_confidence = params$starting_point_confidence))
      predictions2 <- data.frame(coherence = 0.2, DDM_confidence_bounds_binary(v = params$v2, a = params$a, a_slope = 0, ter = params$ter, n_trials = n_trials, a2 = params$a2, vratio = params$vratio, a2_slope_upper = params$a2_slope_upper, a2_slope_lower = params$a2_slope_lower, ter2 = params$ter2, starting_point_confidence = params$starting_point_confidence))
      predictions3 <- data.frame(coherence = 0.4, DDM_confidence_bounds_binary(v = params$v3, a = params$a, a_slope = 0, ter = params$ter, n_trials = n_trials, a2 = params$a2, vratio = params$vratio, a2_slope_upper = params$a2_slope_upper, a2_slope_lower = params$a2_slope_lower, ter2 = params$ter2, starting_point_confidence = params$starting_point_confidence))
    } else if (model_type == 5){
      sourceCpp('Code/C++_generators/DDM_2DSD_binary.cpp')
      predictions1 <- data.frame(coherence = 0.1, DDM_2DSD_binary(v = params$v1, a = params$a, a_slope = 0, ter = params$ter, n_trials = n_trials, ter2 = params$ter2, tau = params$tau, vratio = params$vratio, eta = 0, sz = 0, c1 = params$c1))
      predictions2 <- data.frame(coherence = 0.2, DDM_2DSD_binary(v = params$v2, a = params$a, a_slope = 0, ter = params$ter, n_trials = n_trials, ter2 = params$ter2, tau = params$tau, vratio = params$vratio, eta = 0, sz = 0, c1 = params$c1))
      predictions3 <- data.frame(coherence = 0.4, DDM_2DSD_binary(v = params$v3, a = params$a, a_slope = 0, ter = params$ter, n_trials = n_trials, ter2 = params$ter2, tau = params$tau, vratio = params$vratio, eta = 0, sz = 0, c1 = params$c1))
    } else if (model_type == 6){
      sourceCpp('Code/C++_generators/DDM_CCB_binary.cpp')
      predictions1 <- data.frame(coherence = 0.1, DDM_CCB_binary(v = params$v1, a = params$a, a_slope = 0, ter = params$ter, n_trials = n_trials, vratio = params$vratio, ter2 = params$ter2, absolute_height = params$absolute_height, collapse_timer = params$collapse_timer))
      predictions2 <- data.frame(coherence = 0.2, DDM_CCB_binary(v = params$v2, a = params$a, a_slope = 0, ter = params$ter, n_trials = n_trials, vratio = params$vratio, ter2 = params$ter2, absolute_height = params$absolute_height, collapse_timer = params$collapse_timer))
      predictions3 <- data.frame(coherence = 0.4, DDM_CCB_binary(v = params$v3, a = params$a, a_slope = 0, ter = params$ter, n_trials = n_trials, vratio = params$vratio, ter2 = params$ter2, absolute_height = params$absolute_height, collapse_timer = params$collapse_timer))
    } else if (model_type == 7){
      sourceCpp('Code/C++_generators/DDM_race_binary.cpp')
      predictions1 <- data.frame(coherence = 0.1, DDM_race_binary(v_correct = params$v_correct_1, v_error = params$v_error_1, a = params$a, ter = params$ter, n_trials = n_trials, a2 = params$a2, ter2 = params$ter2))
      predictions2 <- data.frame(coherence = 0.2, DDM_race_binary(v_correct = params$v_correct_2, v_error = params$v_error_2, a = params$a, ter = params$ter, n_trials = n_trials, a2 = params$a2, ter2 = params$ter2))
      predictions3 <- data.frame(coherence = 0.4, DDM_race_binary(v_correct = params$v_correct_3, v_error = params$v_error_3, a = params$a, ter = params$ter, n_trials = n_trials, a2 = params$a2, ter2 = params$ter2))
    }  else if (model_type == 8){
      sourceCpp('Code/C++_generators/DDM_2DSD_critVar_binary.cpp')
      predictions1 <- data.frame(coherence = 0.1, DDM_2DSD_critVar_binary(v = params$v1, a = params$a, a_slope = 0, ter = params$ter, n_trials = n_trials, ter2 = params$ter2, tau = params$tau, vratio = params$vratio, eta = 0, sz = 0, c1 = params$c1, a2_var = params$a2_var))
      predictions2 <- data.frame(coherence = 0.2, DDM_2DSD_critVar_binary(v = params$v2, a = params$a, a_slope = 0, ter = params$ter, n_trials = n_trials, ter2 = params$ter2, tau = params$tau, vratio = params$vratio, eta = 0, sz = 0, c1 = params$c1, a2_var = params$a2_var))
      predictions3 <- data.frame(coherence = 0.4, DDM_2DSD_critVar_binary(v = params$v3, a = params$a, a_slope = 0, ter = params$ter, n_trials = n_trials, ter2 = params$ter2, tau = params$tau, vratio = params$vratio, eta = 0, sz = 0, c1 = params$c1, a2_var = params$a2_var))
    }  else if (model_type == 9){
      sourceCpp('Code/C++_generators/DDM_confidence_bounds_binary.cpp')
      predictions1 <- data.frame(coherence = 0.1, DDM_confidence_bounds_binary(v = params$v1, a = params$a, a_slope = 0, ter = params$ter, n_trials = n_trials, a2 = params$a2, vratio = params$vratio, a2_slope_upper = params$a2_slope, a2_slope_lower = params$a2_slope, ter2 = params$ter2, starting_point_confidence = 0.5))
      predictions2 <- data.frame(coherence = 0.2, DDM_confidence_bounds_binary(v = params$v2, a = params$a, a_slope = 0, ter = params$ter, n_trials = n_trials, a2 = params$a2, vratio = params$vratio, a2_slope_upper = params$a2_slope, a2_slope_lower = params$a2_slope, ter2 = params$ter2, starting_point_confidence = 0.5))
      predictions3 <- data.frame(coherence = 0.4, DDM_confidence_bounds_binary(v = params$v3, a = params$a, a_slope = 0, ter = params$ter, n_trials = n_trials, a2 = params$a2, vratio = params$vratio, a2_slope_upper = params$a2_slope, a2_slope_lower = params$a2_slope, ter2 = params$ter2, starting_point_confidence = 0.5))
    }
    
  } else if (experiment == 2){
    
    if (model_type == 1){
      sourceCpp('Code/C++_generators/DDM_confidence_bounds_scale6.cpp')
      predictions1 <- data.frame(coherence = 0.1, DDM_confidence_bounds_scale6(v = params$v1, a = params$a, a_slope = 0, ter = params$ter, n_trials = n_trials, a2 = params$a2, vratio = params$vratio, a2_slope_upper = params$a2_slope, a2_slope_lower = params$a2_slope, ter2 = params$ter2, starting_point_confidence = 0.5))
      predictions2 <- data.frame(coherence = 0.2, DDM_confidence_bounds_scale6(v = params$v2, a = params$a, a_slope = 0, ter = params$ter, n_trials = n_trials, a2 = params$a2, vratio = params$vratio, a2_slope_upper = params$a2_slope, a2_slope_lower = params$a2_slope, ter2 = params$ter2, starting_point_confidence = 0.5))
      predictions3 <- data.frame(coherence = 0.4, DDM_confidence_bounds_scale6(v = params$v3, a = params$a, a_slope = 0, ter = params$ter, n_trials = n_trials, a2 = params$a2, vratio = params$vratio, a2_slope_upper = params$a2_slope, a2_slope_lower = params$a2_slope, ter2 = params$ter2, starting_point_confidence = 0.5))
    } else if (model_type == 2){
      sourceCpp('Code/C++_generators/DDM_confidence_bounds_scale6.cpp')
      predictions1 <- data.frame(coherence = 0.1, DDM_confidence_bounds_scale6(v = params$v1, a = params$a, a_slope = 0, ter = params$ter, n_trials = n_trials, a2 = params$a2, vratio = params$vratio, a2_slope_upper = params$a2_slope_upper, a2_slope_lower = params$a2_slope_lower, ter2 = params$ter2, starting_point_confidence = 0.5))
      predictions2 <- data.frame(coherence = 0.2, DDM_confidence_bounds_scale6(v = params$v2, a = params$a, a_slope = 0, ter = params$ter, n_trials = n_trials, a2 = params$a2, vratio = params$vratio, a2_slope_upper = params$a2_slope_upper, a2_slope_lower = params$a2_slope_lower, ter2 = params$ter2, starting_point_confidence = 0.5))
      predictions3 <- data.frame(coherence = 0.4, DDM_confidence_bounds_scale6(v = params$v3, a = params$a, a_slope = 0, ter = params$ter, n_trials = n_trials, a2 = params$a2, vratio = params$vratio, a2_slope_upper = params$a2_slope_upper, a2_slope_lower = params$a2_slope_lower, ter2 = params$ter2, starting_point_confidence = 0.5))
    } else if (model_type == 3){
      sourceCpp('Code/C++_generators/DDM_confidence_bounds_scale6.cpp')
      predictions1 <- data.frame(coherence = 0.1, DDM_confidence_bounds_scale6(v = params$v1, a = params$a, a_slope = 0, ter = params$ter, n_trials = n_trials, a2 = params$a2, vratio = params$vratio, a2_slope_upper = params$a2_slope, a2_slope_lower = params$a2_slope, ter2 = params$ter2, starting_point_confidence = params$starting_point_confidence))
      predictions2 <- data.frame(coherence = 0.2, DDM_confidence_bounds_scale6(v = params$v2, a = params$a, a_slope = 0, ter = params$ter, n_trials = n_trials, a2 = params$a2, vratio = params$vratio, a2_slope_upper = params$a2_slope, a2_slope_lower = params$a2_slope, ter2 = params$ter2, starting_point_confidence = params$starting_point_confidence))
      predictions3 <- data.frame(coherence = 0.4, DDM_confidence_bounds_scale6(v = params$v3, a = params$a, a_slope = 0, ter = params$ter, n_trials = n_trials, a2 = params$a2, vratio = params$vratio, a2_slope_upper = params$a2_slope, a2_slope_lower = params$a2_slope, ter2 = params$ter2, starting_point_confidence = params$starting_point_confidence))
    } else if (model_type == 4){
      sourceCpp('Code/C++_generators/DDM_confidence_bounds_scale6.cpp')
      predictions1 <- data.frame(coherence = 0.1, DDM_confidence_bounds_scale6(v = params$v1, a = params$a, a_slope = 0, ter = params$ter, n_trials = n_trials, a2 = params$a2, vratio = params$vratio, a2_slope_upper = params$a2_slope_upper, a2_slope_lower = params$a2_slope_lower, ter2 = params$ter2, starting_point_confidence = params$starting_point_confidence))
      predictions2 <- data.frame(coherence = 0.2, DDM_confidence_bounds_scale6(v = params$v2, a = params$a, a_slope = 0, ter = params$ter, n_trials = n_trials, a2 = params$a2, vratio = params$vratio, a2_slope_upper = params$a2_slope_upper, a2_slope_lower = params$a2_slope_lower, ter2 = params$ter2, starting_point_confidence = params$starting_point_confidence))
      predictions3 <- data.frame(coherence = 0.4, DDM_confidence_bounds_scale6(v = params$v3, a = params$a, a_slope = 0, ter = params$ter, n_trials = n_trials, a2 = params$a2, vratio = params$vratio, a2_slope_upper = params$a2_slope_upper, a2_slope_lower = params$a2_slope_lower, ter2 = params$ter2, starting_point_confidence = params$starting_point_confidence))
    } else if (model_type == 5){
      sourceCpp('Code/C++_generators/DDM_2DSD_scale6.cpp')
      predictions1 <- data.frame(coherence = 0.1, DDM_2DSD_scale6(v = params$v1, a = params$a, a_slope = 0, ter = params$ter, n_trials = n_trials, ter2 = params$ter2, tau = params$tau, vratio = params$vratio, eta = 0, sz = 0, c1 = params$c1, c2 = params$c2, c3 = params$c3, c4 = params$c4, c5 = params$c5))
      predictions2 <- data.frame(coherence = 0.2, DDM_2DSD_scale6(v = params$v2, a = params$a, a_slope = 0, ter = params$ter, n_trials = n_trials, ter2 = params$ter2, tau = params$tau, vratio = params$vratio, eta = 0, sz = 0, c1 = params$c1, c2 = params$c2, c3 = params$c3, c4 = params$c4, c5 = params$c5))
      predictions3 <- data.frame(coherence = 0.4, DDM_2DSD_scale6(v = params$v3, a = params$a, a_slope = 0, ter = params$ter, n_trials = n_trials, ter2 = params$ter2, tau = params$tau, vratio = params$vratio, eta = 0, sz = 0, c1 = params$c1, c2 = params$c2, c3 = params$c3, c4 = params$c4, c5 = params$c5))
    } else if (model_type == 6){
      sourceCpp('Code/C++_generators/DDM_CCB_scale6.cpp')
      predictions1 <- data.frame(coherence = 0.1, DDM_CCB_scale6(v = params$v1, a = params$a, a_slope = 0, ter = params$ter, n_trials = n_trials, vratio = params$vratio, ter2 = params$ter2, absolute_height = params$absolute_height, collapse_timer = params$collapse_timer, collapse_height = params$collapse_height))
      predictions2 <- data.frame(coherence = 0.2, DDM_CCB_scale6(v = params$v2, a = params$a, a_slope = 0, ter = params$ter, n_trials = n_trials, vratio = params$vratio, ter2 = params$ter2, absolute_height = params$absolute_height, collapse_timer = params$collapse_timer, collapse_height = params$collapse_height))
      predictions3 <- data.frame(coherence = 0.4, DDM_CCB_scale6(v = params$v3, a = params$a, a_slope = 0, ter = params$ter, n_trials = n_trials, vratio = params$vratio, ter2 = params$ter2, absolute_height = params$absolute_height, collapse_timer = params$collapse_timer, collapse_height = params$collapse_height))
    } else if (model_type == 7){
      sourceCpp('Code/C++_generators/DDM_race_scale6.cpp')
      predictions1 <- data.frame(coherence = 0.1, DDM_race_scale6(v_correct = params$v_correct_1, v_error = params$v_error_1, a = params$a, ter = params$ter, n_trials = n_trials, a2 = params$a2, ter2 = params$ter2))
      predictions2 <- data.frame(coherence = 0.2, DDM_race_scale6(v_correct = params$v_correct_2, v_error = params$v_error_2, a = params$a, ter = params$ter, n_trials = n_trials, a2 = params$a2, ter2 = params$ter2))
      predictions3 <- data.frame(coherence = 0.4, DDM_race_scale6(v_correct = params$v_correct_3, v_error = params$v_error_3, a = params$a, ter = params$ter, n_trials = n_trials, a2 = params$a2, ter2 = params$ter2))
    } else if (model_type == 8){
      sourceCpp('Code/C++_generators/DDM_2DSD_critVar_scale6.cpp')
      predictions1 <- data.frame(coherence = 0.1, DDM_2DSD_critVar_scale6(v = params$v1, a = params$a, a_slope = 0, ter = params$ter, n_trials = n_trials, ter2 = params$ter2, tau = params$tau, vratio = params$vratio, eta = 0, sz = 0, c1 = params$c1, c2 = params$c2, c3 = params$c3, c4 = params$c4, c5 = params$c5, a2_var = params$a2_var))
      predictions2 <- data.frame(coherence = 0.2, DDM_2DSD_critVar_scale6(v = params$v2, a = params$a, a_slope = 0, ter = params$ter, n_trials = n_trials, ter2 = params$ter2, tau = params$tau, vratio = params$vratio, eta = 0, sz = 0, c1 = params$c1, c2 = params$c2, c3 = params$c3, c4 = params$c4, c5 = params$c5, a2_var = params$a2_var))
      predictions3 <- data.frame(coherence = 0.4, DDM_2DSD_critVar_scale6(v = params$v3, a = params$a, a_slope = 0, ter = params$ter, n_trials = n_trials, ter2 = params$ter2, tau = params$tau, vratio = params$vratio, eta = 0, sz = 0, c1 = params$c1, c2 = params$c2, c3 = params$c3, c4 = params$c4, c5 = params$c5, a2_var = params$a2_var))
    } else if (model_type == 9){
      sourceCpp('Code/C++_generators/DDM_2DSD_OptionalStopping_scale6.cpp')
      predictions1 <- data.frame(coherence = 0.1, DDM_2DSD_OptionalStopping_scale6(v = params$v1, a = params$a, a_slope = 0, ter = params$ter, ter2 = params$ter2, n_trials = n_trials, vratio = params$vratio, cj1 = params$cj1, cj2 = params$cj2, cj3 = params$cj3, cj4 = params$cj4, cj5 = params$cj5, cj6 = params$cj6, cj2_p = params$cj2_p, cj3_p = params$cj3_p, cj4_p = params$cj4_p, cj5_p = params$cj5_p))
      predictions2 <- data.frame(coherence = 0.2, DDM_2DSD_OptionalStopping_scale6(v = params$v2, a = params$a, a_slope = 0, ter = params$ter, ter2 = params$ter2, n_trials = n_trials, vratio = params$vratio, cj1 = params$cj1, cj2 = params$cj2, cj3 = params$cj3, cj4 = params$cj4, cj5 = params$cj5, cj6 = params$cj6, cj2_p = params$cj2_p, cj3_p = params$cj3_p, cj4_p = params$cj4_p, cj5_p = params$cj5_p))
      predictions3 <- data.frame(coherence = 0.4, DDM_2DSD_OptionalStopping_scale6(v = params$v3, a = params$a, a_slope = 0, ter = params$ter, ter2 = params$ter2, n_trials = n_trials, vratio = params$vratio, cj1 = params$cj1, cj2 = params$cj2, cj3 = params$cj3, cj4 = params$cj4, cj5 = params$cj5, cj6 = params$cj6, cj2_p = params$cj2_p, cj3_p = params$cj3_p, cj4_p = params$cj4_p, cj5_p = params$cj5_p))
    }
    
  }
  
  predictions <- rbind(predictions1, predictions2, predictions3)
  names(predictions) <- c('coherence', 'rt', 'cor', 'rtconf', 'cj')
  return(predictions)
  
}


# Make model predictions model recovery ----------------------------------------
# Source cpp files outside of this function


make_predictions_recovery <- function(model_type, experiment, params, n_trials){
  
  if (!is.data.frame(params)){return('Params should be a data.frame()')}
  
  if (experiment == 1){
    
    if (model_type == 1){
      predictions1 <- data.frame(coherence = 0.1, DDM_confidence_bounds_binary(v = params$v1, a = params$a, a_slope = 0, ter = params$ter, n_trials = n_trials, a2 = params$a2, vratio = params$vratio, a2_slope_upper = params$a2_slope, a2_slope_lower = params$a2_slope, ter2 = params$ter2, starting_point_confidence = 0.5))
      predictions2 <- data.frame(coherence = 0.2, DDM_confidence_bounds_binary(v = params$v2, a = params$a, a_slope = 0, ter = params$ter, n_trials = n_trials, a2 = params$a2, vratio = params$vratio, a2_slope_upper = params$a2_slope, a2_slope_lower = params$a2_slope, ter2 = params$ter2, starting_point_confidence = 0.5))
      predictions3 <- data.frame(coherence = 0.4, DDM_confidence_bounds_binary(v = params$v3, a = params$a, a_slope = 0, ter = params$ter, n_trials = n_trials, a2 = params$a2, vratio = params$vratio, a2_slope_upper = params$a2_slope, a2_slope_lower = params$a2_slope, ter2 = params$ter2, starting_point_confidence = 0.5))
    } else if (model_type == 2){
      predictions1 <- data.frame(coherence = 0.1, DDM_confidence_bounds_binary(v = params$v1, a = params$a, a_slope = 0, ter = params$ter, n_trials = n_trials, a2 = params$a2, vratio = params$vratio, a2_slope_upper = params$a2_slope_upper, a2_slope_lower = params$a2_slope_lower, ter2 = params$ter2, starting_point_confidence = 0.5))
      predictions2 <- data.frame(coherence = 0.2, DDM_confidence_bounds_binary(v = params$v2, a = params$a, a_slope = 0, ter = params$ter, n_trials = n_trials, a2 = params$a2, vratio = params$vratio, a2_slope_upper = params$a2_slope_upper, a2_slope_lower = params$a2_slope_lower, ter2 = params$ter2, starting_point_confidence = 0.5))
      predictions3 <- data.frame(coherence = 0.4, DDM_confidence_bounds_binary(v = params$v3, a = params$a, a_slope = 0, ter = params$ter, n_trials = n_trials, a2 = params$a2, vratio = params$vratio, a2_slope_upper = params$a2_slope_upper, a2_slope_lower = params$a2_slope_lower, ter2 = params$ter2, starting_point_confidence = 0.5))
    } else if (model_type == 3){
      predictions1 <- data.frame(coherence = 0.1, DDM_confidence_bounds_binary(v = params$v1, a = params$a, a_slope = 0, ter = params$ter, n_trials = n_trials, a2 = params$a2, vratio = params$vratio, a2_slope_upper = params$a2_slope, a2_slope_lower = params$a2_slope, ter2 = params$ter2, starting_point_confidence = params$starting_point_confidence))
      predictions2 <- data.frame(coherence = 0.2, DDM_confidence_bounds_binary(v = params$v2, a = params$a, a_slope = 0, ter = params$ter, n_trials = n_trials, a2 = params$a2, vratio = params$vratio, a2_slope_upper = params$a2_slope, a2_slope_lower = params$a2_slope, ter2 = params$ter2, starting_point_confidence = params$starting_point_confidence))
      predictions3 <- data.frame(coherence = 0.4, DDM_confidence_bounds_binary(v = params$v3, a = params$a, a_slope = 0, ter = params$ter, n_trials = n_trials, a2 = params$a2, vratio = params$vratio, a2_slope_upper = params$a2_slope, a2_slope_lower = params$a2_slope, ter2 = params$ter2, starting_point_confidence = params$starting_point_confidence))
    } else if (model_type == 4){
      predictions1 <- data.frame(coherence = 0.1, DDM_confidence_bounds_binary(v = params$v1, a = params$a, a_slope = 0, ter = params$ter, n_trials = n_trials, a2 = params$a2, vratio = params$vratio, a2_slope_upper = params$a2_slope_upper, a2_slope_lower = params$a2_slope_lower, ter2 = params$ter2, starting_point_confidence = params$starting_point_confidence))
      predictions2 <- data.frame(coherence = 0.2, DDM_confidence_bounds_binary(v = params$v2, a = params$a, a_slope = 0, ter = params$ter, n_trials = n_trials, a2 = params$a2, vratio = params$vratio, a2_slope_upper = params$a2_slope_upper, a2_slope_lower = params$a2_slope_lower, ter2 = params$ter2, starting_point_confidence = params$starting_point_confidence))
      predictions3 <- data.frame(coherence = 0.4, DDM_confidence_bounds_binary(v = params$v3, a = params$a, a_slope = 0, ter = params$ter, n_trials = n_trials, a2 = params$a2, vratio = params$vratio, a2_slope_upper = params$a2_slope_upper, a2_slope_lower = params$a2_slope_lower, ter2 = params$ter2, starting_point_confidence = params$starting_point_confidence))
    } else if (model_type == 5){
      predictions1 <- data.frame(coherence = 0.1, DDM_2DSD_binary(v = params$v1, a = params$a, a_slope = 0, ter = params$ter, n_trials = n_trials, ter2 = params$ter2, tau = params$tau, vratio = params$vratio, eta = 0, sz = 0, c1 = params$c1))
      predictions2 <- data.frame(coherence = 0.2, DDM_2DSD_binary(v = params$v2, a = params$a, a_slope = 0, ter = params$ter, n_trials = n_trials, ter2 = params$ter2, tau = params$tau, vratio = params$vratio, eta = 0, sz = 0, c1 = params$c1))
      predictions3 <- data.frame(coherence = 0.4, DDM_2DSD_binary(v = params$v3, a = params$a, a_slope = 0, ter = params$ter, n_trials = n_trials, ter2 = params$ter2, tau = params$tau, vratio = params$vratio, eta = 0, sz = 0, c1 = params$c1))
    } else if (model_type == 6){
      predictions1 <- data.frame(coherence = 0.1, DDM_CCB_binary(v = params$v1, a = params$a, a_slope = 0, ter = params$ter, n_trials = n_trials, vratio = params$vratio, ter2 = params$ter2, absolute_height = params$absolute_height, collapse_timer = params$collapse_timer))
      predictions2 <- data.frame(coherence = 0.2, DDM_CCB_binary(v = params$v2, a = params$a, a_slope = 0, ter = params$ter, n_trials = n_trials, vratio = params$vratio, ter2 = params$ter2, absolute_height = params$absolute_height, collapse_timer = params$collapse_timer))
      predictions3 <- data.frame(coherence = 0.4, DDM_CCB_binary(v = params$v3, a = params$a, a_slope = 0, ter = params$ter, n_trials = n_trials, vratio = params$vratio, ter2 = params$ter2, absolute_height = params$absolute_height, collapse_timer = params$collapse_timer))
    } else if (model_type == 7){
      predictions1 <- data.frame(coherence = 0.1, DDM_race_binary(v_correct = params$v_correct_1, v_error = params$v_error_1, a = params$a, ter = params$ter, n_trials = n_trials, a2 = params$a2, ter2 = params$ter2))
      predictions2 <- data.frame(coherence = 0.2, DDM_race_binary(v_correct = params$v_correct_2, v_error = params$v_error_2, a = params$a, ter = params$ter, n_trials = n_trials, a2 = params$a2, ter2 = params$ter2))
      predictions3 <- data.frame(coherence = 0.4, DDM_race_binary(v_correct = params$v_correct_3, v_error = params$v_error_3, a = params$a, ter = params$ter, n_trials = n_trials, a2 = params$a2, ter2 = params$ter2))
    }  else if (model_type == 8){
      predictions1 <- data.frame(coherence = 0.1, DDM_2DSD_critVar_binary(v = params$v1, a = params$a, a_slope = 0, ter = params$ter, n_trials = n_trials, ter2 = params$ter2, tau = params$tau, vratio = params$vratio, eta = 0, sz = 0, c1 = params$c1, a2_var = params$a2_var))
      predictions2 <- data.frame(coherence = 0.2, DDM_2DSD_critVar_binary(v = params$v2, a = params$a, a_slope = 0, ter = params$ter, n_trials = n_trials, ter2 = params$ter2, tau = params$tau, vratio = params$vratio, eta = 0, sz = 0, c1 = params$c1, a2_var = params$a2_var))
      predictions3 <- data.frame(coherence = 0.4, DDM_2DSD_critVar_binary(v = params$v3, a = params$a, a_slope = 0, ter = params$ter, n_trials = n_trials, ter2 = params$ter2, tau = params$tau, vratio = params$vratio, eta = 0, sz = 0, c1 = params$c1, a2_var = params$a2_var))
    }  else if (model_type == 9){
      predictions1 <- data.frame(coherence = 0.1, DDM_confidence_bounds_binary(v = params$v1, a = params$a, a_slope = 0, ter = params$ter, n_trials = n_trials, a2 = params$a2, vratio = params$vratio, a2_slope_upper = params$a2_slope, a2_slope_lower = params$a2_slope, ter2 = params$ter2, starting_point_confidence = 0.5))
      predictions2 <- data.frame(coherence = 0.2, DDM_confidence_bounds_binary(v = params$v2, a = params$a, a_slope = 0, ter = params$ter, n_trials = n_trials, a2 = params$a2, vratio = params$vratio, a2_slope_upper = params$a2_slope, a2_slope_lower = params$a2_slope, ter2 = params$ter2, starting_point_confidence = 0.5))
      predictions3 <- data.frame(coherence = 0.4, DDM_confidence_bounds_binary(v = params$v3, a = params$a, a_slope = 0, ter = params$ter, n_trials = n_trials, a2 = params$a2, vratio = params$vratio, a2_slope_upper = params$a2_slope, a2_slope_lower = params$a2_slope, ter2 = params$ter2, starting_point_confidence = 0.5))
    }
    
  } else if (experiment == 2){
    
    if (model_type == 1){
      predictions1 <- data.frame(coherence = 0.1, DDM_confidence_bounds_scale6(v = params$v1, a = params$a, a_slope = 0, ter = params$ter, n_trials = n_trials, a2 = params$a2, vratio = params$vratio, a2_slope_upper = params$a2_slope, a2_slope_lower = params$a2_slope, ter2 = params$ter2, starting_point_confidence = 0.5))
      predictions2 <- data.frame(coherence = 0.2, DDM_confidence_bounds_scale6(v = params$v2, a = params$a, a_slope = 0, ter = params$ter, n_trials = n_trials, a2 = params$a2, vratio = params$vratio, a2_slope_upper = params$a2_slope, a2_slope_lower = params$a2_slope, ter2 = params$ter2, starting_point_confidence = 0.5))
      predictions3 <- data.frame(coherence = 0.4, DDM_confidence_bounds_scale6(v = params$v3, a = params$a, a_slope = 0, ter = params$ter, n_trials = n_trials, a2 = params$a2, vratio = params$vratio, a2_slope_upper = params$a2_slope, a2_slope_lower = params$a2_slope, ter2 = params$ter2, starting_point_confidence = 0.5))
    } else if (model_type == 2){
      predictions1 <- data.frame(coherence = 0.1, DDM_confidence_bounds_scale6(v = params$v1, a = params$a, a_slope = 0, ter = params$ter, n_trials = n_trials, a2 = params$a2, vratio = params$vratio, a2_slope_upper = params$a2_slope_upper, a2_slope_lower = params$a2_slope_lower, ter2 = params$ter2, starting_point_confidence = 0.5))
      predictions2 <- data.frame(coherence = 0.2, DDM_confidence_bounds_scale6(v = params$v2, a = params$a, a_slope = 0, ter = params$ter, n_trials = n_trials, a2 = params$a2, vratio = params$vratio, a2_slope_upper = params$a2_slope_upper, a2_slope_lower = params$a2_slope_lower, ter2 = params$ter2, starting_point_confidence = 0.5))
      predictions3 <- data.frame(coherence = 0.4, DDM_confidence_bounds_scale6(v = params$v3, a = params$a, a_slope = 0, ter = params$ter, n_trials = n_trials, a2 = params$a2, vratio = params$vratio, a2_slope_upper = params$a2_slope_upper, a2_slope_lower = params$a2_slope_lower, ter2 = params$ter2, starting_point_confidence = 0.5))
    } else if (model_type == 3){
      predictions1 <- data.frame(coherence = 0.1, DDM_confidence_bounds_scale6(v = params$v1, a = params$a, a_slope = 0, ter = params$ter, n_trials = n_trials, a2 = params$a2, vratio = params$vratio, a2_slope_upper = params$a2_slope, a2_slope_lower = params$a2_slope, ter2 = params$ter2, starting_point_confidence = params$starting_point_confidence))
      predictions2 <- data.frame(coherence = 0.2, DDM_confidence_bounds_scale6(v = params$v2, a = params$a, a_slope = 0, ter = params$ter, n_trials = n_trials, a2 = params$a2, vratio = params$vratio, a2_slope_upper = params$a2_slope, a2_slope_lower = params$a2_slope, ter2 = params$ter2, starting_point_confidence = params$starting_point_confidence))
      predictions3 <- data.frame(coherence = 0.4, DDM_confidence_bounds_scale6(v = params$v3, a = params$a, a_slope = 0, ter = params$ter, n_trials = n_trials, a2 = params$a2, vratio = params$vratio, a2_slope_upper = params$a2_slope, a2_slope_lower = params$a2_slope, ter2 = params$ter2, starting_point_confidence = params$starting_point_confidence))
    } else if (model_type == 4){
      predictions1 <- data.frame(coherence = 0.1, DDM_confidence_bounds_scale6(v = params$v1, a = params$a, a_slope = 0, ter = params$ter, n_trials = n_trials, a2 = params$a2, vratio = params$vratio, a2_slope_upper = params$a2_slope_upper, a2_slope_lower = params$a2_slope_lower, ter2 = params$ter2, starting_point_confidence = params$starting_point_confidence))
      predictions2 <- data.frame(coherence = 0.2, DDM_confidence_bounds_scale6(v = params$v2, a = params$a, a_slope = 0, ter = params$ter, n_trials = n_trials, a2 = params$a2, vratio = params$vratio, a2_slope_upper = params$a2_slope_upper, a2_slope_lower = params$a2_slope_lower, ter2 = params$ter2, starting_point_confidence = params$starting_point_confidence))
      predictions3 <- data.frame(coherence = 0.4, DDM_confidence_bounds_scale6(v = params$v3, a = params$a, a_slope = 0, ter = params$ter, n_trials = n_trials, a2 = params$a2, vratio = params$vratio, a2_slope_upper = params$a2_slope_upper, a2_slope_lower = params$a2_slope_lower, ter2 = params$ter2, starting_point_confidence = params$starting_point_confidence))
    } else if (model_type == 5){
      predictions1 <- data.frame(coherence = 0.1, DDM_2DSD_scale6(v = params$v1, a = params$a, a_slope = 0, ter = params$ter, n_trials = n_trials, ter2 = params$ter2, tau = params$tau, vratio = params$vratio, eta = 0, sz = 0, c1 = params$c1, c2 = params$c2, c3 = params$c3, c4 = params$c4, c5 = params$c5))
      predictions2 <- data.frame(coherence = 0.2, DDM_2DSD_scale6(v = params$v2, a = params$a, a_slope = 0, ter = params$ter, n_trials = n_trials, ter2 = params$ter2, tau = params$tau, vratio = params$vratio, eta = 0, sz = 0, c1 = params$c1, c2 = params$c2, c3 = params$c3, c4 = params$c4, c5 = params$c5))
      predictions3 <- data.frame(coherence = 0.4, DDM_2DSD_scale6(v = params$v3, a = params$a, a_slope = 0, ter = params$ter, n_trials = n_trials, ter2 = params$ter2, tau = params$tau, vratio = params$vratio, eta = 0, sz = 0, c1 = params$c1, c2 = params$c2, c3 = params$c3, c4 = params$c4, c5 = params$c5))
    } else if (model_type == 6){
      predictions1 <- data.frame(coherence = 0.1, DDM_CCB_scale6(v = params$v1, a = params$a, a_slope = 0, ter = params$ter, n_trials = n_trials, vratio = params$vratio, ter2 = params$ter2, absolute_height = params$absolute_height, collapse_timer = params$collapse_timer, collapse_height = params$collapse_height))
      predictions2 <- data.frame(coherence = 0.2, DDM_CCB_scale6(v = params$v2, a = params$a, a_slope = 0, ter = params$ter, n_trials = n_trials, vratio = params$vratio, ter2 = params$ter2, absolute_height = params$absolute_height, collapse_timer = params$collapse_timer, collapse_height = params$collapse_height))
      predictions3 <- data.frame(coherence = 0.4, DDM_CCB_scale6(v = params$v3, a = params$a, a_slope = 0, ter = params$ter, n_trials = n_trials, vratio = params$vratio, ter2 = params$ter2, absolute_height = params$absolute_height, collapse_timer = params$collapse_timer, collapse_height = params$collapse_height))
    } else if (model_type == 7){
      predictions1 <- data.frame(coherence = 0.1, DDM_race_scale6(v_correct = params$v_correct_1, v_error = params$v_error_1, a = params$a, ter = params$ter, n_trials = n_trials, a2 = params$a2, ter2 = params$ter2))
      predictions2 <- data.frame(coherence = 0.2, DDM_race_scale6(v_correct = params$v_correct_2, v_error = params$v_error_2, a = params$a, ter = params$ter, n_trials = n_trials, a2 = params$a2, ter2 = params$ter2))
      predictions3 <- data.frame(coherence = 0.4, DDM_race_scale6(v_correct = params$v_correct_3, v_error = params$v_error_3, a = params$a, ter = params$ter, n_trials = n_trials, a2 = params$a2, ter2 = params$ter2))
    } else if (model_type == 8){
      predictions1 <- data.frame(coherence = 0.1, DDM_2DSD_critVar_scale6(v = params$v1, a = params$a, a_slope = 0, ter = params$ter, n_trials = n_trials, ter2 = params$ter2, tau = params$tau, vratio = params$vratio, eta = 0, sz = 0, c1 = params$c1, c2 = params$c2, c3 = params$c3, c4 = params$c4, c5 = params$c5, a2_var = params$a2_var))
      predictions2 <- data.frame(coherence = 0.2, DDM_2DSD_critVar_scale6(v = params$v2, a = params$a, a_slope = 0, ter = params$ter, n_trials = n_trials, ter2 = params$ter2, tau = params$tau, vratio = params$vratio, eta = 0, sz = 0, c1 = params$c1, c2 = params$c2, c3 = params$c3, c4 = params$c4, c5 = params$c5, a2_var = params$a2_var))
      predictions3 <- data.frame(coherence = 0.4, DDM_2DSD_critVar_scale6(v = params$v3, a = params$a, a_slope = 0, ter = params$ter, n_trials = n_trials, ter2 = params$ter2, tau = params$tau, vratio = params$vratio, eta = 0, sz = 0, c1 = params$c1, c2 = params$c2, c3 = params$c3, c4 = params$c4, c5 = params$c5, a2_var = params$a2_var))
    } else if (model_type == 9){
      predictions1 <- data.frame(coherence = 0.1, DDM_2DSD_OptionalStopping_scale6(v = params$v1, a = params$a, a_slope = 0, ter = params$ter, ter2 = params$ter2, n_trials = n_trials, vratio = params$vratio, cj1 = params$cj1, cj2 = params$cj2, cj3 = params$cj3, cj4 = params$cj4, cj5 = params$cj5, cj6 = params$cj6, cj2_p = params$cj2_p, cj3_p = params$cj3_p, cj4_p = params$cj4_p, cj5_p = params$cj5_p))
      predictions2 <- data.frame(coherence = 0.2, DDM_2DSD_OptionalStopping_scale6(v = params$v2, a = params$a, a_slope = 0, ter = params$ter, ter2 = params$ter2, n_trials = n_trials, vratio = params$vratio, cj1 = params$cj1, cj2 = params$cj2, cj3 = params$cj3, cj4 = params$cj4, cj5 = params$cj5, cj6 = params$cj6, cj2_p = params$cj2_p, cj3_p = params$cj3_p, cj4_p = params$cj4_p, cj5_p = params$cj5_p))
      predictions3 <- data.frame(coherence = 0.4, DDM_2DSD_OptionalStopping_scale6(v = params$v3, a = params$a, a_slope = 0, ter = params$ter, ter2 = params$ter2, n_trials = n_trials, vratio = params$vratio, cj1 = params$cj1, cj2 = params$cj2, cj3 = params$cj3, cj4 = params$cj4, cj5 = params$cj5, cj6 = params$cj6, cj2_p = params$cj2_p, cj3_p = params$cj3_p, cj4_p = params$cj4_p, cj5_p = params$cj5_p))
    }
    
  }

  predictions <- rbind(predictions1, predictions2, predictions3)
  names(predictions) <- c('coherence', 'rt', 'cor', 'rtconf', 'cj')
  return(predictions)
  
}


# Compute BIC ------------------------------------------------------------------


compute_BIC <- function(df_observed, df_predicted, model_type, experiment){
  
  
  # (1) compute chi2 -----------------------------------------------------------
  
  
  rt_chi2 <- 0
  rtconf_chi2 <- 0
  confidence_chi2 <- 0
    
  for (coherence_level in 1:3){
    
    df_obs <- df_observed[df_observed$coherence == c(0.1, 0.2, 0.4)[coherence_level],]
    df_pred <- df_predicted[df_predicted$coherence == c(0.1, 0.2, 0.4)[coherence_level],]
    
    # RT QUANTILES
    
    obs_props <- NULL
    pred_props <- NULL
    
    for (accuracy in c(0,1)){
      
      obs_temp <- df_obs[df_obs$cor == accuracy, ]
      pred_temp <- df_pred[df_pred$cor == accuracy, ]
      
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
      
    }
    
    rt_chi2_outliers <- sum(pred_props == 0 & obs_props != 0)
    obs_props[obs_props==0] <- 0.0000001  
    pred_props[pred_props==0] <- 0.0000001  # avoid zeros in the numerator of chi2  
    rt_chi2 <- rt_chi2 + sum( ( (obs_props - pred_props) ^ 2) / pred_props)
    
    # RTCONF QUANTILES
    
    obs_props <- NULL
    pred_props <- NULL
    
    for (accuracy in c(0,1)){
      
      obs_temp <- df_obs[df_obs$cor == accuracy, ]
      pred_temp <- df_pred[df_pred$cor == accuracy, ]
      
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
    
    rtconf_chi2_outliers <- sum(pred_props == 0 & obs_props != 0)
    obs_props[obs_props==0] <- 0.0000001  
    pred_props[pred_props==0] <- 0.0000001  # avoid zeros in the numerator of chi2  
    rtconf_chi2 <- rtconf_chi2 + sum( ( (obs_props - pred_props) ^ 2) / pred_props)
    
    # CJ PROPORTIONS
    
    if (experiment == 1){
      obs_props <- as.vector(prop.table(table(factor(df_obs$cj, 0:1), factor(df_obs$cor, 0:1))))
      pred_props <- as.vector(prop.table(table(factor(df_pred$cj, 0:1), factor(df_pred$cor, 0:1))))
    } else if (experiment == 2){
      obs_props <- as.vector(prop.table(table(factor(df_obs$cj, 1:6), factor(df_obs$cor, 0:1))))
      pred_props <- as.vector(prop.table(table(factor(df_pred$cj, 1:6), factor(df_pred$cor, 0:1))))
    }
    
    confidence_chi2_outliers <- sum(pred_props == 0 & obs_props != 0)
    obs_props[obs_props==0] <- 0.0000001  
    pred_props[pred_props==0] <- 0.0000001  # avoid zeros in the numerator of chi2
    confidence_chi2 <- confidence_chi2 + sum( ( (obs_props - pred_props) ^ 2) / pred_props)
    
  }
  
  chi2 <- rt_chi2 + rtconf_chi2 + confidence_chi2
  
  
  # (2) Compute BIC ------------------------------------------------------------
  
  
  if (experiment == 1){
    n_parameters <- switch(model_type, 
                           '1' = 9,
                           '2' = 10,
                           '3' = 10,
                           '4' = 11,
                           '5' = 9,
                           '6' = 9,
                           '7' = 10,
                           '8' = 10,
                           '9' = 8)
  } else if (experiment == 2){
    n_parameters <- switch(model_type, 
                           '1' = 9,
                           '2' = 10,
                           '3' = 10,
                           '4' = 11,
                           '5' = 13,
                           '6' = 10,
                           '7' = 10,
                           '8' = 14,
                           '9' = 17)}  
  
  n_trials <- nrow(df_observed)
  BIC <- n_parameters * log(n_trials) + n_trials * log(chi2/n_trials)
  
  return(list(
    BIC = BIC,
    chi2 = chi2,
    n_parameters = n_parameters,
    n_trials = n_trials,
    rt_chi2 = rt_chi2,
    rtconf_chi2 = rtconf_chi2,
    confidence_chi2 = confidence_chi2,
    rt_chi2_outliers = rt_chi2_outliers,
    rtconf_chi2_outliers = rtconf_chi2_outliers,
    confidence_chi2_outliers = confidence_chi2_outliers
  ))
}



  

  



