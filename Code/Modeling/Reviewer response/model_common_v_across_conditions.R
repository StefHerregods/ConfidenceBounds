
rm(list = ls())

library(Rcpp)  # To source, compile and run C++ functions
library(DEoptim)  # Evolutionary algorithm

# Parameters

EXPERIMENT <- 2
CLUSTER <- FALSE
subject_ID <- 1
ITERMAX <- 2000  # Higher because 4x the parames


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


model_type = 4
experiment = 2
  
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
  




# Investigating model parameters - are the results equal to the original experiment?

library(rstatix)
library(tidyr)
library(dplyr)
library(ggpubr)
library(ggplot2)
library(ggiraph)

df_params <- df_params_commonV

df_long <- df_params %>%
  pivot_longer(
    cols = matches("^(a|a2|a2_slope_upper|a2_slope_lower|starting_point_confidence|a2_slope)_\\d$"),  # matches a_1 to a_4 and a2_1 to a2_4
    names_to = c(".value", "manip_idx"),
    names_pattern = "^(a2_slope|starting_point_confidence|a2_slope_upper|a2_slope_lower|a2|a)_(\\d)$"
  ) %>%
  mutate(
    manipulation = c('AccAcc', 'AccFast', 'FastFast', 'FastAcc')[as.integer(manip_idx)]
  ) %>%
  select(-manip_idx)

df_long <- df_long %>%
  ungroup() %>%
  mutate(rt_manipulation = as.factor(ifelse(manipulation %in% c("AccAcc", "AccFast"), 1, 0)),
         rtconf_manipulation = as.factor(ifelse(manipulation %in% c("AccAcc", "FastAcc"), 1, 0)))


## Decision bound (a) ----

### Outliers
plot_a <- ggplot(data = df_long) + 
  geom_point_interactive(aes(x = manipulation, y = a, data_id = sub, tooltip = sub))
girafe(ggobj = plot_a)

### Normality
ggqqplot(df_long, 'a', facet.by = c('rt_manipulation', 'rtconf_manipulation'))

### ANOVA
res.aov <- anova_test(
  data = df_long, dv = a, wid = sub,
  within = c(rt_manipulation, rtconf_manipulation),
  effect.size = 'pes'
)
get_anova_table(res.aov)

### Pairwise t-test
df_long %>% group_by(rtconf_manipulation) %>%
  t_test(a ~ rt_manipulation, paired = T)

## Confidence bound (a2) ----

### Outliers
plot_a2 <- ggplot(data = df_long) + 
  geom_point_interactive(aes(x = manipulation, y = a2, data_id = sub, tooltip = sub))
girafe(ggobj = plot_a2)

### Normality
ggqqplot(df_long, 'a2', facet.by = c('rt_manipulation', 'rtconf_manipulation'))

### ANOVA
res.aov <- anova_test(
  data = df_long, dv = a2, wid = sub,
  within = c(rt_manipulation, rtconf_manipulation),
  effect.size = 'pes'
)
get_anova_table(res.aov)


## Confidence bound (starting_point_confidence) ----

### Outliers
plot_starting_point_confidence <- ggplot(data = df_long) + 
  geom_point_interactive(aes(x = manipulation, y = starting_point_confidence, data_id = sub, tooltip = sub))
girafe(ggobj = plot_starting_point_confidence)

### Normality
ggqqplot(df_long, 'starting_point_confidence', facet.by = c('rt_manipulation', 'rtconf_manipulation'))

### ANOVA
res.aov <- anova_test(
  data = df_long, dv = starting_point_confidence, wid = sub,
  within = c(rt_manipulation, rtconf_manipulation),
  effect.size = 'pes'
)
get_anova_table(res.aov)


## Urgency (a2_slope_upper) ----

### Outliers
plot_a2_slope_upper <- ggplot(data = df_long) + 
  geom_point_interactive(aes(x = manipulation, y = a2_slope_upper, data_id = sub, tooltip = sub))
girafe(ggobj = plot_a2_slope_upper)

### Normality
ggqqplot(df_params, 'a2_slope_upper', facet.by = c('rt_manipulation', 'rtconf_manipulation'))

### ANOVA
res.aov <- anova_test(
  data = df_long, dv = a2_slope_upper, wid = sub,
  within = c(rt_manipulation, rtconf_manipulation),
  effect.size = 'pes'
)
get_anova_table(res.aov)

## Urgency (a2_slope_lower) ----

### Outliers
plot_a2_slope_lower <- ggplot(data = df_long) + 
  geom_point_interactive(aes(x = manipulation, y = a2_slope_lower, data_id = sub, tooltip = sub))
girafe(ggobj = plot_a2_slope_lower)

### Normality
ggqqplot(df_long, 'a2_slope_lower', facet.by = c('rt_manipulation', 'rtconf_manipulation'))

### ANOVA
res.aov <- anova_test(
  data = df_long, dv = a2_slope_lower, wid = sub,
  within = c(rt_manipulation, rtconf_manipulation),
  effect.size = 'pes'
)
get_anova_table(res.aov)



## Urgency (a2_slope) ----

### Outliers
plot_a2_slope <- ggplot(data = df_long) + 
  geom_point_interactive(aes(x = manipulation, y = a2_slope, data_id = sub, tooltip = sub))
girafe(ggobj = plot_a2_slope)

### Normality
ggqqplot(df_long, 'a2_slope', facet.by = c('rt_manipulation', 'rtconf_manipulation'))

### ANOVA
res.aov <- anova_test(
  data = df_long, dv = a2_slope, wid = sub,
  within = c(rt_manipulation, rtconf_manipulation),
  effect.size = 'pes'
)
get_anova_table(res.aov)







# -------------- Making predictions

library(dplyr)
df_params_commonV <- data.frame(params)
n_trials <- 10000
predictions <- NULL

for (row in 1:nrow(df_params_commonV)){
  
  params <- df_params_commonV[row,]
  
  if (experiment == 1){
    
    sourceCpp('Code/C++_generators/DDM_confidence_bounds_binary.cpp')
    predictions1 <- data.frame(coherence = 0.1, DDM_confidence_bounds_binary(v = params$v1, a = params$a_1, a_slope = 0, ter = params$ter, n_trials = n_trials, a2 = params$a2_1, vratio = params$vratio, a2_slope_upper = params$a2_slope_1, a2_slope_lower = params$a2_slope_1, ter2 = params$ter2, starting_point_confidence = 0.5))
    predictions2 <- data.frame(coherence = 0.2, DDM_confidence_bounds_binary(v = params$v2, a = params$a_1, a_slope = 0, ter = params$ter, n_trials = n_trials, a2 = params$a2_1, vratio = params$vratio, a2_slope_upper = params$a2_slope_1, a2_slope_lower = params$a2_slope_1, ter2 = params$ter2, starting_point_confidence = 0.5))
    predictions3 <- data.frame(coherence = 0.4, DDM_confidence_bounds_binary(v = params$v3, a = params$a_1, a_slope = 0, ter = params$ter, n_trials = n_trials, a2 = params$a2_1, vratio = params$vratio, a2_slope_upper = params$a2_slope_1, a2_slope_lower = params$a2_slope_1, ter2 = params$ter2, starting_point_confidence = 0.5))
    predictions4 <- data.frame(coherence = 0.1, DDM_confidence_bounds_binary(v = params$v1, a = params$a_2, a_slope = 0, ter = params$ter, n_trials = n_trials, a2 = params$a2_2, vratio = params$vratio, a2_slope_upper = params$a2_slope_2, a2_slope_lower = params$a2_slope_2, ter2 = params$ter2, starting_point_confidence = 0.5))
    predictions5 <- data.frame(coherence = 0.2, DDM_confidence_bounds_binary(v = params$v2, a = params$a_2, a_slope = 0, ter = params$ter, n_trials = n_trials, a2 = params$a2_2, vratio = params$vratio, a2_slope_upper = params$a2_slope_2, a2_slope_lower = params$a2_slope_2, ter2 = params$ter2, starting_point_confidence = 0.5))
    predictions6 <- data.frame(coherence = 0.4, DDM_confidence_bounds_binary(v = params$v3, a = params$a_2, a_slope = 0, ter = params$ter, n_trials = n_trials, a2 = params$a2_2, vratio = params$vratio, a2_slope_upper = params$a2_slope_2, a2_slope_lower = params$a2_slope_2, ter2 = params$ter2, starting_point_confidence = 0.5))
    predictions7 <- data.frame(coherence = 0.1, DDM_confidence_bounds_binary(v = params$v1, a = params$a_3, a_slope = 0, ter = params$ter, n_trials = n_trials, a2 = params$a2_3, vratio = params$vratio, a2_slope_upper = params$a2_slope_3, a2_slope_lower = params$a2_slope_3, ter2 = params$ter2, starting_point_confidence = 0.5))
    predictions8 <- data.frame(coherence = 0.2, DDM_confidence_bounds_binary(v = params$v2, a = params$a_3, a_slope = 0, ter = params$ter, n_trials = n_trials, a2 = params$a2_3, vratio = params$vratio, a2_slope_upper = params$a2_slope_3, a2_slope_lower = params$a2_slope_3, ter2 = params$ter2, starting_point_confidence = 0.5))
    predictions9 <- data.frame(coherence = 0.4, DDM_confidence_bounds_binary(v = params$v3, a = params$a_3, a_slope = 0, ter = params$ter, n_trials = n_trials, a2 = params$a2_3, vratio = params$vratio, a2_slope_upper = params$a2_slope_3, a2_slope_lower = params$a2_slope_3, ter2 = params$ter2, starting_point_confidence = 0.5))
    predictions10 <- data.frame(coherence = 0.1, DDM_confidence_bounds_binary(v = params$v1, a = params$a_4, a_slope = 0, ter = params$ter, n_trials = n_trials, a2 = params$a2_4, vratio = params$vratio, a2_slope_upper = params$a2_slope_4, a2_slope_lower = params$a2_slope_4, ter2 = params$ter2, starting_point_confidence = 0.5))
    predictions11 <- data.frame(coherence = 0.2, DDM_confidence_bounds_binary(v = params$v2, a = params$a_4, a_slope = 0, ter = params$ter, n_trials = n_trials, a2 = params$a2_4, vratio = params$vratio, a2_slope_upper = params$a2_slope_4, a2_slope_lower = params$a2_slope_4, ter2 = params$ter2, starting_point_confidence = 0.5))
    predictions12 <- data.frame(coherence = 0.4, DDM_confidence_bounds_binary(v = params$v3, a = params$a_4, a_slope = 0, ter = params$ter, n_trials = n_trials, a2 = params$a2_4, vratio = params$vratio, a2_slope_upper = params$a2_slope_4, a2_slope_lower = params$a2_slope_4, ter2 = params$ter2, starting_point_confidence = 0.5))
    
  } else if (experiment == 2){
    
    sourceCpp('Code/C++_generators/DDM_confidence_bounds_scale6.cpp')
    predictions1 <- data.frame(coherence = 0.1, DDM_confidence_bounds_scale6(v = params$v1, a = params$a_1, a_slope = 0, ter = params$ter, n_trials = n_trials, a2 = params$a2_1, vratio = params$vratio, a2_slope_upper = params$a2_slope_upper_1, a2_slope_lower = params$a2_slope_lower_1, ter2 = params$ter2, starting_point_confidence = params$starting_point_confidence_1))
    predictions2 <- data.frame(coherence = 0.2, DDM_confidence_bounds_scale6(v = params$v2, a = params$a_1, a_slope = 0, ter = params$ter, n_trials = n_trials, a2 = params$a2_1, vratio = params$vratio, a2_slope_upper = params$a2_slope_upper_1, a2_slope_lower = params$a2_slope_lower_1, ter2 = params$ter2, starting_point_confidence = params$starting_point_confidence_1))
    predictions3 <- data.frame(coherence = 0.4, DDM_confidence_bounds_scale6(v = params$v3, a = params$a_1, a_slope = 0, ter = params$ter, n_trials = n_trials, a2 = params$a2_1, vratio = params$vratio, a2_slope_upper = params$a2_slope_upper_1, a2_slope_lower = params$a2_slope_lower_1, ter2 = params$ter2, starting_point_confidence = params$starting_point_confidence_1))
    predictions4 <- data.frame(coherence = 0.1, DDM_confidence_bounds_scale6(v = params$v1, a = params$a_2, a_slope = 0, ter = params$ter, n_trials = n_trials, a2 = params$a2_2, vratio = params$vratio, a2_slope_upper = params$a2_slope_upper_2, a2_slope_lower = params$a2_slope_lower_2, ter2 = params$ter2, starting_point_confidence = params$starting_point_confidence_2))
    predictions5 <- data.frame(coherence = 0.2, DDM_confidence_bounds_scale6(v = params$v2, a = params$a_2, a_slope = 0, ter = params$ter, n_trials = n_trials, a2 = params$a2_2, vratio = params$vratio, a2_slope_upper = params$a2_slope_upper_2, a2_slope_lower = params$a2_slope_lower_2, ter2 = params$ter2, starting_point_confidence = params$starting_point_confidence_2))
    predictions6 <- data.frame(coherence = 0.4, DDM_confidence_bounds_scale6(v = params$v3, a = params$a_2, a_slope = 0, ter = params$ter, n_trials = n_trials, a2 = params$a2_2, vratio = params$vratio, a2_slope_upper = params$a2_slope_upper_2, a2_slope_lower = params$a2_slope_lower_2, ter2 = params$ter2, starting_point_confidence = params$starting_point_confidence_2))
    predictions7<- data.frame(coherence = 0.1, DDM_confidence_bounds_scale6(v = params$v1, a = params$a_3, a_slope = 0, ter = params$ter, n_trials = n_trials, a2 = params$a2_3, vratio = params$vratio, a2_slope_upper = params$a2_slope_upper_3, a2_slope_lower = params$a2_slope_lower_3, ter2 = params$ter2, starting_point_confidence = params$starting_point_confidence_3))
    predictions8 <- data.frame(coherence = 0.2, DDM_confidence_bounds_scale6(v = params$v2, a = params$a_3, a_slope = 0, ter = params$ter, n_trials = n_trials, a2 = params$a2_3, vratio = params$vratio, a2_slope_upper = params$a2_slope_upper_3, a2_slope_lower = params$a2_slope_lower_3, ter2 = params$ter2, starting_point_confidence = params$starting_point_confidence_3))
    predictions9 <- data.frame(coherence = 0.4, DDM_confidence_bounds_scale6(v = params$v3, a = params$a_3, a_slope = 0, ter = params$ter, n_trials = n_trials, a2 = params$a2_3, vratio = params$vratio, a2_slope_upper = params$a2_slope_upper_3, a2_slope_lower = params$a2_slope_lower_3, ter2 = params$ter2, starting_point_confidence = params$starting_point_confidence_3))
    predictions10 <- data.frame(coherence = 0.1, DDM_confidence_bounds_scale6(v = params$v1, a = params$a_4, a_slope = 0, ter = params$ter, n_trials = n_trials, a2 = params$a2_4, vratio = params$vratio, a2_slope_upper = params$a2_slope_upper_4, a2_slope_lower = params$a2_slope_lower_4, ter2 = params$ter2, starting_point_confidence = params$starting_point_confidence_4))
    predictions11 <- data.frame(coherence = 0.2, DDM_confidence_bounds_scale6(v = params$v2, a = params$a_4, a_slope = 0, ter = params$ter, n_trials = n_trials, a2 = params$a2_4, vratio = params$vratio, a2_slope_upper = params$a2_slope_upper_4, a2_slope_lower = params$a2_slope_lower_4, ter2 = params$ter2, starting_point_confidence = params$starting_point_confidence_4))
    predictions12 <- data.frame(coherence = 0.4, DDM_confidence_bounds_scale6(v = params$v3, a = params$a_4, a_slope = 0, ter = params$ter, n_trials = n_trials, a2 = params$a2_4, vratio = params$vratio, a2_slope_upper = params$a2_slope_upper_4, a2_slope_lower = params$a2_slope_lower_4, ter2 = params$ter2, starting_point_confidence = params$starting_point_confidence_4))
    
  }
  
  predictions_sub <- bind_rows(
    predictions1 %>% mutate(manipulation = "AccAcc"),
    predictions2 %>% mutate(manipulation = "AccAcc"),
    predictions3 %>% mutate(manipulation = "AccAcc"),
    predictions4 %>% mutate(manipulation = "AccFast"),
    predictions5 %>% mutate(manipulation = "AccFast"),
    predictions6 %>% mutate(manipulation = "AccFast"),
    predictions7 %>% mutate(manipulation = "FastFast"),
    predictions8 %>% mutate(manipulation = "FastFast"),
    predictions9 %>% mutate(manipulation = "FastFast"),
    predictions10 %>% mutate(manipulation = "FastAcc"),
    predictions11 %>% mutate(manipulation = "FastAcc"),
    predictions12 %>% mutate(manipulation = "FastAcc"),
  )
  
  names(predictions_sub) <- c('coherence', 'rt', 'cor', 'rtconf', 'cj', 'manipulation')
  predictions_sub$sub <- row
  
  if (is.null(predictions)) {
    print("is null")
    predictions <- predictions_sub[0,]
  }
  predictions <- rbind(predictions, predictions_sub)
}

hist(predictions$rt)

df_pred <- predictions
df_obs <- read.csv('Data/Experiment_2/exp2_data_viable.csv')

coherence_vector <- c(0.1, 0.2, 0.4)
condLab <- c('FastFast', 'AccFast', 'AccAcc', 'FastAcc')
par(mfrow = c(4, 3), mai = c(0.3, 0.3, 0.3, 0.3))



## Marginal predictions - exp 1

df_pred$response <- factor(df_pred$cor, levels = c(1, 0), labels = c("correct", "error"))
df_obs$response <- factor(df_obs$cor, levels = c(1, 0), labels = c("correct", "error"))
colors_accuracy <- c('correct' = "darkgreen", 'error' = "darkred")
labels <- c(
  "AccAcc" = "Accurate decision\nAccurate CJ",
  "AccFast" = "Accurate decision\nFast CJ",
  "FastFast" = "Fast decision\nFast CJ",
  "FastAcc" = "Fast decision\nAccurate CJ"
)

# For rt
ggplot(df_obs, aes(x = rt, fill = response)) +
  geom_histogram(aes(y = after_stat(count / sum(count) * 100)), position = "identity", alpha = 0.5, bins = 30) +
  geom_freqpoly(data = df_pred, aes(x = rt, y = after_stat(count / sum(count) * 100), color = response), bins = 30, size = 1.2) +
  scale_fill_manual(values = colors_accuracy) +
  scale_color_manual(values = colors_accuracy) +
  facet_wrap(~ manipulation, nrow = 1, labeller = labeller(manipulation = labels)) +
  xlim(c(0,5)) +
  theme_bw() +
  labs(title = "RT distributions ~ Accuracy", y = "Percentage", x = 'Response time')

# For rtconf
ggplot(df_obs, aes(x = rtconf, fill = response)) +
  geom_histogram(aes(y = after_stat(count / sum(count) * 100)), position = "identity", alpha = 0.5, bins = 30) +
  geom_freqpoly(data = df_pred, aes(x = rtconf, y = after_stat(count / sum(count) * 100), color = response), bins = 30, size = 1.2) +
  scale_fill_manual(values = colors_accuracy) +
  scale_color_manual(values = colors_accuracy) +
  facet_wrap(~ manipulation, nrow = 1, labeller = labeller(manipulation = labels)) +
  xlim(c(0,3)) +
  theme_bw() +
  labs(title = "Confidence RT distributions ~ Accuracy", y = "Percentage", x = 'Confidence response time')

# For each level of CJ
cj_level <- 6
title <- paste("Confidence RT distributions ~ Confidence rating", cj_level)
ggplot(df_obs[df_obs$cj == cj_level,], aes(x = rtconf)) +
  geom_histogram(aes(y = after_stat(count / sum(count) * 100)), position = "identity", alpha = 0.5, bins = 30) +
  geom_freqpoly(data = df_pred[df_pred$cj == cj_level,], aes(x = rtconf, y = after_stat(count / sum(count) * 100)), bins = 30, size = 1.2) +
  facet_wrap(~ manipulation, nrow = 1, labeller = labeller(manipulation = labels)) +
  xlim(c(0,3)) +
  theme_bw() +
  labs(title = title, y = "Percentage", x = 'Confidence response time')


# 




# Merge the 'cor' column with df_proportions for color mapping
df_plot <- df_obs %>%
  group_by(sub, manipulation, cj, cor) %>%
  summarise(count = n(), .groups = 'drop') %>%
  group_by(sub, manipulation) %>%
  mutate(proportion = count / sum(count))

# Calculate the means for each cj, manipulation, and cor
df_means <- df_plot %>%
  group_by(cj, cor) %>%
  summarise(mean_proportion = mean(proportion), .groups = 'drop')


df_plot_pred <- df_pred %>%
  group_by(sub, manipulation, cj, cor) %>%
  summarise(count = n(), .groups = 'drop') %>%
  group_by(sub, manipulation) %>%
  mutate(proportion = count / sum(count))
df_means_pred <- df_plot_pred %>%
  group_by(cj, cor) %>%
  summarise(mean_proportion = mean(proportion), .groups = 'drop')

# Create the jitter plot with means
(a <- ggplot(df_plot) +
  geom_point(
    aes(x = as.factor(cj), y = proportion, color = as.factor(cor)),
    position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.5), 
    size = 3,
    alpha = 0.03
  ) +
  # Add means as solid points
  geom_point(
    data = df_means, 
    aes(x = as.factor(cj), y = mean_proportion, color = as.factor(cor)),
    position = position_dodge(width = 0.5),  # Only dodge without jitter for mean
    size = 12,
    alpha = 1,
    shape = '-',
  ) +
  geom_point(
    data = df_means_pred, 
    aes(x = as.factor(cj), y = mean_proportion, group = as.factor(cor)),
    position = position_dodge(width = 0.5),  # Only dodge without jitter for mean
    size = 3,
    alpha = 1,
    shape = 4,
    stroke = 1,
    fill = 'black'
  ) +
  ylim(c(0.0,0.5)) +
  scale_color_manual(values = c("0" = "#D81600", "1" = "#8FD694")) +  # Color mapping
  labs(x = "Confidence Judgement (cj)", y = "Proportion", color = "Correct (cor)") +
  theme(legend.position="none") +
  theme_classic())













# For df_observations
means_df_observed <- df_obs %>%
  group_by(cj, cor, sub, manipulation) %>%
  summarize(mean_rtconf = mean(rtconf, na.rm = TRUE), .groups = 'drop')

# For df_predicted_4
means_df_predicted_9 <- df_pred %>%
  group_by(cj, cor, sub, manipulation) %>%
  summarize(mean_rtconf = mean(rtconf, na.rm = TRUE), .groups = 'drop')


# For df_observations
means_df_observed_cj_cor <- df_obs %>%
  group_by(cj, cor) %>%
  summarize(mean_rtconf = mean(rtconf, na.rm = TRUE), .groups = 'drop')

# For df_predicted_4
means_df_predicted_9_cj_cor <- df_pred %>%
  group_by(cj, cor) %>%
  summarize(mean_rtconf = mean(rtconf, na.rm = TRUE), .groups = 'drop')

b <- ggplot(means_df_observed) +
  geom_point(
    aes(x = as.factor(cj), y = mean_rtconf, color = as.factor(cor)),
    position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.5), 
    size = 3,
    alpha = 0.03
  ) +
  # Add means as solid points
  geom_point(
    data = means_df_observed_cj_cor, 
    aes(x = as.factor(cj), y = mean_rtconf, color = as.factor(cor)),
    position = position_dodge(width = 0.5),  # Only dodge without jitter for mean
    size = 12,
    alpha = 1,
    shape = '-',
  ) +
  geom_point(
    data = means_df_predicted_9_cj_cor, 
    aes(x = as.factor(cj), y = mean_rtconf, group = as.factor(cor)),
    position = position_dodge(width = 0.5),  # Only dodge without jitter for mean
    size = 3,
    alpha = 1,
    shape = 4,
    stroke = 1,
    fill = 'black'
  ) +
  ylim(c(0.0,2)) +
  scale_color_manual(values = c("0" = "#D81600", "1" = "#8FD694")) +  # Color mapping
  labs(x = "Confidence Judgement (cj)", y = "Confidence RT", color = "Correct (cor)") +
  theme(legend.position="none") +
  theme_classic()







ggarrange(a, b)





