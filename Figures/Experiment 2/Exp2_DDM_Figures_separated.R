

# To do ----

# Error bars
# Averaged prediction graphs

# Set-up ----


## Packages

library(tidyr)
library(ggplot2)
library(dplyr)
library(Rcpp)  # To source, compile and run C++ functions
library(DEoptim)  # Optimization algorithm
library(RcppZiggurat)  # Random number generator (normal distribution)
library(ggpubr)
library(gridExtra)
library(rstatix)

## Working directory

setwd('C:\\Users\\herre\\Desktop\\Internship\\Results\\Exp2_Results')

## Functions

### Give R access to the DDM simulation function in C++
sourceCpp("C:\\Users\\herre\\OneDrive\\Documenten\\GitHub\\ConfidenceBounds\\Analyses\\Exp1_analyses\\DDM_confidence_bounds_separated_2.cpp") 


# Load data ----


## Behavioral data ----

df_obs <- read.csv(file = "Exp2_data_viable.csv")

## DDM parameters ----

df_DDM <- data.frame(matrix(ncol = 13, nrow = 40*4))
colnames(df_DDM) <- c('sub', 'manipulation', 'v1', 'v2', 'v3', 'a', 'ter', 'a2_upper', 'a2_lower', 'postdriftmod', 'a2_slope_upper', 'a2_slope_lower', 'ter2')
condLab <- c('FastFast', 'AccFast', 'AccAcc', 'FastAcc') 
j <- 1
for (sub_id in (1:40)){  
  for (cond in 1:4){
    file_name <- paste0('Parameter_estimation_separated_2\\exp2_separated_2_results_sub_', sub_id, '_', condLab[cond], '.Rdata')
    load(file_name)
    df_DDM[j,] <- c(sub_id, condLab[cond], results$optim$bestmem[1], results$optim$bestmem[2], results$optim$bestmem[3], results$optim$bestmem[4], results$optim$bestmem[5], results$optim$bestmem[6], results$optim$bestmem[7], results$optim$bestmem[8], results$optim$bestmem[9], results$optim$bestmem[10], results$optim$bestmem[11])
    j <- j + 1
  }
}
df_DDM[3:13] <- lapply(df_DDM[3:13], as.numeric)

## DDM predictions ----

### Variables
z <- 0.5  # Starting point (accuracy-coded dataset -> 0.5)
ntrials <- 1000  # Number of decision-making simulations per observation
sigma <- 1  # Within-trial noise
dt <- 0.001  # Precision
df_predictions <- NULL
coherence_vector <- c(0.1, 0.2, 0.4)

### Generate predictions
for (j in 1:nrow(df_DDM)){
  for (coherence in 1:3){
    df_predictions_temp <- data.frame(rep(df_DDM[j,1], each = ntrials),
                                      rep(df_DDM[j,2], each = ntrials),
                                      rep(coherence_vector[coherence], each = ntrials),
                                      DDM_confidence_bounds(v = df_DDM[j, coherence + 2],
                                                            a = df_DDM[j,]$a,
                                                            ter = df_DDM[j,]$ter,
                                                            z = z,
                                                            ntrials = ntrials,
                                                            s = sigma,
                                                            dt = dt,
                                                            a2_upper = df_DDM[j,]$a2_upper,
                                                            a2_lower = df_DDM[j,]$a2_lower,
                                                            postdriftmod = df_DDM[j,]$postdriftmod,
                                                            a2_slope_upper = df_DDM[j,]$a2_slope_upper,
                                                            a2_slope_lower = df_DDM[j,]$a2_slope_lower,
                                                            ter2 = df_DDM[j,]$ter2))
    names(df_predictions_temp) <- c('sub', 'manipulation', 'coherence', 'rt', 'resp', 'cor', 'evidence2', 'rtfull', 'rtconf', 'cj_2')
    a2_separation <- df_DDM[j,]$a2_lower + df_DDM[j,]$a2_upper
    df_predictions_temp$conf_evidence <- ifelse(df_predictions_temp$resp == 1, df_predictions_temp$evidence2 - df_DDM[j,]$a + df_DDM[j,]$a2_lower, (-1) * df_predictions_temp$evidence2 + df_DDM[j,]$a2_lower)
    df_predictions_temp$cj <- cut(df_predictions_temp$conf_evidence, breaks=c(-Inf, a2_separation / 6, 2 * a2_separation / 6, 3 * a2_separation / 6, 4 * a2_separation / 6, 5 * a2_separation / 6, Inf), labels = c(1, 2, 3, 4, 5, 6))
    df_predictions <- rbind(df_predictions, df_predictions_temp)
  }
}

# Visualisations ----


## DDM parameters ----

### v1
ggplot(df_DDM, aes(x = manipulation, y = v1)) +
  geom_point(colour = "#4D5382", alpha = 0.5, size = 5, shape = 16) +
  geom_line(aes(group = sub), alpha = 0.2) +
  stat_summary(aes(y = v1, group = 1), fun = mean, colour="#4D5382", size = 4, shape = 95) +
  scale_x_discrete(labels = c("Accurate decision\nAccurate confidence rating", "Accurate decision\nFast confidence rating", "Fast decision\nAccurate confidence rating", "Fast decision\nFast confidence rating")) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())
  
### v2
ggplot(df_DDM, aes(x = manipulation, y = v2)) +
  geom_point(colour = "#4D5382", alpha = 0.5, size = 5, shape = 16) +
  geom_line(aes(group = sub), alpha = 0.2) +
  stat_summary(aes(y = v2, group = 1), fun = mean, colour="#4D5382", size = 4, shape = 95) +
  scale_x_discrete(labels = c("Accurate decision\nAccurate confidence rating", "Accurate decision\nFast confidence rating", "Fast decision\nAccurate confidence rating", "Fast decision\nFast confidence rating")) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())

### v3
ggplot(df_DDM, aes(x = manipulation, y = v3)) +
  geom_point(colour = "#4D5382", alpha = 0.5, size = 5, shape = 16) +
  geom_line(aes(group = sub), alpha = 0.2) +
  stat_summary(aes(y = v3, group = 1), fun = mean, colour="#4D5382", size = 4, shape = 95) +
  scale_x_discrete(labels = c("Accurate decision\nAccurate confidence rating", "Accurate decision\nFast confidence rating", "Fast decision\nAccurate confidence rating", "Fast decision\nFast confidence rating")) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())

### v comparison
v_mean <- select(df_DDM, v1, v2, v3, manipulation) %>%
  group_by(manipulation) %>% 
  summarise_each(funs(mean)) %>%
  pivot_longer(cols = c(v1, v2, v3))

ggplot(v_mean, aes(x = manipulation, y = value)) +
  geom_line(aes(group = name, colour = name), lty = 5, size = 1) +
  geom_point()

ggplot(df_DDM, aes(x = manipulation)) +
  geom_point(aes(y = v1), colour = 'darkred') +
  geom_point(aes(y = v2), colour = 'blue') +
  geom_point(aes(y = v3), colour = 'green') +
  geom_line(aes(y = v1, group = sub), alpha = 0.2, colour = 'darkred', lty = 5) +
  geom_line(aes(y = v2, group = sub), alpha = 0.2, colour = 'blue', lty = 5) +
  geom_line(aes(y = v3, group = sub), alpha = 0.2, colour = 'green', lty = 5) +
  stat_summary(aes(y = v1, group = 1), fun = mean, colour= 'darkred', size = 4, shape = 95) +
  stat_summary(aes(y = v2, group = 1), fun = mean, colour= 'blue', size = 4, shape = 95) +
  stat_summary(aes(y = v3, group = 1), fun = mean, colour= 'green', size = 4, shape = 95) +
  scale_x_discrete(labels = c("Accurate decision\nAccurate confidence rating", "Accurate decision\nFast confidence rating", "Fast decision\nAccurate confidence rating", "Fast decision\nFast confidence rating")) 

### a
ggplot(df_DDM, aes(x = manipulation, y = a)) +
  geom_point(colour = "#4D5382", alpha = 0.5, size = 5, shape = 16) +
  geom_line(aes(group = sub), alpha = 0.3) +
  stat_summary(aes(y = a, group = 1), fun = mean, colour="#4D5382", size = 4, shape = 95) +
  scale_x_discrete(labels = c("Accurate decision\nAccurate confidence rating", "Accurate decision\nFast confidence rating", "Fast decision\nAccurate confidence rating", "Fast decision\nFast confidence rating")) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())

### a2_upper
ggplot(df_DDM, aes(x = manipulation, y = a2_upper)) +
  geom_point(colour = "#4D5382", alpha = 0.5, size = 5, shape = 16) +
  geom_line(aes(group = sub), alpha = 0.3) +
  stat_summary(aes(y = a2_upper, group = 1), fun = mean, colour="#4D5382", size = 4, shape = 95) +
  scale_x_discrete(labels = c("Accurate decision\nAccurate confidence rating", "Accurate decision\nFast confidence rating", "Fast decision\nAccurate confidence rating", "Fast decision\nFast confidence rating")) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())

### a2_lower
ggplot(df_DDM, aes(x = manipulation, y = a2_lower)) +
  geom_point(colour = "#4D5382", alpha = 0.5, size = 5, shape = 16) +
  geom_line(aes(group = sub), alpha = 0.3) +
  stat_summary(aes(y = a2_lower, group = 1), fun = mean, colour="#4D5382", size = 4, shape = 95) +
  scale_x_discrete(labels = c("Accurate decision\nAccurate confidence rating", "Accurate decision\nFast confidence rating", "Fast decision\nAccurate confidence rating", "Fast decision\nFast confidence rating")) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())

### ter
ggplot(df_DDM, aes(x = manipulation, y = ter)) +
  geom_point(colour = "#4D5382", alpha = 0.5, size = 5, shape = 16) +
  geom_line(aes(group = sub), alpha = 0.3) +
  stat_summary(aes(y = ter, group = 1), fun = mean, colour="#4D5382", size = 4, shape = 95) +
  scale_x_discrete(labels = c("Accurate decision\nAccurate confidence rating", "Accurate decision\nFast confidence rating", "Fast decision\nAccurate confidence rating", "Fast decision\nFast confidence rating")) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  ylab("non-decision time")

### postdriftmod
ggplot(df_DDM, aes(x = manipulation, y = postdriftmod)) +
  geom_point(colour = "#4D5382", alpha = 0.5, size = 5, shape = 16) +
  geom_line(aes(group = sub), alpha = 0.3) +
  stat_summary(aes(y = postdriftmod, group = 1), fun = mean, colour="#4D5382", size = 4, shape = 95) +
  scale_x_discrete(labels = c("Accurate decision\nAccurate confidence rating", "Accurate decision\nFast confidence rating", "Fast decision\nAccurate confidence rating", "Fast decision\nFast confidence rating")) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  ylab("postdriftmod")

### a2_slope_upper
ggplot(df_DDM, aes(x = manipulation, y = a2_slope_upper)) +
  ylim(c(0,15)) +
  geom_point(colour = "#4D5382", alpha = 0.5, size = 2, shape = 16) +
  geom_line(aes(group = sub), alpha = 0.3) +
  stat_summary(aes(y = a2_slope_upper, group = 1), fun = mean, colour="#4D5382", size = 4, shape = 95) +
  scale_x_discrete(labels = c("Accurate decision\nAccurate confidence rating", "Accurate decision\nFast confidence rating", "Fast decision\nAccurate confidence rating", "Fast decision\nFast confidence rating")) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  ylab("urgency upper")

### a2_slope_lower
ggplot(df_DDM, aes(x = manipulation, y = a2_slope_lower)) +
  ylim(c(0,15)) +
  geom_point(colour = "#4D5382", alpha = 0.5, size = 2, shape = 16) +
  geom_line(aes(group = sub), alpha = 0.3) +
  stat_summary(aes(y = a2_slope_lower, group = 1), fun = mean, colour="#4D5382", size = 4, shape = 95) +
  scale_x_discrete(labels = c("Accurate decision\nAccurate confidence rating", "Accurate decision\nFast confidence rating", "Fast decision\nAccurate confidence rating", "Fast decision\nFast confidence rating")) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  ylab("urgency lower")

### ter2
ggplot(df_DDM, aes(x = manipulation, y = ter2)) +
  geom_point(colour = "#4D5382", alpha = 0.5, size = 5, shape = 16) +
  geom_line(aes(group = sub), alpha = 0.3) +
  stat_summary(aes(y = ter2, group = 1), fun = mean, colour="#4D5382", size = 4, shape = 95) +
  scale_x_discrete(labels = c("Accurate decision\nAccurate confidence rating", "Accurate decision\nFast confidence rating", "Fast decision\nAccurate confidence rating", "Fast decision\nFast confidence rating")) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  ylab("non-decision time 2")

## DDM full predictions ----

par(mfrow = c(4, 3), mai = c(0.3, 0.3, 0.3, 0.3))

### Decision RT ----

for (cond in 1:4){  # Loop through conditions
  for (coherence in 1:3){  # Loop through coherence levels

    # Draw plots
    tempC <- hist(df_obs$rt[df_obs$cor == 1 & df_obs$coherence == coherence_vector[coherence] & df_obs$manipulation == condLab[cond]], 
                  breaks=seq(0,6.2,.1), xlim = c(0,3), ylim = c(0,300), prob = F, col = rgb(0,1,0,.25), border = "white", ylab = "", xlab = "", cex.lab = 2, cex.main = 1.5, cex.axis = 1.5, main = "")
    tempE <- hist(df_obs$rt[df_obs$cor == 0 & df_obs$coherence == coherence_vector[coherence] & df_obs$manipulation == condLab[cond]], 
                  breaks=seq(0,6.2,.1), prob = F, add = T, col = rgb(1,0,0,.25), border = 'white')
    Cors <- hist(df_predictions$rt[df_obs$cor == 1 & df_obs$coherence == coherence_vector[coherence] & df_obs$manipulation == condLab[cond]], 
                 breaks = seq(0,30,.1), plot = F)
    Errs <- hist(df_predictions$rt[df_obs$cor == 0 & df_obs$coherence == coherence_vector[coherence] & df_obs$manipulation == condLab[cond]], 
                 breaks = seq(0,30,.1), plot = F)
    lines(Cors$counts/(sum(Cors$counts)/sum(tempC$counts))~ Cors$mids, type='l', col = 'green', lwd = 3)
    lines(Errs$counts/(sum(Errs$counts)/sum(tempE$counts))~ Errs$mids, type='l', col = 'red', lwd = 3)
    #legend("topright",fill=c("white","white","green","red"),border=F,legend=c("Simulated corrects","Simulated errors","Empirical corrects","Empirical errors"),col=rep(c("Green","Red"),2),bty='n',lwd=c(1,1,-1,-1))

  }
}

### Confidence RT (accuracy) ----

for (cond in 1:4){  # Loop through conditions
  for (coherence in 1:3){  # Loop through coherence levels
    
    # Draw plots
    tempC <- hist(df_obs$rtconf[df_obs$cor == 1 & df_obs$coherence == coherence_vector[coherence] & df_obs$manipulation == condLab[cond]], 
                  breaks=seq(0,6.2,.1), xlim = c(0,3), ylim = c(0,300), prob = F, col = rgb(0,1,0,.25), border = "white", ylab = "", xlab = "", cex.lab = 2, cex.main = 1.5, cex.axis = 1.5, main = "")
    tempE <- hist(df_obs$rtconf[df_obs$cor == 0 & df_obs$coherence == coherence_vector[coherence] & df_obs$manipulation == condLab[cond]], 
                  breaks=seq(0,6.2,.1), prob = F, add = T, col = rgb(1,0,0,.25), border = 'white')
    Cors <- hist(df_predictions$rtconf[df_obs$cor == 1 & df_obs$coherence == coherence_vector[coherence] & df_obs$manipulation == condLab[cond]], 
                 breaks = seq(-2,30,.1), plot = F)
    Errs <- hist(df_predictions$rtconf[df_obs$cor == 0 & df_obs$coherence == coherence_vector[coherence] & df_obs$manipulation == condLab[cond]], 
                 breaks = seq(-2,30,.1), plot = F)
    lines(Cors$counts/(sum(Cors$counts)/sum(tempC$counts))~ Cors$mids, type='l', col = 'green', lwd = 3)
    lines(Errs$counts/(sum(Errs$counts)/sum(tempE$counts))~ Errs$mids, type='l', col = 'red', lwd = 3)
    #legend("topright",fill=c("white","white","green","red"),border=F,legend=c("Simulated corrects","Simulated errors","Empirical corrects","Empirical errors"),col=rep(c("Green","Red"),2),bty='n',lwd=c(1,1,-1,-1))
    
  }
}

### Confidence RT (cj) ----

#### Scale predictions 
df_predictions_proportions <- data.frame(table(df_predictions$cj, df_predictions$cor))
names(df_predictions_proportions) <- c('cj', 'cor', 'freq_pred')
df_predictions_proportions$freq_scaled_pred <- as.numeric(df_predictions_proportions$freq) * nrow(df_obs) / (nrow(df_predictions) * 40)

#### Calculate error bars predictions
prediction_bars <- NULL
for (conf in 1:6){
  for (accuracy in 0:1){
    df_predictions_temp <- df_predictions %>% filter(cj == conf & cor == accuracy)
    prediction_table_temp <- table(df_predictions_temp$sub) * nrow(df_obs) / nrow(df_predictions)
    prediction_bars_temp <- c(conf,
                              accuracy,
                              sum(prediction_table_temp) / 40 + sd(prediction_table_temp)/sqrt(40),
                              sum(prediction_table_temp) / 40 - sd(prediction_table_temp)/sqrt(40))
    prediction_bars <- rbind(prediction_bars, prediction_bars_temp)
  }
}
prediction_bars <- data.frame(prediction_bars)
names(prediction_bars) <- c('cj', 'cor', 'ymax_pred', 'ymin_pred')
prediction_bars <- merge(prediction_bars, df_predictions_proportions, by = c('cj', 'cor'))

#### Scale observations
df_observations_proportions <- data.frame(table(df_obs$cj, df_obs$cor))
names(df_observations_proportions) <- c('cj', 'cor', 'freq_obs')
df_observations_proportions$freq_scaled_obs <- as.numeric(df_observations_proportions$freq) / 40

#### Calculate error bars observations
observation_bars <- NULL
for (conf in 1:6){
  for (accuracy in 0:1){
    df_observations_temp <- df_obs %>% filter(cj == conf & cor == accuracy)
    observation_table_temp <- table(df_observations_temp$sub) 
    observation_bars_temp <- c(conf,
                              accuracy,
                              sum(observation_table_temp) / 40 + sd(observation_table_temp)/sqrt(40),
                              sum(observation_table_temp) / 40 - sd(observation_table_temp)/sqrt(40))
    observation_bars <- rbind(observation_bars, observation_bars_temp)
  }
}
observation_bars <- data.frame(observation_bars)
names(observation_bars) <- c('cj', 'cor', 'ymax_obs', 'ymin_obs')
observation_bars <- merge(observation_bars, df_observations_proportions, by = c('cj', 'cor'))

#### Plot
df <- merge(observation_bars, prediction_bars, by = c('cj', 'cor'))
ggplot(data = df, aes(x = as.factor(cj), y = freq_scaled_obs, fill = as.factor(cor))) +
  geom_bar(stat = 'identity', position = position_dodge(), color = 'black') +
  geom_errorbar(aes(ymin = ymin_obs, ymax = ymax_obs), colour = 'black', size = 0.4, position = position_dodge(1.2), width = 0.2) +
  geom_errorbar(aes(ymin = ymin_pred, ymax = ymax_pred), colour = 'black', size = 0.4, position = position_dodge(0.6), width = 0.2) +
  scale_fill_manual(name = 'Accuracy', 
                    values = c('#C0392B', '#27AE60'),
                    labels = c('Error', 'Correct')) +
  geom_point(aes(x = cj, y = freq_scaled_pred), position = position_dodge(width = 0.60),
             size = 0.7, stroke = 1, color = 'black') +
  theme_bw()



# Confidence rating RT's (high vs. low cj)
# Loop through manipulations

for (j in 1:4){
  
  # Loop through coherence levels
  
  for (k in 1:3){
    
    conf_predicted_1 <- NULL
    conf_predicted_2 <- NULL
    conf_predicted_3 <- NULL
    conf_predicted_4 <- NULL
    conf_predicted_5 <- NULL
    conf_predicted_6 <- NULL
        
    conf_observed_1_temp <- conf_observed_1 %>% filter(manipulation == manipulation_vector[j] & coherence == coherence_vector[k])
    conf_observed_2_temp <- conf_observed_2 %>% filter(manipulation == manipulation_vector[j] & coherence == coherence_vector[k])
    conf_observed_3_temp <- conf_observed_3 %>% filter(manipulation == manipulation_vector[j] & coherence == coherence_vector[k])
    conf_observed_4_temp <- conf_observed_4 %>% filter(manipulation == manipulation_vector[j] & coherence == coherence_vector[k])
    conf_observed_5_temp <- conf_observed_5 %>% filter(manipulation == manipulation_vector[j] & coherence == coherence_vector[k])
    conf_observed_6_temp <- conf_observed_6 %>% filter(manipulation == manipulation_vector[j] & coherence == coherence_vector[k])
    
        
    # Loop through participants
    
    for (i in (1:n)){
      
      # Select correct estimated parameters
      
      df_temp <- df %>% filter(sub == i & manipulation == manipulation_vector[j])
      v <- df_temp[[k + 2]]
      
      # Simulate data    
      predictions <- data.frame(DDM_confidence_bounds(v = v, a = df_temp$a, ter = df_temp$ter, z = z, ntrials = ntrials, s = sigma, dt = dt, a2 = df_temp$a2, postdriftmod = df_temp$postdriftmod, a2_slope = df_temp$a2_slope, ter2 = df_temp$ter2))
      names(predictions) <- c('rt', 'resp', 'cor', 'evidence2', 'rtfull', 'rtconf', 'cj')
      
      # Separate predictions according the the cj
      
      conf_predicted_1_temp <- predictions[predictions$cj == 1,]
      conf_predicted_2_temp <- predictions[predictions$cj == 2,]
      conf_predicted_3_temp <- predictions[predictions$cj == 3,]
      conf_predicted_4_temp <- predictions[predictions$cj == 4,]
      conf_predicted_5_temp <- predictions[predictions$cj == 5,]
      conf_predicted_6_temp <- predictions[predictions$cj == 6,]
      
      # Merge predictions
    
      conf_predicted_1 <- rbind(conf_predicted_1, conf_predicted_1_temp)
      conf_predicted_2 <- rbind(conf_predicted_2, conf_predicted_2_temp)
      conf_predicted_3 <- rbind(conf_predicted_3, conf_predicted_3_temp)
      conf_predicted_4 <- rbind(conf_predicted_4, conf_predicted_4_temp)
      conf_predicted_5 <- rbind(conf_predicted_5, conf_predicted_5_temp)
      conf_predicted_6 <- rbind(conf_predicted_6, conf_predicted_6_temp)

      #print(i)
    }
    
    # Draw plots
    
    temp1 <- hist(conf_observed_1_temp$rtconf, breaks=seq(0,6.2,.1), xlim = c(0,1.5), ylim = c(0,150), prob = F, col = rgb(0.5,0,0,.25), border = "white", ylab = "", xlab = "", cex.lab = 2, cex.main = 1.5, cex.axis = 1.5, main = "")
    temp2 <- hist(conf_observed_2_temp$rtconf, breaks=seq(0,6.2,.1), prob = F, add = T, col = rgb(0.5,0,0,.25), border = 'white')
    temp3 <- hist(conf_observed_3_temp$rtconf, breaks=seq(0,6.2,.1), prob = F, add = T, col = rgb(0.5,0,0,.25), border = 'white')
    temp4 <- hist(conf_observed_4_temp$rtconf, breaks=seq(0,6.2,.1), prob = F, add = T, col = rgb(0.5,0,0,.25), border = 'white')
    temp5 <- hist(conf_observed_5_temp$rtconf, breaks=seq(0,6.2,.1), prob = F, add = T, col = rgb(0.5,0,0,.25), border = 'white')
    temp6 <- hist(conf_observed_6_temp$rtconf, breaks=seq(0,6.2,.1), prob = F, add = T, col = rgb(0.5,0,0,.25), border = 'white')
    pred1 <- hist(conf_predicted_1$rtconf, breaks = seq(-2,30,.1), plot = F)
    pred2 <- hist(conf_predicted_2$rtconf, breaks = seq(-2,30,.1), plot = F)
    pred3 <- hist(conf_predicted_3$rtconf, breaks = seq(-2,30,.1), plot = F)
    pred4 <- hist(conf_predicted_4$rtconf, breaks = seq(-2,30,.1), plot = F)
    pred5 <- hist(conf_predicted_5$rtconf, breaks = seq(-2,30,.1), plot = F)
    pred6 <- hist(conf_predicted_6$rtconf, breaks = seq(-2,30,.1), plot = F)
    lines(pred1$counts/(sum(pred1$counts)/sum(temp1$counts))~pred1$mids,type='l',col='red',lwd=2)
    lines(pred2$counts/(sum(pred2$counts)/sum(temp2$counts))~pred2$mids,type='l',col='red',lwd=2)
    lines(pred3$counts/(sum(pred3$counts)/sum(temp3$counts))~pred3$mids,type='l',col='red',lwd=2)
    lines(pred4$counts/(sum(pred4$counts)/sum(temp4$counts))~pred4$mids,type='l',col='red',lwd=2)
    lines(pred5$counts/(sum(pred5$counts)/sum(temp5$counts))~pred5$mids,type='l',col='red',lwd=2)
    lines(pred6$counts/(sum(pred6$counts)/sum(temp6$counts))~pred6$mids,type='l',col='red',lwd=2)
    #legend("topright",fill=c("white","white","green","red"),border=F,legend=c("Simulated corrects","Simulated errors","Empirical corrects","Empirical errors"),col=rep(c("Green","Red"),2),bty='n',lwd=c(1,1,-1,-1))
    
    print(paste0("Proportion of negative RT's: ", (nrow(high_conf_predicted[high_conf_predicted$rtconf < 0,]) + nrow(low_conf_predicted[low_conf_predicted$rtconf < 0,]))/(nrow(high_conf_predicted) + nrow(low_conf_predicted))))
    
    #hist(high_conf_predicted$rtconf, ylim = c(0, 56000), xlim = c(0, 1.5), breaks=seq(-2,20,.1))
    #hist(low_conf_predicted$rtconf, ylim = c(0, 56000), xlim = c(0, 1.5), breaks=seq(-2,20,.1))
  }
  
}

# Confidence rating RT's (after correct vs. wrong decision)
# Loop through manipulations

for (j in 1:4){
  
  # Loop through coherence levels
  
  for (k in 1:3){
    
    c_conf_predicted <- NULL
    e_conf_predicted <- NULL
    
    c_conf_observed_temp <- c_observed %>% filter(manipulation == manipulation_vector[j] & coherence == coherence_vector[k])
    e_conf_observed_temp <- e_observed %>% filter(manipulation == manipulation_vector[j] & coherence == coherence_vector[k])
    
    # Loop through participants
    
    for (i in (1:n)){
      
      # Select correct estimated parameters
      
      df_temp <- df %>% filter(sub == i & manipulation == manipulation_vector[j])
      v <- df_temp[[k + 2]]
      
      # Simulate data    
      predictions <- data.frame(DDM_confidence_bounds(v = v, a = df_temp$a, ter = df_temp$ter, z = z, ntrials = ntrials, s = sigma, dt = dt, a2_upper = df_temp$a2_upper, a2_lower = df_temp$a2_lower, postdriftmod = df_temp$postdriftmod, a2_slope_upper = df_temp$a2_slope_upper, a2_slope_lower = df_temp$a2_slope_lower, ter2 = df_temp$ter2))
      names(predictions) <- c('rt', 'resp', 'cor', 'evidence2', 'rtfull', 'rtconf', 'cj')
      
      # Separate predictions according the the cj
      
      c_conf_predicted_temp <- predictions[predictions$cor == 1,]
      e_conf_predicted_temp <- predictions[predictions$cor == 0,]
      
      # Merge predictions
      
      c_conf_predicted <- rbind(c_conf_predicted, c_conf_predicted_temp)
      e_conf_predicted <- rbind(e_conf_predicted, e_conf_predicted_temp)
      
      #print(i)
    }
    
    # Draw plots
    
    tempC <- hist(c_conf_observed_temp$rtconf, breaks=seq(0,6.2,.1), xlim = c(0,1.5), ylim = c(0,500), prob = F, col = rgb(0,1,0,.25), border = "white", ylab = "", xlab = "", cex.lab = 2, cex.main = 1.5, cex.axis = 1.5, main = "")
    tempE <- hist(e_conf_observed_temp$rtconf, breaks=seq(0,6.2,.1), prob = F, add = T, col = rgb(1,0,0,.25), border = 'white')
    Cors <- hist(c_conf_predicted$rtconf, breaks = seq(-2,30,.1), plot = F)
    Errs <- hist(e_conf_predicted$rtconf, breaks = seq(-2,30,.1),plot=F)
    lines(Cors$counts/(sum(Cors$counts)/sum(tempC$counts))~Cors$mids,type='l',col='green',lwd=3)
    lines(Errs$counts/(sum(Errs$counts)/sum(tempE$counts))~Errs$mids,type='l',col='red',lwd=3)
    #legend("topright",fill=c("white","white","green","red"),border=F,legend=c("Simulated corrects","Simulated errors","Empirical corrects","Empirical errors"),col=rep(c("Green","Red"),2),bty='n',lwd=c(1,1,-1,-1))
    
    #hist(high_conf_predicted$rtconf, ylim = c(0, 56000), xlim = c(0, 1.5), breaks=seq(-2,20,.1))
    #hist(low_conf_predicted$rtconf, ylim = c(0, 56000), xlim = c(0, 1.5), breaks=seq(-2,20,.1))
  }
  
}

## DDM marginal predictions ----

par(mfrow = c(1, 3), mai = c(0.3, 0.3, 0.7, 0.3))

### Decision RT - coherence ----

### Decision RT - manipulations ----

### Confidence RT - coherence ----

### Confidence RT - manipulations ----





# RT's
# Loop through manipulations

for (k in 1:3){
  
  c_observed_temp <- c_observed %>% filter(coherence == coherence_vector[k])
  e_observed_temp <- e_observed %>% filter(coherence == coherence_vector[k])
  
  c_predicted_full <- NULL
  e_predicted_full <- NULL
  
  # Loop through coherence levels
  
  for (j in 1:4){
    
    c_predicted <- NULL
    e_predicted <- NULL
    
    # Loop through participants
    
    for (i in (1:n)){
      
      # Select correct estimated parameters
      
      df_temp <- df %>% filter(sub == i & manipulation == manipulation_vector[j])
      v <- df_temp[[k + 2]]
      
      # Simulate data     
      predictions <- data.frame(DDM_confidence_bounds(v = v, a = df_temp$a, ter = df_temp$ter, z = z, ntrials = ntrials, s = sigma, dt = dt, a2 = df_temp$a2, postdriftmod = df_temp$postdriftmod, a2_slope = df_temp$a2_slope, ter2 = df_temp$ter2))
      names(predictions) <- c('rt', 'resp', 'cor', 'evidence2', 'rtfull', 'rtconf', 'cj')
      
      # Separate predictions according to the response
      
      c_predicted_temp <- predictions[predictions$cor == 1,]
      e_predicted_temp <- predictions[predictions$cor == 0,]
      
      # Merge predictions
      
      c_predicted <- rbind(c_predicted, c_predicted_temp)
      e_predicted <- rbind(e_predicted, e_predicted_temp)
      print(i)
      
    }
    
    c_predicted_full <- rbind(c_predicted, c_predicted_full)
    e_predicted_full <- rbind(e_predicted, e_predicted_full)
    
  }
  
  labels <- c('Coherence: 0.1', 'Coherence: 0.2', 'Coherence: 0.4')
  
  tempC <- hist(c_observed_temp$rt, breaks=seq(0,6.2,.3), xlim = c(0,3), ylim = c(0,2700), prob = F, col = rgb(0,1,0,.25), border = "white", ylab = "", xlab = "", cex.lab = 2, cex.main = 1.5, cex.axis = 1.5, main = labels[k], yaxt="n")
  tempE <- hist(e_observed_temp$rt, breaks=seq(0,6.2,.3), prob = F, add = T, col = rgb(1,0,0,.25), border = 'white', yaxt="n")
  Cors <- hist(c_predicted_full$rt, breaks = seq(0,30,.3), plot = F)
  Errs <- hist(e_predicted_full$rt, breaks = seq(0,30,.3),plot=F)
  lines(Cors$counts/(sum(Cors$counts)/sum(tempC$counts))~Cors$mids,type='l',col='green',lwd=3)
  lines(Errs$counts/(sum(Errs$counts)/sum(tempE$counts))~Errs$mids,type='l',col='red',lwd=3)
  
  #legend("topright",fill=c("white","white","green","red"),border=F,legend=c("Simulated corrects","Simulated errors","Empirical corrects","Empirical errors"),col=rep(c("Green","Red"),2),bty='n',lwd=c(1,1,-1,-1))
  
  
}




# Confidence rating RT's (high vs. low cj)
# Loop through manipulations

for (k in 1:3){
  
  high_conf_observed_temp <- high_conf_observed %>% filter(coherence == coherence_vector[k])
  low_conf_observed_temp <- low_conf_observed %>% filter(coherence == coherence_vector[k])
  
  high_conf_predicted_full <- NULL
  low_conf_predicted_full <- NULL
  
  # Loop through coherence levels
  
  for (j in 1:4){
    
    high_conf_predicted <- NULL
    low_conf_predicted <- NULL
    
    # Loop through participants
    
    for (i in (1:n)){
      
      # Select correct estimated parameters
      
      df_temp <- df %>% filter(sub == i & manipulation == manipulation_vector[j])
      v <- df_temp[[k + 2]]
      
      # Simulate data    
      predictions <- data.frame(DDM_confidence_bounds(v = v, a = df_temp$a, ter = df_temp$ter, z = z, ntrials = ntrials, s = sigma, dt = dt, a2 = df_temp$a2, postdriftmod = df_temp$postdriftmod, a2_slope = df_temp$a2_slope, ter2 = df_temp$ter2))
      names(predictions) <- c('rt', 'resp', 'cor', 'evidence2', 'rtfull', 'rtconf', 'cj')
      
      # Separate predictions according the the cj
      
      high_conf_predicted_temp <- predictions[predictions$cj == 1,]
      low_conf_predicted_temp <- predictions[predictions$cj == 0,]
      
      # Merge predictions
      
      high_conf_predicted <- rbind(high_conf_predicted, high_conf_predicted_temp)
      low_conf_predicted <- rbind(low_conf_predicted, low_conf_predicted_temp)
      
      #print(i)
    }
    
    high_conf_predicted_full <- rbind(high_conf_predicted, high_conf_predicted_full)
    low_conf_predicted_full <- rbind(low_conf_predicted, low_conf_predicted_full)
    
  }
  
  labels <- c('Coherence: 0.1', 'Coherence: 0.2', 'Coherence: 0.4')
  
  # Draw plots
  
  tempC <- hist(high_conf_observed_temp$rtconf, breaks=seq(0,6.2,.15), xlim = c(0,2), ylim = c(0,2000), prob = F, col = rgb(0.952941, 0.611765, 0.070588, .35), border = "white", ylab = "", xlab = "", cex.lab = 2, cex.main = 1.5, cex.axis = 1.5, main = labels[k], yaxt="n")
  tempE <- hist(low_conf_observed_temp$rtconf, breaks=seq(0,6.2,.15), prob = F, add = T, col = rgb(0.121569,0.380392,0.552941,.35), border = 'white')
  Cors <- hist(high_conf_predicted_full$rtconf, breaks = seq(-2,30,.15), plot = F)
  Errs <- hist(low_conf_predicted_full$rtconf, breaks = seq(-2,30,.15),plot=F)
  lines(Cors$counts/(sum(Cors$counts)/sum(tempC$counts))~Cors$mids,type='l',col='#F39C12',lwd=3)
  lines(Errs$counts/(sum(Errs$counts)/sum(tempE$counts))~Errs$mids,type='l',col='#1F618D',lwd=3)
  #legend("topright",fill=c("white","white","green","red"),border=F,legend=c("Simulated corrects","Simulated errors","Empirical corrects","Empirical errors"),col=rep(c("Green","Red"),2),bty='n',lwd=c(1,1,-1,-1))
  
}


# Confidence rating RT's (after correct vs. wrong decision)
# Loop through manipulations

for (k in 1:3){
  
  c_conf_observed_temp <- c_observed %>% filter(coherence == coherence_vector[k])
  e_conf_observed_temp <- e_observed %>% filter(coherence == coherence_vector[k])
  
  c_conf_predicted_full <- NULL
  e_conf_predicted_full <- NULL
  
  # Loop through coherence levels
  
  for (j in 1:4){
    
    c_conf_predicted <- NULL
    e_conf_predicted <- NULL
    
    # Loop through participants
    
    for (i in (1:n)){
      
      # Select correct estimated parameters
      
      df_temp <- df %>% filter(sub == i & manipulation == manipulation_vector[j])
      v <- df_temp[[k + 2]]
      
      # Simulate data    
      predictions <- data.frame(DDM_confidence_bounds(v = v, a = df_temp$a, ter = df_temp$ter, z = z, ntrials = ntrials, s = sigma, dt = dt, a2 = df_temp$a2, postdriftmod = df_temp$postdriftmod, a2_slope = df_temp$a2_slope, ter2 = df_temp$ter2))
      names(predictions) <- c('rt', 'resp', 'cor', 'evidence2', 'rtfull', 'rtconf', 'cj')
      
      # Separate predictions according the the cj
      
      c_conf_predicted_temp <- predictions[predictions$cor == 1,]
      e_conf_predicted_temp <- predictions[predictions$cor == 0,]
      
      # Merge predictions
      
      c_conf_predicted <- rbind(c_conf_predicted, c_conf_predicted_temp)
      e_conf_predicted <- rbind(e_conf_predicted, e_conf_predicted_temp)
      
      #print(i)
    }
    
    c_conf_predicted_full <- rbind(c_conf_predicted, c_conf_predicted_full)
    e_conf_predicted_full <- rbind(e_conf_predicted, e_conf_predicted_full)
    
  }
  
  labels <- c('Coherence: 0.1', 'Coherence: 0.2', 'Coherence: 0.4')
  
  # Draw plots
  
  tempC <- hist(c_conf_observed_temp$rtconf, breaks=seq(0,6.2,.15), xlim = c(0,2), ylim = c(0,2200), prob = F, col = rgb(0.952941, 0.611765, 0.070588, .35), border = "white", ylab = "", xlab = "", cex.lab = 2, cex.main = 1.5, cex.axis = 1.5, main = labels[k], yaxt="n")
  tempE <- hist(e_conf_observed_temp$rtconf, breaks=seq(0,6.2,.15), prob = F, add = T, col = rgb(0.121569,0.380392,0.552941,.35), border = 'white')
  Cors <- hist(c_conf_predicted_full$rtconf, breaks = seq(-2,30,.15), plot = F)
  Errs <- hist(e_conf_predicted_full$rtconf, breaks = seq(-2,30,.15),plot=F)
  lines(Cors$counts/(sum(Cors$counts)/sum(tempC$counts))~Cors$mids,type='l',col='#F39C12',lwd=3)
  lines(Errs$counts/(sum(Errs$counts)/sum(tempE$counts))~Errs$mids,type='l',col='#1F618D',lwd=3)
  #legend("topright",fill=c("white","white","green","red"),border=F,legend=c("Simulated corrects","Simulated errors","Empirical corrects","Empirical errors"),col=rep(c("Green","Red"),2),bty='n',lwd=c(1,1,-1,-1))
  
}

