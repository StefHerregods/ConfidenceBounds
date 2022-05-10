

# To do ----

# Error bars

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

## Give R access to the DDM simulation function in C++
sourceCpp("C:\\Users\\herre\\OneDrive\\Documenten\\GitHub\\ConfidenceBounds\\Analyses\\Exp1_analyses\\DDM_confidence_bounds_separated_2.cpp") 


# Load data ----


## Behavioral data ----

df_obs <- read.csv(file = "Exp2_data_viable.csv")
df_obs <- df_obs %>% mutate(rt_manipulation = ifelse(df_obs$manipulation %in% c("AccAcc", "AccFast"), 1, 0),
                            rtconf_manipulation = ifelse(df_obs$manipulation %in% c("AccAcc", "FastAcc"), 1, 0))

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
pb = txtProgressBar(min = 0, max = 160, initial = 0, style = 3) 
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
    setTxtProgressBar(pb, j)
  }
}
close(pb)
df_predictions <- df_predictions %>% mutate(rt_manipulation = ifelse(df_predictions$manipulation %in% c("AccAcc", "AccFast"), 1, 0),
                                            rtconf_manipulation = ifelse(df_predictions$manipulation %in% c("AccAcc", "FastAcc"), 1, 0))


# Visualizations ----

cj_mean_obs <- df_obs %>%
  group_by(sub, cj) %>% 
  summarise_each(funs(mean))
cj_mean_pred <- df_predictions %>%
  group_by(sub, cj) %>% 
  summarise_each(funs(mean))
ggplot(cj_mean_obs, aes(x = as.factor(cj), y = rtconf)) +
  geom_point() +
  stat_summary(aes(y = rtconf, group = 1), fun = mean, colour="Black", size = 4, shape = 95) +
  geom_point(data = cj_mean_pred, aes(x = as.factor(cj), y = rtconf), color = 'red', position = position_nudge(0.2)) +
  stat_summary(data = cj_mean_pred, aes(y = rtconf, group = 1), fun = mean, colour="Red", size = 4, shape = 95, position = position_nudge(0.2)) 


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
    Cors <- hist(df_predictions$rt[df_predictions$cor == 1 & df_predictions$coherence == coherence_vector[coherence] & df_predictions$manipulation == condLab[cond]], 
                 breaks = seq(0,30,.1), plot = F)
    Errs <- hist(df_predictions$rt[df_predictions$cor == 0 & df_predictions$coherence == coherence_vector[coherence] & df_predictions$manipulation == condLab[cond]], 
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
    Cors <- hist(df_predictions$rtconf[df_predictions$cor == 1 & df_predictions$coherence == coherence_vector[coherence] & df_predictions$manipulation == condLab[cond]], 
                 breaks = seq(-2,30,.1), plot = F)
    Errs <- hist(df_predictions$rtconf[df_predictions$cor == 0 & df_predictions$coherence == coherence_vector[coherence] & df_predictions$manipulation == condLab[cond]], 
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

#### Plot together
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

#### Plot correct
observation_bars_2 <- observation_bars
names(observation_bars_2) <- c('cj', 'cor', 'ymax', 'ymin', 'freq', 'freq_scaled')
observation_bars_2$data <- 'obs'
prediction_bars_2 <- prediction_bars
names(prediction_bars_2) <- c('cj', 'cor', 'ymax', 'ymin', 'freq', 'freq_scaled')
prediction_bars_2$data <- 'pred'
df_2 <- rbind(observation_bars_2, prediction_bars_2)
ggplot(data = df_2[df_2$cor == 1,], aes(x = as.factor(cj), y = freq_scaled, fill = data)) +
  geom_bar(stat = 'identity', position = position_dodge(), color = 'black') +
  geom_errorbar(aes(ymin = ymin, ymax = ymax), colour = 'black', size = 0.4, position = position_dodge(0.9), width = 0.2) +
  scale_fill_manual(name = '', 
                    values = c('#F18F01', '#006E90'),
                    labels = c('Observed', 'Predicted')) +
  theme_bw()

#### Plot incorrect
ggplot(data = df_2[df_2$cor == 0,], aes(x = as.factor(cj), y = freq_scaled, fill = data)) +
  geom_bar(stat = 'identity', position = position_dodge(), color = 'black') +
  geom_errorbar(aes(ymin = ymin, ymax = ymax), colour = 'black', size = 0.4, position = position_dodge(0.9), width = 0.2) +
  scale_fill_manual(name = '', 
                    values = c('#F18F01', '#006E90'),
                    labels = c('Observed', 'Predicted')) +
  theme_bw()

## DDM marginal predictions ----

### Decision RT - manipulations ----

par(mfrow = c(1, 4), mai = c(0.3, 0.3, 0.7, 0.3))

for (cond in 1:4){  # Loop through conditions

    # Draw plots
    tempC <- hist(df_obs$rt[df_obs$cor == 1 & df_obs$manipulation == condLab[cond]], 
                  breaks=seq(0,6.2,.1), xlim = c(0,3), ylim = c(0,700), prob = F, col = rgb(0,1,0,.25), border = "white", ylab = "", xlab = "", cex.lab = 2, cex.main = 1.5, cex.axis = 1.5, main = "")
    tempE <- hist(df_obs$rt[df_obs$cor == 0 & df_obs$manipulation == condLab[cond]], 
                  breaks=seq(0,6.2,.1), prob = F, add = T, col = rgb(1,0,0,.25), border = 'white')
    Cors <- hist(df_predictions$rt[df_predictions$cor == 1 & df_predictions$manipulation == condLab[cond]], 
                 breaks = seq(0,30,.1), plot = F)
    Errs <- hist(df_predictions$rt[df_predictions$cor == 0 & df_predictions$manipulation == condLab[cond]], 
                 breaks = seq(0,30,.1), plot = F)
    lines(Cors$counts/(sum(Cors$counts)/sum(tempC$counts))~ Cors$mids, type='l', col = 'green', lwd = 3)
    lines(Errs$counts/(sum(Errs$counts)/sum(tempE$counts))~ Errs$mids, type='l', col = 'red', lwd = 3)
    #legend("topright",fill=c("white","white","green","red"),border=F,legend=c("Simulated corrects","Simulated errors","Empirical corrects","Empirical errors"),col=rep(c("Green","Red"),2),bty='n',lwd=c(1,1,-1,-1))
    
}

### Decision RT - coherence ----

par(mfrow = c(1, 3), mai = c(0.3, 0.3, 0.7, 0.3))

for (coherence in 1:3){  # Loop through coherence levels
  
  # Draw plots
  tempC <- hist(df_obs$rt[df_obs$cor == 1 & df_obs$coherence == coherence_vector[coherence]], 
                breaks=seq(0,6.2,.1), xlim = c(0,3), ylim = c(0,1000), prob = F, col = rgb(0,1,0,.25), border = "white", ylab = "", xlab = "", cex.lab = 2, cex.main = 1.5, cex.axis = 1.5, main = "")
  tempE <- hist(df_obs$rt[df_obs$cor == 0 & df_obs$coherence == coherence_vector[coherence]], 
                breaks=seq(0,6.2,.1), prob = F, add = T, col = rgb(1,0,0,.25), border = 'white')
  Cors <- hist(df_predictions$rt[df_predictions$cor == 1 & df_predictions$coherence == coherence_vector[coherence]], 
               breaks = seq(0,30,.1), plot = F)
  Errs <- hist(df_predictions$rt[df_predictions$cor == 0 & df_predictions$coherence == coherence_vector[coherence]], 
               breaks = seq(0,30,.1), plot = F)
  lines(Cors$counts/(sum(Cors$counts)/sum(tempC$counts))~ Cors$mids, type='l', col = 'green', lwd = 3)
  lines(Errs$counts/(sum(Errs$counts)/sum(tempE$counts))~ Errs$mids, type='l', col = 'red', lwd = 3)
  #legend("topright",fill=c("white","white","green","red"),border=F,legend=c("Simulated corrects","Simulated errors","Empirical corrects","Empirical errors"),col=rep(c("Green","Red"),2),bty='n',lwd=c(1,1,-1,-1))
  
}

### Confidence RT - manipulations ----

par(mfrow = c(1, 4), mai = c(0.3, 0.3, 0.7, 0.3))

for (cond in 1:4){  # Loop through conditions
  
  # Draw plots
  tempC <- hist(df_obs$rtconf[df_obs$cor == 1 & df_obs$manipulation == condLab[cond]], 
                breaks=seq(0,6.2,.3), xlim = c(0,3), ylim = c(0,1500), prob = F, col = rgb(0,1,0,.25), border = "white", ylab = "", xlab = "", cex.lab = 2, cex.main = 1.5, cex.axis = 1.5, main = "")
  tempE <- hist(df_obs$rtconf[df_obs$cor == 0 & df_obs$manipulation == condLab[cond]], 
                breaks=seq(0,6.2,.3), prob = F, add = T, col = rgb(1,0,0,.25), border = 'white')
  Cors <- hist(df_predictions$rtconf[df_predictions$cor == 1 & df_predictions$manipulation == condLab[cond]], 
               breaks = seq(-2,30,.3), plot = F)
  Errs <- hist(df_predictions$rtconf[df_predictions$cor == 0 & df_predictions$manipulation == condLab[cond]], 
               breaks = seq(-2,30,.3), plot = F)
  lines(Cors$counts/(sum(Cors$counts)/sum(tempC$counts))~ Cors$mids, type='l', col = 'green', lwd = 3)
  lines(Errs$counts/(sum(Errs$counts)/sum(tempE$counts))~ Errs$mids, type='l', col = 'red', lwd = 3)
  #legend("topright",fill=c("white","white","green","red"),border=F,legend=c("Simulated corrects","Simulated errors","Empirical corrects","Empirical errors"),col=rep(c("Green","Red"),2),bty='n',lwd=c(1,1,-1,-1))
  
}

### Confidence RT - coherence ----

par(mfrow = c(1, 3), mai = c(0.3, 0.3, 0.7, 0.3))

for (coherence in 1:3){  # Loop through coherence levels
  
  # Draw plots
  tempC <- hist(df_obs$rtconf[df_obs$cor == 1 & df_obs$coherence == coherence_vector[coherence]], 
                breaks=seq(0,6.2,.1), xlim = c(0,3), ylim = c(0,1000), prob = F, col = rgb(0,1,0,.25), border = "white", ylab = "", xlab = "", cex.lab = 2, cex.main = 1.5, cex.axis = 1.5, main = "")
  tempE <- hist(df_obs$rtconf[df_obs$cor == 0 & df_obs$coherence == coherence_vector[coherence]], 
                breaks=seq(0,6.2,.1), prob = F, add = T, col = rgb(1,0,0,.25), border = 'white')
  Cors <- hist(df_predictions$rtconf[df_predictions$cor == 1 & df_predictions$coherence == coherence_vector[coherence]], 
               breaks = seq(-2,30,.1), plot = F)
  Errs <- hist(df_predictions$rtconf[df_predictions$cor == 0 & df_predictions$coherence == coherence_vector[coherence]], 
               breaks = seq(-2,30,.1), plot = F)
  lines(Cors$counts/(sum(Cors$counts)/sum(tempC$counts))~ Cors$mids, type='l', col = 'green', lwd = 3)
  lines(Errs$counts/(sum(Errs$counts)/sum(tempE$counts))~ Errs$mids, type='l', col = 'red', lwd = 3)
  #legend("topright",fill=c("white","white","green","red"),border=F,legend=c("Simulated corrects","Simulated errors","Empirical corrects","Empirical errors"),col=rep(c("Green","Red"),2),bty='n',lwd=c(1,1,-1,-1))
  
}

## Behavioral data ----

### Preparation ----

df_obs_mean <- df_obs %>%
  group_by(sub, coherence, manipulation) %>% 
  summarise_each(funs(mean))

df_predictions_mean <- df_predictions %>%
  mutate(cj = as.integer(cj))
  group_by(sub, coherence, manipulation) %>% 
  summarise_each(funs(mean))

### RT ---- 

a <- ggplot(data = df_obs_mean, aes(x = coherence, y = rt, color = as.factor(rt_manipulation))) +
  geom_point(size = 3, stroke = 1, shape = 16, alpha = 0.2, position = position_jitterdodge(jitter.width = 0.04, dodge.width = 0.07)) +
  stat_summary(geom = "point", size = 5, fun = "mean", position = position_dodge(width = 0.07)) +
  stat_summary(geom = "line", size = 1, fun = "mean", position = position_dodge(width = 0.07)) +
  stat_summary(data = df_predictions_mean, aes(x = coherence, y = rt, group = as.factor(rt_manipulation)), color = 'black', geom = "point", size = 2, stroke = 1, shape = 4, fun = "mean", position = position_dodge(width = 0.07)) +
  theme(axis.line = element_line(colour = 'black'),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = '#e0e0e0', size = 0.7),
        panel.grid.minor.x = element_blank(),
        panel.border = element_blank(),
        axis.ticks.length = unit(.2, 'cm'),
        panel.background = element_blank(),
        text = element_text(family = 'font', size = 12),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
        legend.position = 'none') + 
  scale_color_manual(values = c('#7EA172', '#C7CB85'))


b <- ggplot(data = df_obs_mean, aes(x = coherence, y = rt, color = as.factor(rtconf_manipulation))) +
  geom_point(size = 3, stroke = 1, shape = 16, alpha = 0.2, position = position_jitterdodge(jitter.width = 0.04, dodge.width = 0.07)) +
  stat_summary(geom = "point", size = 5, fun = "mean", position = position_dodge(width = 0.07)) +
  stat_summary(geom = "line", size = 1, fun = "mean", position = position_dodge(width = 0.07)) +
  stat_summary(data = df_predictions_mean, aes(x = coherence, y = rt, group = as.factor(rtconf_manipulation)), color = 'black', geom = "point", size = 2, stroke = 1, shape = 4, fun = "mean", position = position_dodge(width = 0.07)) +
  theme(axis.line = element_line(colour = 'black'),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = '#e0e0e0', size = 0.7),
        panel.grid.minor.x = element_blank(),
        panel.border = element_blank(),
        axis.ticks.length = unit(.2, 'cm'),
        panel.background = element_blank(),
        text = element_text(family = 'font', size = 12),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
        legend.position = 'none') +
  scale_color_manual(values = c('#CA3C25', '#FFA630'))


ggarrange(a, b)

ggplot(data = df_obs_mean, aes(x = coherence, y = rt, color = as.factor(rt_manipulation))) +
  geom_point(size = 4, stroke = 1, shape = 16, alpha = 0.2, position = position_jitterdodge(jitter.width = 0.04, dodge.width = 0.07)) +
  stat_summary(geom = "point", size = 4, fun = "mean", position = position_dodge(width = 0.07)) +
  stat_summary(geom = "line", size = 1, fun = "mean", position = position_dodge(width = 0.07)) +
  stat_summary(data = df_predictions_mean, aes(x = coherence, y = rt, group = as.factor(rt_manipulation)), color = 'black', geom = "point", size = 2, stroke = 1, shape = 4, fun = "mean", position = position_dodge(width = 0.07)) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_color_manual(values = c('#CA3C25', '#FFA630')) +
  ylab(label = 'Reaction Time (s)') +
  xlab(label = 'Coherence') +
  theme(axis.line = element_line(colour = 'black'),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = '#e0e0e0', size = 0.7),
        panel.grid.minor.x = element_blank(),
        panel.border = element_blank(),
        axis.ticks.length = unit(.2, 'cm'),
        panel.background = element_blank(),
        text = element_text(family = 'font', size = 12),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
        legend.position = 'none') +
  facet_wrap(~rtconf_manipulation)

### confidence RT ----

ggplot(data = df_obs_mean, aes(x = coherence, y = rtconf, color = as.factor(rt_manipulation))) +
  geom_point(size = 4, stroke = 1, shape = 16, alpha = 0.2, position = position_jitterdodge(jitter.width = 0.04, dodge.width = 0.07)) +
  stat_summary(geom = "point", size = 4, fun = "mean", position = position_dodge(width = 0.07)) +
  stat_summary(geom = "line", size = 1, fun = "mean", position = position_dodge(width = 0.07)) +
  stat_summary(data = df_predictions_mean, aes(x = coherence, y = rtconf, group = as.factor(rt_manipulation)), color = 'black', geom = "point", size = 2, stroke = 1, shape = 4, fun = "mean", position = position_dodge(width = 0.07)) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_color_manual(values = c('#CA3C25', '#FFA630')) +
  ylab(label = 'Confidence Reaction Time (s)') +
  xlab(label = 'Coherence') +
  theme(axis.line = element_line(colour = 'black'),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = '#e0e0e0', size = 0.7),
        panel.grid.minor.x = element_blank(),
        panel.border = element_blank(),
        axis.ticks.length = unit(.2, 'cm'),
        panel.background = element_blank(),
        text = element_text(family = 'font', size = 12),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
        legend.position = 'none') +
  facet_wrap(~rtconf_manipulation)

### Accuracy ----

ggplot(data = df_obs_mean, aes(x = coherence, y = cor, color = as.factor(rt_manipulation))) +
  geom_point(size = 4, stroke = 1, shape = 16, alpha = 0.2, position = position_jitterdodge(jitter.width = 0.04, dodge.width = 0.07)) +
  stat_summary(geom = "point", size = 4, fun = "mean", position = position_dodge(width = 0.07)) +
  stat_summary(geom = "line", size = 1, fun = "mean", position = position_dodge(width = 0.07)) +
  stat_summary(data = df_predictions_mean, aes(x = coherence, y = cor, group = as.factor(rt_manipulation)), color = 'black', geom = "point", size = 2, stroke = 1, shape = 4, fun = "mean", position = position_dodge(width = 0.07)) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_color_manual(values = c('#CA3C25', '#FFA630')) +
  ylab(label = 'Accuracy') +
  xlab(label = 'Coherence') +
  theme(axis.line = element_line(colour = 'black'),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = '#e0e0e0', size = 0.7),
        panel.grid.minor.x = element_blank(),
        panel.border = element_blank(),
        axis.ticks.length = unit(.2, 'cm'),
        panel.background = element_blank(),
        text = element_text(family = 'font', size = 12),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
        legend.position = 'none') +
  facet_wrap(~rtconf_manipulation)

### Confidence judgement ----

ggplot(data = df_obs_mean, aes(x = coherence, y = cj, color = as.factor(rt_manipulation))) +
  geom_point(size = 4, stroke = 1, shape = 16, alpha = 0.2, position = position_jitterdodge(jitter.width = 0.04, dodge.width = 0.07)) +
  stat_summary(geom = "point", size = 4, fun = "mean", position = position_dodge(width = 0.07)) +
  stat_summary(geom = "line", size = 1, fun = "mean", position = position_dodge(width = 0.07)) +
  stat_summary(data = df_predictions_mean, aes(x = coherence, y = cj, group = as.factor(rt_manipulation)), color = 'black', geom = "point", size = 2, stroke = 1, shape = 4, fun = "mean", position = position_dodge(width = 0.07)) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_color_manual(values = c('#CA3C25', '#FFA630')) +
  ylab(label = 'Confidence Judgement') +
  xlab(label = 'Coherence') +
  theme(axis.line = element_line(colour = 'black'),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = '#e0e0e0', size = 0.7),
        panel.grid.minor.x = element_blank(),
        panel.border = element_blank(),
        axis.ticks.length = unit(.2, 'cm'),
        panel.background = element_blank(),
        text = element_text(family = 'font', size = 12),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
        legend.position = 'none') +
  facet_wrap(~rtconf_manipulation)
