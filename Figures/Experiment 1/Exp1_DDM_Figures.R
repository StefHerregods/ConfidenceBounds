
# Load packages

library(tidyr)
library(ggplot2)
library(dplyr)
library(Rcpp)  # To source, compile and run C++ functions
library(DEoptim)  # Optimization algorithm
library(RcppZiggurat)  # Random number generator (normal distribution)
library(ggpubr)
library(gridExtra)

# Give R access to the DDM simulation function in C++

sourceCpp("C:\\Users\\herre\\OneDrive\\Documenten\\GitHub\\ConfidenceBounds\\Analyses\\Exp1_analyses\\DDM_confidence_bounds.cpp") 

# Set working directory

setwd('C:\\Users\\herre\\Desktop\\Internship\\Results\\Exp1_Results')

# Load data (long format)

df <- data.frame(matrix(ncol = 11, nrow = 40*4))
colnames(df) <- c('sub', 'manipulation', 'v1', 'v2', 'v3', 'a', 'ter', 'a2', 'postdriftmod', 'a2_slope', 'ter2')
condLab <- c('FastFast', 'AccFast', 'AccAcc', 'FastAcc') 
j <- 1
for (i in 1:40){ 
  for(c in 1:4){
    file_name <- paste0('Parameter_estimation_both\\Parameter_estimation_both%5Cresults_sub_', i, '_', condLab[c], '.Rdata')
    load(file_name)
    df[j,] <- c(i, condLab[c], results$optim$bestmem[1], results$optim$bestmem[2], results$optim$bestmem[3], results$optim$bestmem[4], results$optim$bestmem[5], results$optim$bestmem[6], results$optim$bestmem[7], results$optim$bestmem[8], results$optim$bestmem[9])
    j <- j + 1
  }
}

df[3:11] <- lapply(df[3:11], as.numeric)


# v1

ggplot(df, aes(x = manipulation, y = v1)) +
  geom_point() +
  geom_line(aes(group = sub), alpha = 0.2) +
  stat_summary(aes(y = v1, group = 1), fun = mean, colour="Blue", size = 4, shape = 95) +
  scale_x_discrete(labels = c("Accurate decision\nAccurate confidence rating", "Accurate decision\nFast confidence rating", "Fast decision\nAccurate confidence rating", "Fast decision\nFast confidence rating")) 
  
# v2

ggplot(df, aes(x = manipulation, y = v2)) +
  geom_point() +
  geom_line(aes(group = sub), alpha = 0.2) +
  stat_summary(aes(y = v2, group = 1), fun = mean, colour="Blue", size = 4, shape = 95) +
  scale_x_discrete(labels = c("Accurate decision\nAccurate confidence rating", "Accurate decision\nFast confidence rating", "Fast decision\nAccurate confidence rating", "Fast decision\nFast confidence rating")) 

# v3

ggplot(df, aes(x = manipulation, y = v3)) +
  geom_point() +
  geom_line(aes(group = sub), alpha = 0.2) +
  stat_summary(aes(y = v3, group = 1), fun = mean, colour="Blue", size = 4, shape = 95) +
  scale_x_discrete(labels = c("Accurate decision\nAccurate confidence rating", "Accurate decision\nFast confidence rating", "Fast decision\nAccurate confidence rating", "Fast decision\nFast confidence rating")) 

# v comparison

ggplot(df, aes(x = manipulation)) +
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


# a

ggplot(df, aes(x = manipulation, y = a)) +
  geom_point() +
  geom_line(aes(group = sub), alpha = 0.2) +
  stat_summary(aes(y = a, group = 1), fun = mean, colour="Blue", size = 4, shape = 95) +
  scale_x_discrete(labels = c("Accurate decision\nAccurate confidence rating", "Accurate decision\nFast confidence rating", "Fast decision\nAccurate confidence rating", "Fast decision\nFast confidence rating")) 

# a2

ggplot(df, aes(x = manipulation, y = a2)) +
  geom_point() +
  geom_line(aes(group = sub), alpha = 0.2) +
  stat_summary(aes(y = a2, group = 1), fun = mean, colour="Blue", size = 4, shape = 95) +
  scale_x_discrete(labels = c("Accurate decision\nAccurate confidence rating", "Accurate decision\nFast confidence rating", "Fast decision\nAccurate confidence rating", "Fast decision\nFast confidence rating")) 

# ter

ggplot(df, aes(x = manipulation, y = ter)) +
  geom_point() +
  geom_line(aes(group = sub), alpha = 0.2) +
  stat_summary(aes(y = ter, group = 1), fun = mean, colour="Blue", size = 4, shape = 95) +
  scale_x_discrete(labels = c("Accurate decision\nAccurate confidence rating", "Accurate decision\nFast confidence rating", "Fast decision\nAccurate confidence rating", "Fast decision\nFast confidence rating")) 

# postdriftmod

ggplot(df, aes(x = manipulation, y = postdriftmod)) +
  geom_point() +
  geom_line(aes(group = sub), alpha = 0.2) +
  stat_summary(aes(y = postdriftmod, group = 1), fun = mean, colour="Blue", size = 4, shape = 95) +
  scale_x_discrete(labels = c("Accurate decision\nAccurate confidence rating", "Accurate decision\nFast confidence rating", "Fast decision\nAccurate confidence rating", "Fast decision\nFast confidence rating")) 

# a2_slope

ggplot(df, aes(x = manipulation, y = a2_slope)) +
  geom_point() +
  geom_line(aes(group = sub), alpha = 0.2) +
  stat_summary(aes(y = a2_slope, group = 1), fun = mean, colour="Blue", size = 4, shape = 95) +
  scale_x_discrete(labels = c("Accurate decision\nAccurate confidence rating", "Accurate decision\nFast confidence rating", "Fast decision\nAccurate confidence rating", "Fast decision\nFast confidence rating")) 

# ter2

ggplot(df, aes(x = manipulation, y = ter2)) +
  geom_point() +
  geom_line(aes(group = sub), alpha = 0.2) +
  stat_summary(aes(y = ter2, group = 1), fun = mean, colour="Blue", size = 4, shape = 95) +
  scale_x_discrete(labels = c("Accurate decision\nAccurate confidence rating", "Accurate decision\nFast confidence rating", "Fast decision\nAccurate confidence rating", "Fast decision\nFast confidence rating")) 




### Simulations based on estimated parameters ### 

par(mfrow = c(4, 3))

# DDM parameters

z <- 0.5  # Starting point (accuracy-coded dataset -> 0.5)
ntrials <- 10000  # Number of decision-making simulations per observation
sigma <- 1  # Within-trial noise
dt <- 0.01  # Precision

# Loop parameters

n <- 40  # Number of participants to include (40)

# Vectors

coherence_vector <- c(0.1, 0.2, 0.4)
manipulation_vector <- c('FastFast', 'AccFast', 'AccAcc', 'FastAcc')

# Selecting observation data

df_obs <- read.csv(file = "Exp1_data_viable.csv")
c_observed <- df_obs %>% filter(sub <= n & cor == 1) 
e_observed <- df_obs %>% filter(sub <= n & cor == 0)
high_conf_observed <- df_obs %>% filter(sub <= n & cj == 1)
low_conf_observed <- df_obs %>% filter(sub <= n & cj == 0)

# RT's
# Loop through manipulations

for (j in 1:4){
  
  # Loop through coherence levels
  
  for (k in 1:3){
    
    c_predicted <- NULL
    e_predicted <- NULL
    
    c_observed_temp <- c_observed %>% filter(manipulation == manipulation_vector[j] & coherence == coherence_vector[k])
    e_observed_temp <- e_observed %>% filter(manipulation == manipulation_vector[j] & coherence == coherence_vector[k])
    
    # Loop through participants
    
    for (i in 1:n){
      
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
    
    # Draw plots
    
    tempC <- hist(c_observed_temp$rt, breaks=seq(0,6.2,.1), xlim = c(0,3), prob = F, col = rgb(0,1,0,.25), border = "white", ylab = "", xlab = "", cex.lab = 2, cex.main = 1.5, cex.axis = 1.5, main = "")
    tempE <- hist(e_observed_temp$rt, breaks=seq(0,6.2,.1), prob = F, add = T, col = rgb(1,0,0,.25), border = 'white')
    Cors <- hist(c_predicted$rt, breaks = seq(0,30,.1), plot = F)
    Errs <- hist(e_predicted$rt, breaks = seq(0,30,.1),plot=F)
    lines(Cors$counts/(sum(Cors$counts)/sum(tempC$counts))~Cors$mids,type='l',col='green',lwd=3)
    lines(Errs$counts/(sum(Errs$counts)/sum(tempE$counts))~Errs$mids,type='l',col='red',lwd=3)
    #legend("topright",fill=c("white","white","green","red"),border=F,legend=c("Simulated corrects","Simulated errors","Empirical corrects","Empirical errors"),col=rep(c("Green","Red"),2),bty='n',lwd=c(1,1,-1,-1))

  }
  
}

# Confidence rating RT's
# Loop through manipulations

for (j in 1:4){
  
  # Loop through coherence levels
  
  for (k in 1:3){
    
    high_conf_predicted <- NULL
    low_conf_predicted <- NULL
    
    high_conf_observed_temp <- high_conf_observed %>% filter(manipulation == manipulation_vector[j] & coherence == coherence_vector[k])
    low_conf_observed_temp <- low_conf_observed %>% filter(manipulation == manipulation_vector[j] & coherence == coherence_vector[k])
    
    # Loop through participants
    
    for (i in 1:n){
      
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

      print(i)
    }
    
    # Draw plots
    
    tempC <- hist(high_conf_observed$rtconf, breaks=seq(0,6.2,.1), xlim = c(0,3), prob = F, col = rgb(0,1,0,.25), border = "white", ylab = "", xlab = "", cex.lab = 2, cex.main = 1.5, cex.axis = 1.5, main = "")
    tempE <- hist(low_conf_observed$rtconf, breaks=seq(0,6.2,.1), prob = F, add = T, col = rgb(1,0,0,.25), border = 'white')
    Cors <- hist(high_conf_predicted$rtconf, breaks = seq(-1,30,.1), plot = F)
    Errs <- hist(low_conf_predicted$rtconf, breaks = seq(-1,30,.1),plot=F)
    lines(Cors$counts/(sum(Cors$counts)/sum(tempC$counts))~Cors$mids,type='l',col='green',lwd=3)
    lines(Errs$counts/(sum(Errs$counts)/sum(tempE$counts))~Errs$mids,type='l',col='red',lwd=3)
    #legend("topright",fill=c("white","white","green","red"),border=F,legend=c("Simulated corrects","Simulated errors","Empirical corrects","Empirical errors"),col=rep(c("Green","Red"),2),bty='n',lwd=c(1,1,-1,-1))
  }
  
}



