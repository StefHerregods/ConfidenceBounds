
# Load packages

library(rstatix)
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
for (i in (1:40)){ 
  for(c in 1:4){
    file_name <- paste0('Parameter_estimation_both_2\\exp1_simple_results_sub_', i, '_', condLab[c], '.Rdata')
    load(file_name)
    df[j,] <- c(i, condLab[c], results$optim$bestmem[1], results$optim$bestmem[2], results$optim$bestmem[3], results$optim$bestmem[4], results$optim$bestmem[5], results$optim$bestmem[6], results$optim$bestmem[7], results$optim$bestmem[8], results$optim$bestmem[9])
    j <- j + 1
  }
}

df[3:11] <- lapply(df[3:11], as.numeric)


# v1

ggplot(df, aes(x = manipulation, y = v1)) +
  geom_point(colour = "#4D5382", alpha = 0.5, size = 5, shape = 16) +
  geom_line(aes(group = sub), alpha = 0.2) +
  stat_summary(aes(y = v1, group = 1), fun = mean, colour="#4D5382", size = 4, shape = 95) +
  scale_x_discrete(labels = c("Accurate decision\nAccurate confidence rating", "Accurate decision\nFast confidence rating", "Fast decision\nAccurate confidence rating", "Fast decision\nFast confidence rating")) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())
  
# v2

ggplot(df, aes(x = manipulation, y = v2)) +
  geom_point(colour = "#4D5382", alpha = 0.5, size = 5, shape = 16) +
  geom_line(aes(group = sub), alpha = 0.2) +
  stat_summary(aes(y = v2, group = 1), fun = mean, colour="#4D5382", size = 4, shape = 95) +
  scale_x_discrete(labels = c("Accurate decision\nAccurate confidence rating", "Accurate decision\nFast confidence rating", "Fast decision\nAccurate confidence rating", "Fast decision\nFast confidence rating")) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())
# v3

ggplot(df, aes(x = manipulation, y = v3)) +
  geom_point(colour = "#4D5382", alpha = 0.5, size = 5, shape = 16) +
  geom_line(aes(group = sub), alpha = 0.2) +
  stat_summary(aes(y = v3, group = 1), fun = mean, colour="#4D5382", size = 4, shape = 95) +
  scale_x_discrete(labels = c("Accurate decision\nAccurate confidence rating", "Accurate decision\nFast confidence rating", "Fast decision\nAccurate confidence rating", "Fast decision\nFast confidence rating")) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())

# v comparison

df_temp <- select(df, v1, v2, v3, manipulation)
v_mean <- df_temp %>%
  group_by(manipulation) %>% 
  summarise_each(funs(mean)) %>%
  pivot_longer(cols = c(v1, v2, v3))

ggplot(v_mean, aes(x = manipulation, y = value)) +
  geom_line(aes(group = name, colour = name), lty = 5, plt = 5, size = 1) +
  geom_point() 

###
df2 <- pivot_longer(data = df, cols = c(v1, v2, v3), names_to = 'v_var', values_to = 'v')

res.aov <- anova_test(data = df2, dv = v, wid = sub, within = c(v_var, manipulation))
res.aov

pwc <- df2 %>%
  group_by(v_var) %>%
  pairwise_t_test(
    v ~ manipulation, paired = TRUE,
    p.adjust.method = "bonferroni"
  )
data.frame(pwc)

pwc <- df2 %>%
  group_by(manipulation) %>%
  pairwise_t_test(
    v ~ v_var, paired = TRUE,
    p.adjust.method = "bonferroni"
  )
data.frame(pwc)




  
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
  #geom_violin(outlier.shape = NA, coef = 0) +
  geom_point(colour = "#4D5382", alpha = 0.5, size = 5, shape = 16) +
  geom_line(aes(group = sub), alpha = 0.3) +
  stat_summary(aes(y = a, group = 1), fun = mean, colour="#4D5382", size = 4, shape = 95) +
  scale_x_discrete(labels = c("Accurate decision\nAccurate confidence rating", "Accurate decision\nFast confidence rating", "Fast decision\nAccurate confidence rating", "Fast decision\nFast confidence rating")) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())

###

res.aov <- anova_test(data = df, dv = a, wid = sub, within = manipulation)
res.aov

pwc <- df %>%
  pairwise_t_test(
    a ~ manipulation, paired = TRUE,
    p.adjust.method = "bonferroni"
  )
data.frame(pwc)





library(afex)
#Model Specification
SCR_ANOVA <- aov_car(a ~ manipulation + Error(sub/manipulation), data = df)
SCR_ANOVA_summary <- summary(SCR_ANOVA)
SCR_ANOVA_summary #Significant main effect of Phase

##Contrasts (based on the model with outliers): As the F-test of the model with outliers is insignificant for the interaction, contrasts are expected to be insignificant
###Setting up a reference grid
library(lsmeans)
ref_SCR <- lsmeans(SCR_ANOVA, c("manipulation"))
ref_SCR

###Specifying the contrasts as a list on the reference grid. Note that we test 2 pre-planned contrasts in which we compare the general SCR pattern between both groups, once for the first two phases and once for the last two phases.
list_of_contrasts_SCR <- list(contrast1_SCR  = c(1, 0, 0, -1),
                              contrast2_SCR  = c(0, 1, -1, 0), 
                              contrast3_SCR  = c(0.5, -0.5, -0.5, 0.5)) #We compare the general SCR pattern between both groups in the acquisition and extinction phase. A positive score indicates higher values in the experimental group as expected.

###Test whether the contrasts are significant. Note that as we have multiple pre-planned contrasts, we have to apply the Bonferroni-Holm correction to avoid inflating the type I error.
Planned_contrasts_SCR <- summary(contrast(ref_SCR, list_of_contrasts_SCR), adjust = "holm") 
Planned_contrasts_SCR #As expected, the contrasts are insignificant. However, the absolute value of the contrast suggests that the SCR is higher in the experimental group, particularly in the acquisition phase.


# a2

ggplot(df, aes(x = manipulation, y = a2)) +
  #geom_boxplot(outlier.shape = NA, coef = 0) +
  geom_point(colour = "#4D5382", alpha = 0.5, size = 5, shape = 16) +
  geom_line(aes(group = sub), alpha = 0.3) +
  stat_summary(aes(y = a2, group = 1), fun = mean, colour="#4D5382", size = 4, shape = 95) +
  scale_x_discrete(labels = c("Accurate decision\nAccurate confidence rating", "Accurate decision\nFast confidence rating", "Fast decision\nAccurate confidence rating", "Fast decision\nFast confidence rating")) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())

###

pwc <- df %>%
  pairwise_t_test(
    a2 ~ manipulation, paired = TRUE,
    p.adjust.method = "bonferroni"
  )
data.frame(pwc)

# ter

ggplot(df, aes(x = manipulation, y = ter)) +
  #geom_boxplot(outlier.shape = NA, coef = 0) +
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

res.aov <- anova_test(data = df, dv = ter, wid = sub, within = manipulation)
res.aov # significant


pwc <- df %>%
  pairwise_t_test(
    ter ~ manipulation, paired = TRUE,
    p.adjust.method = "bonferroni"
  )
data.frame(pwc)

# postdriftmod

ggplot(df, aes(x = manipulation, y = postdriftmod)) +
  #geom_boxplot(outlier.shape = NA, coef = 0) +
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

res.aov <- anova_test(data = df, dv = postdriftmod, wid = sub, within = manipulation)
res.aov # not significant

pwc <- df %>%
  pairwise_t_test(
    postdriftmod ~ manipulation, paired = TRUE,
    p.adjust.method = "bonferroni"
  )
data.frame(pwc)

# a2_slope

ggplot(df, aes(x = manipulation, y = a2_slope)) +
  #geom_boxplot(outlier.shape = NA, coef = 0) +
  ylim(c(0,5)) +
  geom_point(colour = "#4D5382", alpha = 0.5, size = 2, shape = 16) +
  geom_line(aes(group = sub), alpha = 0.3) +
  stat_summary(aes(y = a2_slope, group = 1), fun = mean, colour="#4D5382", size = 4, shape = 95) +
  scale_x_discrete(labels = c("Accurate decision\nAccurate confidence rating", "Accurate decision\nFast confidence rating", "Fast decision\nAccurate confidence rating", "Fast decision\nFast confidence rating")) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  ylab("urgency")

res.aov <- anova_test(data = df, dv = a2_slope, wid = sub, within = manipulation)
res.aov # not significant




# ter2

ggplot(df, aes(x = manipulation, y = ter2)) +
  #geom_boxplot(outlier.shape = NA, coef = 0) +
  geom_point(colour = "#4D5382", alpha = 0.5, size = 5, shape = 16) +
  geom_line(aes(group = sub), alpha = 0.3) +
  stat_summary(aes(y = ter2, group = 1), fun = mean, colour="#4D5382", size = 4, shape = 95) +
  scale_x_discrete(labels = c("Accurate decision\nAccurate confidence rating", "Accurate decision\nFast confidence rating", "Fast decision\nAccurate confidence rating", "Fast decision\nFast confidence rating")) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  ylab("non-decision time")

res.aov <- anova_test(data = df, dv = ter2, wid = sub, within = manipulation)
res.aov # not significant

pwc <- df %>%
  pairwise_t_test(
    ter2 ~ manipulation, paired = TRUE,
    p.adjust.method = "bonferroni"
  )
data.frame(pwc)




### Simulations based on estimated parameters ### 

par(mfrow = c(4, 3), mai = c(0.3, 0.3, 0.3, 0.3))

# DDM parameters

z <- 0.5  # Starting point (accuracy-coded dataset -> 0.5)
ntrials <- 1000  # Number of decision-making simulations per observation
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
    
    # Draw plots
    
    tempC <- hist(c_observed_temp$rt, breaks=seq(0,6.2,.1), xlim = c(0,3), ylim = c(0,300), prob = F, col = rgb(0,1,0,.25), border = "white", ylab = "", xlab = "", cex.lab = 2, cex.main = 1.5, cex.axis = 1.5, main = "")
    tempE <- hist(e_observed_temp$rt, breaks=seq(0,6.2,.1), prob = F, add = T, col = rgb(1,0,0,.25), border = 'white')
    Cors <- hist(c_predicted$rt, breaks = seq(0,30,.1), plot = F)
    Errs <- hist(e_predicted$rt, breaks = seq(0,30,.1),plot=F)
    lines(Cors$counts/(sum(Cors$counts)/sum(tempC$counts))~Cors$mids,type='l',col='green',lwd=3)
    lines(Errs$counts/(sum(Errs$counts)/sum(tempE$counts))~Errs$mids,type='l',col='red',lwd=3)
    #legend("topright",fill=c("white","white","green","red"),border=F,legend=c("Simulated corrects","Simulated errors","Empirical corrects","Empirical errors"),col=rep(c("Green","Red"),2),bty='n',lwd=c(1,1,-1,-1))

  }
  
}




# Confidence rating RT's (high vs. low cj)
# Loop through manipulations

for (j in 1:4){
  
  # Loop through coherence levels
  
  for (k in 1:3){
    
    high_conf_predicted <- NULL
    low_conf_predicted <- NULL
    
    high_conf_observed_temp <- high_conf_observed %>% filter(manipulation == manipulation_vector[j] & coherence == coherence_vector[k])
    low_conf_observed_temp <- low_conf_observed %>% filter(manipulation == manipulation_vector[j] & coherence == coherence_vector[k])
    
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
    
    # Draw plots
    
    tempC <- hist(high_conf_observed_temp$rtconf, breaks=seq(0,6.2,.1), xlim = c(0,1.5), ylim = c(0,500), prob = F, col = rgb(0,1,0,.25), border = "white", ylab = "", xlab = "", cex.lab = 2, cex.main = 1.5, cex.axis = 1.5, main = "")
    tempE <- hist(low_conf_observed_temp$rtconf, breaks=seq(0,6.2,.1), prob = F, add = T, col = rgb(1,0,0,.25), border = 'white')
    Cors <- hist(high_conf_predicted$rtconf, breaks = seq(-2,30,.1), plot = F)
    Errs <- hist(low_conf_predicted$rtconf, breaks = seq(-2,30,.1),plot=F)
    lines(Cors$counts/(sum(Cors$counts)/sum(tempC$counts))~Cors$mids,type='l',col='green',lwd=3)
    lines(Errs$counts/(sum(Errs$counts)/sum(tempE$counts))~Errs$mids,type='l',col='red',lwd=3)
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








### (1)

par(mfrow = c(1, 4), mai = c(0.3, 0.3, 0.7, 0.3))



# RT's
# Loop through manipulations

for (j in 1:4){
  
  c_observed_temp <- c_observed %>% filter(manipulation == manipulation_vector[j])
  e_observed_temp <- e_observed %>% filter(manipulation == manipulation_vector[j])
  
  c_predicted_full <- NULL
  e_predicted_full <- NULL
  
  # Loop through coherence levels
  
  for (k in 1:3){
    
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
  
  labels <- c("Fast decision\nFast confidence rating", "Accurate decision\nFast confidence rating", "Accurate decision\nAccurate confidence rating", "Fast decision\nAccurate confidence rating")
  
  tempC <- hist(c_observed_temp$rt, breaks=seq(0,6.2,.3), xlim = c(0,3), ylim = c(0,1800), prob = F, col = rgb(0,1,0,.25), border = "white", ylab = "", xlab = "", cex.lab = 2, cex.main = 1.5, cex.axis = 1.5, main = labels[j], yaxt="n")
  tempE <- hist(e_observed_temp$rt, breaks=seq(0,6.2,.3), prob = F, add = T, col = rgb(1,0,0,.25), border = 'white', yaxt="n")
  Cors <- hist(c_predicted_full$rt, breaks = seq(0,30,.3), plot = F)
  Errs <- hist(e_predicted_full$rt, breaks = seq(0,30,.3),plot=F)
  lines(Cors$counts/(sum(Cors$counts)/sum(tempC$counts))~Cors$mids,type='l',col='green',lwd=3)
  lines(Errs$counts/(sum(Errs$counts)/sum(tempE$counts))~Errs$mids,type='l',col='red',lwd=3)
    
  #legend("topright",fill=c("white","white","green","red"),border=F,legend=c("Simulated corrects","Simulated errors","Empirical corrects","Empirical errors"),col=rep(c("Green","Red"),2),bty='n',lwd=c(1,1,-1,-1))
  
  
}


# Confidence rating RT's (high vs. low cj)
# Loop through manipulations

for (j in 1:4){
  
  high_conf_observed_temp <- high_conf_observed %>% filter(manipulation == manipulation_vector[j])
  low_conf_observed_temp <- low_conf_observed %>% filter(manipulation == manipulation_vector[j])
  
  high_conf_predicted_full <- NULL
  low_conf_predicted_full <- NULL
  
  # Loop through coherence levels
  
  for (k in 1:3){
    
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
  
  labels <- c("Fast decision\nFast confidence rating", "Accurate decision\nFast confidence rating", "Accurate decision\nAccurate confidence rating", "Fast decision\nAccurate confidence rating")
  
  # Draw plots
  
  tempC <- hist(high_conf_observed_temp$rtconf, breaks=seq(0,6.2,.15), xlim = c(0,1.5), ylim = c(0,2000), prob = F, col = rgb(0.952941, 0.611765, 0.070588, .35), border = "white", ylab = "", xlab = "", cex.lab = 2, cex.main = 1.5, cex.axis = 1.5, main = labels[j], yaxt="n")
  tempE <- hist(low_conf_observed_temp$rtconf, breaks=seq(0,6.2,.15), prob = F, add = T, col = rgb(0.121569,0.380392,0.552941,.35), border = 'white')
  Cors <- hist(high_conf_predicted_full$rtconf, breaks = seq(-2,30,.15), plot = F)
  Errs <- hist(low_conf_predicted_full$rtconf, breaks = seq(-2,30,.15),plot=F)
  lines(Cors$counts/(sum(Cors$counts)/sum(tempC$counts))~Cors$mids,type='l',col='#F39C12',lwd=3)
  lines(Errs$counts/(sum(Errs$counts)/sum(tempE$counts))~Errs$mids,type='l',col='#1F618D',lwd=3)
  #legend("topright",fill=c("white","white","green","red"),border=F,legend=c("Simulated corrects","Simulated errors","Empirical corrects","Empirical errors"),col=rep(c("Green","Red"),2),bty='n',lwd=c(1,1,-1,-1))
  
  print(paste0("Proportion of negative RT's: ", (nrow(high_conf_predicted[high_conf_predicted$rtconf < 0,]) + nrow(low_conf_predicted[low_conf_predicted$rtconf < 0,]))/(nrow(high_conf_predicted) + nrow(low_conf_predicted))))
  
}


# Confidence rating RT's (after correct vs. wrong decision)
# Loop through manipulations

for (j in 1:4){
  
  c_conf_observed_temp <- c_observed %>% filter(manipulation == manipulation_vector[j])
  e_conf_observed_temp <- e_observed %>% filter(manipulation == manipulation_vector[j])
  
  c_conf_predicted_full <- NULL
  e_conf_predicted_full <- NULL
  
  # Loop through coherence levels
  
  for (k in 1:3){
    
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
  
  # Draw plots
  
  tempC <- hist(c_conf_observed_temp$rtconf, breaks=seq(0,6.2,.15), xlim = c(0,2), ylim = c(0,2200), prob = F, col = rgb(0.952941, 0.611765, 0.070588, .35), border = "white", ylab = "", xlab = "", cex.lab = 2, cex.main = 1.5, cex.axis = 1.5, main = labels[j], yaxt="n")
  tempE <- hist(e_conf_observed_temp$rtconf, breaks=seq(0,6.2,.15), prob = F, add = T, col = rgb(0.121569,0.380392,0.552941,.35), border = 'white')
  Cors <- hist(c_conf_predicted_full$rtconf, breaks = seq(-2,30,.15), plot = F)
  Errs <- hist(e_conf_predicted_full$rtconf, breaks = seq(-2,30,.15),plot=F)
  lines(Cors$counts/(sum(Cors$counts)/sum(tempC$counts))~Cors$mids,type='l',col='#F39C12',lwd=3)
  lines(Errs$counts/(sum(Errs$counts)/sum(tempE$counts))~Errs$mids,type='l',col='#1F618D',lwd=3)
  #legend("topright",fill=c("white","white","green","red"),border=F,legend=c("Simulated corrects","Simulated errors","Empirical corrects","Empirical errors"),col=rep(c("Green","Red"),2),bty='n',lwd=c(1,1,-1,-1))
  
}







### (2)

par(mfrow = c(1, 3), mai = c(0.3, 0.3, 0.7, 0.3))



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
