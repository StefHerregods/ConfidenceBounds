
# Set-up ----


## Load packages

library(tidyr)
library(ggplot2)
library(dplyr)
library(Rcpp)  # To source, compile and run C++ functions
library(DEoptim)  # Optimization algorithm
library(RcppZiggurat)  # Random number generator (normal distribution)
library(gridExtra)
library(ggpubr)

## Give R access to the DDM simulation function in C++

sourceCpp("C:\\Users\\herre\\OneDrive\\Documenten\\GitHub\\ConfidenceBounds\\Analyses\\Exp1_analyses\\DDM_confidence_bounds.cpp") 

## Set working directory

setwd('C:\\Users\\herre\\Desktop\\Internship\\Results\\Exp1_Results')

## Set font

windowsFonts(font = windowsFont("Times New Roman"))


# Load data ----


## Behavioral data ----

df_obs <- read.csv(file = "Exp1_data_viable.csv")
df_obs <- df_obs %>% mutate(rt_manipulation = ifelse(df_obs$manipulation %in% c("AccAcc", "AccFast"), 1, 0),
                            rtconf_manipulation = ifelse(df_obs$manipulation %in% c("AccAcc", "FastAcc"), 1, 0))

## DDM parameters ----

df_DDM <- data.frame(matrix(ncol = 11, nrow = 40*4))
colnames(df_DDM) <- c('sub', 'manipulation', 'v1', 'v2', 'v3', 'a', 'ter', 'a2', 'postdriftmod', 'a2_slope', 'ter2')
condLab <- c('FastFast', 'AccFast', 'AccAcc', 'FastAcc') 
j <- 1
for (i in (1:40)){ 
  for(c in 1:4){
    file_name <- paste0('Parameter_estimation\\exp1_simple_results_sub_', i, '_', condLab[c], '.Rdata')
    load(file_name)
    df_DDM[j,] <- c(i, condLab[c], results$optim$bestmem[1], results$optim$bestmem[2], results$optim$bestmem[3], results$optim$bestmem[4], results$optim$bestmem[5], results$optim$bestmem[6], results$optim$bestmem[7], results$optim$bestmem[8], results$optim$bestmem[9])
    j <- j + 1
  }
}
df_DDM[3:11] <- lapply(df_DDM[3:11], as.numeric)

## DDM predictions ----

### Variables
z <- 0.5  # Starting point (accuracy-coded data set -> 0.5)
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
                                                            a2 = df_DDM[j,]$a2,
                                                            postdriftmod = df_DDM[j,]$postdriftmod,
                                                            a2_slope = df_DDM[j,]$a2_slope,
                                                            ter2 = df_DDM[j,]$ter2))
    names(df_predictions_temp) <- c('sub', 'manipulation', 'coherence', 'rt', 'resp', 'cor', 'evidence2', 'rtfull', 'rtconf', 'cj')
    df_predictions <- rbind(df_predictions, df_predictions_temp)
    setTxtProgressBar(pb, j)
  }
}
close(pb)
df_predictions <- df_predictions %>% mutate(rt_manipulation = ifelse(df_predictions$manipulation %in% c("AccAcc", "AccFast"), 1, 0),
                                            rtconf_manipulation = ifelse(df_predictions$manipulation %in% c("AccAcc", "FastAcc"), 1, 0))


# Visualizations ----


## DDM parameters ----

### Transformations

df_DDM$manipulation_jitter <- jitter(unclass(as.factor(df_DDM$manipulation)), factor = 0.6)
df_DDM <- df_DDM %>% 
  mutate(rt_manipulation = as.factor(ifelse(df_DDM$manipulation %in% c("AccAcc", "AccFast"), 1, 0)),
         rtconf_manipulation = as.factor(ifelse(df_DDM$manipulation %in% c("AccAcc", "FastAcc"), 1, 0))) %>%
  group_by(manipulation) %>% 
  mutate(a_mean = mean(a),
         a_sd = sd(a),
         a2_mean = mean(a2),
         a2_sd = sd(a2),
         a2_slope_mean = mean(a2_slope),
         a2_slope_sd = sd(a2_slope))

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
        panel.background = element_blank(),
        text = element_text(family = 'font', size = 11))
  
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
        panel.background = element_blank(),
        text = element_text(family = 'font', size = 11))
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
        panel.background = element_blank(),
        text = element_text(family = 'font', size = 11))

### v comparison

df_temp <- select(df_DDM, v1, v2, v3, manipulation)
v_mean <- df_temp %>%
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

a <- ggplot(data = df_DDM) +
  geom_line(aes(x = manipulation_jitter, y = a, group = sub), alpha = 0.15, size = 0.3) +
  geom_point(aes(x = manipulation_jitter, y = a), fill = '#999999', color = 'white', size = 4, shape = 21, stroke = 1) +
  geom_linerange(aes(x = manipulation, y = a, ymin = a_mean - a_sd / sqrt(40), ymax = a_mean + a_sd / sqrt(40)), position = position_nudge(-0.28), size = 1) +
  geom_point(aes(x = manipulation, y = a_mean), position = position_nudge(-0.28)) +
  geom_line(aes(x = manipulation, y = a_mean, group = sub), position = position_nudge(-0.28), size = 1, linetype = 'dashed') +
  scale_x_discrete(labels = c(1, 2, 3, 4)) +
  ylim(c(.66, 3.3)) +
  xlab(label = 'Condition') +
  ylab(label = 'Decision boundary separation') +
  theme(axis.line = element_line(colour = 'black'),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = '#e0e0e0', size = 0.7),
        panel.grid.minor.x = element_blank(),
        panel.border = element_blank(),
        axis.ticks.length = unit(.2, 'cm'),
        panel.background = element_blank(),
        text = element_text(family = 'font', size = 12),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)))

### a2

a2 <- ggplot(data = df_DDM) +
  geom_line(aes(x = manipulation_jitter, y = a2, group = sub), alpha = 0.15, size = 0.3) +
  geom_point(aes(x = manipulation_jitter, y = a2), fill = '#999999', color = 'white', size = 4, shape = 21, stroke = 1) +
  geom_linerange(aes(x = manipulation, y = a2, ymin = a2_mean - a2_sd / sqrt(40), ymax = a2_mean + a2_sd / sqrt(40)), position = position_nudge(-0.28), size = 1) +
  geom_point(aes(x = manipulation, y = a2_mean), position = position_nudge(-0.28)) +
  geom_line(aes(x = manipulation, y = a2_mean, group = sub), position = position_nudge(-0.28), size = 1, linetype = 'dashed') +
  scale_x_discrete(labels = c(1, 2, 3, 4)) +
  ylim(c(.5, 5.5)) +
  xlab(label = 'Condition') +
  ylab(label = 'Confidence boundary separation') +
  theme(axis.line = element_line(colour = 'black'),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = '#e0e0e0', size = 0.7),
        panel.grid.minor.x = element_blank(),
        panel.border = element_blank(),
        axis.ticks.length = unit(.2, 'cm'),
        panel.background = element_blank(),
        text = element_text(family = 'font', size = 12),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)))

### a2_slope

a2_slope <- ggplot(data = df_DDM) +
  geom_line(aes(x = manipulation_jitter, y = a2_slope, group = sub), alpha = 0.15, size = 0.3) +
  geom_point(aes(x = manipulation_jitter, y = a2_slope), fill = '#999999', color = 'white', size = 4, shape = 21, stroke = 1) +
  geom_linerange(aes(x = manipulation, y = a2_slope, ymin = a2_slope_mean - a2_slope_sd / sqrt(40), ymax = a2_slope_mean + a2_slope_sd / sqrt(40)), position = position_nudge(-0.28), size = 1) +
  geom_point(aes(x = manipulation, y = a2_slope_mean), position = position_nudge(-0.28)) +
  geom_line(aes(x = manipulation, y = a2_slope_mean, group = sub), position = position_nudge(-0.28), size = 1, linetype = 'dashed') +
  scale_x_discrete(labels = c(1, 2, 3, 4)) +
  ylim(c(-0.2, 8)) +
  xlab(label = 'Condition') +
  ylab(label = 'Urgency') +
  theme(axis.line = element_line(colour = 'black'),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = '#e0e0e0', size = 0.7),
        panel.grid.minor.x = element_blank(),
        panel.border = element_blank(),
        axis.ticks.length = unit(.2, 'cm'),
        panel.background = element_blank(),
        text = element_text(family = 'font', size = 12),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)))

ggsave(filename = 'test.png',
       plot = ggarrange(a, a2, a2_slope, ncol = 3),
       device = 'png',
       width = 19,
       height = 10,
       units = 'cm')

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
        panel.background = element_blank(),
        text = element_text(family = 'font', size = 11)) +
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
        panel.background = element_blank(),
        text = element_text(family = 'font', size = 11)) +
  ylab("postdriftmod")

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
        panel.background = element_blank(),
        text = element_text(family = 'font', size = 11)) +
  ylab("non-decision time")

## DDM full predictions ----

par(mfrow = c(4, 3), mai = c(0.3, 0.3, 0.3, 0.3))

### Decision RT ----

for (cond in 1:4){  # Loop through conditions
  for (coherence in 1:3){  # Loop through coherence levels
    
    # Draw plots
    tempC <- hist(family='font', df_obs$rt[df_obs$cor == 1 & df_obs$coherence == coherence_vector[coherence] & df_obs$manipulation == condLab[cond]], 
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
    tempC <- hist(family='font', df_obs$rtconf[df_obs$cor == 1 & df_obs$coherence == coherence_vector[coherence] & df_obs$manipulation == condLab[cond]], 
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

for (cond in 1:4){  # Loop through conditions
  for (coherence in 1:3){  # Loop through coherence levels
    
    # Draw plots
    tempC <- hist(family='font', df_obs$rtconf[df_obs$cj == 1 & df_obs$coherence == coherence_vector[coherence] & df_obs$manipulation == condLab[cond]], 
                  breaks=seq(0,6.2,.1), xlim = c(0,3), ylim = c(0,300), prob = F, col = rgb(.86,.52,.12,.85), border = "white", ylab = "", xlab = "", cex.lab = 2, cex.main = 1.5, cex.axis = 1.5, main = "")
    tempE <- hist(df_obs$rtconf[df_obs$cj == 0 & df_obs$coherence == coherence_vector[coherence] & df_obs$manipulation == condLab[cond]], 
                  breaks=seq(0,6.2,.1), prob = F, add = T, col = rgb(.26,.49,.56,.85), border = 'white')
    Cors <- hist(df_predictions$rtconf[df_predictions$cj == 1 & df_predictions$coherence == coherence_vector[coherence] & df_predictions$manipulation == condLab[cond]], 
                 breaks = seq(-2,30,.1), plot = F)
    Errs <- hist(df_predictions$rtconf[df_predictions$cj == 0 & df_predictions$coherence == coherence_vector[coherence] & df_predictions$manipulation == condLab[cond]], 
                 breaks = seq(-2,30,.1), plot = F)
    lines(Cors$counts/(sum(Cors$counts)/sum(tempC$counts))~ Cors$mids, type='l', col = rgb(.86,.52,.12,.85), lwd = 3)
    lines(Errs$counts/(sum(Errs$counts)/sum(tempE$counts))~ Errs$mids, type='l', col = rgb(.26,.49,.56,.85), lwd = 3)
    #legend("topright",fill=c("white","white","green","red"),border=F,legend=c("Simulated corrects","Simulated errors","Empirical corrects","Empirical errors"),col=rep(c("Green","Red"),2),bty='n',lwd=c(1,1,-1,-1))
    
  }
}

## DDM marginal predictions ----

### Decision RT - manipulations ----

par(mfrow = c(1, 4), mai = c(0.4, 0.1, 0, 0.1))

for (cond in 1:4){  # Loop through conditions
  
  # Draw plots
  tempC <- hist(family='font', df_obs$rt[df_obs$cor == 1 & df_obs$manipulation == condLab[cond]], 
                breaks=seq(0,6.2,.3), xlim = c(0,3), ylim = c(0,2000), prob = F, col = rgb(0,1,0,.25), border = "white", ylab = "", xlab = "", cex.lab = 2, cex.main = 1.5, cex.axis = 1.5, main = "", yaxt = 'n', xaxt = 'n')
  tempE <- hist(df_obs$rt[df_obs$cor == 0 & df_obs$manipulation == condLab[cond]], 
                breaks=seq(0,6.2,.3), prob = F, add = T, col = rgb(1,0,0,.25), border = 'white')
  Cors <- hist(df_predictions$rt[df_predictions$cor == 1 & df_predictions$manipulation == condLab[cond]], 
               breaks = seq(0,30,.3), plot = F)
  Errs <- hist(df_predictions$rt[df_predictions$cor == 0 & df_predictions$manipulation == condLab[cond]], 
               breaks = seq(0,30,.3), plot = F)
  lines(Cors$counts/(sum(Cors$counts)/sum(tempC$counts))~ Cors$mids, type='l', col = 'green', lwd = 3)
  lines(Errs$counts/(sum(Errs$counts)/sum(tempE$counts))~ Errs$mids, type='l', col = 'red', lwd = 3)
  #legend("topright",fill=c("white","white","green","red"),border=F,legend=c("Simulated corrects","Simulated errors","Empirical corrects","Empirical errors"),col=rep(c("Green","Red"),2),bty='n',lwd=c(1,1,-1,-1))
  axis(side=1, at=c(0,1,2,3))

}

png(file="test.png",
    width=1200, 
    height=300,
    res = 170)
dev.off()

### Decision RT - coherence ----

par(mfrow = c(1, 3), mai = c(0.4, 0.1, 0.0, 0.1))

for (coherence in 1:3){  # Loop through coherence levels
  
  # Draw plots
  tempC <- hist(family='font', df_obs$rt[df_obs$cor == 1 & df_obs$coherence == coherence_vector[coherence]], 
                breaks=seq(0,6.2,.3), xlim = c(0,3), ylim = c(0,3000), prob = F, col = rgb(0,1,0,.25), border = "white", ylab = "", xlab = "", cex.lab = 2, cex.main = 1.5, cex.axis = 1.5, main = "", yaxt = 'n', xaxt = 'n')
  tempE <- hist(df_obs$rt[df_obs$cor == 0 & df_obs$coherence == coherence_vector[coherence]], 
                breaks=seq(0,6.2,.3), prob = F, add = T, col = rgb(1,0,0,.25), border = 'white')
  Cors <- hist(df_predictions$rt[df_predictions$cor == 1 & df_predictions$coherence == coherence_vector[coherence]], 
               breaks = seq(0,30,.3), plot = F)
  Errs <- hist(df_predictions$rt[df_predictions$cor == 0 & df_predictions$coherence == coherence_vector[coherence]], 
               breaks = seq(0,30,.3), plot = F)
  lines(Cors$counts/(sum(Cors$counts)/sum(tempC$counts))~ Cors$mids, type='l', col = 'green', lwd = 3)
  lines(Errs$counts/(sum(Errs$counts)/sum(tempE$counts))~ Errs$mids, type='l', col = 'red', lwd = 3)
  #legend("topright",fill=c("white","white","green","red"),border=F,legend=c("Simulated corrects","Simulated errors","Empirical corrects","Empirical errors"),col=rep(c("Green","Red"),2),bty='n',lwd=c(1,1,-1,-1))
  axis(side=1, at=c(0,1,2,3))
  
}

png(file="test.png",
    width=900, 
    height=300,
    res = 170)
dev.off()

### Confidence RT (acc) - manipulations ----

par(mfrow = c(1, 4), mai = c(0.4, 0.1, 0, 0.1))

for (cond in 1:4){  # Loop through conditions
  
  # Draw plots
  tempC <- hist(family='font', df_obs$rtconf[df_obs$cor == 1 & df_obs$manipulation == condLab[cond]], 
                breaks=seq(-1,6.2,.3), xlim = c(0,3), ylim = c(0,2700), prob = F, col = rgb(0,1,0,.25), border = "white", ylab = "", xlab = "", cex.lab = 2, cex.main = 1.5, cex.axis = 1.5, main = "", yaxt = 'n', xaxt = 'n')
  tempE <- hist(df_obs$rtconf[df_obs$cor == 0 & df_obs$manipulation == condLab[cond]], 
                breaks=seq(-1,6.2,.3), prob = F, add = T, col = rgb(1,0,0,.25), border = 'white')
  Cors <- hist(df_predictions$rtconf[df_predictions$cor == 1 & df_predictions$manipulation == condLab[cond]], 
               breaks = seq(-1,30,.3), plot = F)
  Errs <- hist(df_predictions$rtconf[df_predictions$cor == 0 & df_predictions$manipulation == condLab[cond]], 
               breaks = seq(-1,30,.3), plot = F)
  lines(Cors$counts/(sum(Cors$counts)/sum(tempC$counts))~ Cors$mids, type='l', col = 'green', lwd = 3)
  lines(Errs$counts/(sum(Errs$counts)/sum(tempE$counts))~ Errs$mids, type='l', col = 'red', lwd = 3)
  #legend("topright",fill=c("white","white","green","red"),border=F,legend=c("Simulated corrects","Simulated errors","Empirical corrects","Empirical errors"),col=rep(c("Green","Red"),2),bty='n',lwd=c(1,1,-1,-1))
  axis(side=1, at=c(0,1,2,3))
  
}

### Confidence RT (acc) - coherence ----

par(mfrow = c(1, 3), mai = c(0.4, 0.1, 0, 0.1))

for (coherence in 1:3){  # Loop through coherence levels
  
  # Draw plots
  tempC <- hist(family='font', df_obs$rtconf[df_obs$cor == 1 & df_obs$coherence == coherence_vector[coherence]], 
                breaks=seq(-1,6.2,.3), xlim = c(0,3), ylim = c(0,3700), prob = F, col = rgb(0,1,0,.25), border = "white", ylab = "", xlab = "", cex.lab = 2, cex.main = 1.5, cex.axis = 1.5, main = "", yaxt = 'n', xaxt = 'n')
  tempE <- hist(df_obs$rtconf[df_obs$cor == 0 & df_obs$coherence == coherence_vector[coherence]], 
                breaks=seq(-1,6.2,.3), prob = F, add = T, col = rgb(1,0,0,.25), border = 'white')
  Cors <- hist(df_predictions$rtconf[df_predictions$cor == 1 & df_predictions$coherence == coherence_vector[coherence]], 
               breaks = seq(-1,30,.3), plot = F)
  Errs <- hist(df_predictions$rtconf[df_predictions$cor == 0 & df_predictions$coherence == coherence_vector[coherence]], 
               breaks = seq(-1,30,.3), plot = F)
  lines(Cors$counts/(sum(Cors$counts)/sum(tempC$counts))~ Cors$mids, type='l', col = 'green', lwd = 3)
  lines(Errs$counts/(sum(Errs$counts)/sum(tempE$counts))~ Errs$mids, type='l', col = 'red', lwd = 3)
  #legend("topright",fill=c("white","white","green","red"),border=F,legend=c("Simulated corrects","Simulated errors","Empirical corrects","Empirical errors"),col=rep(c("Green","Red"),2),bty='n',lwd=c(1,1,-1,-1))
  axis(side=1, at=c(0,1,2,3))
  
}

### Confidence RT (cj) - manipulations ----

par(mfrow = c(1, 4), mai = c(0.4, 0.1, 0, 0.1))

for (cond in 1:4){  # Loop through conditions
  
  # Draw plots
  tempC <- hist(family='font', df_obs$rtconf[df_obs$cor == 1 & df_obs$manipulation == condLab[cond]], 
                breaks=seq(-1,6.2,.3), xlim = c(0,3), ylim = c(0,2800), prob = F, col = rgb(.86,.52,.12,.65), border = "white", ylab = "", xlab = "", cex.lab = 2, cex.main = 1.5, cex.axis = 1.5, main = "", yaxt = 'n', xaxt = 'n')
  tempE <- hist(df_obs$rtconf[df_obs$cor == 0 & df_obs$manipulation == condLab[cond]], 
                breaks=seq(-1,6.2,.3), prob = F, add = T, col = rgb(.26,.49,.56,.65), border = 'white')
  Cors <- hist(df_predictions$rtconf[df_predictions$cor == 1 & df_predictions$manipulation == condLab[cond]], 
               breaks = seq(-1,30,.3), plot = F)
  Errs <- hist(df_predictions$rtconf[df_predictions$cor == 0 & df_predictions$manipulation == condLab[cond]], 
               breaks = seq(-1,30,.3), plot = F)
  lines(Cors$counts/(sum(Cors$counts)/sum(tempC$counts))~ Cors$mids, type='l', col = rgb(.86,.52,.12,.85), lwd = 3)
  lines(Errs$counts/(sum(Errs$counts)/sum(tempE$counts))~ Errs$mids, type='l', col = rgb(.26,.49,.56,.85), lwd = 3)
  #legend("topright",fill=c("white","white","green","red"),border=F,legend=c("Simulated corrects","Simulated errors","Empirical corrects","Empirical errors"),col=rep(c("Green","Red"),2),bty='n',lwd=c(1,1,-1,-1))
  axis(side=1, at=c(0,1,2,3))
  
}

### Confidence RT (cj) - coherence ----

par(mfrow = c(1, 3), mai = c(0.4, 0.1, 0, 0.1))

for (coherence in 1:3){  # Loop through coherence levels
  
  # Draw plots
  tempC <- hist(family='font', df_obs$rtconf[df_obs$cor == 1 & df_obs$coherence == coherence_vector[coherence]], 
                breaks=seq(-1,6.2,.3), xlim = c(0,3), ylim = c(0,3500), prob = F, col = rgb(.86,.52,.12,.65), border = "white", ylab = "", xlab = "", cex.lab = 2, cex.main = 1.5, cex.axis = 1.5, main = "", yaxt = 'n', xaxt = 'n')
  tempE <- hist(df_obs$rtconf[df_obs$cor == 0 & df_obs$coherence == coherence_vector[coherence]], 
                breaks=seq(-1,6.2,.3), prob = F, add = T, col = rgb(.26,.49,.56,.65), border = 'white')
  Cors <- hist(df_predictions$rtconf[df_predictions$cor == 1 & df_predictions$coherence == coherence_vector[coherence]], 
               breaks = seq(-1,30,.3), plot = F)
  Errs <- hist(df_predictions$rtconf[df_predictions$cor == 0 & df_predictions$coherence == coherence_vector[coherence]], 
               breaks = seq(-1,30,.3), plot = F)
  lines(Cors$counts/(sum(Cors$counts)/sum(tempC$counts))~ Cors$mids, type='l', col = rgb(.86,.52,.12,.85), lwd = 3)
  lines(Errs$counts/(sum(Errs$counts)/sum(tempE$counts))~ Errs$mids, type='l', col = rgb(.26,.49,.56,.85), lwd = 3)
  #legend("topright",fill=c("white","white","green","red"),border=F,legend=c("Simulated corrects","Simulated errors","Empirical corrects","Empirical errors"),col=rep(c("Green","Red"),2),bty='n',lwd=c(1,1,-1,-1))
  axis(side=1, at=c(0,1,2,3))
  
}

## Behavioral data ----

### Preparation ----

df_obs_mean <- df_obs %>%
  group_by(sub, coherence, manipulation) %>% 
  summarise_each(funs(mean))

df_predictions_mean <- df_predictions %>%
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
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
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

