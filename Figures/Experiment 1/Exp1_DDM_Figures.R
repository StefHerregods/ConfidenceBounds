
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

if(Sys.info()[["user"]]=="u0136938"){
  sourceCpp("C:/Users/u0136938/OneDrive - KU Leuven/Documents/Projecten/Stef - sato op confidence/ConfidenceBounds\\Analyses\\Exp1_analyses\\DDM_confidence_bounds.cpp") 
}else{
  sourceCpp("C:\\Users\\herre\\OneDrive\\Documenten\\GitHub\\ConfidenceBounds\\Analyses\\Exp1_analyses\\DDM_confidence_bounds.cpp") 
}

## Set working directory

if(Sys.info()[["user"]]=="u0136938"){
  setwd("C:/Users/u0136938/OneDrive - KU Leuven/Documents/Projecten/Stef - sato op confidence/ConfidenceBounds/Data")
}else{
  setwd('C:\\Users\\herre\\Desktop\\Internship\\Results\\Exp1_Results')
}

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
  mutate(rt_manipulation = as.factor(ifelse(manipulation %in% c("AccAcc", "AccFast"), 1, 0)),
         rtconf_manipulation = as.factor(ifelse(manipulation %in% c("AccAcc", "FastAcc"), 1, 0))) %>%
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
  geom_errorbar(aes(x = manipulation, y = a, ymin = a_mean - a_sd / sqrt(40), ymax = a_mean + a_sd / sqrt(40)), position = position_nudge(-0.28), size = 1, width = 0.2) +
  geom_point(aes(x = manipulation, y = a_mean), position = position_nudge(-0.28)) +
  geom_line(aes(x = manipulation, y = a_mean, group = sub), position = position_nudge(-0.28), size = 1) +
  scale_x_discrete(labels = c('AA', 'AF', 'FA', 'FF')) +
  ylim(c(.66, 3.3)) +
  xlab(label = 'Condition') +
  ylab(label = 'Decision Boundary Separation') +
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

a <- ggplot(data = df_DDM, aes(x = strtoi(rt_manipulation), y = a, color = as.factor(rtconf_manipulation))) +
  geom_point(size = 2.5, stroke = 1, shape = 16, alpha = 0.2, position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.5)) +
  stat_summary(geom = "line", size = 1, fun = "mean", position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = a_mean - a_sd / sqrt(40), ymax = a_mean + a_sd / sqrt(40), group = as.factor(rtconf_manipulation)), position = position_dodge(width = 0.5), size = 1, width = 0) +
  stat_summary(geom = "point", size = 2.5, fun = "mean", position = position_dodge(width = 0.5)) +
  scale_x_continuous(labels = c('"Make fast\ndecisions"', '"Make accurate\n decisions"'),breaks = c(0, 1)) +
  scale_color_manual(values = c('#CA3C25', '#FFA630')) +
  ylab(label = 'Decision Boundary Separation') +
  xlab(label = '') +
  theme(axis.line = element_line(colour = 'black'),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = '#e0e0e0', size = 0.7),
        panel.grid.minor.x = element_blank(),
        panel.border = element_blank(),
        axis.ticks.length = unit(.2, 'cm'),
        panel.background = element_blank(),
        text = element_text(family = 'font', size = 12),
        axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 5, r = 0, b = 0, l = 0)),
        legend.position = 'none',
        strip.background = element_blank(),
        strip.text.x = element_text(size = 11),
        plot.margin=unit(c(.5,0.2,.5,0.2),"cm"))



### a2

a2 <- ggplot(data = df_DDM) +
  geom_line(aes(x = manipulation_jitter, y = a2, group = sub), alpha = 0.15, size = 0.3) +
  geom_point(aes(x = manipulation_jitter, y = a2), fill = '#999999', color = 'white', size = 4, shape = 21, stroke = 1) +
  geom_errorbar(aes(x = manipulation, y = a2, ymin = a2_mean - a2_sd / sqrt(40), ymax = a2_mean + a2_sd / sqrt(40)), position = position_nudge(-0.28), size = 1, width = 0.2) +
  geom_point(aes(x = manipulation, y = a2_mean), position = position_nudge(-0.28)) +
  geom_line(aes(x = manipulation, y = a2_mean, group = sub), position = position_nudge(-0.28), size = 1) +
  scale_x_discrete(labels = c('AA', 'AF', 'FA', 'FF')) +
  ylim(c(.5, 5.5)) +
  xlab(label = 'Condition') +
  ylab(label = 'Confidence Boundary Separation') +
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

a2 <- ggplot(data = df_DDM, aes(x = strtoi(rt_manipulation), y = a2, color = as.factor(rtconf_manipulation))) +
  geom_point(size = 2.5, stroke = 1, shape = 16, alpha = 0.2, position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.5)) +
  stat_summary(geom = "point", size = 2.5, fun = "mean", position = position_dodge(width = 0.5)) +
  stat_summary(geom = "line", size = 1, fun = "mean", position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = a2_mean - a2_sd / sqrt(40), ymax = a2_mean + a2_sd / sqrt(40), group = as.factor(rtconf_manipulation)), position = position_dodge(width = 0.5), size = 1, width = 0) +
  scale_x_continuous(labels = c('"Make fast\ndecisions"', '"Make accurate\n decisions"'),breaks = c(0, 1)) +
  scale_color_manual(values = c('#CA3C25', '#FFA630')) +
  ylab(label = 'Confidence Boundary Separation') +
  xlab(label = '') +
  theme(axis.line = element_line(colour = 'black'),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = '#e0e0e0', size = 0.7),
        panel.grid.minor.x = element_blank(),
        panel.border = element_blank(),
        axis.ticks.length = unit(.2, 'cm'),
        panel.background = element_blank(),
        text = element_text(family = 'font', size = 12),
        axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 5, r = 0, b = 0, l = 0)),
        legend.position = 'none',
        strip.background = element_blank(),
        strip.text.x = element_text(size = 11),
        plot.margin=unit(c(.5,0.2,.5,0.2),"cm"))

### a2_slope

a2_slope <- ggplot(data = df_DDM) +
  geom_line(aes(x = manipulation_jitter, y = a2_slope, group = sub), alpha = 0.15, size = 0.3) +
  geom_point(aes(x = manipulation_jitter, y = a2_slope), fill = '#999999', color = 'white', size = 4, shape = 21, stroke = 1) +
  geom_errorbar(aes(x = manipulation, y = a2_slope, ymin = a2_slope_mean - a2_slope_sd / sqrt(40), ymax = a2_slope_mean + a2_slope_sd / sqrt(40)), position = position_nudge(-0.28), size = 1, width = 0.2) +
  geom_point(aes(x = manipulation, y = a2_slope_mean), position = position_nudge(-0.28)) +
  geom_line(aes(x = manipulation, y = a2_slope_mean, group = sub), position = position_nudge(-0.28), size = 1) +
  scale_x_discrete(labels = c('AA', 'AF', 'FA', 'FF')) +
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

a2_slope <- ggplot(data = df_DDM, aes(x = strtoi(rt_manipulation), y = a2_slope, color = as.factor(rtconf_manipulation))) +
  geom_errorbar(aes(ymin = a2_slope_mean - a2_slope_sd / sqrt(40), ymax = a2_slope_mean + a2_slope_sd / sqrt(40), group = as.factor(rtconf_manipulation)), position = position_dodge(width = 0.5), size = 1, width = 0) +
  geom_point(size = 2.5, stroke = 1, shape = 16, alpha = 0.2, position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.5)) +
  stat_summary(geom = "line", size = 1, fun = "mean", position = position_dodge(width = 0.5)) +
  stat_summary(geom = "point", size = 2.5, fun = "mean", position = position_dodge(width = 0.5)) +
  scale_x_continuous(labels = c('"Make fast\ndecisions"', '"Make accurate\n decisions"'),breaks = c(0, 1)) +
  scale_color_manual(values = c('#CA3C25', '#FFA630')) +
  ylab(label = 'Urgency') +
  xlab(label = '') +
  theme(axis.line = element_line(colour = 'black'),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = '#e0e0e0', size = 0.7),
        panel.grid.minor.x = element_blank(),
        panel.border = element_blank(),
        axis.ticks.length = unit(.2, 'cm'),
        panel.background = element_blank(),
        text = element_text(family = 'font', size = 12),
        axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 5, r = 0, b = 0, l = 0)),
        legend.position = 'none',
        strip.background = element_blank(),
        strip.text.x = element_text(size = 11),
        plot.margin=unit(c(.5,0.2,.5,0.2),"cm"))




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

par(mfrow = c(1, 4), mai = c(0.4, 0.17, 0.2, 0.17))

for (cond in 1:4){  # Loop through conditions

  # Draw plots
  tempC <- hist(family='font', df_obs$rt[df_obs$cor == 1 & df_obs$manipulation == condLab[cond]], 
                breaks=seq(0,6.2,.25), xlim = c(0,5), ylim = c(0,2000), prob = F, col = rgb(0,1,0,.25), border = "white", ylab = "", xlab = "", cex.lab = 1, cex.main = 1.5, cex.axis = 1, main = "", yaxp = c(0, 3000, 1), tck = -0.03)
  tempE <- hist(df_obs$rt[df_obs$cor == 0 & df_obs$manipulation == condLab[cond]], 
                breaks=seq(0,6.2,.25), prob = F, add = T, col = rgb(1,0,0,.25), border = 'white')
  Cors <- hist(df_predictions$rt[df_predictions$cor == 1 & df_predictions$manipulation == condLab[cond]], 
               breaks = seq(0,30,.25), plot = F)
  Errs <- hist(df_predictions$rt[df_predictions$cor == 0 & df_predictions$manipulation == condLab[cond]], 
               breaks = seq(0,30,.25), plot = F)
  lines(Cors$counts/(sum(Cors$counts)/sum(tempC$counts))~ Cors$mids, type='l', col = 'green', lwd = 3)
  lines(Errs$counts/(sum(Errs$counts)/sum(tempE$counts))~ Errs$mids, type='l', col = 'red', lwd = 3)
  #legend("topright",fill=c("white","white","green","red"),border=F,legend=c("Simulated corrects","Simulated errors","Empirical corrects","Empirical errors"),col=rep(c("Green","Red"),2),bty='n',lwd=c(1,1,-1,-1))

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
                breaks=seq(0,6.2,.25), xlim = c(0,3), ylim = c(0,2000), prob = F, col = rgb(0,1,0,.25), border = "white", ylab = "", xlab = "", cex.lab = 2, cex.main = 1.5, cex.axis = 1.5, main = "", yaxt = 'n', xaxt = 'n')
  tempE <- hist(df_obs$rt[df_obs$cor == 0 & df_obs$coherence == coherence_vector[coherence]], 
                breaks=seq(0,6.2,.25), prob = F, add = T, col = rgb(1,0,0,.25), border = 'white')
  Cors <- hist(df_predictions$rt[df_predictions$cor == 1 & df_predictions$coherence == coherence_vector[coherence]], 
               breaks = seq(0,30,.25), plot = F)
  Errs <- hist(df_predictions$rt[df_predictions$cor == 0 & df_predictions$coherence == coherence_vector[coherence]], 
               breaks = seq(0,30,.25), plot = F)
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

par(mfrow = c(1, 4), mai = c(0.4, 0.17, 0.2, 0.17))

for (cond in 1:4){  # Loop through conditions
  
  # Draw plots
  tempC <- hist(family='font', df_obs$rtconf[df_obs$cor == 1 & df_obs$manipulation == condLab[cond]], 
                breaks=seq(-1,6.2,.16), xlim = c(-.333,3), ylim = c(0,2000), prob = F, col = rgb(0,1,0,.25), border = "white", ylab = "", xlab = "", cex.lab = 2, cex.main = 1.5, cex.axis = 1, main = "", yaxp = c(0, 3800, 1), tck = -0.03)
  tempE <- hist(df_obs$rtconf[df_obs$cor == 0 & df_obs$manipulation == condLab[cond]], 
                breaks=seq(-1,6.2,.16), prob = F, add = T, col = rgb(1,0,0,.25), border = 'white')
  Cors <- hist(df_predictions$rtconf[df_predictions$cor == 1 & df_predictions$manipulation == condLab[cond]], 
               breaks = seq(-1,30,.16), plot = F)
  Errs <- hist(df_predictions$rtconf[df_predictions$cor == 0 & df_predictions$manipulation == condLab[cond]], 
               breaks = seq(-1,30,.16), plot = F)
  lines(Cors$counts/(sum(Cors$counts)/sum(tempC$counts))~ Cors$mids, type='l', col = 'green', lwd = 3)
  lines(Errs$counts/(sum(Errs$counts)/sum(tempE$counts))~ Errs$mids, type='l', col = 'red', lwd = 3)
  #legend("topright",fill=c("white","white","green","red"),border=F,legend=c("Simulated corrects","Simulated errors","Empirical corrects","Empirical errors"),col=rep(c("Green","Red"),2),bty='n',lwd=c(1,1,-1,-1))

}

### Confidence RT (acc) - coherence ----

par(mfrow = c(1, 3), mai = c(0.4, 0.1, 0, 0.1))

for (coherence in 1:3){  # Loop through coherence levels
  
  # Draw plots
  tempC <- hist(family='font', df_obs$rtconf[df_obs$cor == 1 & df_obs$coherence == coherence_vector[coherence]], 
                breaks=seq(-1,6.2,.15), xlim = c(0,3), ylim = c(0,3700), prob = F, col = rgb(0,1,0,.25), border = "white", ylab = "", xlab = "", cex.lab = 2, cex.main = 1.5, cex.axis = 1.5, main = "", yaxt = 'n', xaxt = 'n')
  tempE <- hist(df_obs$rtconf[df_obs$cor == 0 & df_obs$coherence == coherence_vector[coherence]], 
                breaks=seq(-1,6.2,.15), prob = F, add = T, col = rgb(1,0,0,.25), border = 'white')
  Cors <- hist(df_predictions$rtconf[df_predictions$cor == 1 & df_predictions$coherence == coherence_vector[coherence]], 
               breaks = seq(-1,30,.15), plot = F)
  Errs <- hist(df_predictions$rtconf[df_predictions$cor == 0 & df_predictions$coherence == coherence_vector[coherence]], 
               breaks = seq(-1,30,.15), plot = F)
  lines(Cors$counts/(sum(Cors$counts)/sum(tempC$counts))~ Cors$mids, type='l', col = 'green', lwd = 3)
  lines(Errs$counts/(sum(Errs$counts)/sum(tempE$counts))~ Errs$mids, type='l', col = 'red', lwd = 3)
  #legend("topright",fill=c("white","white","green","red"),border=F,legend=c("Simulated corrects","Simulated errors","Empirical corrects","Empirical errors"),col=rep(c("Green","Red"),2),bty='n',lwd=c(1,1,-1,-1))
  axis(side=1, at=c(0,1,2,3))
  
}

### Confidence RT (cj) - manipulations ----

par(mfrow = c(1, 4), mai = c(0.4, 0.17, 0.2, 0.17))

for (cond in 1:4){  # Loop through conditions
  
  # Draw plots
  tempC <- hist(family='font', df_obs$rtconf[df_obs$cj == 1 & df_obs$manipulation == condLab[cond]], 
                breaks=seq(-1,6.2,.15), xlim = c(-.333,3), ylim = c(0,2000), prob = F, col = rgb(.98,.55,.14,.75), border = "white", ylab = "", xlab = "", cex.lab = 2, cex.main = 1.5, cex.axis = 1, main = "", yaxp = c(0, 3800, 1), tck = -0.03)
  tempE <- hist(df_obs$rtconf[df_obs$cj == 0 & df_obs$manipulation == condLab[cond]], 
                breaks=seq(-1,6.2,.15), prob = F, add = T, col = rgb(.06,.30,.36,.65), border = 'white')
  Cors <- hist(df_predictions$rtconf[df_predictions$cor == 1 & df_predictions$manipulation == condLab[cond]], 
               breaks = seq(-1,30,.15), plot = F)
  Errs <- hist(df_predictions$rtconf[df_predictions$cor == 0 & df_predictions$manipulation == condLab[cond]], 
               breaks = seq(-1,30,.15), plot = F)
  lines(Cors$counts/(sum(Cors$counts)/sum(tempC$counts))~ Cors$mids, type='l', col = rgb(.98,.55,.14,.85), lwd = 3)
  lines(Errs$counts/(sum(Errs$counts)/sum(tempE$counts))~ Errs$mids, type='l', col = rgb(.06,.30,.36,.85), lwd = 3)
  #legend("topright",fill=c("white","white","green","red"),border=F,legend=c("Simulated corrects","Simulated errors","Empirical corrects","Empirical errors"),col=rep(c("Green","Red"),2),bty='n',lwd=c(1,1,-1,-1))

}

### Confidence RT (cj) - coherence ----

par(mfrow = c(1, 3), mai = c(0.4, 0.1, 0, 0.1))

for (coherence in 1:3){  # Loop through coherence levels
  
  # Draw plots
  tempC <- hist(family='font', df_obs$rtconf[df_obs$cj == 1 & df_obs$coherence == coherence_vector[coherence]], 
                breaks=seq(-1,6.2,.3), xlim = c(0,3), ylim = c(0,3500), prob = F, col = rgb(.86,.52,.12,.65), border = "white", ylab = "", xlab = "", cex.lab = 2, cex.main = 1.5, cex.axis = 1.5, main = "", yaxt = 'n', xaxt = 'n')
  tempE <- hist(df_obs$rtconf[df_obs$cj == 0 & df_obs$coherence == coherence_vector[coherence]], 
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
  summarise_each(funs(mean)) %>%
  group_by(coherence, manipulation) %>%
  mutate(rt_mean = mean(rt),
         rt_sd = sd(rt),
         rtconf_mean = mean(rtconf),
         rtconf_sd = sd(rtconf),
         accuracy_mean = mean(cor),
         accuracy_sd = sd(cor),
         cj_mean = mean(cj),
         cj_sd = sd(cj))


df_predictions_mean <- df_predictions %>%
  mutate(cj = as.integer(cj)) %>%
  group_by(sub, coherence, manipulation) %>% 
  summarise_each(funs(mean)) %>%
  group_by(coherence, manipulation) %>%
  mutate(rt_mean = mean(rt),
         rt_sd = sd(rt),
         rtconf_mean = mean(rtconf),
         rtconf_sd = sd(rtconf),
         accuracy_mean = mean(cor),
         accuracy_sd = sd(cor),
         cj_mean = mean(cj),
         cj_sd = sd(cj))

manipulations <- c('0' = '"Make fast decisions"',
                   '1' = '"Make accurate\ndecisions"')

### RT ---- 

rt <- ggplot(data = df_obs_mean, aes(x = coherence, y = rt, color = as.factor(rtconf_manipulation))) +
  geom_point(size = 2.5, stroke = 1, shape = 16, alpha = 0.1, position = position_jitterdodge(jitter.width = 0.04, dodge.width = 0.07)) +
  stat_summary(geom = "point", size = 2.5, fun = "mean", position = position_dodge(width = 0.07)) +
  stat_summary(geom = "line", size = 1, fun = "mean", position = position_dodge(width = 0.07)) +
  geom_errorbar(aes(ymin = rt_mean - rt_sd / sqrt(40), ymax = rt_mean + rt_sd / sqrt(40), group = as.factor(rtconf_manipulation)), position = position_dodge(width = 0.07), size = 1, width = 0) +
  stat_summary(data = df_predictions_mean, aes(x = coherence, y = rt, group = as.factor(rtconf_manipulation)), color = 'black', geom = "point", size = 1.5, stroke = 1, shape = 4, fun = "mean", position = position_dodge(width = 0.07)) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1), breaks = c(0.1, 0.2, 0.4)) +
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
        axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 5, r = 0, b = 0, l = 0)),
        legend.position = 'none',
        strip.background = element_blank(),
        strip.text.x = element_text(size = 11),
        plot.margin=unit(c(.5,0.2,.5,0.2),"cm")) +
  facet_wrap(~rt_manipulation, labeller = as_labeller(manipulations))

### confidence RT ----

rtconf <- ggplot(data = df_obs_mean, aes(x = coherence, y = rtconf, color = as.factor(rtconf_manipulation))) +
  geom_point(size = 2.5, stroke = 1, shape = 16, alpha = 0.1, position = position_jitterdodge(jitter.width = 0.04, dodge.width = 0.07)) +
  stat_summary(geom = "point", size = 2.5, fun = "mean", position = position_dodge(width = 0.07)) +
  stat_summary(geom = "line", size = 1, fun = "mean", position = position_dodge(width = 0.07)) +
  geom_errorbar(aes(ymin = rtconf_mean - rtconf_sd / sqrt(40), ymax = rtconf_mean + rtconf_sd / sqrt(40), group = as.factor(rtconf_manipulation)), position = position_dodge(width = 0.07), size = 1, width = 0) +
  stat_summary(data = df_predictions_mean, aes(x = coherence, y = rtconf, group = as.factor(rtconf_manipulation)), color = 'black', geom = "point", size = 1.5, stroke = 1, shape = 4, fun = "mean", position = position_dodge(width = 0.07)) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1), breaks = c(0.1, 0.2, 0.4)) +
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
        axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 5, r = 0, b = 0, l = 0)),
        legend.position = 'none',
        strip.background = element_blank(),
        strip.text.x = element_text(size = 11),
        plot.margin=unit(c(.5,0.2,.5,0.2),"cm")) +
  facet_wrap(~rt_manipulation, labeller = as_labeller(manipulations))

### Accuracy ----

cor <- ggplot(data = df_obs_mean, aes(x = coherence, y = cor, color = as.factor(rtconf_manipulation))) +
  geom_point(size = 2.5, stroke = 1, shape = 16, alpha = 0.1, position = position_jitterdodge(jitter.width = 0.04, dodge.width = 0.07)) +
  stat_summary(geom = "point", size = 2.5, fun = "mean", position = position_dodge(width = 0.07)) +
  stat_summary(geom = "line", size = 1, fun = "mean", position = position_dodge(width = 0.07)) +
  geom_errorbar(aes(ymin = accuracy_mean - accuracy_sd / sqrt(40), ymax = accuracy_mean + accuracy_sd / sqrt(40), group = as.factor(rtconf_manipulation)), position = position_dodge(width = 0.07), size = 1, width = 0) +
  stat_summary(data = df_predictions_mean, aes(x = coherence, y = cor, group = as.factor(rtconf_manipulation)), color = 'black', geom = "point", size = 1.5, stroke = 1, shape = 4, fun = "mean", position = position_dodge(width = 0.07)) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1), breaks = c(0.1, 0.2, 0.4)) +
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
        axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 5, r = 0, b = 0, l = 0)),
        legend.position = 'none',
        strip.background = element_blank(),
        strip.text.x = element_text(size = 11),
        plot.margin=unit(c(.5,0.2,.5,0.2),"cm")) +
  facet_wrap(~rt_manipulation, labeller = as_labeller(manipulations))

### Confidence judgement ----

cj <- ggplot(data = df_obs_mean, aes(x = coherence, y = cj, color = as.factor(rtconf_manipulation))) +
  geom_point(size = 2.5, stroke = 1, shape = 16, alpha = 0.1, position = position_jitterdodge(jitter.width = 0.04, dodge.width = 0.07)) +
  stat_summary(geom = "point", size = 2.5, fun = "mean", position = position_dodge(width = 0.07)) +
  stat_summary(geom = "line", size = 1, fun = "mean", position = position_dodge(width = 0.07)) +
  geom_errorbar(aes(ymin = cj_mean - cj_sd / sqrt(40), ymax = cj_mean + cj_sd / sqrt(40), group = as.factor(rtconf_manipulation)), position = position_dodge(width = 0.07), size = 1, width = 0) +
  stat_summary(data = df_predictions_mean, aes(x = coherence, y = cj, group = as.factor(rtconf_manipulation)), color = 'black', geom = "point", size = 1.5, stroke = 1, shape = 4, fun = "mean", position = position_dodge(width = 0.07)) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1), breaks = c(0.1, 0.2, 0.4)) +
  scale_color_manual(values = c('#CA3C25', '#FFA630')) +
  ylab(label = 'Mean Confidence Judgement') +
  xlab(label = 'Coherence') +
  theme(axis.line = element_line(colour = 'black'),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = '#e0e0e0', size = 0.7),
        panel.grid.minor.x = element_blank(),
        panel.border = element_blank(),
        axis.ticks.length = unit(.2, 'cm'),
        panel.background = element_blank(),
        text = element_text(family = 'font', size = 12),
        axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 5, r = 0, b = 0, l = 0)),
        legend.position = 'none',
        strip.background = element_blank(),
        strip.text.x = element_text(size = 11),
        plot.margin=unit(c(.5,0.2,.5,0.2),"cm")) +
  facet_wrap(~rt_manipulation, labeller = as_labeller(manipulations))

ggsave(filename = 'test.png',
       plot = ggarrange(rt, cor, rtconf, cj, nrow = 2, ncol = 2),
       device = 'png',
       width = 17,
       height = 19,
       units = 'cm')

<<<<<<< Updated upstream
#Extra plot kobe
df_obs$cond1 <- "Fast" 
df_obs$cond1[df_obs$manipulation=="AccAcc"] <- "Acc" 
df_obs$cond1[df_obs$manipulation=="AccFast"] <- "Acc" 
df_obs$cond2 <- "Fast" 
df_obs$cond2[df_obs$manipulation=="AccAcc"] <- "Acc" 
df_obs$cond2[df_obs$manipulation=="FastAcc"] <- "Acc" 
table(df_obs$cond1,df_obs$cond2,df_obs$manipulation)

fit <- lme4::glmer(cj~coherence*cond1*cond2+(1|sub),df_obs,family=binomial)
car::Anova(fit)
plot(effects::effect('cond1:cond2',fit))
data.frame(effects::effect('cond1:cond2',fit))
plot(effects::effect('coherence:cond1',fit))
data.frame(effects::effect('coherence:cond1',fit))

temp <- with(df_obs,aggregate(cj,by=list(cond1=cond1,cond2=cond2,sub=sub),mean));temp<-reshape::cast(temp,sub~cond1+cond2)

par(mfrow=c(1,1))
locX <- t(jitter(matrix(rep(1:2,N),2,N),.35)) #jitter for individual points
plot(colMeans(temp)[1:2],frame=F,cex.lab=1.5,type='n',ylim=range(temp),xlim=c(.75,2.25),ylab="Confidence",xlab="Choice SAT",xaxt='n');axis(1,at=1:2,labels=c('Accurate','Fast'))
for(i in 1:N) points(locX[i,],temp[i,4:5],pch=21,bg=rgb(.75, 0,0,.25),col="white",cex=2)
for(i in 1:N) points(locX[i,]+.15,temp[i,2:3],pch=21,bg=rgb(.75,.75,0,.25),col="white",cex=2)
points(1:2,colMeans(temp)[1:2],pch=19,type='b',col="red",lwd=3,cex=2);
error.bar(1:2,colMeans(temp)[1:2],matrixStats::colSds(as.matrix(temp))[1:2]/sqrt(N),length=0,lwd=3,col="red")
points(1:2+.15,colMeans(temp)[3:4],pch=19,type='b',col=rgb(.75,.75,0,1),lwd=3,cex=2,lty=2);
error.bar(1:2+.15,colMeans(temp)[3:4],matrixStats::colSds(as.matrix(temp))[3:4]/sqrt(N),length=0,lwd=3,col=rgb(.75,.75,0,1))
legend("top",inset=.01,legend=c('Careful confidence','Fast confidence'),pch=19,box.lty=0,lty=1:2,col=c('red','green'),cex=1.25)


temp <- with(df_obs,aggregate(cj,by=list(coherence=coherence,cond1=cond1,sub=sub),mean));temp<-reshape::cast(temp,sub~cond1+coherence)
N <- dim(temp)[1]

par(mfrow=c(1,1))
locX <- t(jitter(matrix(rep(1:3,N),3,N),.35)) #jitter for individual points
plot(colMeans(temp)[1:3],frame=F,cex.lab=1.5,type='n',ylim=range(temp),xlim=c(.5,3.5),ylab="Confidence",xlab="Coherence",xaxt='n');axis(1,at=1:3,labels=coherence_vector)
for(i in 1:N) points(locX[i,],temp[i,5:7],pch=21,bg=rgb(.75, 0,0,.25),col="white",cex=2)
for(i in 1:N) points(locX[i,]+.15,temp[i,2:4],pch=21,bg=rgb(.75,.75,0,.25),col="white",cex=2)
points(1:3,colMeans(temp)[1:3],pch=19,type='b',col="red",lwd=3,cex=2);
error.bar(1:3,colMeans(temp)[1:3],matrixStats::colSds(as.matrix(temp))[1:3]/sqrt(N),length=0,lwd=3,col="red")
points(1:3+.15,colMeans(temp)[4:6],pch=19,type='b',col=rgb(.75,.75,0,1),lwd=3,cex=2);
error.bar(1:3+.15,colMeans(temp)[4:6],matrixStats::colSds(as.matrix(temp))[4:6]/sqrt(N),length=0,lwd=3,col=rgb(.75,.75,0,1))
=======

















>>>>>>> Stashed changes
