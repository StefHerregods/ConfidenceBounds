# All figures used in 'Modelling Speed-Accuracy Tradeoffs in the Stopping Rule 
# for Confidence Judgments' of Experiment 2


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

## Give R access to the DDM simulation function in C++

sourceCpp("Analyses\\Exp1_analyses\\Exp2_DDM_confidence_bounds.cpp") 

## Set font

windowsFonts(font = windowsFont("Times New Roman"))


# Load data ----


## Behavioral data ----

df_obs <- read.csv(file = "Data\\Experiment_2\\Exp2_data_viable.csv")
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


## DDM parameters ----

### Transformations

df_DDM$manipulation_jitter <- jitter(unclass(as.factor(df_DDM$manipulation)), factor = 0.6)
df_DDM <- df_DDM %>% 
  mutate(rt_manipulation = as.factor(ifelse(manipulation %in% c("AccAcc", "AccFast"), 1, 0)),
         rtconf_manipulation = as.factor(ifelse(manipulation %in% c("AccAcc", "FastAcc"), 1, 0))) %>%
  group_by(manipulation) %>% 
  mutate(a_mean = mean(a),
         a_sd = sd(a),
         a2_upper_mean = mean(a2_upper),
         a2_upper_sd = sd(a2_upper),
         a2_lower_mean = mean(a2_lower),
         a2_lower_sd = sd(a2_lower),
         a2_slope_upper_mean = mean(a2_slope_upper),
         a2_slope_upper_sd = sd(a2_slope_upper),
         a2_slope_lower_mean = mean(a2_slope_lower),
         a2_slope_lower_sd = sd(a2_slope_lower))

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

a <- ggplot(data = df_DDM) +
  geom_line(aes(x = manipulation_jitter, y = a, group = sub), alpha = 0.15, size = 0.3) +
  geom_point(aes(x = manipulation_jitter, y = a), fill = '#999999', color = 'white', size = 4, shape = 21, stroke = 1) +
  geom_errorbar(aes(x = manipulation, y = a, ymin = a_mean - a_sd / sqrt(40), ymax = a_mean + a_sd / sqrt(40)), position = position_nudge(-0.28), size = 1, width = 0.2) +
  geom_point(aes(x = manipulation, y = a_mean), position = position_nudge(-0.28)) +
  geom_line(aes(x = manipulation, y = a_mean, group = sub), position = position_nudge(-0.28), size = 1) +
  scale_x_discrete(labels = c('AA', 'AF', 'FA', 'FF')) +
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
  geom_errorbar(aes(ymin = a_mean - a_sd / sqrt(40), ymax = a_mean + a_sd / sqrt(40), group = as.factor(rtconf_manipulation)), position = position_dodge(width = 0.5), size = 1, width = 0) +
  geom_point(size = 2.5, stroke = 1, shape = 16, alpha = 0.2, position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.5)) +
  stat_summary(geom = "line", size = 1, fun = "mean", position = position_dodge(width = 0.5)) +
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

### a2_upper

a2_upper <- ggplot(data = df_DDM) +
  geom_line(aes(x = manipulation_jitter, y = a2_upper, group = sub), alpha = 0.15, size = 0.3) +
  geom_point(aes(x = manipulation_jitter, y = a2_upper), fill = '#999999', color = 'white', size = 4, shape = 21, stroke = 1) +
  geom_errorbar(aes(x = manipulation, y = a2_upper, ymin = a2_upper_mean - a2_upper_sd / sqrt(40), ymax = a2_upper_mean + a2_upper_sd / sqrt(40)), position = position_nudge(-0.28), size = 1, width = 0.2) +
  geom_point(aes(x = manipulation, y = a2_upper_mean), position = position_nudge(-0.28)) +
  geom_line(aes(x = manipulation, y = a2_upper_mean, group = sub), position = position_nudge(-0.28), size = 1) +
  scale_x_discrete(labels = c('AA', 'AF', 'FA', 'FF')) +
  xlab(label = 'Condition') +
  ylab(label = 'Upper Confidence Boundary') +
  ylim(c(0,15)) +
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

a2_upper <- ggplot(data = df_DDM, aes(x = strtoi(rt_manipulation), y = a2_upper, color = as.factor(rtconf_manipulation))) +
  geom_errorbar(aes(ymin = a2_upper_mean - a2_upper_sd / sqrt(40), ymax = a2_upper_mean + a2_upper_sd / sqrt(40), group = as.factor(rtconf_manipulation)), position = position_dodge(width = 0.5), size = 1, width = 0) +
  geom_point(size = 2.5, stroke = 1, shape = 16, alpha = 0.2, position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.5)) +
  stat_summary(geom = "line", size = 1, fun = "mean", position = position_dodge(width = 0.5)) +
  stat_summary(geom = "point", size = 2.5, fun = "mean", position = position_dodge(width = 0.5)) +
  scale_x_continuous(labels = c('"Make fast\ndecisions"', '"Make accurate\n decisions"'),breaks = c(0, 1)) +
  scale_color_manual(values = c('#CA3C25', '#FFA630')) +
  ylab(label = 'Upper Confidence Boundary') +
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

### a2_lower

a2_lower <- ggplot(data = df_DDM) +
  geom_line(aes(x = manipulation_jitter, y = -1*a2_lower, group = sub), alpha = 0.15, size = 0.3) +
  geom_point(aes(x = manipulation_jitter, y = -1*a2_lower), fill = '#999999', color = 'white', size = 4, shape = 21, stroke = 1) +
  geom_errorbar(aes(x = manipulation, y = -1*a2_lower, ymin = -1*a2_lower_mean - a2_lower_sd / sqrt(40), ymax = -1*a2_lower_mean + a2_lower_sd / sqrt(40)), position = position_nudge(-0.28), size = 1, width = 0.2) +
  geom_point(aes(x = manipulation, y = -1*a2_lower_mean), position = position_nudge(-0.28)) +
  geom_line(aes(x = manipulation, y = -1*a2_lower_mean, group = sub), position = position_nudge(-0.28), size = 1) +
  scale_x_discrete(labels = c('AA', 'AF', 'FA', 'FF')) +
  xlab(label = 'Condition') +
  ylab(label = 'Lower Confidence Boundary') +
  ylim(c(0,-15)) +
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

a2_lower <- ggplot(data = df_DDM, aes(x = strtoi(rt_manipulation), y = -1*a2_lower, color = as.factor(rtconf_manipulation))) +
  geom_errorbar(aes(ymin = -1*a2_lower_mean - a2_lower_sd / sqrt(40), ymax = -1*a2_lower_mean + a2_lower_sd / sqrt(40), group = as.factor(rtconf_manipulation)), position = position_dodge(width = 0.5), size = 1, width = 0) +
  geom_point(size = 2.5, stroke = 1, shape = 16, alpha = 0.2, position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.5)) +
  stat_summary(geom = "line", size = 1, fun = "mean", position = position_dodge(width = 0.5)) +
  stat_summary(geom = "point", size = 2.5, fun = "mean", position = position_dodge(width = 0.5)) +
  scale_x_continuous(labels = c('"Make fast\ndecisions"', '"Make accurate\n decisions"'),breaks = c(0, 1)) +
  scale_color_manual(values = c('#CA3C25', '#FFA630')) +
  ylab(label = 'Lower Confidence Boundary') +
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

### a2_slope_lower

a2_slope_lower <- ggplot(data = df_DDM) +
  geom_line(aes(x = manipulation_jitter, y = a2_slope_lower, group = sub), alpha = 0.15, size = 0.3) +
  geom_point(aes(x = manipulation_jitter, y = a2_slope_lower), fill = '#999999', color = 'white', size = 4, shape = 21, stroke = 1) +
  geom_errorbar(aes(x = manipulation, y = a2_slope_lower, ymin = a2_slope_lower_mean - a2_slope_lower_sd / sqrt(40), ymax = a2_slope_lower_mean + a2_slope_lower_sd / sqrt(40)), position = position_nudge(-0.28), size = 1, width = 0.2) +
  geom_point(aes(x = manipulation, y = a2_slope_lower_mean), position = position_nudge(-0.28)) +
  geom_line(aes(x = manipulation, y = a2_slope_lower_mean, group = sub), position = position_nudge(-0.28), size = 1) +
  scale_x_discrete(labels = c('AA', 'AF', 'FA', 'FF')) +
  xlab(label = 'Condition') +
  ylab(label = 'Lower Confidence Urgency') +
  ylim(c(0,15)) +
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

a2_slope_lower <- ggplot(data = df_DDM, aes(x = strtoi(rt_manipulation), y = a2_slope_lower, color = as.factor(rtconf_manipulation))) +
  geom_errorbar(aes(ymin = a2_slope_lower_mean - a2_slope_lower_sd / sqrt(40), ymax = a2_slope_lower_mean + a2_slope_lower_sd / sqrt(40), group = as.factor(rtconf_manipulation)), position = position_dodge(width = 0.5), size = 1, width = 0) +
  geom_point(size = 2.5, stroke = 1, shape = 16, alpha = 0.2, position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.5)) +
  stat_summary(geom = "line", size = 1, fun = "mean", position = position_dodge(width = 0.5)) +
  stat_summary(geom = "point", size = 2.5, fun = "mean", position = position_dodge(width = 0.5)) +
  scale_x_continuous(labels = c('"Make fast\ndecisions"', '"Make accurate\n decisions"'),breaks = c(0, 1)) +
  scale_color_manual(values = c('#CA3C25', '#FFA630')) +
  ylab(label = 'Lower Confidence Urgency') +
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

### a2_slope_upper

a2_slope_upper <- ggplot(data = df_DDM) +
  geom_line(aes(x = manipulation_jitter, y = a2_slope_upper, group = sub), alpha = 0.15, size = 0.3) +
  geom_point(aes(x = manipulation_jitter, y = a2_slope_upper), fill = '#999999', color = 'white', size = 4, shape = 21, stroke = 1) +
  geom_errorbar(aes(x = manipulation, y = a2_slope_upper, ymin = a2_slope_upper_mean - a2_slope_upper_sd / sqrt(40), ymax = a2_slope_upper_mean + a2_slope_upper_sd / sqrt(40)), position = position_nudge(-0.28), size = 1, width = 0.2) +
  geom_point(aes(x = manipulation, y = a2_slope_upper_mean), position = position_nudge(-0.28)) +
  geom_line(aes(x = manipulation, y = a2_slope_upper_mean, group = sub), position = position_nudge(-0.28), size = 1) +
  scale_x_discrete(labels = c('AA', 'AF', 'FA', 'FF')) +
  xlab(label = 'Condition') +
  ylab(label = 'Upper Confidence Urgency') +
  ylim(c(0,15)) +
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

a2_slope_upper <- ggplot(data = df_DDM, aes(x = strtoi(rt_manipulation), y = -a2_slope_upper, color = as.factor(rtconf_manipulation))) +
  geom_errorbar(aes(ymin = -a2_slope_upper_mean - a2_slope_upper_sd / sqrt(40), ymax = -a2_slope_upper_mean + a2_slope_upper_sd / sqrt(40), group = as.factor(rtconf_manipulation)), position = position_dodge(width = 0.5), size = 1, width = 0) +
  geom_point(size = 2.5, stroke = 1, shape = 16, alpha = 0.2, position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.5)) +
  stat_summary(geom = "line", size = 1, fun = "mean", position = position_dodge(width = 0.5)) +
  stat_summary(geom = "point", size = 2.5, fun = "mean", position = position_dodge(width = 0.5)) +
  scale_x_continuous(labels = c('"Make fast\ndecisions"', '"Make accurate\n decisions"'), breaks = c(0, 1)) +
  scale_color_manual(values = c('#CA3C25', '#FFA630')) +
  ylab(label = 'Upper Confidence Urgency') +
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
       plot = ggarrange(a, a2_upper, a2_lower, a2_slope_upper, a2_slope_lower, ncol = 3, nrow = 2),
       device = 'png',
       width = 19,
       height = 20,
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
a <- ggplot(data = df_2[df_2$cor == 1,], aes(x = as.factor(cj), y = freq_scaled, fill = data)) +
  geom_bar(stat = 'identity', position = position_dodge(), color = 'black') +
  geom_errorbar(aes(ymin = ymin, ymax = ymax), colour = 'black', size = 0.4, position = position_dodge(0.9), width = 0.2) +
  scale_fill_manual(name = '', 
                    values = c('#F18F01', '#006E90'),
                    labels = c('Observed', 'Predicted')) +
  theme_bw() +
  ylab('Frequency') +
  xlab('Confidence judgment') +
  theme(text = element_text(family = 'font', size = 12))


#### Plot incorrect
b <- ggplot(data = df_2[df_2$cor == 0,], aes(x = as.factor(cj), y = freq_scaled, fill = data)) +
  geom_bar(stat = 'identity', position = position_dodge(), color = 'black') +
  geom_errorbar(aes(ymin = ymin, ymax = ymax), colour = 'black', size = 0.4, position = position_dodge(0.9), width = 0.2) +
  scale_fill_manual(name = '', 
                    values = c('#F18F01', '#006E90'),
                    labels = c('Observed', 'Predicted')) +
  theme_bw() +
  ylab('Frequency') +
  xlab('Confidence judgment') +
  theme(text = element_text(family = 'font', size = 12))

ggsave(filename = 'test.png',
       plot = ggarrange(a, b, ncol = 2, nrow = 1),
       device = 'png',
       width = 16,
       height = 7,
       units = 'cm')




df <- data.frame(table(df_obs$cor, df_obs$cj, df_obs$sub))
names(df) <- c('cor', 'cj', 'sub', 'sum')
df$mean <- NULL
df$sd <- NULL
for(cj in 1:6){
  for(cor in 0:1){
    df$mean[df$cj == cj & df$cor == cor] <- mean(df$sum[df$cj == cj & df$cor == cor])
    df$sd[df$cj == cj & df$cor == cor] <- sd(df$sum[df$cj == cj & df$cor == cor])
  }
}

df2 <- data.frame(table(df_predictions$cor, df_predictions$cj, df_predictions$sub))
names(df2) <- c('cor', 'cj', 'sub', 'sum')
df2$sum <- df2$sum/16.76973  # Scaling, because more predictions than observations


x <- ggplot(data = df, aes(x = as.factor(cj), y = sum, color = as.factor(cor))) +
  geom_point(size = 3, stroke = 1, shape = 16, alpha = 0.04, position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.7)) +
  stat_summary(geom = "point", size = 3, fun = "mean", position = position_dodge(width = 0.7)) +
  geom_errorbar(aes(ymin = mean - sd / sqrt(40), ymax = mean + sd / sqrt(40), group = as.factor(cor)), position = position_dodge(width = 0.7), size = 1, width = 0) +
  stat_summary(data = df2, aes(x = as.factor(cj), y = sum, group = as.factor(cor)), color = 'black', geom = "point", size = 1.5, stroke = 1, shape = 4, fun = "mean", position = position_dodge(width = 0.7)) +
  scale_color_manual(values = c('#D81600', '#8FD694')) +
  ylab(label = '') +
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





df_observations_mean_cj <- df_obs %>%
  mutate(cj = as.integer(cj)) %>%
  group_by(sub, cj, cor) %>% 
  summarise_each(funs(mean)) %>%
  group_by(coherence, manipulation)

df_observations_mean_cj$rtconf_mean <- ave(df_observations_mean_cj$rtconf, list(df_observations_mean_cj$cj, df_observations_mean_cj$cor), FUN = mean)
df_observations_mean_cj$rtconf_sd <- ave(df_observations_mean_cj$rtconf, list(df_observations_mean_cj$cj, df_observations_mean_cj$cor), FUN = sd)

df_predictions_mean_cj <- df_predictions %>%
  mutate(cj = as.integer(cj)) %>%
  group_by(sub, cj, cor) %>% 
  summarise_each(funs(mean)) %>%
  group_by(coherence, manipulation)


y <- ggplot(data = df_observations_mean_cj, aes(x = as.factor(cj), y = rtconf, color = as.factor(cor))) +
  geom_point(size = 3, stroke = 1, shape = 16, alpha = 0.04, position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.7)) +
  stat_summary(geom = "point", size = 3, fun = "mean", position = position_dodge(width = 0.7)) +
  geom_errorbar(aes(ymin = rtconf_mean - rtconf_sd / sqrt(40), ymax = rtconf_mean + rtconf_sd / sqrt(40), group = as.factor(cor)), position = position_dodge(width = 0.7), size = 1, width = 0) +
  stat_summary(data = df_predictions_mean_cj, aes(x = as.factor(cj), y = rtconf, group = as.factor(cor)), color = 'black', geom = "point", size = 1.5, stroke = 1, shape = 4, fun = "mean", position = position_dodge(width = 0.7)) +
  scale_color_manual(values = c('#D81600', '#8FD694')) +
  ylab(label = '') +
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
       plot = ggarrange(x, y, ncol = 2, nrow = 1),
       device = 'png',
       width = 16,
       height = 6,
       units = 'cm')




## DDM marginal predictions ----

png(file="test.png",
    width=1200, 
    height=700,
    res = 170)
dev.off()

### Decision RT - manipulations ----

par(mfrow = c(1, 4), mai = c(0.4, 0.17, 0.2, 0.17))

for (cond in 1:4){  # Loop through conditions

    # Draw plots
    tempC <- hist(family='font', df_obs$rt[df_obs$cor == 1 & df_obs$manipulation == condLab[cond]], 
                  breaks=seq(0,6.2,.25), xlim = c(0,3), ylim = c(0,2000), prob = F, col = rgb(0,1,0,.25), border = "white", ylab = "", xlab = "", cex.lab = 1, cex.main = 1.5, cex.axis = 1, main = "", yaxp = c(0, 3000, 1), tck = -0.03)
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

par(mfrow = c(1, 4), mai = c(0.4, 0.17, 0.2, 0.17))

for (cond in 1:4){  # Loop through conditions
  
  # Draw plots
  tempC <- hist(df_obs$rtconf[df_obs$cor == 1 & df_obs$manipulation == condLab[cond]], 
                breaks=seq(-2,6.2,.25), xlim = c(-.333,3), ylim = c(0,3000), prob = F, col = rgb(0,1,0,.25), border = "white", ylab = "", xlab = "", cex.lab = 2, cex.main = 1.5, cex.axis = 1, main = "", yaxp = c(0, 3800, 1), tck = -0.03)
  tempE <- hist(df_obs$rtconf[df_obs$cor == 0 & df_obs$manipulation == condLab[cond]], 
                breaks=seq(-2,6.2,.25), prob = F, add = T, col = rgb(1,0,0,.25), border = 'white')
  Cors <- hist(df_predictions$rtconf[df_predictions$cor == 1 & df_predictions$manipulation == condLab[cond]], 
               breaks = seq(-2,30,.25), plot = F)
  Errs <- hist(df_predictions$rtconf[df_predictions$cor == 0 & df_predictions$manipulation == condLab[cond]], 
               breaks = seq(-2,30,.25), plot = F)
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

### Confidence resolution (type II AUC) ----

df <- df_obs
subs <- unique(df$sub);N <- length(subs)
roc <- data.frame(matrix(NA,N,4));names(roc) <- unique(df$manipulation) 
for(i in 1:N){
  tempDat <- subset(df,sub==subs[i])
  for(c in unique(df$manipulation)){
    temp <- subset(tempDat,manipulation==c)
    roc[i,c] <- pROC::auc(as.numeric(temp$cor),as.numeric(temp$cj))
  }
}
roc_long <- reshape::melt(roc)
roc_long <- roc_long %>% mutate(rt_manipulation = as.factor(ifelse(roc_long$variable %in% c("AccAcc", "AccFast"), 1, 0)),
                                rtconf_manipulation = as.factor(ifelse(roc_long$variable %in% c("AccAcc", "FastAcc"), 1, 0))) %>%
  group_by(rt_manipulation, rtconf_manipulation) %>%
  mutate(roc_mean = mean(value),
         roc_sd = sd(value))

roc_plot <- ggplot(data = roc_long, aes(x = strtoi(rt_manipulation), y = value, color = as.factor(rtconf_manipulation))) +
  geom_errorbar(aes(ymin = roc_mean - roc_sd / sqrt(40), ymax = roc_mean + roc_sd / sqrt(40), group = as.factor(rtconf_manipulation)), position = position_dodge(width = 0.5), size = 1, width = 0) +
  geom_point(size = 2.5, stroke = 1, shape = 16, alpha = 0.2, position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.5)) +
  stat_summary(geom = "line", size = 1, fun = "mean", position = position_dodge(width = 0.5)) +
  stat_summary(geom = "point", size = 2.5, fun = "mean", position = position_dodge(width = 0.5)) +
  scale_x_continuous(labels = c('"Make fast\ndecisions"', '"Make accurate\n decisions"'),breaks = c(0, 1)) +
  scale_color_manual(values = c('#CA3C25', '#FFA630')) +
  ylab(label = 'Type II AUC') +
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


