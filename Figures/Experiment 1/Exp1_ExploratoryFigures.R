# November 2021
# Script contains exploratory graphs
# Used to check for irregularities in the data

# Load packages

library(ggplot2)
library(dplyr)
library(gridExtra)
library(Rcpp)

# Setting working directory

setwd("C:\\Users\\herre\\Desktop\\Internship\\Results\\Exp1_Results") 

# Loading data

data_full <- read.csv(file="Exp1_data_full.csv")
colnames(data_full)[1] <- gsub('^...','',colnames(data_full)[1])
data_viable <- read.csv(file="Exp1_data_viable.csv")

data_viable_c <- data_viable[data_viable$cor == 1,]
data_viable_e <- data_viable[data_viable$cor == 0,]

# Simulating data based on estimated parameters

sourceCpp("C:\\Users\\herre\\OneDrive\\Documenten\\GitHub\\ConfidenceBounds\\Analyses\\Exp1_analyses\\DDM_confidence_bounds.cpp") 

df <- data.frame(matrix(ncol = 11, nrow = 40*4))
colnames(df) <- c('sub', 'manipulation', 'v1', 'v2', 'v3', 'a', 'ter', 'a2', 'postdriftmod', 'a2_slope', 'ter2')
condLab <- c('FastFast', 'AccFast', 'AccAcc', 'FastAcc') 
j <- 1
for (i in (1:40)){ 
  for(c in 1:4){
    file_name <- paste0('Parameter_estimation_both\\test_results_sub_', i, '_', condLab[c], '.Rdata')
    load(file_name)
    df[j,] <- c(i, condLab[c], results$optim$bestmem[1], results$optim$bestmem[2], results$optim$bestmem[3], results$optim$bestmem[4], results$optim$bestmem[5], results$optim$bestmem[6], results$optim$bestmem[7], results$optim$bestmem[8], results$optim$bestmem[9])
    j <- j + 1
  }
}
df[3:11] <- lapply(df[3:11], as.numeric)

z <- 0.5  # Starting point (accuracy-coded dataset -> 0.5)
ntrials <- 1000  # Number of decision-making simulations per observation
sigma <- 1  # Within-trial noise
dt <- 0.01  # Precision
n <- 40  # Number of participants to include (40)

predictions <- NULL
coherence_vector <- c(0.1, 0.2, 0.4)
manipulation_vector <- c('FastFast', 'AccFast', 'AccAcc', 'FastAcc')

for (i in 1:n){
  for (j in 1:4){
    for (k in 1:3){
      
      df_temp <- df %>% filter(sub == i & manipulation == manipulation_vector[j])
      v <- df_temp[[k + 2]]
      predictions_temp <- data.frame(DDM_confidence_bounds(v = v, a = df_temp$a, ter = df_temp$ter, z = z, ntrials = ntrials, s = sigma, dt = dt, a2 = df_temp$a2, postdriftmod = df_temp$postdriftmod, a2_slope = df_temp$a2_slope, ter2 = df_temp$ter2))
      predictions_temp <- cbind(predictions_temp, coherence_vector[k], manipulation_vector[j], i)
      names(predictions_temp) <- c('rt', 'resp', 'cor', 'evidence2', 'rtfull', 'rtconf', 'cj', 'coherence', 'manipulation', 'sub')
      
      predictions <- rbind(predictions, predictions_temp)
    }
  }
}

predictions_c <- predictions[predictions$cor == 1,]
predictions_e <- predictions[predictions$cor == 0,]

# Calculating averages per participant

sub_mean <- data_viable %>%
  group_by(sub) %>% 
  summarise_each(funs(mean))

# Calculating averages per participant/coherence

coherence_mean <- data_viable %>%
  group_by(sub, coherence) %>% 
  summarise_each(funs(mean))

coherence_mean_c <- data_viable_c %>%
  group_by(sub, coherence) %>% 
  summarise_each(funs(mean))

coherence_mean_e <- data_viable_e %>%
  group_by(sub, coherence) %>% 
  summarise_each(funs(mean))

# Calculating averages per participant/manipulation

manipulation_mean <- data_viable %>%
  group_by(sub, manipulation) %>% 
  summarise_each(funs(mean))

predictions_manipulation_mean <- predictions %>%
  group_by(sub, manipulation) %>% 
  summarise_each(funs(mean))

manipulation_mean_c <- data_viable_c %>%
  group_by(sub, manipulation) %>% 
  summarise_each(funs(mean))

manipulation_mean_e <- data_viable_e %>%
  group_by(sub, manipulation) %>% 
  summarise_each(funs(mean))

manipulation_mean_2 <- rbind(manipulation_mean_c, manipulation_mean_e)

# Calculating averages per participant/manipulation/coherence

manipulation_coherence_mean <- data_viable %>%
  group_by(sub, manipulation, coherence) %>% 
  summarise_each(funs(mean))

# Calculating training trials repetitions per sub

block_repetition <- data_full %>%
  group_by(sub, block, batch) %>% 
  summarise_each(funs(max))
block_repetition <- block_repetition[block_repetition$block < 4,]



### Figures ###


# Training blocks required

for (batch in unique(data_full$batch)){
  
  temp <- block_repetition[block_repetition$batch == batch,]
  
  print(ggplot(data = temp, aes(fill = as.factor(block), y = block_repetition, x = sub)) +  # Plot of training block 1 and 2
          geom_bar(position = position_stack(reverse = TRUE), stat = 'identity') +
          coord_flip() +
          labs(x = "Subject number", y = "Number of repetitions", fill = "Training block") +
          geom_hline(yintercept = 7, linetype = "dashed") +
          annotate(geom = "text", x = 42, y = 6, label = "Cutoff", fontface = "bold")) +
    xlim(c(0,42))
}

# CJ checks (Sanders et al., 2016)

plot1 <- ggplot(data_viable, aes(x = cj, y = cor)) + 
  stat_summary(aes(group = 1), fun.y = mean, colour = "black", geom = "line", size = 2) + 
  xlab('Mean confidence rating') +
  ylab('% correct') +
  theme_bw()

plot2 <- ggplot(data_viable_c, aes(x = coherence, y = cj)) + 
  stat_summary(aes(group = 1), fun.y = mean, colour = "green", geom = "line", size = 2) + 
  stat_summary(data = data_viable_e, aes(x = coherence, y = cj, group = 1), fun.y = mean, colour = "red", geom = "line", size = 2) + 
  xlab('Coherence') +
  ylab('Mean confidence rating') +
  theme_bw()

plot3 <- ggplot(data_viable[data_viable$cj == 1,], aes(x = coherence, y = cor)) + 
  stat_summary(aes(group = 1), fun.y = mean, colour = "grey", geom = "line", size = 2) + 
  stat_summary(data = data_viable[data_viable$cj == 0,], aes(x = coherence, y = cor, group = 1), fun.y = mean, colour = "black", geom = "line", size = 2) + 
  xlab('Coherence') +
  ylab('% correct') +
  theme_bw()

grid.arrange(plot1, plot2, plot3, ncol=3)

# RT manipulations (all)

ggplot(data = data_viable, aes(x = manipulation, y = rt)) +
  geom_boxplot(outlier.shape = NA)  +
  theme_bw() +
  coord_flip() +
  geom_jitter(width = 0.1, alpha = 0.2) +
  theme(legend.position = "none") +
  scale_x_discrete(labels = c("AccAcc" = "Accurate decision\nAccurate confidence rating", "AccFast" = "Accurate decision\nFast confidence rating", "FastFast" = "Fast decision\nFast confidence rating", "FastAcc" = "Fast decision\nAccurate confidence rating")) +
  labs(x = "Manipulation", y = "Decision reaction time")

# RT manipulations (averages)

ggplot(data = manipulation_mean, aes(x = manipulation, y = rt), shape = 5) +
  geom_line(aes(group = sub), alpha = 0.2) +
  geom_point(shape = 16, size = 5, colour = "#4D5382", alpha = 0.5) +
  stat_summary(aes(y = rt, group = 1), fun = mean, colour="#4D5382", size = 4, shape = 95) +
  scale_x_discrete(labels = c("AccAcc" = "Accurate decision\nAccurate confidence rating", "AccFast" = "Accurate decision\nFast confidence rating", "FastFast" = "Fast decision\nFast confidence rating", "FastAcc" = "Fast decision\nAccurate confidence rating")) +
  labs(x = "Manipulation", y = "Decision RT") +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())

# RT manipulations (predictions)

manipulation_sub_mean <- data_viable %>%
  group_by(manipulation) %>% 
  summarise_each(funs(mean))

predictions_manipulation_sub_mean <- predictions %>%
  group_by(manipulation) %>% 
  summarise_each(funs(mean))

manipulation_sub_mean$standard_error <- NULL
for(i in 1:4){
  manipulation_sub_mean$standard_error[manipulation_sub_mean$manipulation == condLab[i]] <- sd(manipulation_mean$rt[manipulation_mean$manipulation == condLab[i]]) / sqrt(length(manipulation_mean$rt[manipulation_mean$manipulation == condLab[i]]))
}

predictions_manipulation_sub_mean$standard_error <- NULL
for(i in 1:4){
  predictions_manipulation_sub_mean$standard_error[predictions_manipulation_sub_mean$manipulation == condLab[i]] <- sd(predictions_manipulation_mean$rt[predictions_manipulation_mean$manipulation == condLab[i]]) / sqrt(length(predictions_manipulation_mean$rt[predictions_manipulation_mean$manipulation == condLab[i]]))
}

ggplot(data = manipulation_mean, aes(x = manipulation, y = rt)) +
  geom_jitter(shape = 'circle filled', size = 6, fill = "grey", color = 'white', alpha = 1, stroke = 1, width = 0.2) + 
  geom_point(data = manipulation_sub_mean, aes(y = rt, x = manipulation), size = 3) +
  geom_errorbar(data = manipulation_sub_mean, aes(ymin = rt - standard_error, ymax = rt + standard_error), size = 0.7, width = 0.1) +
  #geom_line(data = manipulation_sub_mean, aes(y = rt, x = manipulation), color = 'black', group = 1, linetype = 'dashed', size = 1) +
  geom_point(data = predictions_manipulation_sub_mean, color = 'red', aes(y = rt, x = manipulation), size = 3, position = position_nudge(x = 0.12)) +
  geom_errorbar(data = predictions_manipulation_sub_mean, aes(ymin = rt - standard_error, ymax = rt + standard_error), colour = 'red', size = 0.7, position = position_nudge(x = 0.12), width = 0.1) +
  scale_x_discrete(labels = c("AccAcc" = "Accurate decision\nAccurate confidence rating", "AccFast" = "Accurate decision\nFast confidence rating", "FastFast" = "Fast decision\nFast confidence rating", "FastAcc" = "Fast decision\nAccurate confidence rating")) +
  labs(x = "Manipulation", y = "Decision RT") +
  theme( 
    panel.grid.major.x = element_blank() ,
    panel.grid.major.y = element_line( size=.1, color="grey" ), 
    panel.background = element_blank(),
    axis.line = element_line(size = 0.5))

# RT ~ coherence (averages)

ggplot(data = coherence_mean, aes(x = coherence, y = rt), shape = 5) +
  geom_line(aes(group = sub), alpha = 0.2) +
  geom_point(shape = 16, size = 5, colour = "#4D5382", alpha = 0.5) +
  stat_summary(aes(y = rt, group = 1), fun = mean, colour="#4D5382", size = 4, shape = 95) +
  labs(x = "Manipulation", y = "Decision RT") +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())

# RT ~ coherence (predictions)

coherence_sub_mean <- data_viable %>%
  group_by(coherence) %>% 
  summarise_each(funs(mean))

predictions_coherence_sub_mean <- predictions %>%
  group_by(coherence) %>% 
  summarise_each(funs(mean))

ggplot(data = coherence_mean, aes(x = coherence, y = rt), shape = 5) +
  geom_jitter(shape = 'circle filled', size = 7, fill = "grey", color = 'white', alpha = 1, width = 0.02, stroke = 1) + 
  geom_point(data = coherence_sub_mean, aes(y = rt, x = coherence), size = 7) +
  geom_line(data = coherence_sub_mean, aes(y = rt, x = coherence), color = 'black', group = 1, linetype = 'dashed', size = 1) +
  geom_point(data = predictions_coherence_sub_mean, color = 'red', aes(y = rt, x = coherence), shape = 4, size = 3, stroke = 2) +
  labs(x = "Coherence", y = "Decision RT") +
  theme( 
    panel.grid.major.x = element_blank() ,
    panel.grid.major.y = element_line( size=.1, color="grey" ), 
    panel.background = element_blank(),
    axis.line = element_line(size = 0.5))

# Confidence rating RT manipulations (all)

ggplot(data = data_viable, aes(x = manipulation, y = rtconf)) +
  geom_boxplot(outlier.shape = NA)  +
  theme_bw() +
  coord_flip() +
  geom_jitter(width=0.1,alpha=0.2) +
  theme(legend.position = "none") +
  scale_x_discrete(labels = c("AccAcc" = "Accurate decision\nAccurate confidence rating", "AccFast" = "Accurate decision\nFast confidence rating", "FastFast" = "Fast decision\nFast confidence rating", "FastAcc" = "Fast decision\nAccurate confidence rating")) +
  labs(x = "Manipulation", y = "Confidence rating reaction time")

# confidence rating RT manipulation (averages)

ggplot(data = manipulation_mean, aes(x = manipulation, y = rtconf), shape = 5) +
  geom_line(aes(group = sub), alpha = 0.2) +
  geom_point(shape = 16, size = 3, colour = "Blue", alpha = 0.3) +
  stat_summary(aes(y = rtconf, group = 1), fun = mean, colour="Blue", size = 4, shape = 95) +
  scale_x_discrete(labels = c("AccAcc" = "Accurate decision\nAccurate confidence rating", "AccFast" = "Accurate decision\nFast confidence rating", "FastFast" = "Fast decision\nFast confidence rating", "FastAcc" = "Fast decision\nAccurate confidence rating")) +
  labs(x = "Manipulation", y = "Mean confidence rating reaction time")

# confidence RT manipulations (predictions)

ggplot(data = manipulation_mean, aes(x = manipulation, y = rtconf), shape = 5) +
  geom_jitter(shape = 'circle filled', size = 7, fill = "grey", color = 'white', alpha = 1, width = 0.2, stroke = 1) + 
  geom_point(data = manipulation_sub_mean, aes(y = rtconf, x = manipulation), size = 7) +
  geom_line(data = manipulation_sub_mean, aes(y = rtconf, x = manipulation), color = 'black', group = 1, linetype = 'dashed', size = 1) +
  geom_point(data = predictions_manipulation_sub_mean, color = 'red', aes(y = rtconf, x = manipulation), shape = 4, size = 3, stroke = 2) +
  scale_x_discrete(labels = c("AccAcc" = "Accurate decision\nAccurate confidence rating", "AccFast" = "Accurate decision\nFast confidence rating", "FastFast" = "Fast decision\nFast confidence rating", "FastAcc" = "Fast decision\nAccurate confidence rating")) +
  labs(x = "Manipulation", y = "Confidence judgement RT") +
  theme( 
    panel.grid.major.x = element_blank() ,
    panel.grid.major.y = element_line( size=.1, color="grey" ), 
    panel.background = element_blank(),
    axis.line = element_line(size = 0.5))

# RTconf ~ coherence (predictions)

ggplot(data = coherence_mean, aes(x = coherence, y = rtconf), shape = 5) +
  geom_jitter(shape = 'circle filled', size = 7, fill = "grey", color = 'white', alpha = 1, width = 0.02, stroke = 1) + 
  geom_point(data = coherence_sub_mean, aes(y = rtconf, x = coherence), size = 7) +
  geom_line(data = coherence_sub_mean, aes(y = rtconf, x = coherence), color = 'black', group = 1, linetype = 'dashed', size = 1) +
  geom_point(data = predictions_coherence_sub_mean, color = 'red', aes(y = rtconf, x = coherence), shape = 4, size = 3, stroke = 2) +
  labs(x = "Coherence", y = "Confidence judgement RT") +
  theme( 
    panel.grid.major.x = element_blank() ,
    panel.grid.major.y = element_line( size=.1, color="grey" ), 
    panel.background = element_blank(),
    axis.line = element_line(size = 0.5))

# Accuracy plots

ggplot(data = coherence_mean, aes(x = as.factor(coherence), y = cor), shape = 5) +
  geom_line(aes(group = sub), alpha = 0.2) +
  geom_point(shape = 16, size = 3, colour = "Blue", alpha = 0.3) +
  stat_summary(aes(y = cor, group = 1), fun = mean, colour = "Blue", size = 4, shape = 95) +
  labs(x = "Coherence", y = "Mean accuracy")

ggplot(data = coherence_mean, aes(x = coherence, y = cor), shape = 5) +
  geom_line(aes(group = sub), alpha = 0.2) +
  geom_point(shape = 16, size = 3, colour = "Blue", alpha = 0.3) +
  stat_summary(aes(y = cor, group = 1), fun = mean, colour = "Blue", size = 4, shape = 95) +
  labs(x = "Coherence", y = "Mean accuracy")

ggplot(data = manipulation_mean, aes(x = manipulation, y = cor), shape = 5) +
  geom_line(aes(group = sub), alpha = 0.2) +
  geom_point(shape = 16, size = 3, colour = "Blue", alpha = 0.3) +
  stat_summary(aes(y = cor, group = 1), fun = mean, colour="Blue", size = 4, shape = 95) +
  scale_x_discrete(labels = c("AccAcc" = "Accurate decision\nAccurate confidence rating", "AccFast" = "Accurate decision\nFast confidence rating", "FastFast" = "Fast decision\nFast confidence rating", "FastAcc" = "Fast decision\nAccurate confidence rating")) +
  labs(x = "Manipulation", y = "Mean accuracy")

# Accuracy plots + predictions

ggplot(data = coherence_mean, aes(x = coherence, y = cor), shape = 5) +
  geom_jitter(shape = 'circle filled', size = 7, fill = "grey", color = 'white', alpha = 1, width = 0.02, stroke = 1) + 
  geom_point(data = coherence_sub_mean, aes(y = cor, x = coherence), size = 7) +
  geom_line(data = coherence_sub_mean, aes(y = cor, x = coherence), color = 'black', group = 1, linetype = 'dashed', size = 1) +
  geom_point(data = predictions_coherence_sub_mean, color = 'red', aes(y = cor, x = coherence), shape = 4, size = 3, stroke = 2) +
  labs(x = "Coherence", y = "Accuracy") +
  theme( 
    panel.grid.major.x = element_blank() ,
    panel.grid.major.y = element_line( size=.1, color="grey" ), 
    panel.background = element_blank(),
    axis.line = element_line(size = 0.5))

ggplot(data = coherence_mean, aes(x = coherence, y = cj), shape = 5) +
  geom_jitter(shape = 'circle filled', size = 7, fill = "grey", color = 'white', alpha = 1, width = 0.02, stroke = 1) + 
  geom_point(data = coherence_sub_mean, aes(y = cj, x = coherence), size = 7) +
  geom_line(data = coherence_sub_mean, aes(y = cj, x = coherence), color = 'black', group = 1, linetype = 'dashed', size = 1) +
  geom_point(data = predictions_coherence_sub_mean, color = 'red', aes(y = cj, x = coherence), shape = 4, size = 3, stroke = 2) +
  labs(x = "Coherence", y = "Confidence judgement") +
  theme( 
    panel.grid.major.x = element_blank() ,
    panel.grid.major.y = element_line( size=.1, color="grey" ), 
    panel.background = element_blank(),
    axis.line = element_line(size = 0.5))

ggplot(data = manipulation_mean, aes(x = manipulation, y = cor), shape = 5) +
  geom_jitter(shape = 'circle filled', size = 7, fill = "grey", color = 'white', alpha = 1, width = 0.2, stroke = 1) + 
  geom_point(data = manipulation_sub_mean, aes(y = cor, x = manipulation), size = 7) +
  geom_line(data = manipulation_sub_mean, aes(y = cor, x = manipulation), color = 'black', group = 1, linetype = 'dashed', size = 1) +
  geom_point(data = predictions_manipulation_sub_mean, color = 'red', aes(y = cor, x = manipulation), shape = 4, size = 3, stroke = 2) +
  scale_x_discrete(labels = c("AccAcc" = "Accurate decision\nAccurate confidence rating", "AccFast" = "Accurate decision\nFast confidence rating", "FastFast" = "Fast decision\nFast confidence rating", "FastAcc" = "Fast decision\nAccurate confidence rating")) +
  labs(x = "Manipulation", y = "Accuracy") +
  theme( 
    panel.grid.major.x = element_blank() ,
    panel.grid.major.y = element_line( size=.1, color="grey" ), 
    panel.background = element_blank(),
    axis.line = element_line(size = 0.5))

ggplot(data = manipulation_mean, aes(x = manipulation, y = cj), shape = 5) +
  geom_jitter(shape = 'circle filled', size = 7, fill = "grey", color = 'white', alpha = 1, width = 0.2, stroke = 1) + 
  geom_point(data = manipulation_sub_mean, aes(y = cj, x = manipulation), size = 7) +
  geom_line(data = manipulation_sub_mean, aes(y = cj, x = manipulation), color = 'black', group = 1, linetype = 'dashed', size = 1) +
  geom_point(data = predictions_manipulation_sub_mean, color = 'red', aes(y = cj, x = manipulation), shape = 4, size = 3, stroke = 2) +
  scale_x_discrete(labels = c("AccAcc" = "Accurate decision\nAccurate confidence rating", "AccFast" = "Accurate decision\nFast confidence rating", "FastFast" = "Fast decision\nFast confidence rating", "FastAcc" = "Fast decision\nAccurate confidence rating")) +
  labs(x = "Manipulation", y = "Confidence judgement") +
  theme( 
    panel.grid.major.x = element_blank() ,
    panel.grid.major.y = element_line( size=.1, color="grey" ), 
    panel.background = element_blank(),
    axis.line = element_line(size = 0.5))

### cj & accuracy plots for correct- and for error-trials

# correct 

manipulation_sub_mean_c <- data_viable_c %>%
  group_by(manipulation) %>% 
  summarise_each(funs(mean))

predictions_manipulation_sub_mean_c <- predictions_c %>%
  group_by(manipulation) %>% 
  summarise_each(funs(mean))

coherence_sub_mean_c <- data_viable_c %>%
  group_by(coherence) %>% 
  summarise_each(funs(mean))

predictions_coherence_sub_mean_c <- predictions_c %>%
  group_by(coherence) %>% 
  summarise_each(funs(mean))

ggplot(data = coherence_mean_c, aes(x = coherence, y = cj), shape = 5) +
  geom_jitter(shape = 'circle filled', size = 7, fill = "grey", color = 'white', alpha = 1, width = 0.02, stroke = 1) + 
  geom_point(data = coherence_sub_mean_c, aes(y = cj, x = coherence), size = 7) +
  geom_line(data = coherence_sub_mean_c, aes(y = cj, x = coherence), color = 'black', group = 1, linetype = 'dashed', size = 1) +
  geom_point(data = predictions_coherence_sub_mean_c, color = 'red', aes(y = cj, x = coherence), shape = 4, size = 3, stroke = 2) +
  labs(x = "Coherence", y = "Confidence judgement") +
  theme( 
    panel.grid.major.x = element_blank() ,
    panel.grid.major.y = element_line( size=.1, color="grey" ), 
    panel.background = element_blank(),
    axis.line = element_line(size = 0.5))

ggplot(data = manipulation_mean_c, aes(x = manipulation, y = cj), shape = 5) +
  geom_jitter(shape = 'circle filled', size = 7, fill = "grey", color = 'white', alpha = 1, width = 0.2, stroke = 1) + 
  geom_point(data = manipulation_sub_mean_c, aes(y = cj, x = manipulation), size = 7) +
  geom_line(data = manipulation_sub_mean_c, aes(y = cj, x = manipulation), color = 'black', group = 1, linetype = 'dashed', size = 1) +
  geom_point(data = predictions_manipulation_sub_mean_c, color = 'red', aes(y = cj, x = manipulation), shape = 4, size = 3, stroke = 2) +
  scale_x_discrete(labels = c("AccAcc" = "Accurate decision\nAccurate confidence rating", "AccFast" = "Accurate decision\nFast confidence rating", "FastFast" = "Fast decision\nFast confidence rating", "FastAcc" = "Fast decision\nAccurate confidence rating")) +
  labs(x = "Manipulation", y = "Confidence judgement") +
  theme( 
    panel.grid.major.x = element_blank() ,
    panel.grid.major.y = element_line( size=.1, color="grey" ), 
    panel.background = element_blank(),
    axis.line = element_line(size = 0.5))

# error

manipulation_sub_mean_e <- data_viable_e %>%
  group_by(manipulation) %>% 
  summarise_each(funs(mean))

predictions_manipulation_sub_mean_e <- predictions_e %>%
  group_by(manipulation) %>% 
  summarise_each(funs(mean))

coherence_sub_mean_e <- data_viable_e %>%
  group_by(coherence) %>% 
  summarise_each(funs(mean))

predictions_coherence_sub_mean_e <- predictions_e %>%
  group_by(coherence) %>% 
  summarise_each(funs(mean))

ggplot(data = coherence_mean_e, aes(x = coherence, y = cj), shape = 5) +
  geom_jitter(shape = 'circle filled', size = 7, fill = "grey", color = 'white', alpha = 1, width = 0.02, stroke = 1) + 
  geom_point(data = coherence_sub_mean_e, aes(y = cj, x = coherence), size = 7) +
  geom_line(data = coherence_sub_mean_e, aes(y = cj, x = coherence), color = 'black', group = 1, linetype = 'dashed', size = 1) +
  geom_point(data = predictions_coherence_sub_mean_e, color = 'red', aes(y = cj, x = coherence), shape = 4, size = 3, stroke = 2) +
  labs(x = "Coherence", y = "Confidence judgement") +
  theme( 
    panel.grid.major.x = element_blank() ,
    panel.grid.major.y = element_line( size=.1, color="grey" ), 
    panel.background = element_blank(),
    axis.line = element_line(size = 0.5))

ggplot(data = manipulation_mean_e, aes(x = manipulation, y = cj), shape = 5) +
  geom_jitter(shape = 'circle filled', size = 7, fill = "grey", color = 'white', alpha = 1, width = 0.2, stroke = 1) + 
  geom_point(data = manipulation_sub_mean_e, aes(y = cj, x = manipulation), size = 7) +
  geom_line(data = manipulation_sub_mean_e, aes(y = cj, x = manipulation), color = 'black', group = 1, linetype = 'dashed', size = 1) +
  geom_point(data = predictions_manipulation_sub_mean_e, color = 'red', aes(y = cj, x = manipulation), shape = 4, size = 3, stroke = 2) +
  scale_x_discrete(labels = c("AccAcc" = "Accurate decision\nAccurate confidence rating", "AccFast" = "Accurate decision\nFast confidence rating", "FastFast" = "Fast decision\nFast confidence rating", "FastAcc" = "Fast decision\nAccurate confidence rating")) +
  labs(x = "Manipulation", y = "Confidence judgement") +
  theme( 
    panel.grid.major.x = element_blank() ,
    panel.grid.major.y = element_line( size=.1, color="grey" ), 
    panel.background = element_blank(),
    axis.line = element_line(size = 0.5))





# Confidence rating plots

# Option 1
ggplot(data = coherence_mean, aes(x = as.factor(coherence), y = cj), shape = 5) +
  geom_line(aes(group = sub), alpha = 0.2) +
  geom_point(shape = 16, size = 3, colour = "Blue", alpha = 0.3) +  stat_summary(aes(y = cj, group = 1), fun = mean, colour="Blue", size = 4, shape = 95) +
  labs(x = "Coherence", y = "Mean confidence rating") 

# Option 2
ggplot(data = coherence_mean, aes(x = coherence, y = cj), shape = 5) +
  geom_line(aes(group = sub), alpha = 0.2) +
  geom_point(shape = 16, size = 3, colour = "Blue", alpha = 0.3) +  stat_summary(aes(y = cj, group = 1), fun = mean, colour="Blue", size = 4, shape = 95) +
  labs(x = "Coherence", y = "Mean confidence rating") 


ggplot(data = manipulation_mean, aes(x = manipulation, y = cj), shape = 5) +
  geom_line(aes(group = sub), alpha = 0.2) +
  geom_point(shape = 16, size = 3, colour = "Blue", alpha = 0.3) +  stat_summary(aes(y = cj, group = 1), fun = mean, colour="Blue", size = 4, shape = 95) +
  stat_summary(aes(y = cj, group=1), fun = mean, colour = "Blue", size = 4, shape = 95) +
  scale_x_discrete(labels = c("AccAcc" = "Accurate decision\nAccurate confidence rating", "AccFast" = "Accurate decision\nFast confidence rating", "FastFast" = "Fast decision\nFast confidence rating", "FastAcc" = "Fast decision\nAccurate confidence rating")) +
  labs(x = "Manipulation", y = "Mean confidence rating")

ggplot(data = manipulation_mean_2, aes(x = manipulation, y = cj, colour = as.factor(cor)), shape = 5) +
  geom_jitter(shape = 16, size = 3, alpha = 0.4, width = 0.15) +
  stat_summary(aes(y = cj, group = as.factor(cor)), fun = mean, size = 4, shape = 95) +
  scale_x_discrete(labels = c("AccAcc" = "Accurate decision\nAccurate confidence rating", "AccFast" = "Accurate decision\nFast confidence rating", "FastFast" = "Fast decision\nFast confidence rating", "FastAcc" = "Fast decision\nAccurate confidence rating")) +
  labs(x = "Manipulation", y = "Mean confidence rating") +
  theme_bw()

ggplot(data = coherence_mean, aes(x = as.factor(coherence), y = metacognition), shape = 5) +
  geom_line(aes(group = sub), alpha = 0.2) +
  geom_point(shape = 16, size = 3, colour = "Blue", alpha = 0.3) +  stat_summary(aes(y = cj, group = 1), fun = mean, colour="Blue", size = 4, shape = 95) +
  stat_summary(aes(y = metacognition, group=1), fun = mean, colour = "Blue", size = 4, shape = 95) +
  scale_x_discrete(labels = c("AccAcc" = "Accurate decision\nAccurate confidence rating", "AccFast" = "Accurate decision\nFast confidence rating", "FastFast" = "Fast decision\nFast confidence rating", "FastAcc" = "Fast decision\nAccurate confidence rating")) +
  labs(x = "Coherence", y = "Mean confidence rating accuracy")

ggplot(data = manipulation_mean, aes(x = manipulation, y = metacognition), shape = 5) +
  geom_line(aes(group = sub), alpha = 0.2) +
  geom_point(shape = 16, size = 3, colour = "Blue", alpha = 0.3) +  stat_summary(aes(y = cj, group = 1), fun = mean, colour="Blue", size = 4, shape = 95) +
  stat_summary(aes(y = metacognition, group=1), fun = mean, colour = "Blue", size = 4, shape = 95) +
  scale_x_discrete(labels = c("AccAcc" = "Accurate decision\nAccurate confidence rating", "AccFast" = "Accurate decision\nFast confidence rating", "FastFast" = "Fast decision\nFast confidence rating", "FastAcc" = "Fast decision\nAccurate confidence rating")) +
  labs(x = "Manipulation", y = "Mean confidence rating accuracy")

# RT histograms over all participants 

# Necessary data manipulations
data_viable$Response <- as.factor(data_viable$cor)  
data_viable$Condition <- as.factor(data_viable$manipulation) 
data_viable$rt_manipulation <- ifelse(data_viable$manipulation %in% c("AccAcc", "AccFast"), 1, 0)
data_viable$rtconf_manipulation <- ifelse(data_viable$manipulation %in% c("AccAcc", "FastAcc"), 1, 0)

# RT of correct versus wrong response
ggplot(data = data_viable, aes(x = rt, color = Response, fill = Response)) +
  geom_histogram(alpha=0.5, position= "identity", bins = 28) +
  scale_color_manual(labels = c("Correct", "Wrong"), values=c("#C0392B", "#27AE60")) +
  scale_fill_manual(labels = c("Correct", "Wrong"), values=c("#C0392B", "#27AE60")) +
  xlab("Reaction time") +
  ylab("Count")

# Confidence RT of correct versus wrong response
ggplot(data = data_viable, aes(x = rtconf, color = Response, fill = Response)) +
  geom_histogram(alpha=0.5, position= "identity", bins = 28) +
  xlim(0, 5) +
  scale_color_manual(labels = c("Correct", "Wrong"), values=c("#C0392B", "#27AE60")) +
  scale_fill_manual(labels = c("Correct", "Wrong"), values=c("#C0392B", "#27AE60")) +
  xlab("Confidence rating reaction time") +
  ylab("Count")

# RT of fast versus accurate decision manipulation
ggplot(data = data_viable, aes(x = rt, color = as.factor(rt_manipulation), fill = as.factor(rt_manipulation))) +
  geom_histogram(alpha=0.5, position= "identity", bins = 28) +
  scale_color_manual(labels = c("Accurate", "Fast"), values=c("#F39C12", "#1F618D")) +
  scale_fill_manual(labels = c("Accurate", "Fast"), values=c("#F39C12", "#1F618D")) +
  xlab("Reaction time") +
  ylab("Count")

# Confidence RT of fast versus accurate confidence rating manipulation
ggplot(data = data_viable, aes(x = rtconf, color = as.factor(rtconf_manipulation), fill = as.factor(rtconf_manipulation))) +
  geom_histogram(alpha=0.5, position= "identity", bins = 100) +
  scale_color_manual(labels = c("Accurate", "Fast"), values=c("#F39C12", "#1F618D")) +
  scale_fill_manual(labels = c("Accurate", "Fast"), values=c("#F39C12", "#1F618D")) +
  xlab("Confidence rating reaction time") +
  ylab("Count") +
  theme_minimal()




# mean graphs
# standard error (sd gecontroleerd voor pp)
# pp verbinden met lijn

# zelfde bij confidence

# accuraatheid per conditie mean plot
# confidence mena per conditie

# coherence distance change