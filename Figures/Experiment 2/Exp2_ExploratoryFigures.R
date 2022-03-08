# November 2021
# Script contains exploratory graphs
# Used to check for irregularities in the data


# Loading packages

library(dplyr)
library(ggplot2)
library(forcats)

# Setting working directory

setwd("C:\\Users\\herre\\Desktop\\Internship\\Results\\Exp2_Results") 



## Data preparation ###


# Loading data

data_full <- read.csv(file="Exp2_data_full.csv")
colnames(data_full)[1] <- gsub('^...','',colnames(data_full)[1])
data_viable <- read.csv(file="Exp2_data_viable.csv")

# Removing outliers

data_viable <- data_viable[data_viable$block > 3,] # Remove training trials
data_viable <- data_viable[data_viable$slow_trial == 0,]  # Remove too slow trials

data_viable_c <- data_viable[data_viable$cor == 1,]
data_viable_e <- data_viable[data_viable$cor == 0,]


# Calculating averages per participant

sub_mean <- data_viable %>%
    group_by(sub) %>% 
    summarise_each(funs(mean))

# Calculating averages per participant/coherence

coherence_mean <- data_viable %>%
  group_by(sub, coherence) %>% 
  summarise_each(funs(mean))

# Calculating averages per participant/manipulation

manipulation_mean <- data_viable %>%
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
  geom_point(shape = 16, size = 3, colour = "Blue", alpha = 0.3) +
  stat_summary(aes(y = rt, group = 1), fun = mean, colour="Blue", size = 4, shape = 95) +
  scale_x_discrete(labels = c("AccAcc" = "Accurate decision\nAccurate confidence rating", "AccFast" = "Accurate decision\nFast confidence rating", "FastFast" = "Fast decision\nFast confidence rating", "FastAcc" = "Fast decision\nAccurate confidence rating")) +
  labs(x = "Manipulation", y = "Mean decision reaction time") 

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

# Accuracy plots

ggplot(data = coherence_mean, aes(x = as.factor(coherence), y = cor), shape = 5) +
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

ggplot(data = manipulation_mean_2, aes(x = manipulation, y = cj, colour = as.factor(cor)), shape = 5) +
  geom_jitter(shape = 16, size = 3, alpha = 0.4, width = 0.15) +
  stat_summary(aes(y = cj, group = as.factor(cor)), fun = mean, size = 4, shape = 95) +
  scale_x_discrete(labels = c("AccAcc" = "Accurate decision\nAccurate confidence rating", "AccFast" = "Accurate decision\nFast confidence rating", "FastFast" = "Fast decision\nFast confidence rating", "FastAcc" = "Fast decision\nAccurate confidence rating")) +
  labs(x = "Manipulation", y = "Mean confidence rating") +
  theme_bw()

ggplot(data = manipulation_mean, aes(x = manipulation, y = cj), shape = 5) +
  geom_line(aes(group = sub), alpha = 0.2) +
  geom_point(shape = 16, size = 3, colour = "Blue", alpha = 0.3) +  stat_summary(aes(y = cj, group = 1), fun = mean, colour="Blue", size = 4, shape = 95) +
  stat_summary(aes(y = cj, group=1), fun = mean, colour = "Blue", size = 4, shape = 95) +
  scale_x_discrete(labels = c("AccAcc" = "Accurate decision\nAccurate confidence rating", "AccFast" = "Accurate decision\nFast confidence rating", "FastFast" = "Fast decision\nFast confidence rating", "FastAcc" = "Fast decision\nAccurate confidence rating")) +
  labs(x = "Manipulation", y = "Mean confidence rating")

# RT histograms over all participants 

# Necessary data manipulations
data_viable$Response <- as.factor(data_viable$cor)  
data_viable$Condition <- as.factor(data_viable$manipulation) 
data_viable$rt_manipulation <- fct_collapse(data_viable$Condition, rt_fast = c("FastFast","FastAcc"), rt_slow = c("AccFast", "AccAcc"))
data_viable$rtconf_manipulation <- fct_collapse(data_viable$Condition, rtconf_fast = c("FastFast","AccFast"), rtconf_slow = c("FastAcc", "AccAcc"))

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
ggplot(data = data_viable, aes(x = rt, color = rt_manipulation, fill = rt_manipulation)) +
  geom_histogram(alpha=0.5, position= "identity", bins = 28) +
  scale_color_manual(labels = c("Accurate", "Fast"), values=c("#F39C12", "#1F618D")) +
  scale_fill_manual(labels = c("Accurate", "Fast"), values=c("#F39C12", "#1F618D")) +
  xlab("Reaction time") +
  ylab("Count")

# Confidence RT of fast versus accurate confidence rating manipulation
ggplot(data = data_viable, aes(x = rtconf, color = rtconf_manipulation, fill = rtconf_manipulation)) +
  geom_histogram(alpha=0.5, position= "identity", bins = 100) +
  xlim(0, 5) +
  scale_color_manual(labels = c("Accurate", "Fast"), values=c("#F39C12", "#1F618D")) +
  scale_fill_manual(labels = c("Accurate", "Fast"), values=c("#F39C12", "#1F618D")) +
  xlab("Confidence rating reaction time") +
  ylab("Count")



