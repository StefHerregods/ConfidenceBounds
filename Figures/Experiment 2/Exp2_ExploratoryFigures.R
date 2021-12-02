# November 2021
# Script contains exploratory graphs
# Used to check for irregularities in the data


# Loading packages

library(dplyr)

# Setting working directory

setwd("C:\\Users\\herre\\Desktop\\Internship\\Results\\Exp2_Results") 

# Loading data

data_full <- read.csv(file="Exp2_data_full.csv")
colnames(data_full)[1] <- gsub('^...','',colnames(data_full)[1])
data_viable <- read.csv(file="Exp2_data_viable.csv")

# Removing outliers

data_viable <- data_viable[data_viable$block > 3,] # Remove training trials
data_viable <- data_viable[data_viable$slow_trial == 0,]  # Remove too slow trials


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

# Calculating training trials repetitions per sub

block_repetition <- data_full %>%
  group_by(sub, block, batch) %>% 
  summarise_each(funs(max))
block_repetition <- block_repetition[block_repetition$block < 4,]

# Training blocks required

for (batch in unique(data_full$batch)){
  
  temp <- block_repetition[block_repetition$batch == batch,]
  
  print(ggplot(data = temp, aes(fill = as.factor(block), y = block_repetition, x = sub)) +  # Plot of training block 1 and 2
    geom_bar(position = position_stack(reverse = TRUE), stat='identity') +
    coord_flip() +
    labs(x = "Subject number", y = "Number of repetitions", fill = "Training block") +
    geom_hline(yintercept = 7, linetype = "dashed") +
    annotate(geom="text", x = 42, y = 6, label="Cutoff", fontface = "bold")) +
    xlim(c(0,42))
}

# RT manipulations (all)

ggplot(data = data_viable, aes(x = manipulation, y = rt)) +
  geom_boxplot(outlier.shape = NA)  +
  theme_bw() +
  coord_flip() +
  geom_jitter(width=0.1,alpha=0.2) +
  theme(legend.position = "none") +
  scale_x_discrete(labels = c("AccAcc" = "Accurate decision\nAccurate confidence rating", "AccFast" = "Accurate decision\nFast confidence rating", "FastFast" = "Fast decision\nFast confidence rating", "FastAcc" = "Fast decision\nAccurate confidence rating")) +
  labs(x = "Manipulation", y = "Decision reaction time")



