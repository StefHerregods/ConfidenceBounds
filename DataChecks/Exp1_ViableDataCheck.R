# Project: Confidence bounds (2021-2022)
# Script checks for (a) viability of the data (as described in preregistration experiment 1) 
# and (b) irregularities in data of experiment 1

library(ggplot2)

setwd('C:\\Users\\herre\\OneDrive\\Bureaublad\\Internship\\Results\\Exp1_results_temp')


alpha <- 0.05

# (a) Checking viability of the data
# See preregistration experiment 1 for more information

# Step 0: loading data

data_viable <- NULL
data_full <- NULL
last_block <- NULL

for (i in 1:40){
  
  file_name <- paste0("DotsTask_sub",i,".csv",collapse="")
  if (file.exists(file_name)){
    
    data_temp <- read.csv(file=file_name)
    data_temp$subject <- i
    
    data_full <- rbind(data_temp,data_full)
    
    data_temp$check1 <- FALSE
    data_temp$check2 <- FALSE
    data_temp$check3 <- FALSE
    data_temp$check4 <- FALSE
    
    # Step 1: Did not finish 
    
    final_block <- max(data_temp$block)
    last_block <- append(last_block,final_block)
    if (final_block == 15){
      final_trial <- max(data_temp$ï..withinblocktrial[data_temp$block == 15])
      if (final_trial == 59){
        data_temp$check1 <- TRUE
        
        # Step 2: Required more than 7 training blocks
        
        training_blocks <- max(data_temp$block_repetition[data_temp$block == 1]) + max(data_temp$block_repetition[data_temp$block == 2]) + max(data_temp$block_repetition[data_temp$block == 3])
        if (training_blocks <= 7){
          data_temp$check2 <- TRUE
        } 
        
        # Step 3: Same confidence rating in more than 95% of the trials
        
        data_temp <- data_temp[data_temp$block > 3,] # Remove training trials
        data_temp <- data_temp[data_temp$slow_trial == 0,]  # Remove too slow trials
        confidence_average <- mean(data_temp$cj)
        if (0.05 <= confidence_average && confidence_average <= 0.95){
          data_temp$check3 <- TRUE
        }
        
        # Step 4: Performance at chance level as assessed by a binomial test
        
        correct_responses <- sum(data_temp$cor)
        total_responses <- nrow(data_temp)
        binomial <- binom.test(correct_responses,total_responses,1/2,alternative="greater")
        if (binomial[3] <= alpha){ #is this correct?
          data_temp$check4 <- TRUE
        }
        data_viable <- rbind(data_viable,data_temp) 
      } 
    }
  }
}


# (b) Checking for irregularities in the data 

# Calculating averages per participant

sub <- NULL
p_correct_tot <- NULL
p_correct <- NULL
difficulty <- NULL
for (i in unique(data_viable$sub)){
  table <- prop.table(table(data_viable$cor[data_viable$sub==i], data_viable$coherence[data_viable$sub==i]),2)
  for (j in 1:3){
    sub <- append(sub,i)
    p_correct_tot <- append(p_correct_tot,prop.table(table(data_viable$cor[data_viable$sub==i]))[2])
  }
  p_correct <- append(p_correct,table[2,1])
  difficulty <- append(difficulty,0.1)
  p_correct <- append(p_correct,table[2,2])
  difficulty <- append(difficulty,0.2)
  p_correct <- append(p_correct,table[2,3])
  difficulty <- append(difficulty,0.4)
}
df_participant <- data.frame(sub, p_correct, difficulty, p_correct_tot)

# Calculating averages per participant and condition

sub <- NULL
manipulation <- NULL
rt_mean <- NULL
rtconf_mean <- NULL
for (i in unique(data_viable$sub)){
  for (j in unique(data_viable$manipulation)){
    temp <- subset(data_viable, sub == i & manipulation == j)
    sub <- append(sub, i)
    manipulation <- append(manipulation, j)
    rt_mean <- append(rt_mean, mean(temp$rt))
    rtconf_mean <- append(rtconf_mean, mean(temp$rtconf))
  }
}  
df_participant_manipulation <- data.frame(sub, manipulation, rt_mean, rtconf_mean)

# Training blocks required

sub <- NULL
block <- NULL
repetitions <- NULL
for (i in unique(data_full$sub)){
  for (j in unique(data_full$block)){
    temp <- subset(data_full, sub == i & block == j)
    sub <- append(sub, i)
    block <- append(block, j)
    if (max(temp$block_repetition) == -Inf){
      repetitions <- append(repetitions, 0)
    } else {
      repetitions <- append(repetitions, max(temp$block_repetition))
    }
  }
}  
attempted_blocks <- data.frame(sub, block, repetitions)
attempted_blocks_training <- subset(attempted_blocks, block==1 | block == 2 | block == 3)

ggplot(data=attempted_blocks,aes(fill=as.factor(block),y=repetitions,x=sub)) +  # Plot of all blocks
  geom_bar(position = position_stack(reverse = TRUE), stat='identity') +
  coord_flip() +
  labs(x = "Subject number", y = "Number of repetitions", fill = "Training block")

ggplot(data=attempted_blocks_training,aes(fill=as.factor(block),y=repetitions,x=sub)) +  # Plot of training block 1 and 2
  geom_bar(position = position_stack(reverse = TRUE), stat='identity') +
  coord_flip() +
  labs(x = "Subject number", y = "Number of repetitions", fill = "Training block") +
  geom_hline(yintercept = 7, linetype = "dashed") +
  annotate(geom="text", x=38, y=11, label="Cutoff", fontface = "bold")

ggplot(data=attempted_blocks_training, aes(x=as.factor(block), y=repetitions)) +  # Box plot training blocks
  geom_boxplot(width=0.4) +
  labs(x = "Training blocks", y = "Number of repetitions")

# RT manipulations (all)

ggplot(data = data_viable, aes(x = manipulation, y = rt)) +
  geom_boxplot(outlier.shape = NA)  +
  theme_bw() +
  coord_flip() +
  geom_jitter(width=0.1,alpha=0.2) +
  theme(legend.position = "none") +
  scale_x_discrete(labels = c("AccAcc" = "Accurate decision\nAccurate confidence rating", "AccFast" = "Accurate decision\nFast confidence rating", "FastFast" = "Fast decision\nFast confidence rating", "FastAcc" = "Fast decision\nAccurate confidence rating")) +
  labs(x = "Manipulation", y = "Decision reaction time")

# RT manipulations (averages)

ggplot(data = df_participant_manipulation, aes(x = manipulation, y = rt_mean, group = sub)) +
  geom_line() +
  theme_bw() +
  coord_flip() +
  theme(legend.position = "none") +
  scale_x_discrete(labels = c("AccAcc" = "Accurate decision\nAccurate confidence rating", "AccFast" = "Accurate decision\nFast confidence rating", "FastFast" = "Fast decision\nFast confidence rating", "FastAcc" = "Fast decision\nAccurate confidence rating")) +
  labs(x = "Manipulation", y = "Decision reaction time")

# Confidence rating RT manipulations (all)

ggplot(data = data_viable, aes(x = manipulation, y = rtconf)) +
  geom_boxplot(outlier.shape = NA)  +
  theme_bw() +
  coord_flip() +
  geom_jitter(width=0.1,alpha=0.2) +
  theme(legend.position = "none") +
  scale_x_discrete(labels = c("AccAcc" = "Accurate decision\nAccurate confidence rating", "AccFast" = "Accurate decision\nFast confidence rating", "FastFast" = "Fast decision\nFast confidence rating", "FastAcc" = "Fast decision\nAccurate confidence rating")) +
  labs(x = "Manipulation", y = "Confidence rating reaction time")

# Confidence rating RT manipulations (averages)

ggplot(data = df_participant_manipulation, aes(x = manipulation, y = rtconf_mean, group = sub)) +
  geom_line() +
  theme_bw() +
  coord_flip() +
  theme(legend.position = "none") +
  scale_x_discrete(labels = c("AccAcc" = "Accurate decision\nAccurate confidence rating", "AccFast" = "Accurate decision\nFast confidence rating", "FastFast" = "Fast decision\nFast confidence rating", "FastAcc" = "Fast decision\nAccurate confidence rating")) +
  labs(x = "Manipulation", y = "Confidence rating reaction time")
  
# Proportion correct responses

ggplot(data = df_participant, aes(x = as.factor(difficulty), y = p_correct)) +
  geom_boxplot() +
  labs(x = "Coherence level", y = "Percentage correct responses")

plot(df_participant$sub, df_participant$p_correct_tot, pch = 19, xlab = "Subject number", ylab = "Percentage correct responses")
plot(df_participant$p_correct_tot, df_participant$p_correct, col = sub, pch = 19, xlab = "Total percentage correct responses", ylab = "Percentage correct responses for each coherence level")

