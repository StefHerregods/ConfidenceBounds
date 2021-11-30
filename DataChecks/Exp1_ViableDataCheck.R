# October 2021
# Script checks for viability of the data as described in the preregistration of
# experiment 1: https://doi.org/10.17605/OSF.IO/Z2UCM



# Variables

alpha <- 0.05  # Significance level

# Setting working directory

setwd("C:\\Users\\herre\\Desktop\\Internship\\Results\\Exp1_Results") 

# Loading data

data_full <- read.csv(file="Exp1_data_full.csv")
colnames(data_full)[1] <- gsub('^...','',colnames(data_full)[1])

# Add data checks

data_full$check1 <- FALSE
data_full$check2 <- FALSE
data_full$check3 <- FALSE
data_full$check4 <- FALSE

# Loop through participants

for (i in unique(data_full$sub)){
  for (j in unique(data_full$batch)){
    
    data_temp <- subset(data_full, data_full$batch == j & data_full$sub == i)
    
    # Check 1: Not finishing the experiment
    
    final_block <- suppressWarnings(max(subset(data_temp, select = block)))
    final_trial <- suppressWarnings(max(subset(data_temp, 
                                               data_temp$block == final_block, 
                                               select = withinblocktrial)))
    if (final_block == 15 & final_trial == 59){
      data_full$check1[data_full$batch == j & data_full$sub == i] <- TRUE
    }
    
    # Check 2: Requiring more than 7 training blocks
    
    training_blocks <- suppressWarnings(
      max(data_temp$block_repetition[data_temp$block == 1]) + 
      max(data_temp$block_repetition[data_temp$block == 2]) + 
      max(data_temp$block_repetition[data_temp$block == 3]))
    if (training_blocks <= 7){
      data_full$check2[data_full$batch == j & data_full$sub == i] <- TRUE
    }
    
    # Check 3: Same confidence rating in more than 95% of the trials
    
    data_temp <- data_temp[data_temp$block > 3,] # Remove training trials
    data_temp <- data_temp[data_temp$slow_trial == 0,]  # Remove too slow trials
    if (!(is.na(mean(data_temp$cj)))){
      confidence_average <- mean(data_temp$cj)
      if (0.05 <= confidence_average & confidence_average <= 0.95){
        data_full$check3[data_full$batch == j & data_full$sub == i] <- TRUE
      }
    }
    
    # Check 4: Performance at chance level as assessed by a binomial test
    
    correct_responses <- sum(data_temp$cor)
    total_responses <- nrow(data_temp)
    if (total_responses > 0){
      binomial <- binom.test(correct_responses,total_responses,1/2,alternative="greater")
      if (binomial[3] <= alpha){ 
        data_full$check4[data_full$batch == j & data_full$sub == i] <- TRUE
      }
    }
    
  }
}

# What participants have non-viable data?

for (batch in unique(data_full$batch)){
  print(paste0("Batch ",batch,":",collapse=""))
  print(unique(data_full$sub[data_full$check1 == FALSE & data_full$batch == batch]))
  print(unique(data_full$sub[data_full$check2 == FALSE & data_full$batch == batch]))
  print(unique(data_full$sub[data_full$check3 == FALSE & data_full$batch == batch]))
  print(unique(data_full$sub[data_full$check4 == FALSE & data_full$batch == batch]))
}

# Subset viable data from full data

data_viable <- subset(data_full, check1 == T & check2 == T & check3 == T & 
                        check4 == T & block > 3 & slow_trial == 0)

# Save viable data

write.csv(data_viable,"Exp1_data_viable.csv",row.names=FALSE)
