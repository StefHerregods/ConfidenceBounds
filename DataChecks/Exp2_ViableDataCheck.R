# November 2021
# Script checks for viability of the data as described in the preregistration of
# experiment 2: https://doi.org/10.17605/OSF.IO/VYH4K


# Loading packages

library(ggplot2)
library(shiny)
library(dplyr)

# Variables

alpha <- 0.05  # Significance level

# Setting working directory

setwd("C:\\Users\\herre\\Desktop\\Internship\\Results\\Exp2_Results") 

# Loading data

data_full <- read.csv(file="Exp2_data_full.csv")
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
    max_p_cj <- suppressWarnings(max(prop.table(table(data_temp$cj))))
    if (0.05 <= max_p_cj & max_p_cj <= 0.95){
      data_full$check3[data_full$batch == j & data_full$sub == i] <- TRUE
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
                                 check4 == T & block > 3 & slow_trial == 0 & rt >= 0.2)
data_viable <- arrange(data_viable, sub)

# Manual check #!!!

par(mfrow=c(2,2))
for(i in unique(data_viable$sub)){
  tempDat <- subset(data_viable, data_viable$sub == i)
  tempDat$response <- ifelse(tempDat$resp == "['c']", 0, 1)
  acc_block <- with(tempDat, aggregate(cor, by = list(block = block), mean))
  bias_block <- with(tempDat, aggregate(response, by = list(block = block), mean))
  plot(acc_block, ylab = "Acc (.) and bias (x)", frame = F, ylim = c(0,1)); abline(h = .5, lty = 2, col = "grey")
  points(bias_block, pch = 4)
  plot(tempDat$rt, frame = F, main = paste('subject',i), ylab = "RT", ylim = c(0,5), col = c('black','grey','grey','black')[as.factor(tempDat$manipulation)])
  cj_block <- with(tempDat, aggregate(cj, by = list(block = block), mean))
  plot(cj_block, frame = F, ylab = "confidence judgement", ylim = c(1,6)); abline(h = 3.5, lty = 2, col = "grey")
  plot(tempDat$rtconf, frame = F, ylab = "RT_conf", col = c('black','grey','grey','black')[as.factor(tempDat$manipulation)])
}


# Save viable data

write.csv(data_viable,"Exp2_data_viable.csv",row.names=FALSE)
