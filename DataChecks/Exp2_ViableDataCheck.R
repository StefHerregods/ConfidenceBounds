# Project: Confidence bounds (2021-2022)
# Script checks for (a) viability of the data (as described in preregistration experiment 2) 
# and (b) irregularities in data of experiment 2

library(ggplot2)
library(forcats)

setwd('C:\\Users\\herre\\OneDrive\\Bureaublad\\Internship\\Results\\Exp1_results_tot') # temp(_2) tot


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
        if (binomial[3] <= alpha){ # is this correct?
          data_temp$check4 <- TRUE
        }
        data_viable <- rbind(data_viable,data_temp) 
      } 
    }
  }
}
unique(data_viable$sub[data_viable$check2 == FALSE])
unique(data_viable$sub[data_viable$check3 == FALSE])
unique(data_viable$sub[data_viable$check4 == FALSE])

data_viable <- subset(data_viable, c(check2 == TRUE & check3 == TRUE & check4 == TRUE))
