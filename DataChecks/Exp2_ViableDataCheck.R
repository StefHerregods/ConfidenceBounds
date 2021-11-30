# November 2021
# Script checks for (a) viability of the data (as described in preregistration 
# experiment 2) and (b) irregularities in data of experiment 2



# Variables

alpha <- 0.05  # Significance level

# Setting working directory

setwd("C:\\Users\\herre\\Desktop\\Internship\\Results\\Exp2_Results") 

# Loading required packages

library(ggplot2)
library(forcats)



# (a) Checking viability of the data
# See preregistration experiment 2 for more information


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

    # Check 1: Finishing the experiment

    final_block <- suppressWarnings(max(subset(data_full, data_full$batch == j 
                                               & data_full$sub == i, 
                                               select = block)))
    final_trial <- suppressWarnings(max(subset(data_full, data_full$batch == j 
                                               & data_full$sub == i & 
                                                 data_full$block == final_block, 
                                               select = withinblocktrial)))
    if (final_block == 15 & final_trial == 59){
      data_full$check1[data_full$batch == j & data_full$sub == i] <- TRUE  

      
    
      
    }
  }
}




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
