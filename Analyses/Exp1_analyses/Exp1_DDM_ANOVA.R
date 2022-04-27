
# Set-up ----


## Load packages

library(rstatix)
library(tidyr)
library(dplyr)

## Set working directory

setwd('C:\\Users\\herre\\Desktop\\Internship\\Results\\Exp1_Results')


# Load data ----


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


# Transform data ----





# Repeated measures ANOVA ----


