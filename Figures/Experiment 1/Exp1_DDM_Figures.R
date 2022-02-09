
# Load packages

library(tidyr)
library(ggplot2)

# Set working directory

setwd('C:\\Users\\herre\\Desktop\\Internship\\Results\\Exp1_Results')

# Load data (long format)

df <- data.frame(matrix(ncol = 7, nrow = 40*4))
colnames(df) <- c('sub', 'manipulation', 'v', 'a', 'ter', 'a2', 'postdriftmod')
condLab <- c('AccAcc', 'AccFast', 'FastFast', 'FastAcc')  #!!! Change to all forms of manipulations
j <- 1
for (i in 1:40){
  for(c in 1:4){
    file_name <- paste0('Parameter_estimation\\results_sub_', i, '_', condLab[c], '.Rdata')
    load(file_name)
    df[j,] <- c(i, condLab[c], results$optim$bestmem[1], results$optim$bestmem[2], results$optim$bestmem[3], results$optim$bestmem[4], results$optim$bestmem[5])
    j <- j + 1
  }
}
df$a2 <- as.numeric(data.frame(df$a2)) #!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

# v

ggplot(df, aes(x = manipulation, y = v)) +
  geom_point() +
  geom_line(aes(group = sub), alpha = 0.2) +
  stat_summary(aes(y = v, group = 1), fun = mean, colour="Blue", size = 4, shape = 95) +
  scale_x_discrete(labels = c("Fast decision\nFast confidence rating", "Accurate decision\nFast confidence rating", "Accurate decision\nAccurate confidence rating", "Fast decision\nAccurate confidence rating")) 
  
# a

a_df_wide <- data.frame(a_matrix)
a_df_wide$index <- 1:nrow(a_df_wide)
a_df <- a_df_wide %>% pivot_longer(cols = !index, names_to = 'condition', values_to = 'a')

ggplot(a_df, aes(x = condition, y = a)) +
  geom_point() +
  geom_line(aes(group = index), alpha = 0.2) +
  stat_summary(aes(y = a, group = 1), fun = mean, colour="Blue", size = 4, shape = 95) +
  scale_x_discrete(labels = c("Fast decision\nFast confidence rating", "Accurate decision\nFast confidence rating", "Accurate decision\nAccurate confidence rating", "Fast decision\nAccurate confidence rating")) 

# a2

a2_df_wide <- data.frame(a2_matrix)
a2_df_wide$index <- 1:nrow(a2_df_wide)
a2_df <- a2_df_wide %>% pivot_longer(cols = !index, names_to = 'condition', values_to = 'a2')

ggplot(a2_df, aes(x = condition, y = a2)) +
  geom_point() +
  geom_line(aes(group = index), alpha = 0.2) +
  stat_summary(aes(y = a2, group = 1), fun = mean, colour="Blue", size = 4, shape = 95) +
  scale_x_discrete(labels = c("Fast decision\nFast confidence rating", "Accurate decision\nFast confidence rating", "Accurate decision\nAccurate confidence rating", "Fast decision\nAccurate confidence rating")) 

# ter

ter_df_wide <- data.frame(ter_matrix)
ter_df_wide$index <- 1:nrow(ter_df_wide)
ter_df <- ter_df_wide %>% pivot_longer(cols = !index, names_to = 'condition', values_to = 'ter')

ggplot(ter_df, aes(x = condition, y = ter)) +
  geom_point() +
  geom_line(aes(group = index), alpha = 0.2) +
  stat_summary(aes(y = ter, group = 1), fun = mean, colour="Blue", size = 4, shape = 95) +
  scale_x_discrete(labels = c("Fast decision\nFast confidence rating", "Accurate decision\nFast confidence rating", "Accurate decision\nAccurate confidence rating", "Fast decision\nAccurate confidence rating")) 

# postdriftmod

postdriftmod_df_wide <- data.frame(postdriftmod_matrix)
postdriftmod_df_wide$index <- 1:nrow(postdriftmod_df_wide)
postdriftmod_df <- postdriftmod_df_wide %>% pivot_longer(cols = !index, names_to = 'condition', values_to = 'postdriftmod')

ggplot(postdriftmod_df, aes(x = condition, y = postdriftmod)) +
  geom_point() +
  geom_line(aes(group = index), alpha = 0.2) +
  stat_summary(aes(y = postdriftmod, group = 1), fun = mean, colour="Blue", size = 4, shape = 95) +
  scale_x_discrete(labels = c("Fast decision\nFast confidence rating", "Accurate decision\nFast confidence rating", "Accurate decision\nAccurate confidence rating", "Fast decision\nAccurate confidence rating")) 





### Simulations based on estimated parameters ### 

# to do: loop this

# sub 1

n <- 10000
z = 0.5  # Starting point (accuracy-coded dataset -> 0.5)
ntrials = 1000  # Number of decision-making simulations per observation
sigma = 1  # Within-trial noise
dt = 0.01  # Precision

c_predicted <- NULL
e_predicted <- NULL

for (i in 1:n){
  
  predictions <- data.frame(DDM_confidence_bounds(v = df$v[1], a = df$a[1], ter = df$ter[1], z = z, ntrials = ntrials, s = sigma, dt = dt, a2 = df$a2[1], postdriftmod = df$postdriftmod[1]))
  names(predictions) <- c('rt', 'resp', 'cor', 'raw_evidence2', 'rtfull', 'confidence', 'rtconf', 'cj')
  
  # Separate predictions according to the response
  
  c_predicted_temp <- predictions[predictions$cor == 1,]
  e_predicted_temp <- predictions[predictions$cor == 0,]
  
  # Merge predictions
  
  c_predicted <- rbind(c_predicted, c_predicted_temp)
  e_predicted <- rbind(e_predicted, e_predicted_temp)
  
  
}









