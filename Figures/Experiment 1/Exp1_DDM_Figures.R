
# Load packages

library(tidyr)
library(ggplot2)

# Set working directory

setwd('C:\\Users\\herre\\Desktop\\Internship\\Results\\Exp1_Results')

# Load data (long format)

df <- data.frame(matrix(ncol = 9, nrow = 40*4))
colnames(df) <- c('sub', 'manipulation', 'v1', 'v2', 'v3', 'a', 'ter', 'a2', 'postdriftmod')
condLab <- c('FastFast', 'AccFast', 'AccAcc', 'FastAcc') 
j <- 1
for (i in 1:20){ #!!!change
  for(c in 1:4){
    file_name <- paste0('Parameter_estimation\\results_sub_', i, '_', condLab[c], '.Rdata')
    load(file_name)
    df[j,] <- c(i, condLab[c], results$optim$bestmem[1], results$optim$bestmem[2], results$optim$bestmem[3], results$optim$bestmem[4], results$optim$bestmem[5], results$optim$bestmem[6], results$optim$bestmem[7])
    j <- j + 1
  }
}

df[3:9] <- lapply(df[3:9], as.numeric)


# v1

ggplot(df, aes(x = manipulation, y = v1)) +
  geom_point() +
  geom_line(aes(group = sub), alpha = 0.2) +
  stat_summary(aes(y = v1, group = 1), fun = mean, colour="Blue", size = 4, shape = 95) +
  scale_x_discrete(labels = c("Fast decision\nFast confidence rating", "Accurate decision\nFast confidence rating", "Accurate decision\nAccurate confidence rating", "Fast decision\nAccurate confidence rating")) 
  
# v2

ggplot(df, aes(x = manipulation, y = v2)) +
  geom_point() +
  geom_line(aes(group = sub), alpha = 0.2) +
  stat_summary(aes(y = v2, group = 1), fun = mean, colour="Blue", size = 4, shape = 95) +
  scale_x_discrete(labels = c("Fast decision\nFast confidence rating", "Accurate decision\nFast confidence rating", "Accurate decision\nAccurate confidence rating", "Fast decision\nAccurate confidence rating")) 

# v3

ggplot(df, aes(x = manipulation, y = v3)) +
  geom_point() +
  geom_line(aes(group = sub), alpha = 0.2) +
  stat_summary(aes(y = v3, group = 1), fun = mean, colour="Blue", size = 4, shape = 95) +
  scale_x_discrete(labels = c("Fast decision\nFast confidence rating", "Accurate decision\nFast confidence rating", "Accurate decision\nAccurate confidence rating", "Fast decision\nAccurate confidence rating")) 

# v comparison

ggplot(df, aes(x = manipulation)) +
  geom_point(aes(y = v1), colour = 'darkred') +
  geom_point(aes(y = v2), colour = 'blue') +
  geom_point(aes(y = v3), colour = 'green') +
  geom_line(aes(y = v1, group = sub), alpha = 0.2, colour = 'darkred', lty = 5) +
  geom_line(aes(y = v2, group = sub), alpha = 0.2, colour = 'blue', lty = 5) +
  geom_line(aes(y = v3, group = sub), alpha = 0.2, colour = 'green', lty = 5) +
  stat_summary(aes(y = v1, group = 1), fun = mean, colour= 'darkred', size = 4, shape = 95) +
  stat_summary(aes(y = v2, group = 1), fun = mean, colour= 'blue', size = 4, shape = 95) +
  stat_summary(aes(y = v3, group = 1), fun = mean, colour= 'green', size = 4, shape = 95) +
  scale_x_discrete(labels = c("Fast decision\nFast confidence rating", "Accurate decision\nFast confidence rating", "Accurate decision\nAccurate confidence rating", "Fast decision\nAccurate confidence rating")) 


# a

ggplot(df, aes(x = manipulation, y = a)) +
  geom_point() +
  geom_line(aes(group = sub), alpha = 0.2) +
  stat_summary(aes(y = a, group = 1), fun = mean, colour="Blue", size = 4, shape = 95) +
  scale_x_discrete(labels = c("Fast decision\nFast confidence rating", "Accurate decision\nFast confidence rating", "Accurate decision\nAccurate confidence rating", "Fast decision\nAccurate confidence rating")) 

# a2

ggplot(df, aes(x = manipulation, y = a2)) +
  geom_point() +
  geom_line(aes(group = sub), alpha = 0.2) +
  stat_summary(aes(y = a2, group = 1), fun = mean, colour="Blue", size = 4, shape = 95) +
  scale_x_discrete(labels = c("Fast decision\nFast confidence rating", "Accurate decision\nFast confidence rating", "Accurate decision\nAccurate confidence rating", "Fast decision\nAccurate confidence rating")) 

# ter

ggplot(df, aes(x = manipulation, y = ter)) +
  geom_point() +
  geom_line(aes(group = sub), alpha = 0.2) +
  stat_summary(aes(y = ter, group = 1), fun = mean, colour="Blue", size = 4, shape = 95) +
  scale_x_discrete(labels = c("Fast decision\nFast confidence rating", "Accurate decision\nFast confidence rating", "Accurate decision\nAccurate confidence rating", "Fast decision\nAccurate confidence rating")) 

# postdriftmod

ggplot(df, aes(x = manipulation, y = postdriftmod)) +
  geom_point() +
  geom_line(aes(group = sub), alpha = 0.2) +
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









