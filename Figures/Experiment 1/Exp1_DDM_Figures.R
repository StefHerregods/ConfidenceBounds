
# Load packages

library(tidyr)

# Load data



# Merge data

df.list <- list(v_matrix, a_matrix, a2_matrix, ter_matrix, postdriftmod_matrix)
df.list <- lapply(df.list, as.data.frame())

# v

v_df_wide <- data.frame(v_matrix)
v_df_wide$index <- 1:nrow(v_df_wide)
v_df <- v_df_wide %>% pivot_longer(cols = !index, names_to = 'condition', values_to = 'v')

ggplot(v_df, aes(x = condition, y = v)) +
  geom_point() +
  geom_line(aes(group = index), alpha = 0.2) +
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







ggplot(data = v_matrix, aes(x = !!!, y = !!!), shape = 5) +
  geom_line(aes(group = !!!sub), alpha = 0.2) +
  geom_point(shape = 16, size = 3, colour = "Blue", alpha = 0.3) +
  stat_summary(aes(y = !!!, group = 1), fun = mean, colour="Blue", size = 4, shape = 95) +
  scale_x_discrete(labels = c("AccAcc" = "Accurate decision\nAccurate confidence rating", "AccFast" = "Accurate decision\nFast confidence rating", "FastFast" = "Fast decision\nFast confidence rating", "FastAcc" = "Fast decision\nAccurate confidence rating")) +
  labs(x = "Manipulation", y = "Mean decision reaction time") 
