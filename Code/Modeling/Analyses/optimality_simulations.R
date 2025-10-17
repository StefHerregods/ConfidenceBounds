

rm(list=ls())

library(lattice)
library(Rcpp)  
library(DEoptim)
library(gridExtra)

sourceCpp("code/C++_generators/DDM_confidence_bounds_scale6.cpp") 

text_settings <- list(par.settings = list(fontsize = list(text = 8, axis.text = 8)))


# binary confidence options variant --------------------------------------------


# Define model parameter values
v <- 1
a <- 1
a_slope <- 0
ter <- 0
a2 <- 5
vratio <- 1
ter2 <- 0
z <- 0.5  
ntrials <- 50000
s <- 1  
dt <- 0.001 
a2_slope_upper <- seq(from = 0.1, to = 0.5, by = 0.02)
a2_slope_lower <- seq(from = 0.1, to = 0.5, by = 0.02)
starting_point_confidence <- 0.5

# Define rewards and costs
reward_matrix_1 <- data.frame(
  '1' = c(3, -3),  # cj 1
  '2' = c(2, -2),  # cj 2
  '3' = c(1, -1),  # cj 3
  '4' = c(-1, 1),  # cj 4
  '5' = c(-2, 2),  # cj 5
  '6' = c(-3, 3)   # cj 6
)
reward_matrix_2 <- data.frame(
  '1' = c(3, -9),  # cj 1
  '2' = c(2, -6),  # cj 2
  '3' = c(1, -3),  # cj 3
  '4' = c(-1, 3),  # cj 4
  '5' = c(-2, 6),  # cj 5
  '6' = c(-3, 9)   # cj 6
)
rownames(reward_matrix_1) <- c(0, 1)  # 0 == incorrect, 1 == correct
rownames(reward_matrix_2) <- c(0, 1)  # 0 == incorrect, 1 == correct
rtconf_cost <- 1  # cost for every extra rtconf second


# Compute score function -------------------------------------------------------


compute_score <- function(predictions, reward_matrix) {
  
  # Compute reward
  rewards <- reward_matrix[cbind(predictions$cor + 1, predictions$cj)]
  
  # Apply time penalty
  scores <- rewards - (rtconf_cost * predictions$rtconf)
  
  return(mean(scores))
  
}


# Simulate data ----------------------------------------------------------------

# Make predictions
df_scores_1 <- NULL
df_scores_2 <- NULL
for (i_a2_slope_upper in a2_slope_lower){
  for (i_a2_slope_lower in a2_slope_upper){
    
    predictions_1 <- data.frame(DDM_confidence_bounds_scale6(v = v, a = a, a_slope = a_slope, ter = ter, a2 = a2, vratio = vratio, ter2 = ter2, a2_slope_upper = i_a2_slope_upper, a2_slope_lower = i_a2_slope_lower, starting_point_confidence = starting_point_confidence, n_trials = ntrials))
    names(predictions_1) <- c('rt', 'cor', 'rtconf', 'cj')
    score <- compute_score(predictions_1, reward_matrix_1)
    df_scores_1 <- rbind(df_scores_1, c(i_a2_slope_upper, i_a2_slope_lower, score))
    
    predictions_2 <- data.frame(DDM_confidence_bounds_scale6(v = v, a = a, a_slope = a_slope, ter = ter, a2 = a2, vratio = vratio, ter2 = ter2, a2_slope_upper = i_a2_slope_upper, a2_slope_lower = i_a2_slope_lower, starting_point_confidence = starting_point_confidence, n_trials = ntrials))
    names(predictions_2) <- c('rt', 'cor', 'rtconf', 'cj')
    score <- compute_score(predictions_2, reward_matrix_2)
    df_scores_2 <- rbind(df_scores_2, c(i_a2_slope_upper, i_a2_slope_lower, score))
    
  }
  print(i_a2_slope_upper)
}  
df_scores_1 <- data.frame(df_scores_1)
df_scores_2 <- data.frame(df_scores_2)
names(df_scores_1) <- c('a2_slope_upper', 'a2_slope_lower', 'score')
names(df_scores_2) <- c('a2_slope_upper', 'a2_slope_lower', 'score')

# Transforming dataframe into a matrix
matrix_1 <- matrix(df_scores_1$score, nrow = length(a2_slope_upper), ncol = length(a2_slope_lower))
rownames(matrix_1) <- a2_slope_upper
colnames(matrix_1) <- a2_slope_lower
matrix_2 <- matrix(df_scores_2$score, nrow = length(a2_slope_upper), ncol = length(a2_slope_lower))
rownames(matrix_2) <- a2_slope_upper
colnames(matrix_2) <- a2_slope_lower


# Plots ------------------------------------------------------------------------


p1 <- levelplot(matrix_1,
                xlab = "Lower confidence boundary urgency", 
                ylab = "Upper confidence boundary urgency",
                main = "Reward matrix 1",
                scales = list(x = list(at = c(1,6,11,16,21), labels = c(0.1,0.2,0.3,0.4,0.5)), 
                              y = list(at = c(1,6,11,16,21), labels = c(0.1,0.2,0.3,0.4,0.5)),
                              tck = c(1,0)))
p2 <- levelplot(matrix_2,
                xlab = "Lower confidence boundary urgency", 
                ylab = "Upper confidence boundary urgency",
                main = "Reward matrix 2",
                scales = list(x = list(at = c(1,6,11,16,21), labels = c(0.1,0.2,0.3,0.4,0.5)), 
                              y = list(at = c(1,6,11,16,21), labels = c(0.1,0.2,0.3,0.4,0.5)),
                              tck = c(1,0)))

png("figures/heatmap_scores_1.png", width = 3, height = 3, units = "in", res = 500)
print(p1)
dev.off()
png("figures/heatmap_scores_2.png", width = 3, height = 3, units = "in", res = 500)
print(p2)
dev.off()

