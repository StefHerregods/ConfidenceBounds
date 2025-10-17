# Purpose: Why does our model do better than other models?


rm(list = ls())
source('Code/model_functions.R')
library(ggplot2)
library(Rcpp)
library(lattice)

EXPERIMENT <- 1
df_obs <- read.csv(paste0('Data/Experiment_', EXPERIMENT, '/exp', EXPERIMENT, '_data_viable.csv'))
 

# A - Which model fits what participant best? ----------------------------------
 

# Computing individual condition BIC

if (EXPERIMENT == 1) {
  model_types <- c(1, 6, 7, 8)
} else {
  model_types <- c(4, 6, 7, 8, 9)
}
n_models <- length(model_types)
df_BIC <- data.frame(sub = rep(1:40, each = 4), manipulation = rep(unique(df_obs$manipulation), times = 40))
model_cols <- setNames(as.data.frame(matrix(0, nrow = nrow(df_BIC), ncol = n_models)), paste0("model_", model_types))
df_BIC <- cbind(df_BIC, model_cols)

for (model in 1:n_models){
  print(model)
  print(paste('Computing BIC values model', model_types[model]))
  df_pred <- read.csv(paste0('Data/Experiment_', EXPERIMENT,'/Model_predictions/exp', EXPERIMENT, '_model', model_types[model], '_predictions.csv'))

  for (sub in unique(df_obs$sub)){
    for (manipulation in unique(df_obs$manipulation)){
      
      df_obs_sub <- df_obs[df_obs$sub == sub & df_obs$manipulation == manipulation,]
      df_pred_sub <- df_pred[df_pred$sub == sub & df_pred$manipulation == manipulation,]
      BIC_values <- compute_BIC(df_observed = df_obs_sub, df_predicted = df_pred_sub, model_type = model_types[model], experiment = EXPERIMENT)
      df_BIC[df_BIC$sub == sub & df_BIC$manipulation == manipulation, model + 2] <- unlist(BIC_values$BIC)
      
    }
  } 
}

# Compute best fitting model for each condition, merge with observations

model_names <- paste0("model_", model_types)
df_BIC$best_model <- model_names[max.col(-df_BIC[model_names])]
table(df_BIC$best_model)/160
df_merged <- merge(df_obs, df_BIC[, c("sub", "manipulation", "best_model")],
                   by = c("sub", "manipulation"), all.x = TRUE)

mean(df_BIC$model_1)
mean(df_BIC$model_9)


# Plot aggregated results: rtconf ~ cj

MODEL <- 'model_4'
df_aggregated <- aggregate(rtconf ~ cj + sub, data = df_merged[df_merged$best_model == MODEL,], FUN = mean)
df_mean <- aggregate(rtconf ~ cj, data = df_aggregated, FUN = mean)
ggplot(data = df_aggregated, aes(x = cj, y = rtconf, group = sub)) +
  geom_line(alpha = 0.5) +
  geom_line(data = df_mean, aes(x = cj, y = rtconf), col = 'darkred', size = 1) +
  theme_bw() +
  ylim(c(0,2)) +
  ggtitle('2DSD optional stopping')

MODEL <- 'model_4'
df_merged$rtconf_cut <- cut(df_merged$rtconf, breaks = quantile(df_merged$rtconf, probs = seq(0, 1, length.out = 11)))
df_aggregated <- aggregate(cj ~ rtconf_cut + sub, data = df_merged[df_merged$best_model == MODEL,], FUN = mean)
df_mean <- aggregate(cj ~ rtconf_cut, data = df_aggregated, FUN = mean)
ggplot(data = df_aggregated, aes(x = rtconf_cut, y = cj, group = sub)) +
  geom_line(alpha = 0.5) +
  geom_line(data = df_mean, aes(x = rtconf_cut, y = cj), col = 'darkred', size = 1) +
  theme_bw() +
  ggtitle('2DSD optional stopping')

# Plot aggregated results: rtconf ~ certainty

library(dplyr)

df_merged <- df_merged %>%
  mutate(certainty = case_when(
    cj %in% c(3, 4) ~ 1,
    cj %in% c(2, 5) ~ 2,
    cj %in% c(1, 6) ~ 3
  ))

MODEL <- 'model_8'
df_aggregated <- aggregate(rtconf ~ certainty + sub, data = df_merged[df_merged$best_model == MODEL,], FUN = mean)
df_mean <- aggregate(rtconf ~ certainty, data = df_aggregated, FUN = mean)
ggplot(data = df_aggregated, aes(x = certainty, y = rtconf, group = sub)) +
  geom_line(alpha = 0.5) +
  geom_line(data = df_mean, aes(x = certainty, y = rtconf), col = 'darkred', size = 1) +
  theme_bw() +
  ylim(c(0,2)) +
  ggtitle('2DSD + variation')

MODEL <- 'model_9'
df_merged$rtconf_cut <- cut(df_merged$rtconf, breaks = quantile(df_merged$rtconf, probs = seq(0, 1, length.out = 11)))
df_aggregated <- aggregate(certainty ~ rtconf_cut + sub, data = df_merged[df_merged$best_model == MODEL,], FUN = mean)
df_mean <- aggregate(certainty ~ rtconf_cut, data = df_aggregated, FUN = mean)
ggplot(data = df_aggregated, aes(x = rtconf_cut, y = certainty, group = sub)) +
  geom_line(alpha = 0.5) +
  geom_line(data = df_mean, aes(x = rtconf_cut, y = certainty), col = 'darkred', size = 1) +
  theme_bw() +
  ggtitle('2DSD optional stopping')

# Confidence accuracy


library(dplyr)
library(pROC)

df_merged <- df_merged %>%
  group_by(sub, manipulation) %>%
  mutate(auc2 = as.numeric(auc(as.numeric(cor), as.numeric(cj)))) %>%
  ungroup()

MODEL <- 'model_9'
df_merged$rtconf_cut <- cut(df_merged$rtconf, breaks = quantile(df_merged$rtconf, probs = seq(0, 1, length.out = 11)))
df_aggregated <- aggregate(auc2 ~ rtconf_cut + sub, data = df_merged[df_merged$best_model == MODEL,], FUN = mean)
df_mean <- aggregate(auc2 ~ rtconf_cut, data = df_aggregated, FUN = mean)
ggplot(data = df_aggregated, aes(x = rtconf_cut, y = auc2, group = sub)) +
  geom_line(alpha = 0.5) +
  geom_line(data = df_mean, aes(x = rtconf_cut, y = auc2), col = 'darkred', size = 1) +
  theme_bw() +
  ggtitle('optional stopping')


### Heatmap rtconf-cj  correlations  -  observations ~ predictions

participants <- 1:40
manipulations <- c('AccAcc', 'FastAcc', 'FastFast', 'AccFast')

compute_cor <- function(df){
  cors <- NULL
  for (participant_ID in participants){
    for (manipulation in manipulations){
      df_temp <- df[df$sub == participant_ID & df$manipulation == manipulation,]
      cors <- c(cors, cor(df_temp$cj, df_temp$rtconf))
    }
  }
  return(cors)
}

df_cors <- compute_cor(df_merged)

for (model_type in 1:9){
  
  df_pred <- read.csv(paste0('Data/Experiment_', EXPERIMENT, '/Model_predictions/exp', EXPERIMENT, '_model', model_type, '_predictions_recovery_180.csv'))
  df_cors <- cbind(df_cors, compute_cor(df_pred))

}

df_cors <- data.frame(df_cors)
names(df_cors) <- c('observed', 'predictions_M1', 'predictions_M2', 'predictions_M3', 'predictions_M4', 'predictions_M5', 'predictions_M6', 'predictions_M7', 'predictions_M8', 'predictions_M9')

library(ggplot2)
library(gridExtra)

plots <- list()
for (i in 2:10) {
  p <- ggplot(df_cors, aes_string(x = names(df_cors)[1], y = names(df_cors)[i])) +
    geom_point() +
    ggtitle(paste(names(df_cors)[1], "vs", names(df_cors)[i])) +
    xlim(-1, 1) +
    ylim(-1, 1)
  plots[[i - 1]] <- p
}

grid.arrange(grobs = plots, ncol = 3)
cor(df_cors$observed, df_cors$predictions_M1)
cor(df_cors$observed, df_cors$predictions_M2)
cor(df_cors$observed, df_cors$predictions_M3)
cor(df_cors$observed, df_cors$predictions_M4)
cor(df_cors$observed, df_cors$predictions_M5)
cor(df_cors$observed, df_cors$predictions_M6)
cor(df_cors$observed, df_cors$predictions_M7)
cor(df_cors$observed, df_cors$predictions_M8)
cor(df_cors$observed, df_cors$predictions_M9)


### Heatmap correlations  -  observations ~ predictions

precision <- 10


# M1 (free parameters: a2 + confidence boundary slope)

par1_list <- seq(0.1, 5, length.out = precision)  # a2
par2_list <- seq(0, 5, length.out = precision)  # confidence boundary slope
cor_matrix <- data.frame()

sourceCpp('Code/C++_generators/DDM_confidence_bounds_scale6.cpp')

for (par1 in par1_list){
  for (par2 in par2_list){
      
    predictions <- data.frame(DDM_confidence_bounds_scale6(n_trials = 10000, 
                                                           a = 1, 
                                                           v = 1, 
                                                           a_slope = 0, 
                                                           ter = 0, 
                                                           a2 = par1,
                                                           vratio = 1, 
                                                           a2_slope_upper = par2, 
                                                           a2_slope_lower = par2, 
                                                           ter2 = 0, 
                                                           starting_point_confidence = 0.5))
    names(predictions) <- c('rt', 'cor', 'rtconf', 'cj')
    cor_matrix <- rbind(cor_matrix, c(par1, par2, cor(predictions$cj, predictions$rtconf)))
  }
}
names(cor_matrix) <- c('a2', 'confidence_boundary_slope', 'correlation')
p1 <- levelplot(cor_matrix, 
          colorkey = FALSE,
          xlab = "CB urgency", 
          ylab = "CB separation", 
          at = breaks, 
          col.regions = colorRampPalette(c("#2A6A98", "#3B97DA", "#9ECAED", "#FFFFFF", "#FFCDA8", "#FE9C51", "#B16E39")),
          #main = "v_ratio = 1",
          scales = list(x = list(at = c(3,7,11,15,19), labels = c(2,4,6,8,10)), 
                        y = list(at = c(3,7,11,15,19), labels = c(2,4,6,8,10)),
                        tck = c(1,0)),
          par.settings = text_settings$par.settings)
levelplot(correlation ~ a2 * confidence_boundary_slope, data = cor_matrix,
          at = seq(-1, 1, length.out = 100),
          col.regions = colorRampPalette(c("#2A6A98", "#3B97DA", "#9ECAED", "#FFFFFF", "#FFCDA8", "#FE9C51", "#B16E39")))


# M1 (free parameters: a2 + confidence boundary slope)

par1_list <- seq(0.1, 5, length.out = precision)  # a2
par2_list <- seq(0, 5, length.out = precision)  # confidence boundary slope
cor_matrix <- data.frame()

sourceCpp('Code/C++_generators/DDM_confidence_bounds_scale6.cpp')

for (par1 in par1_list){
  for (par2 in par2_list){
    
    predictions <- data.frame(DDM_confidence_bounds_scale6(n_trials = 10000, 
                                                           a = 1, 
                                                           v = 1, 
                                                           a_slope = 0, 
                                                           ter = 0, 
                                                           a2 = par1,
                                                           vratio = 1, 
                                                           a2_slope_upper = par2, 
                                                           a2_slope_lower = par2, 
                                                           ter2 = 0, 
                                                           starting_point_confidence = 0.5))
    names(predictions) <- c('rt', 'cor', 'rtconf', 'cj')
    cor_matrix <- rbind(cor_matrix, c(par1, par2, cor(predictions$cj, predictions$rtconf)))
  }
}
names(cor_matrix) <- c('a2', 'confidence_boundary_slope', 'correlation')
p1 <- levelplot(cor_matrix, 
                colorkey = FALSE,
                xlab = "CB urgency", 
                ylab = "CB separation", 
                at = breaks, 
                col.regions = colorRampPalette(c("#2A6A98", "#3B97DA", "#9ECAED", "#FFFFFF", "#FFCDA8", "#FE9C51", "#B16E39")),
                #main = "v_ratio = 1",
                scales = list(x = list(at = c(3,7,11,15,19), labels = c(2,4,6,8,10)), 
                              y = list(at = c(3,7,11,15,19), labels = c(2,4,6,8,10)),
                              tck = c(1,0)),
                par.settings = text_settings$par.settings)
levelplot(correlation ~ a2 * confidence_boundary_slope, data = cor_matrix,
          at = seq(-1, 1, length.out = 100),
          col.regions = colorRampPalette(c("#2A6A98", "#3B97DA", "#9ECAED", "#FFFFFF", "#FFCDA8", "#FE9C51", "#B16E39")))


# M1 (free parameters: a2 + confidence boundary slope)

par1_list <- seq(0.1, 3, length.out = precision)  # lower confidence boundary slope
par2_list <- seq(0, 3, length.out = precision)  # upper confidence boundary slope
cor_matrix <- data.frame()

sourceCpp('Code/C++_generators/DDM_confidence_bounds_scale6.cpp')

for (par1 in par1_list){
  for (par2 in par2_list){
    
    predictions <- data.frame(DDM_confidence_bounds_scale6(n_trials = 10000, 
                                                           a = 1, 
                                                           v = 1, 
                                                           a_slope = 0, 
                                                           ter = 0, 
                                                           a2 = par1,
                                                           vratio = 1, 
                                                           a2_slope_upper = par2, 
                                                           a2_slope_lower = 1, 
                                                           ter2 = 0, 
                                                           starting_point_confidence = 0.5))
    names(predictions) <- c('rt', 'cor', 'rtconf', 'cj')
    cor_matrix <- rbind(cor_matrix, c(par1, par2, cor(predictions$cj, predictions$rtconf)))
  }
}
names(cor_matrix) <- c('lower_confidence_boundary_slope', 'upper_confidence_boundary_slope', 'correlation')
p1 <- levelplot(cor_matrix, 
                colorkey = FALSE,
                xlab = "CB urgency", 
                ylab = "CB separation", 
                at = breaks, 
                col.regions = colorRampPalette(c("#2A6A98", "#3B97DA", "#9ECAED", "#FFFFFF", "#FFCDA8", "#FE9C51", "#B16E39")),
                #main = "v_ratio = 1",
                scales = list(x = list(at = c(3,7,11,15,19), labels = c(2,4,6,8,10)), 
                              y = list(at = c(3,7,11,15,19), labels = c(2,4,6,8,10)),
                              tck = c(1,0)),
                par.settings = text_settings$par.settings)
levelplot(correlation ~ lower_confidence_boundary_slope * upper_confidence_boundary_slope, data = cor_matrix,
          at = seq(-1, 1, length.out = 100),
          col.regions = colorRampPalette(c("#2A6A98", "#3B97DA", "#9ECAED", "#FFFFFF", "#FFCDA8", "#FE9C51", "#B16E39")))


# Mixed effects on BIC

names(df_merged)

library(tidyr)
library(dplyr)

# Convert to long
df_long <- df_BIC %>%
  pivot_longer(
    cols = starts_with("model_"),
    names_to = "model",
    values_to = "BIC"
  )

library(lme4)

fit <- lmer(BIC ~ model + (1|sub/manipulation),data=df_long)
anova(fit)
a <- pairs(emmeans::emmeans(fit, ~ model),adjust="none") 










library(dplyr)

EXPERIMENT <- 1
df_obs <- read.csv(paste0('Data/Experiment_', EXPERIMENT, '/exp', EXPERIMENT, '_data_viable.csv'))
df_obs_summary <- df_obs[c('coherence', 'rt', 'cor', 'rtconf', 'cj', 'manipulation', 'sub')]

m1 <- read.csv(paste0('Data/Experiment_', EXPERIMENT, '/Model_predictions/exp', EXPERIMENT, '_model1_predictions.csv'))
#m2 <- read.csv(paste0('Data/Experiment_', EXPERIMENT, '/Model_predictions/exp', EXPERIMENT, '_model2_predictions.csv'))
#m3 <- read.csv(paste0('Data/Experiment_', EXPERIMENT, '/Model_predictions/exp', EXPERIMENT, '_model3_predictions.csv'))
m4 <- read.csv(paste0('Data/Experiment_', EXPERIMENT, '/Model_predictions/exp', EXPERIMENT, '_model4_predictions.csv'))
m6 <- read.csv(paste0('Data/Experiment_', EXPERIMENT, '/Model_predictions/exp', EXPERIMENT, '_model6_predictions.csv'))
m7 <- read.csv(paste0('Data/Experiment_', EXPERIMENT, '/Model_predictions/exp', EXPERIMENT, '_model7_predictions.csv'))
m8 <- read.csv(paste0('Data/Experiment_', EXPERIMENT, '/Model_predictions/exp', EXPERIMENT, '_model8_predictions.csv'))
m9 <- read.csv(paste0('Data/Experiment_', EXPERIMENT, '/Model_predictions/exp', EXPERIMENT, '_model9_predictions.csv'))

if (EXPERIMENT == 1){
  model_preds <- bind_rows(
    mutate(df_obs_summary, model = "obs"),
    mutate(m1, model = "m1"),
    mutate(m6, model = "m6"),
    mutate(m7, model = "m7"),
    mutate(m8, model = "m8")
  )
} else {
  model_preds <- bind_rows(
    mutate(df_obs_summary, model = "obs"),
    mutate(m4, model = "m4"),
    mutate(m6, model = "m6"),
    mutate(m7, model = "m7"),
    mutate(m8, model = "m8"),
    mutate(m9, model = "m9")
  )
}


library(tidyr)


# RT - accuracy

hist(df_obs$rt, breaks = 50, main = ('RT histogram (5%, 95% quantiles)'), xlab = 'RT')
abline(v = quantile(df_obs$rt, probs = c(0.05, 0.95)),
       lwd = 2, lty = 2, col = 'red')
rt_min = quantile(df_obs$rt, probs = 0.05)
rt_max = quantile(df_obs$rt, probs = 0.95)

breaks <- seq(0, 5, length.out = 50)
labels <- head(breaks, -1) + diff(breaks)/2
model_preds_binned <- model_preds %>%
  mutate(rt_bin = as.numeric(as.character(cut(rt, breaks = breaks, labels = labels)))) %>%
  group_by(sub, manipulation, model, rt_bin) %>%
  summarise(mean_cor = mean(cor, na.rm = TRUE), .groups = "drop")

model_preds_binned <- model_preds_binned %>% separate(manipulation, into = c("rt_manipulation", "rtconf_manipulation"), sep = "(?<=Acc|Fast)")

model_preds_binned <- aggregate(cbind(mean_cor) ~ model + rt_bin, data = model_preds_binned, FUN = mean)

table(model_preds_binned$rt_bin)

ggplot(data = model_preds_binned, aes(x = as.numeric(rt_bin), y = mean_cor, color = model)) +
  geom_line(aes(linetype = model), se = F) +
  scale_color_manual(
    values = c("obs" = "black", "m1" = "#1b9e77", "m6" = "#d95f02", "m7" = "#7570b3", "m8" = "#e7298a", "m9" = "#66a61e"),
    labels = c("obs" = "Observed", "m1" = "FCB", "m6" = "CCB", "m7" = "Van Zandt", "m8" = "2DSD", "m9" = "optional stopping 2DSD")) +
  scale_linetype_manual(
    values = c("obs" = "dashed", "m1" = "solid", "m6" = "solid", "m7" = "solid", "m8" = "solid", "m9" = "solid"),
    labels = c("obs" = "Observed", "m1" = "FCB", "m6" = "CCB", "m7" = "Van Zandt", "m8" = "2DSD", "m9" = "optional stopping 2DSD")) +
  xlim(c(rt_min, rt_max)) +
  xlab("Reaction time") +
  ylab("Accuracy (%)") +
  theme_bw()


# RTconf - accuracy

hist(df_obs$rtconf, breaks = 50, main = ('confidence RT histogram (5%, 95% quantiles)'), xlab = 'confidence RT')
abline(v = quantile(df_obs$rtconf, probs = c(0.05, 0.95)),
       lwd = 2, lty = 2, col = 'red')
rt_min = quantile(df_obs$rtconf, probs = 0.05)
rt_max = quantile(df_obs$rtconf, probs = 0.95)

breaks <- seq(0, 5, length.out = 50)
labels <- head(breaks, -1) + diff(breaks)/2
model_preds_binned <- model_preds %>%
  mutate(rtconf_bin = as.numeric(as.character(cut(rtconf, breaks = breaks, labels = labels)))) %>%
  group_by(sub, manipulation, model, rtconf_bin) %>%
  summarise(mean_cor = mean(cor, na.rm = TRUE), .groups = "drop")
model_preds_binned <- model_preds_binned %>% separate(manipulation, into = c("rt_manipulation", "rtconf_manipulation"), sep = "(?<=Acc|Fast)")

# EXP1
p1 <- ggplot(data = model_preds_binned, aes(x = as.numeric(rtconf_bin), y = mean_cor, color = model)) +
  geom_smooth(aes(linetype = model), se = F, size = 1.5) +
  scale_color_manual(
    values = c("obs" = "black", "m1" = "#1b9e77", "m6" = "#d95f02", "m7" = "#7570b3", "m8" = "#e7298a"),
    labels = c("obs" = "Observed", "m1" = "FCB", "m6" = "CCB", "m7" = "Van Zandt", "m8" = "2DSD")) +
  scale_linetype_manual(
    values = c("obs" = "dashed", "m1" = "solid", "m6" = "solid", "m7" = "solid", "m8" = "solid"),
    labels = c("obs" = "Observed", "m1" = "FCB", "m6" = "CCB", "m7" = "Van Zandt", "m8" = "2DSD")) +
  xlim(c(rt_min, rt_max)) +
  xlab("Confidence RT") +
  ylab("Accuracy (%)") +
  theme_bw() +
  theme(legend.position = "none")

# EXP2
p1 <- ggplot(data = model_preds_binned, aes(x = as.numeric(rtconf_bin), y = mean_cor, color = model)) +
  geom_smooth(aes(linetype = model), se = F, size = 1.5) +
  scale_color_manual(
    values = c("obs" = "black", "m4" = "#1b9e77", "m6" = "#d95f02", "m7" = "#7570b3", "m8" = "#e7298a", "m9" = "#66a61e"),
    labels = c("obs" = "Observed", "m4" = expression("FCB"[separate]), "m6" = "CCB", "m7" = "Van Zandt", "m8" = "2DSD", "m9" = "optional stopping 2DSD")) +
  scale_linetype_manual(
    values = c("obs" = "dashed", "m4" = "solid", "m6" = "solid", "m7" = "solid", "m8" = "solid", "m9" = "solid"),
    labels = c("obs" = "Observed", "m4" = expression("FCB"[separate]), "m6" = "CCB", "m7" = "Van Zandt", "m8" = "2DSD", "m9" = "optional stopping 2DSD")) +
  xlim(c(rt_min, rt_max)) +
  xlab("Confidence RT") +
  ylab("Accuracy (%)") +
  theme_bw()  +
  theme(legend.position = "none")

# RTconf - cj

hist(df_obs$rtconf, breaks = 50, main = ('confidence RT histogram (5%, 95% quantiles)'), xlab = 'confidence RT')
abline(v = quantile(df_obs$rtconf, probs = c(0.05, 0.95)),
       lwd = 2, lty = 2, col = 'red')
rt_min = quantile(df_obs$rtconf, probs = 0.05)
rt_max = quantile(df_obs$rtconf, probs = 0.95)

breaks <- seq(0, 5, length.out = 50)
labels <- head(breaks, -1) + diff(breaks)/2
model_preds_binned <- model_preds %>%
  mutate(rtconf_bin = as.numeric(as.character(cut(rtconf, breaks = breaks, labels = labels)))) %>%
  group_by(sub, manipulation, model, rtconf_bin) %>%
  summarise(mean_cj = mean(cj, na.rm = TRUE), .groups = "drop")
model_preds_binned <- model_preds_binned %>% separate(manipulation, into = c("rt_manipulation", "rtconf_manipulation"), sep = "(?<=Acc|Fast)")

#exp1
p2 <- ggplot(data = model_preds_binned, aes(x = as.numeric(rtconf_bin), y = mean_cj, color = model)) +
  geom_smooth(aes(linetype = model), se = F, size = 1.5) +
  scale_color_manual(
    values = c("obs" = "black", "m1" = "#1b9e77", "m6" = "#d95f02", "m7" = "#7570b3", "m8" = "#e7298a"),
    labels = c("obs" = "Observed", "m1" = "FCB", "m6" = "CCB", "m7" = "Van Zandt", "m8" = "2DSD")) +
  scale_linetype_manual(
    values = c("obs" = "dashed", "m1" = "solid", "m6" = "solid", "m7" = "solid", "m8" = "solid"),
    labels = c("obs" = "Observed", "m1" = "FCB", "m6" = "CCB", "m7" = "Van Zandt", "m8" = "2DSD")) +
  xlim(c(rt_min, rt_max)) +
  xlab("Confidence RT") +
  ylab("Mean confidence judgment") +
  theme_bw()   +
  theme(legend.position = "none")
#exp2
p2 <- ggplot(data = model_preds_binned, aes(x = as.numeric(rtconf_bin), y = mean_cj, color = model)) +
  geom_smooth(aes(linetype = model), se = F, size = 1.5) +
  scale_color_manual(
    values = c("obs" = "black", "m4" = "#1b9e77", "m6" = "#d95f02", "m7" = "#7570b3", "m8" = "#e7298a", "m9" = "#66a61e"),
    labels = c("obs" = "Observed", "m4" = expression("FCB"[separate]), "m6" = "CCB", "m7" = "Van Zandt", "m8" = "2DSD", "m9" = "optional stopping 2DSD")) +
  scale_linetype_manual(
    values = c("obs" = "dashed", "m4" = "solid", "m6" = "solid", "m7" = "solid", "m8" = "solid", "m9" = "solid"),
    labels = c("obs" = "Observed", "m4" = expression("FCB"[separate]), "m6" = "CCB", "m7" = "Van Zandt", "m8" = "2DSD", "m9" = "optional stopping 2DSD")) +
  xlim(c(rt_min, rt_max)) +
  xlab("Confidence RT") +
  ylab("Mean confidence judgment") +
  theme_bw()   +
  theme(legend.position = "none")


ggplot(data = model_preds_binned[model_preds_binned$model == 'obs',], aes(x = as.numeric(rtconf_bin), y = mean_cj)) +
  geom_line(aes(group = sub), colour = "black", se = F, size = 0.2, alpha = 0.5) +
  xlab("Confidence RT") +
  ylab("Mean cj") +
  theme_bw() +
  facet_wrap(~ manipulation)


# RTconf - certainty

model_preds <- model_preds %>%
  mutate(certainty = case_when(
    cj %in% c(3, 4) ~ 1,
    cj %in% c(2, 5) ~ 2,
    cj %in% c(1, 6) ~ 3
  ))

hist(df_obs$rtconf, breaks = 50, main = ('confidence RT histogram (5%, 95% quantiles)'), xlab = 'confidence RT')
abline(v = quantile(df_obs$rtconf, probs = c(0.05, 0.95)),
       lwd = 2, lty = 2, col = 'red')
rt_min = quantile(df_obs$rtconf, probs = 0.05)
rt_max = quantile(df_obs$rtconf, probs = 0.95)

breaks <- seq(0, 5, length.out = 50)
labels <- head(breaks, -1) + diff(breaks)/2
model_preds_binned <- model_preds %>%
  mutate(rtconf_bin = as.numeric(as.character(cut(rtconf, breaks = breaks, labels = labels)))) %>%
  group_by(sub, manipulation, model, rtconf_bin) %>%
  summarise(mean_certainty = mean(certainty, na.rm = TRUE), .groups = "drop")

ggplot(data = model_preds_binned, aes(x = as.numeric(rtconf_bin), y = mean_certainty, color = model)) +
  geom_smooth(aes(linetype = model), se = F) +
  scale_color_manual(
    values = c("obs" = "black", "m4" = "#1b9e77", "m6" = "#d95f02", "m7" = "#7570b3", "m8" = "#e7298a", "m9" = "#66a61e"),
    labels = c("obs" = "Observed", "m4" = "FCB", "m6" = "CCB", "m7" = "Van Zandt", "m8" = "2DSD", "m9" = "optional stopping 2DSD")) +
  scale_linetype_manual(
    values = c("obs" = "dashed", "m4" = "solid", "m6" = "solid", "m7" = "solid", "m8" = "solid", "m9" = "solid"),
    labels = c("obs" = "Observed", "m4" = "FCB", "m6" = "CCB", "m7" = "Van Zandt", "m8" = "2DSD", "m9" = "optional stopping 2DSD")) +
  xlim(c(rt_min, rt_max)) +
  xlab("Confidence reaction time") +
  ylab("Average certainty") +
  theme_bw() 


ggplot(data = model_preds_binned[model_preds_binned$model == 'obs',], aes(x = as.numeric(rtconf_bin), y = mean_certainty)) +
  geom_line(aes(group = sub), colour = "black", se = F, size = 0.2, alpha = 0.5) +
  xlab("Confidence reaction time") +
  ylab("Mean certainty") +
  theme_bw() +
  facet_wrap(~ manipulation)



# RTconf - AUC2 (confidence accuracy)

library(pROC)

hist(df_obs$rtconf, breaks = 50, main = ('confidence RT histogram (5%, 95% quantiles)'), xlab = 'confidence RT')
abline(v = quantile(df_obs$rtconf, probs = c(0.05, 0.95)),
       lwd = 2, lty = 2, col = 'red')
rt_min = quantile(df_obs$rtconf, probs = 0.05)
rt_max = quantile(df_obs$rtconf, probs = 0.95)

breaks <- seq(-1, 5, length.out = 20)
labels <- head(breaks, -1) + diff(breaks)/2

model_preds_binned <- model_preds %>%
  mutate(rtconf_bin = as.numeric(as.character(cut(rtconf, breaks = breaks, labels = labels, )))) %>%
  group_by(sub, manipulation, model, rtconf_bin) %>%
  summarise(
    auc2 = {
      # keep only non-NA
      ok <- which(!is.na(cor) & !is.na(cj))
      resp <- cor[ok]
      pred <- cj[ok]
      # need exactly two levels in resp
      if (length(unique(resp)) == 2 && length(resp) > 1) {
        as.numeric(auc(response = resp, predictor = pred))
      } else {
        NA_real_
      }
    },n_trials = n(),
    .groups = "drop"
  )

auc_df <- model_preds %>%
  group_by(manipulation, sub, model) %>%
  summarise(
    auc = as.numeric(roc(response = cor, predictor = cj, quiet = TRUE)$auc),
    .groups = "drop"
  )

#auc_df <- auc_df %>% separate(manipulation, into = c("rt_manipulation", "rtconf_manipulation"), sep = "(?<=Acc|Fast)")
auc_means <- aggregate(auc ~ model, data = auc_df, FUN = mean)

#exp1
p3 <- ggplot(data = auc_df, aes(x = model, y = auc)) +
  geom_jitter(aes(color = model), width = 0.2, alpha = 0.3) +
  geom_crossbar(data = auc_means, aes(y = auc, ymin = auc, ymax = auc, color = model), width = 0.5, size = 1) +
  #facet_wrap(~ rtconf_manipulation) +
  scale_color_manual(
    values = c("obs" = "black", "m1" = "#1b9e77", "m6" = "#d95f02", "m7" = "#7570b3", "m8" = "#e7298a"),
    labels = c("obs" = "Observed", "m1" = "FCB", "m6" = "CCB", "m7" = "Van Zandt", "m8" = "2DSD")) +
  scale_x_discrete(labels = c(
    "obs" = "Observed",
    "m1" = "FCB",
    "m6" = "CCB",
    "m7" = "Van Zandt",
    "m8" = "2DSD"
  )) + 
  xlab('Model type') +
  theme_bw() +
  ylab('AUC2') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(color = "Model type")

#exp2
p3 <- ggplot(data = auc_df, aes(x = model, y = auc)) +
  geom_jitter(aes(color = model), width = 0.2, alpha = 0.3) +
  geom_crossbar(data = auc_means, aes(y = auc, ymin = auc, ymax = auc, color = model), width = 0.5, size = 1) +
  #facet_wrap(~ rtconf_manipulation) +
  scale_color_manual(
    values = c("obs" = "black", "m4" = "#1b9e77", "m6" = "#d95f02", "m7" = "#7570b3", "m8" = "#e7298a", "m9" = "#66a61e"),
    labels = c("obs" = "Observed", "m4" = expression("FCB"[separate]), "m6" = "CCB", "m7" = "Van Zandt", "m8" = "2DSD", "m9" = "optional stopping 2DSD")) +
  theme_bw() +
  scale_x_discrete(labels = c(
    "obs" = "Observed",
    "m4" = expression("FCB"[separate]),
    "m6" = "CCB",
    "m7" = "Van Zandt",
    "m8" = "2DSD",
    "m9" = "Optional Stopping"
  )) +
  xlab('Model type') +
  ylab('AUC2') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(color = "Model type") +
  theme(legend.position = "none")
  


# Exp1
p_tot <- ggarrange(p1, p2, p4, p3, nrow = 1, widths = c(1,1,1,1.5))
ggsave(plot = p_tot, filename = 'exp1_ptot.png', device = png)


# Exp2

means <- aggregate(rtconf ~ cj + model, data = model_preds[model_preds$sub == 1 & model_preds$manipulation == 'FastAcc',], FUN = mean)
p0 <- ggplot(data = means, aes(x = cj, y = rtconf, color = model)) +
  geom_line(size = 1.5, aes(linetype = model)) +
  scale_color_manual(
    values = c("obs" = "black", "m4" = "#1b9e77", "m6" = "#d95f02", "m7" = "#7570b3", "m8" = "#e7298a", "m9" = "#66a61e"),
    labels = c("obs" = "Observed", "m4" = expression("FCB"[separate]), "m6" = "CCB", "m7" = "Van Zandt", "m8" = "2DSD", "m9" = "Optional Stopping")) +
  scale_linetype_manual(
    values = c("obs" = "dashed", "m4" = "solid", "m6" = "solid", "m7" = "solid", "m8" = "solid", "m9" = "solid"),
    labels = c("obs" = "Observed", "m4" = "FCB", "m6" = "CCB", "m7" = "Van Zandt", "m8" = "2DSD", "m9" = "optional Stopping")) +
  geom_jitter(data = model_preds[model_preds$sub == 1 & model_preds$manipulation == 'FastAcc' & model_preds$model == 'obs',], alpha = 0.3, width = 0.3) +
  xlab("Confidence judgment") +
  ylab("Confidence RT") +
  ylim(c(0,2)) +
  theme_bw() +
  guides(linetype = "none") +
  labs(color = "Model type")

p_tot <- ggarrange(
  ggarrange(p0, ncol = 1),                    # First row
  ggarrange(p1, p2, p4, p3, ncol = 4),            # Second row
  nrow = 2,
  heights = c(1, 1)  # Adjust heights if needed
)





ggsave(plot = p_tot, filename = 'exp2_ptot.png', device = png)










# RTconf - cj

hist(df_obs$rtconf, breaks = 50, main = ('confidence RT histogram (5%, 95% quantiles)'), xlab = 'confidence RT')
abline(v = quantile(df_obs$rtconf, probs = c(0.05, 0.95)),
       lwd = 2, lty = 2, col = 'red')
rt_min = quantile(df_obs$rtconf, probs = 0.05)
rt_max = quantile(df_obs$rtconf, probs = 0.95)

breaks <- seq(0, 5, length.out = 50)
labels <- head(breaks, -1) + diff(breaks)/2
model_preds_binned <- model_preds %>%
  mutate(rtconf_bin = as.numeric(as.character(cut(rtconf, breaks = breaks, labels = labels)))) %>%
  group_by(sub, manipulation, model, rtconf_bin) %>%
  summarise(std_cj = sd(cj, na.rm = TRUE), .groups = "drop")
model_preds_binned <- model_preds_binned %>% separate(manipulation, into = c("rt_manipulation", "rtconf_manipulation"), sep = "(?<=Acc|Fast)")

#exp1
p4 <- ggplot(data = model_preds_binned, aes(x = as.numeric(rtconf_bin), y = std_cj, color = model)) +
  geom_smooth(aes(linetype = model), se = F, size = 1.5) +
  scale_color_manual(
    values = c("obs" = "black", "m1" = "#1b9e77", "m6" = "#d95f02", "m7" = "#7570b3", "m8" = "#e7298a"),
    labels = c("obs" = "Observed", "m1" = "FCB", "m6" = "CCB", "m7" = "Van Zandt", "m8" = "2DSD")) +
  scale_linetype_manual(
    values = c("obs" = "dashed", "m1" = "solid", "m6" = "solid", "m7" = "solid", "m8" = "solid"),
    labels = c("obs" = "Observed", "m1" = "FCB", "m6" = "CCB", "m7" = "Van Zandt", "m8" = "2DSD")) +
  xlim(c(rt_min, rt_max)) +
  xlab("Confidence RT") +
  ylab("SD confidence judgment") +
  theme_bw()   +
  theme(legend.position = "none")
#exp2
p4 <- ggplot(data = model_preds_binned, aes(x = as.numeric(rtconf_bin), y = std_cj, color = model)) +
  geom_smooth(aes(linetype = model), se = F, size = 1.5) +
  scale_color_manual(
    values = c("obs" = "black", "m4" = "#1b9e77", "m6" = "#d95f02", "m7" = "#7570b3", "m8" = "#e7298a", "m9" = "#66a61e"),
    labels = c("obs" = "Observed", "m4" = expression("FCB"[separate]), "m6" = "CCB", "m7" = "Van Zandt", "m8" = "2DSD", "m9" = "optional stopping 2DSD")) +
  scale_linetype_manual(
    values = c("obs" = "dashed", "m4" = "solid", "m6" = "solid", "m7" = "solid", "m8" = "solid", "m9" = "solid"),
    labels = c("obs" = "Observed", "m4" = expression("FCB"[separate]), "m6" = "CCB", "m7" = "Van Zandt", "m8" = "2DSD", "m9" = "optional stopping 2DSD")) +
  xlim(c(rt_min, rt_max)) +
  xlab("Confidence RT") +
  ylab("SD confidence judgment") +
  theme_bw()   +
  theme(legend.position = "none")











# Experiment 1 ------------------------------





library(dplyr)

EXPERIMENT <- 1

df_obs <- read.csv(paste0('Data/Experiment_', EXPERIMENT, '/exp', EXPERIMENT, '_data_viable.csv'))
df_obs_summary <- df_obs[c('coherence', 'rt', 'cor', 'rtconf', 'cj', 'manipulation', 'sub')]

m1 <- read.csv('Data/Experiment_1/Model_predictions/exp1_model1_predictions.csv')
#m2 <- read.csv('Data/Experiment_2/Model_predictions/exp2_model2_predictions.csv')
#m3 <- read.csv('Data/Experiment_2/Model_predictions/exp2_model3_predictions.csv')
m4 <- read.csv('Data/Experiment_1/Model_predictions/exp1_model4_predictions.csv')
m6 <- read.csv('Data/Experiment_1/Model_predictions/exp1_model6_predictions.csv')
m7 <- read.csv('Data/Experiment_1/Model_predictions/exp1_model7_predictions.csv')
m8 <- read.csv('Data/Experiment_1/Model_predictions/exp1_model8_predictions.csv')

model_preds <- bind_rows(
  mutate(df_obs_summary, model = "obs"),
  mutate(m1, model = "m1"),
  mutate(m6, model = "m6"),
  mutate(m7, model = "m7"),
  mutate(m8, model = "m8")
)


# RT - accuracy

hist(df_obs$rt, breaks = 50, main = ('RT histogram (5%, 95% quantiles)'), xlab = 'RT')
abline(v = quantile(df_obs$rt, probs = c(0.05, 0.95)),
       lwd = 2, lty = 2, col = 'red')
rt_min = quantile(df_obs$rt, probs = 0.05)
rt_max = quantile(df_obs$rt, probs = 0.95)

breaks <- seq(0, 5, length.out = 50)
labels <- head(breaks, -1) + diff(breaks)/2
model_preds_binned <- model_preds %>%
  mutate(rt_bin = as.numeric(as.character(cut(rt, breaks = breaks, labels = labels)))) %>%
  group_by(sub, manipulation, model, rt_bin) %>%
  summarise(mean_cor = mean(cor, na.rm = TRUE), .groups = "drop")

ggplot(data = model_preds_binned, aes(x = as.numeric(rt_bin), y = mean_cor, color = model)) +
  geom_smooth(aes(linetype = model), se = F) +
  scale_color_manual(
    values = c("obs" = "black", "m1" = "#1b9e77", "m6" = "#d95f02", "m7" = "#7570b3", "m8" = "#e7298a"),
    labels = c("obs" = "Observed", "m1" = "FCB", "m6" = "CCB", "m7" = "Van Zandt", "m8" = "2DSD")) +
  scale_linetype_manual(
    values = c("obs" = "dashed", "m1" = "solid", "m6" = "solid", "m7" = "solid", "m8" = "solid"),
    labels = c("obs" = "Observed", "m1" = "FCB", "m6" = "CCB", "m7" = "Van Zandt", "m8" = "2DSD")) +
  xlim(c(rt_min, rt_max)) +
  xlab("Reaction time") +
  ylab("Accuracy (%)") +
  theme_bw() 



# RTconf - accuracy

hist(df_obs$rtconf, breaks = 50, main = ('confidence RT histogram (5%, 95% quantiles)'), xlab = 'confidence RT')
abline(v = quantile(df_obs$rtconf, probs = c(0.05, 0.95)),
       lwd = 2, lty = 2, col = 'red')
rt_min = quantile(df_obs$rtconf, probs = 0.05)
rt_max = quantile(df_obs$rtconf, probs = 0.95)

breaks <- seq(0, 5, length.out = 50)
labels <- head(breaks, -1) + diff(breaks)/2
model_preds_binned <- model_preds %>%
  mutate(rtconf_bin = as.numeric(as.character(cut(rtconf, breaks = breaks, labels = labels)))) %>%
  group_by(sub, manipulation, model, rtconf_bin) %>%
  summarise(mean_cor = mean(cor, na.rm = TRUE), .groups = "drop")

ggplot(data = model_preds_binned, aes(x = as.numeric(rtconf_bin), y = mean_cor, color = model)) +
  geom_smooth(aes(linetype = model), se = F) +
  scale_color_manual(
    values = c("obs" = "black", "m1" = "#1b9e77", "m6" = "#d95f02", "m7" = "#7570b3", "m8" = "#e7298a"),
    labels = c("obs" = "Observed", "m1" = "FCB", "m6" = "CCB", "m7" = "Van Zandt", "m8" = "2DSD")) +
  scale_linetype_manual(
    values = c("obs" = "dashed", "m1" = "solid", "m6" = "solid", "m7" = "solid", "m8" = "solid"),
    labels = c("obs" = "Observed", "m1" = "FCB", "m6" = "CCB", "m7" = "Van Zandt", "m8" = "2DSD")) +
  xlim(c(rt_min, rt_max)) +
  xlab("Confidence reaction time") +
  ylab("Accuracy (%)") +
  theme_bw()


# RTconf - cj

hist(df_obs$rtconf, breaks = 50, main = ('confidence RT histogram (5%, 95% quantiles)'), xlab = 'confidence RT')
abline(v = quantile(df_obs$rtconf, probs = c(0.05, 0.95)),
       lwd = 2, lty = 2, col = 'red')
rt_min = quantile(df_obs$rtconf, probs = 0.05)
rt_max = quantile(df_obs$rtconf, probs = 0.95)

breaks <- seq(0, 5, length.out = 50)
labels <- head(breaks, -1) + diff(breaks)/2
model_preds_binned <- model_preds %>%
  mutate(rtconf_bin = as.numeric(as.character(cut(rtconf, breaks = breaks, labels = labels)))) %>%
  group_by(sub, manipulation, model, rtconf_bin) %>%
  summarise(mean_cj = mean(cj, na.rm = TRUE), .groups = "drop")

ggplot(data = model_preds_binned, aes(x = as.numeric(rtconf_bin), y = mean_cj, color = model)) +
  geom_smooth(aes(linetype = model), se = F) +
  scale_color_manual(
    values = c("obs" = "black", "m1" = "#1b9e77", "m6" = "#d95f02", "m7" = "#7570b3", "m8" = "#e7298a"),
    labels = c("obs" = "Observed", "m1" = "FCB", "m6" = "CCB", "m7" = "Van Zandt", "m8" = "2DSD")) +
  scale_linetype_manual(
    values = c("obs" = "dashed", "m1" = "solid", "m6" = "solid", "m7" = "solid", "m8" = "solid"),
    labels = c("obs" = "Observed", "m1" = "FCB", "m6" = "CCB", "m7" = "Van Zandt", "m8" = "2DSD")) +
  xlim(c(rt_min, rt_max)) +
  xlab("Confidence reaction time") +
  ylab("Average confidence judgment") +
  theme_bw()

ggplot(data = model_preds_binned[model_preds_binned$model == 'obs',], aes(x = as.numeric(rtconf_bin), y = mean_cj)) +
  geom_line(aes(group = sub), colour = "black", se = F, size = 0.2, alpha = 0.5) +
  xlab("Confidence reaction time") +
  ylab("Mean cj") +
  theme_bw() +
  facet_wrap(~ manipulation)


# RTconf - certainty

model_preds <- model_preds %>%
  mutate(certainty = case_when(
    cj %in% c(3, 4) ~ 1,
    cj %in% c(2, 5) ~ 2,
    cj %in% c(1, 6) ~ 3
  ))

hist(df_obs$rtconf, breaks = 50, main = ('confidence RT histogram (5%, 95% quantiles)'), xlab = 'confidence RT')
abline(v = quantile(df_obs$rtconf, probs = c(0.05, 0.95)),
       lwd = 2, lty = 2, col = 'red')
rt_min = quantile(df_obs$rtconf, probs = 0.05)
rt_max = quantile(df_obs$rtconf, probs = 0.95)

breaks <- seq(0, 5, length.out = 50)
labels <- head(breaks, -1) + diff(breaks)/2
model_preds_binned <- model_preds %>%
  mutate(rtconf_bin = as.numeric(as.character(cut(rtconf, breaks = breaks, labels = labels)))) %>%
  group_by(sub, manipulation, model, rtconf_bin) %>%
  summarise(mean_certainty = mean(certainty, na.rm = TRUE), .groups = "drop")

ggplot(data = model_preds_binned, aes(x = as.numeric(rtconf_bin), y = mean_certainty, color = model)) +
  geom_smooth(aes(linetype = model), se = F) +
  scale_color_manual(
    values = c("obs" = "black", "m1" = "#1b9e77", "m6" = "#d95f02", "m7" = "#7570b3", "m8" = "#e7298a"),
    labels = c("obs" = "Observed", "m1" = "FCB", "m6" = "CCB", "m7" = "Van Zandt", "m8" = "2DSD")) +
  scale_linetype_manual(
    values = c("obs" = "dashed", "m1" = "solid", "m6" = "solid", "m7" = "solid", "m8" = "solid"),
    labels = c("obs" = "Observed", "m1" = "FCB", "m6" = "CCB", "m7" = "Van Zandt", "m8" = "2DSD")) +
  xlim(c(rt_min, rt_max)) +
  xlab("Confidence reaction time") +
  ylab("Average certainty") +
  theme_bw() +
  facet_wrap(~ manipulation)


ggplot(data = model_preds_binned[model_preds_binned$model == 'obs',], aes(x = as.numeric(rtconf_bin), y = mean_certainty)) +
  geom_line(aes(group = sub), colour = "black", se = F, size = 0.2, alpha = 0.5) +
  xlab("Confidence reaction time") +
  ylab("Mean certainty") +
  theme_bw() +
  facet_wrap(~ manipulation)



# RTconf - AUC2 (confidence accuracy)

library(pROC)

hist(df_obs$rtconf, breaks = 50, main = ('confidence RT histogram (5%, 95% quantiles)'), xlab = 'confidence RT')
abline(v = quantile(df_obs$rtconf, probs = c(0.05, 0.95)),
       lwd = 2, lty = 2, col = 'red')
rt_min = quantile(df_obs$rtconf, probs = 0.05)
rt_max = quantile(df_obs$rtconf, probs = 0.95)

breaks <- seq(-1, 5, length.out = 20)
labels <- head(breaks, -1) + diff(breaks)/2

model_preds_binned <- model_preds %>%
  mutate(rtconf_bin = as.numeric(as.character(cut(rtconf, breaks = breaks, labels = labels, )))) %>%
  group_by(sub, manipulation, model, rtconf_bin) %>%
  summarise(
    auc2 = {
      # keep only non-NA
      ok <- which(!is.na(cor) & !is.na(cj))
      resp <- cor[ok]
      pred <- cj[ok]
      # need exactly two levels in resp
      if (length(unique(resp)) == 2 && length(resp) > 1) {
        as.numeric(auc(response = resp, predictor = pred))
      } else {
        NA_real_
      }
    },
    .groups = "drop"
  )



ggplot(data = model_preds_binned, aes(x = as.numeric(rtconf_bin), y = auc2, color = model)) +
  geom_smooth(aes(linetype = model), se = F) +
  scale_color_manual(
    values = c("obs" = "black", "m1" = "#1b9e77", "m6" = "#d95f02", "m7" = "#7570b3", "m8" = "#e7298a"),
    labels = c("obs" = "Observed", "m1" = "FCB", "m6" = "CCB", "m7" = "Van Zandt", "m8" = "2DSD")) +
  scale_linetype_manual(
    values = c("obs" = "dashed", "m1" = "solid", "m6" = "solid", "m7" = "solid", "m8" = "solid"),
    labels = c("obs" = "Observed", "m1" = "FCB", "m6" = "CCB", "m7" = "Van Zandt", "m8" = "2DSD")) +
  xlab("Confidence reaction time") +
  ylab("AUC2") +
  facet_wrap(~ manipulation) +
  theme_bw() 

# 2DSD AUC2 goes up over time, even within a single participant
ggplot(data = model_preds_binned[model_preds_binned$model == 'm8',], aes(x = as.numeric(rtconf_bin), y = auc2)) +
  geom_line(aes(group = sub), colour = "#e7298a", se = F, size = 0.2, alpha = 0.5) +
  xlab("Confidence reaction time") +
  ylab("AUC2") +
  theme_bw() +
  facet_wrap(~ manipulation)
hist(model_preds$rtconf[model_preds$model == "m8"], xlim = c(0,5), breaks = 100)

ggplot(data = model_preds_binned[model_preds_binned$model == 'm7',], aes(x = as.numeric(rtconf_bin), y = auc2)) +
  geom_line(aes(group = sub), colour = "#7570b3", se = F, size = 0.2, alpha = 0.5) +
  xlab("Confidence reaction time") +
  ylab("AUC2") +
  theme_bw() +
  facet_wrap(~ manipulation)
hist(model_preds$rtconf[model_preds$model == "m7"], xlim = c(0,5), breaks = 1000)

ggplot(data = model_preds_binned[model_preds_binned$model == 'm4',], aes(x = as.numeric(rtconf_bin), y = auc2)) +
  geom_line(aes(group = sub), colour = "#1b9e77", se = F, size = 0.2, alpha = 0.5) +
  xlab("Confidence reaction time") +
  ylab("AUC2") +
  theme_bw() +
  facet_wrap(~ manipulation)

ggplot(data = model_preds_binned[model_preds_binned$model == 'obs',], aes(x = as.numeric(rtconf_bin), y = auc2)) +
  geom_line(aes(group = sub), colour = "black", se = F, size = 0.2, alpha = 0.5) +
  xlab("Confidence reaction time") +
  ylab("AUC2") +
  theme_bw() +
  facet_wrap(~ manipulation)





















# What when fitting multiple things at once?

df <- read.csv('data/Experiment_2/Model_parameters/exp2_model4_parameters.csv')

manipulation <- 'FastFast'

mean(df$ter[df$manipulation == manipulation])
mean(df$ter2[df$manipulation == manipulation])
mean(df$starting_point_confidence[df$manipulation == manipulation])
mean(df$a[df$manipulation == manipulation])
mean(df$a2[df$manipulation == manipulation])
mean(df$a2_slope_lower[df$manipulation == manipulation])
mean(df$a2_slope_upper[df$manipulation == manipulation])









##############################
### Qualitative Signatures ###
##############################


library(dplyr)
library(tidyr)
library(ggpubr)
library(pROC)


### -- EXPERIMENT 1 -- ###


# Load data

df_obs <- read.csv(paste0('Data/Experiment_1/exp1_data_viable.csv'))
df_obs_summary <- df_obs[c('coherence', 'rt', 'cor', 'rtconf', 'cj', 'manipulation', 'sub')]

m1 <- read.csv(paste0('Data/Experiment_1/Model_predictions/exp1_model1_predictions.csv'))
m6 <- read.csv(paste0('Data/Experiment_1/Model_predictions/exp1_model6_predictions.csv'))
m7 <- read.csv(paste0('Data/Experiment_1/Model_predictions/exp1_model7_predictions.csv'))
m8 <- read.csv(paste0('Data/Experiment_1/Model_predictions/exp1_model8_predictions.csv'))

model_preds <- bind_rows(
  mutate(df_obs_summary, model = "obs"),
  mutate(m1, model = "m1"),
  mutate(m6, model = "m6"),
  mutate(m7, model = "m7"),
  mutate(m8, model = "m8")
)

#  Reviewers final: m6 predictions based on averaged parameter values

source('Code/model_functions.R')

predictions <- NULL
file_name_params <- paste0('Data/Experiment_1/Model_parameters/exp1_model6_parameters.csv')
params <- read.csv(file = file_name_params, stringsAsFactors = FALSE, check.names = FALSE)
params_avg <- as.data.frame(t(colMeans(df[sapply(params, is.numeric)], na.rm = TRUE)))
m6_predictions_avg <- make_predictions(model_type = 6, experiment = 1, params = params_avg, n_trials = 50000)

# Plot margins

hist(df_obs$rt, breaks = 50, main = ('RT histogram (5%, 95% quantiles)'), xlab = 'RT')
abline(v = quantile(df_obs$rt, probs = c(0.05, 0.95)),
       lwd = 2, lty = 2, col = 'red')
rt_min = quantile(df_obs$rt, probs = 0.05)
rt_max = quantile(df_obs$rt, probs = 0.95)

hist(df_obs$rtconf, breaks = 50, main = ('RTconf histogram (5%, 95% quantiles)'), xlab = 'RT')
abline(v = quantile(df_obs$rtconf, probs = c(0.05, 0.95)),
       lwd = 2, lty = 2, col = 'red')
rtconf_min = quantile(df_obs$rtconf, probs = 0.05)
rtconf_max = quantile(df_obs$rtconf, probs = 0.95)

# Plot colors

colors_exp1 <- c("obs" = "black", "m1" = "#1b9e77", "m6_avg" = "#d95f02", "m7" = "#7570b3", "m8" = "#e7298a")
labels_exp1 <- c("obs" = "Observed", "m1" = expression("FCB"[simple]), "m6_avg" = "CCB", "m7" = "Van Zandt", "m8" = "2DSD")
linetypes_exp1 <- c("obs" = "dashed", "m1" = "solid", "m6_avg" = "solid", "m7" = "solid", "m8" = "solid")
colors_exp1_auc <- c("obs" = "black", "m1" = "#1b9e77", "m6" = "#d95f02", "m7" = "#7570b3", "m8" = "#e7298a")
labels_exp1_auc <- c("obs" = "Observed", "m1" = expression("FCB"[simple]), "m6" = "CCB", "m7" = "Van Zandt", "m8" = "2DSD")
linetypes_exp1_auc <- c("obs" = "dashed", "m1" = "solid", "m6" = "solid", "m7" = "solid", "m8" = "solid")

# Choice RT binning data

breaks <- seq(0, 5, length.out = 25)
labels <- head(breaks, -1) + diff(breaks)/2

binned_preds_RT <- model_preds %>%
  mutate(rt_bin = as.numeric(as.character(cut(rt, breaks = breaks, labels = labels)))) %>%
  group_by(sub, manipulation, model, rt_bin) %>%
  summarise(mean_cor = mean(cor, na.rm = TRUE), .groups = "drop")
binned_preds_RT_avg_m6 <- m6_predictions_avg %>%
  mutate(rt_bin = as.numeric(as.character(cut(rt, breaks = breaks, labels = labels)))) %>%
  group_by(rt_bin) %>%
  summarise(mean_cor = mean(cor, na.rm = TRUE), .groups = "drop")
binned_preds_RT_avg_m6$model <- 'm6_avg'
binned_preds_RT <- aggregate(cbind(mean_cor) ~ model + rt_bin, data = binned_preds_RT, FUN = mean)
binned_preds_RT <- rbind(binned_preds_RT, binned_preds_RT_avg_m6)

# Confidence RT binning data

breaks <- seq(0, 5, length.out = 25)
labels <- head(breaks, -1) + diff(breaks)/2

binned_preds_RTconf <- model_preds %>%
  mutate(rtconf_bin = as.numeric(as.character(cut(rtconf, breaks = breaks, labels = labels)))) %>%
  group_by(sub, manipulation, model, rtconf_bin) %>%
  summarise(mean_cor = mean(cor, na.rm = TRUE),
            mean_cj = mean(cj, na.rm = TRUE),
            SD_cj = sd(cj, na.rm = TRUE),
            .groups = "drop")
binned_preds_RTconf_avg_m6 <- m6_predictions_avg %>%
  mutate(rtconf_bin = as.numeric(as.character(cut(rtconf, breaks = breaks, labels = labels)))) %>%
  group_by(rtconf_bin) %>%
  summarise(mean_cor = mean(cor, na.rm = TRUE),
            mean_cj = mean(cj, na.rm = TRUE),
            SD_cj = sd(cj, na.rm = TRUE),
            .groups = "drop")
binned_preds_RTconf_avg_m6$model <- 'm6_avg'
binned_preds_RTconf <- aggregate(cbind(mean_cor, mean_cj, SD_cj) ~ model + rtconf_bin, data = binned_preds_RTconf, FUN = mean)
binned_preds_RTconf <- rbind(binned_preds_RTconf, binned_preds_RTconf_avg_m6)

# ROC

auc_individual <- model_preds %>%
  group_by(manipulation, sub, model) %>%
  summarise(
    auc = as.numeric(roc(response = cor, predictor = cj, quiet = TRUE)$auc),
    .groups = "drop"
  )
auc_mean <- aggregate(auc ~ model, data = auc_individual, FUN = mean)

# Plots 

# AUC2
(p_auc2 <- ggplot(data = auc_individual, aes(x = model, y = auc)) +
    geom_jitter(aes(color = model), width = 0.2, alpha = 0.25, type = 19, show.legend = FALSE) +
    geom_crossbar(data = auc_mean, aes(y = auc, ymin = auc, ymax = auc, color = model), width = 0.5, size = 1) +
    scale_color_manual(
      values = colors_exp1_auc,
      labels = labels_exp1_auc) +
    scale_x_discrete(labels = labels_exp1_auc) + 
    xlab('Model type') +
    theme_classic() +
    ylab('Type II AUC') +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(color = "Model type")) 

# RT ~ accuracy
(p_RT_acc <- ggplot(data = binned_preds_RT[binned_preds_RT$model != 'm6',], aes(x = as.numeric(rt_bin), y = mean_cor, color = model)) +
    geom_line(size = 1) +
    scale_color_manual(
      values = colors_exp1,
      labels = labels_exp1) +
    xlim(c(rt_min, rt_max)) +
    ylim(c(0.55, 0.8)) +
    xlab("Choice RT") +
    ylab("Accuracy (%)") +
    labs(color = "Model type:", linetype = 'Model type') +
    theme_classic())

# RTconf ~ accuracy
(p_RTconf_acc <- ggplot(data = binned_preds_RTconf[binned_preds_RTconf$model != 'm6',], aes(x = as.numeric(rtconf_bin), y = mean_cor, color = model)) +
    geom_line(size = 1) +
    scale_color_manual(
      values = colors_exp1,
      labels = labels_exp1) +
    xlim(c(rtconf_min, rtconf_max)) +
    ylim(c(0.5, 0.8)) +
    xlab("Confidence RT") +
    ylab("Accuracy (%)") +
    labs(color = "Model type", linetype = 'Model type') +
    theme_classic())

# RTconf ~ CJ
(p_RTconf_cj <- ggplot(data = binned_preds_RTconf[binned_preds_RTconf$model != 'm6',], aes(x = as.numeric(rtconf_bin), y = mean_cj, color = model)) +
    geom_line(size = 1) +
    scale_color_manual(
      values = colors_exp1,
      labels = labels_exp1) +
    xlim(c(rtconf_min, rtconf_max)) +
    ylim(c(0, 1)) +
    xlab("Confidence RT") +
    ylab("Confidence (M)") +
    labs(color = "Model type", linetype = 'Model type') +
    theme_classic()) 




# extra!!!
breaks <- seq(0, 5, length.out = 25)
labels <- head(breaks, -1) + diff(breaks)/2

binned_preds_RTconf <- model_preds %>%
  mutate(rtconf_bin = as.numeric(as.character(cut(rtconf, breaks = breaks, labels = labels)))) %>%
  group_by(sub, manipulation, model, rtconf_bin) %>%
  summarise(mean_cor = mean(cor, na.rm = TRUE),
            mean_cj = mean(cj, na.rm = TRUE),
            SD_cj = sd(cj, na.rm = TRUE),
            n = n(),
            .groups = "drop")
binned_preds_RTconf_averaged_manipulation <- aggregate(cbind(mean_cor, mean_cj, SD_cj) ~ rtconf_bin + model + sub, data = binned_preds_RTconf, FUN = mean)
binned_preds_RTconf_averaged <- aggregate(cbind(mean_cor, mean_cj, SD_cj) ~ rtconf_bin + model, data = binned_preds_RTconf, FUN = mean)

groups <- interaction(binned_preds_RTconf[binned_preds_RTconf$model=='m6',]$sub, binned_preds_RTconf[binned_preds_RTconf$model=='m6',]$manipulation)
plot(group = groups, y = binned_preds_RTconf[binned_preds_RTconf$model=='m6',]$n, x = jitter(binned_preds_RTconf[binned_preds_RTconf$model=='m6',]$rtconf_bin), xlim = c(rtconf_min, rtconf_max))


(p_RTconf_cj <- ggplot(data = binned_preds_RTconf_averaged[binned_preds_RTconf_averaged$model=='m6',], aes(x = as.numeric(rtconf_bin), y = mean_cj)) +
    geom_line(size = 1) +
    ylim(c(0.25, 1)) +
    xlim(c(rtconf_min, rtconf_max)) +
    xlab("Confidence RT") +
    ylab("Confidence (M)") +
    labs(color = "Model type", linetype = 'Model type') +
    theme_classic()) 

(p_RTconf_cj <- ggplot(data = binned_preds_RTconf_averaged_manipulation[binned_preds_RTconf_averaged_manipulation$model=='m6',], aes(x = as.numeric(rtconf_bin), y = mean_cj, group = sub)) +
    geom_line(size = 0.1) +
    ylim(c(0.25, 1)) +
    xlab("Confidence RT") +
    ylab("Confidence (M)") +
    labs(color = "Model type", linetype = 'Model type') +
    xlim(c(rtconf_min, rtconf_max)) +
    theme_classic()) 


hist(binned_preds_RTconf_averaged_manipulation[binned_preds_RTconf_averaged_manipulation$model=='m6',]$rtconf_bin, 
     xlim = c(rtconf_min, rtconf_max), breaks = (50))

hist(model_preds[model_preds$model == 'm6']$rt)
# end extra!!!



# RTconf ~ sd(CJ)
(p_RTconf_sdcj <- ggplot(data = binned_preds_RTconf, aes(x = as.numeric(rtconf_bin), y = SD_cj, color = model)) +
    geom_line(size = 1) +
    scale_color_manual(
      values = colors_exp1,
      labels = labels_exp1) +
    xlim(c(rtconf_min, rtconf_max)) +
    ylim(c(0, 0.6)) +
    xlab("Confidence RT") +
    ylab("Confidence (SD)") +
    labs(color = "Model type", linetype = 'Model type') +
    theme_classic()) 

# Combine plots
p_auc2_noLegend <- p_auc2 + theme(legend.position = "none")
(combined_plots <- ggarrange(ggarrange(p_RTconf_acc, p_RTconf_cj, p_RTconf_sdcj, 
          common.legend = TRUE, legend = "bottom", ncol = 3), p_auc2_noLegend,
          ncol = 2, widths = c(3,1)))
ggsave(plot = combined_plots, filename = 'exp1_qualitative_reviewers.jpeg')



### -- EXPERIMENT 2 -- ###


# Load data

df_obs <- read.csv(paste0('Data/Experiment_2/exp2_data_viable.csv'))
df_obs_summary <- df_obs[c('coherence', 'rt', 'cor', 'rtconf', 'cj', 'manipulation', 'sub')]

m4 <- read.csv(paste0('Data/Experiment_2/Model_predictions/exp2_model4_predictions.csv'))
m6 <- read.csv(paste0('Data/Experiment_2/Model_predictions/exp2_model6_predictions.csv'))
m7 <- read.csv(paste0('Data/Experiment_2/Model_predictions/exp2_model7_predictions.csv'))
m8 <- read.csv(paste0('Data/Experiment_2/Model_predictions/exp2_model8_predictions.csv'))
m9 <- read.csv(paste0('Data/Experiment_2/Model_predictions/exp2_model9_predictions.csv'))

model_preds <- bind_rows(
  mutate(df_obs_summary, model = "obs"),
  mutate(m4, model = "m4"),
  mutate(m6, model = "m6"),
  mutate(m7, model = "m7"),
  mutate(m8, model = "m8"),
  mutate(m9, model = "m9")
)

# Plot margins

hist(df_obs$rt, breaks = 50, main = ('RT histogram (5%, 95% quantiles)'), xlab = 'RT')
abline(v = quantile(df_obs$rt, probs = c(0.05, 0.95)),
       lwd = 2, lty = 2, col = 'red')
rt_min = quantile(df_obs$rt, probs = 0.05)
rt_max = quantile(df_obs$rt, probs = 0.95)

hist(df_obs$rtconf, breaks = 50, main = ('RTconf histogram (5%, 95% quantiles)'), xlab = 'RT')
abline(v = quantile(df_obs$rtconf, probs = c(0.05, 0.95)),
       lwd = 2, lty = 2, col = 'red')
rtconf_min = quantile(df_obs$rtconf, probs = 0.05)
rtconf_max = quantile(df_obs$rtconf, probs = 0.95)

# Plot colors

colors_exp1 <- c("obs" = "black", "m4" = "#1b9e77", "m6" = "#d95f02", "m7" = "#7570b3", "m8" = "#e7298a", "m9" = "#66a61e")
labels_exp1 <- c("obs" = "Observed", "m4" = expression("FCB"[full]), "m6" = "CCB", "m7" = "Van Zandt", "m8" = "2DSD", "m9" = "Optional Stopping")
linetypes_exp1 <- c("obs" = "dashed", "m4" = "solid", "m6" = "solid", "m7" = "solid", "m8" = "solid", "m9" = "solid")

# Choice RT binning data

breaks <- seq(0, 5, length.out = 25)
labels <- head(breaks, -1) + diff(breaks)/2

binned_preds_RT <- model_preds %>%
  mutate(rt_bin = as.numeric(as.character(cut(rt, breaks = breaks, labels = labels)))) %>%
  group_by(sub, manipulation, model, rt_bin) %>%
  summarise(mean_cor = mean(cor, na.rm = TRUE), .groups = "drop")
binned_preds_RT <- aggregate(cbind(mean_cor) ~ model + rt_bin, data = binned_preds_RT, FUN = mean)

# Confidence RT binning data

breaks <- seq(0, 5, length.out = 25)
labels <- head(breaks, -1) + diff(breaks)/2

binned_preds_RTconf <- model_preds %>%
  mutate(rtconf_bin = as.numeric(as.character(cut(rtconf, breaks = breaks, labels = labels)))) %>%
  group_by(sub, manipulation, model, rtconf_bin) %>%
  summarise(mean_cor = mean(cor, na.rm = TRUE),
            mean_cj = mean(cj, na.rm = TRUE),
            SD_cj = sd(cj, na.rm = TRUE),
            .groups = "drop")
binned_preds_RTconf <- aggregate(cbind(mean_cor, mean_cj, SD_cj) ~ model + rtconf_bin, data = binned_preds_RTconf, FUN = mean)

# ROC

auc_individual <- model_preds %>%
  group_by(manipulation, sub, model) %>%
  summarise(
    auc = as.numeric(roc(response = cor, predictor = cj, quiet = TRUE)$auc),
    .groups = "drop"
  )
auc_mean <- aggregate(auc ~ model, data = auc_individual, FUN = mean)

# Plots 

# AUC2
(p_auc2 <- ggplot(data = auc_individual, aes(x = model, y = auc)) +
    geom_jitter(aes(color = model), width = 0.2, alpha = 0.25, show.legend = FALSE) +
    geom_crossbar(data = auc_mean, aes(y = auc, ymin = auc, ymax = auc, color = model), width = 0.5, size = 1) +
    scale_color_manual(
      values = colors_exp1,
      labels = labels_exp1) +
    scale_x_discrete(labels = labels_exp1) + 
    xlab('Model type') +
    theme_classic() +
    ylab('Type II AUC') +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(color = "Model type")) 

# RT ~ accuracy
(p_RT_acc <- ggplot(data = binned_preds_RT, aes(x = as.numeric(rt_bin), y = mean_cor, color = model)) +
    geom_line(size = 1) +
    scale_color_manual(
      values = colors_exp1,
      labels = labels_exp1) +
    xlim(c(rt_min, rt_max)) +
    ylim(c(0.55, 0.85)) +
    xlab("Choice RT") +
    ylab("Accuracy (%)") +
    labs(color = "Model type:", linetype = 'Model type') +
    theme_classic())

# RTconf ~ accuracy
(p_RTconf_acc <- ggplot(data = binned_preds_RTconf, aes(x = as.numeric(rtconf_bin), y = mean_cor, color = model)) +
    geom_line(size = 1) +
    scale_color_manual(
      values = colors_exp1,
      labels = labels_exp1) +
    xlim(c(rtconf_min, rtconf_max)) +
    ylim(c(0.55, 0.8)) +
    xlab("Confidence RT") +
    ylab("Accuracy (%)") +
    labs(color = "Model type", linetype = 'Model type') +
    theme_classic())

# RTconf ~ CJ
(p_RTconf_cj <- ggplot(data = binned_preds_RTconf, aes(x = as.numeric(rtconf_bin), y = mean_cj, color = model)) +
    geom_line(size = 1) +
    scale_color_manual(
      values = colors_exp1,
      labels = labels_exp1) +
    xlim(c(rtconf_min, rtconf_max)) +
    ylim(c(2.5, 6)) +
    xlab("Confidence RT") +
    ylab("Confidence (M)") +
    labs(color = "Model type", linetype = 'Model type') +
    theme_classic()) 

# RTconf ~ sd(CJ)
(p_RTconf_sdcj <- ggplot(data = binned_preds_RTconf, aes(x = as.numeric(rtconf_bin), y = SD_cj, color = model)) +
    geom_line(size = 1) +
    scale_color_manual(
      values = colors_exp1,
      labels = labels_exp1) +
    xlim(c(rtconf_min, rtconf_max)) +
    #ylim(c(0, 0.6)) +
    xlab("Confidence RT") +
    ylab("Confidence (SD)") +
    labs(color = "Model type", linetype = 'Model type') +
    theme_classic()) 

# Combine plots
p_auc2_noLegend <- p_auc2 + theme(legend.position = "none")
(combined_plots <- ggarrange(ggarrange(p_RTconf_acc, p_RTconf_cj, p_RTconf_sdcj, 
                                                        common.legend = TRUE, legend = "bottom", ncol = 3), p_auc2_noLegend ,
                             ncol = 2, widths = c(3,1)))
ggsave(plot = combined_plots, filename = 'exp2_qualitative.jpeg')





means <- aggregate(rtconf ~ cj + model, data = model_preds[model_preds$sub == 1 & model_preds$manipulation == 'FastAcc',], FUN = mean)
p0 <- ggplot(data = means, aes(x = cj, y = rtconf, color = model)) +
  geom_line(size = 1.5, aes(linetype = model)) +
  scale_color_manual(
    values = c("obs" = "black", "m4" = "#1b9e77", "m6" = "#d95f02", "m7" = "#7570b3", "m8" = "#e7298a", "m9" = "#66a61e"),
    labels = c("obs" = "Participant 1", "m4" = expression("FCB"[full]), "m6" = "CCB", "m7" = "Van Zandt", "m8" = "2DSD", "m9" = "Optional Stopping")) +
  scale_linetype_manual(
    values = c("obs" = "dashed", "m4" = "solid", "m6" = "solid", "m7" = "solid", "m8" = "solid", "m9" = "solid"),
    labels = c("obs" = "Participant 1", "m4" = expression("FCB"[full]), "m6" = "CCB", "m7" = "Van Zandt", "m8" = "2DSD", "m9" = "optional Stopping")) +
  geom_jitter(data = model_preds[model_preds$sub == 1 & model_preds$manipulation == 'FastAcc' & model_preds$model == 'obs',], alpha = 0.3, width = 0.3) +
  scale_x_continuous(breaks = 1:6) +
  xlab("Confidence judgement") +
  ylab("Confidence RT") +
  ylim(c(0,2)) +
  theme_classic() +
  guides(linetype = "none") +
  labs(color = "Model type")


p_tot <- ggarrange(
  ggarrange(p0, ncol = 1),                    # First row
  (combined_plots <- ggarrange(ggarrange(p_RTconf_acc, p_RTconf_cj, p_RTconf_sdcj, 
                                         common.legend = TRUE, legend = "bottom", ncol = 3), p_auc2_noLegend ,
                               ncol = 2, widths = c(3,1))),            # Second row
  nrow = 2,
  heights = c(1, 1)  # Adjust heights if needed
)
ggsave(plot = p_tot, filename = 'exp2_qualitative.jpeg')

