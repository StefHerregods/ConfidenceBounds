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

#  Reviewers final: predictions based on averaged parameter values

source('Code/model_functions.R')

predictions <- NULL
file_name_params_m1 <- paste0('Data/Experiment_1/Model_parameters/exp1_model1_parameters.csv')
file_name_params_m6 <- paste0('Data/Experiment_1/Model_parameters/exp1_model6_parameters.csv')
file_name_params_m7 <- paste0('Data/Experiment_1/Model_parameters/exp1_model7_parameters.csv')
file_name_params_m8 <- paste0('Data/Experiment_1/Model_parameters/exp1_model8_parameters.csv')
params_m1 <- read.csv(file = file_name_params_m1, stringsAsFactors = FALSE, check.names = FALSE)
params_m6 <- read.csv(file = file_name_params_m6, stringsAsFactors = FALSE, check.names = FALSE)
params_m7 <- read.csv(file = file_name_params_m7, stringsAsFactors = FALSE, check.names = FALSE)
params_m8 <- read.csv(file = file_name_params_m8, stringsAsFactors = FALSE, check.names = FALSE)

params_avg_m1 <- as.data.frame(t(colMeans(params_m1[sapply(params_m1, is.numeric)], na.rm = TRUE)))
params_avg_m6 <- as.data.frame(t(colMeans(params_m6[sapply(params_m6, is.numeric)], na.rm = TRUE)))
params_avg_m7 <- as.data.frame(t(colMeans(params_m7[sapply(params_m7, is.numeric)], na.rm = TRUE)))
params_avg_m8 <- as.data.frame(t(colMeans(params_m8[sapply(params_m8, is.numeric)], na.rm = TRUE)))

m1_predictions_avg <- make_predictions(model_type = 1, experiment = 1, params = params_avg_m1, n_trials = 50000)
m6_predictions_avg <- make_predictions(model_type = 6, experiment = 1, params = params_avg_m6, n_trials = 50000)
m7_predictions_avg <- make_predictions(model_type = 7, experiment = 1, params = params_avg_m7, n_trials = 50000)
m8_predictions_avg <- make_predictions(model_type = 8, experiment = 1, params = params_avg_m8, n_trials = 50000)

model_preds <- bind_rows(
  mutate(df_obs_summary, model = "obs"),
  mutate(m1_predictions_avg, model = "m1"),
  mutate(m6_predictions_avg, model = "m6"),
  mutate(m7_predictions_avg, model = "m7"),
  mutate(m8_predictions_avg, model = "m8")
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

colors_exp1 <- c("obs" = "black", "m1" = "#1b9e77", "m6" = "#d95f02", "m7" = "#7570b3", "m8" = "#e7298a")
labels_exp1 <- c("obs" = "Observed", "m1" = expression("FCB"[simple]), "m6" = "CCB", "m7" = "Van Zandt", "m8" = "2DSD")
linetypes_exp1 <- c("obs" = "dashed", "m1" = "solid", "m6" = "solid", "m7" = "solid", "m8" = "solid")
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
(p_RT_acc <- ggplot(data = binned_preds_RT, aes(x = as.numeric(rt_bin), y = mean_cor, color = model)) +
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
(p_RTconf_acc <- ggplot(data = binned_preds_RTconf, aes(x = as.numeric(rtconf_bin), y = mean_cor, color = model)) +
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
(p_RTconf_cj <- ggplot(data = binned_preds_RTconf, aes(x = as.numeric(rtconf_bin), y = mean_cj, color = model)) +
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
ggsave(plot = combined_plots, filename = 'exp1_qualitative_averagedParams_reviewers.jpeg')



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


#  Reviewers final: predictions based on averaged parameter values

source('Code/model_functions.R')

predictions <- NULL
file_name_params_m4 <- paste0('Data/Experiment_2/Model_parameters/exp2_model4_parameters.csv')
file_name_params_m6 <- paste0('Data/Experiment_2/Model_parameters/exp2_model6_parameters.csv')
file_name_params_m7 <- paste0('Data/Experiment_2/Model_parameters/exp2_model7_parameters.csv')
file_name_params_m8 <- paste0('Data/Experiment_2/Model_parameters/exp2_model8_parameters.csv')
file_name_params_m9 <- paste0('Data/Experiment_2/Model_parameters/exp2_model9_parameters.csv')
params_m4 <- read.csv(file = file_name_params_m4, stringsAsFactors = FALSE, check.names = FALSE)
params_m6 <- read.csv(file = file_name_params_m6, stringsAsFactors = FALSE, check.names = FALSE)
params_m7 <- read.csv(file = file_name_params_m7, stringsAsFactors = FALSE, check.names = FALSE)
params_m8 <- read.csv(file = file_name_params_m8, stringsAsFactors = FALSE, check.names = FALSE)
params_m9 <- read.csv(file = file_name_params_m9, stringsAsFactors = FALSE, check.names = FALSE)

params_avg_m4 <- as.data.frame(t(colMeans(params_m4[sapply(params_m4, is.numeric)], na.rm = TRUE)))
params_avg_m6 <- as.data.frame(t(colMeans(params_m6[sapply(params_m6, is.numeric)], na.rm = TRUE)))
params_avg_m7 <- as.data.frame(t(colMeans(params_m7[sapply(params_m7, is.numeric)], na.rm = TRUE)))
params_avg_m8 <- as.data.frame(t(colMeans(params_m8[sapply(params_m8, is.numeric)], na.rm = TRUE)))
params_avg_m9 <- as.data.frame(t(colMeans(params_m9[sapply(params_m9, is.numeric)], na.rm = TRUE)))

m4_predictions_avg <- make_predictions(model_type = 4, experiment = 2, params = params_avg_m4, n_trials = 50000)
m6_predictions_avg <- make_predictions(model_type = 6, experiment = 2, params = params_avg_m6, n_trials = 50000)
m7_predictions_avg <- make_predictions(model_type = 7, experiment = 2, params = params_avg_m7, n_trials = 50000)
m8_predictions_avg <- make_predictions(model_type = 8, experiment = 2, params = params_avg_m8, n_trials = 50000)
m9_predictions_avg <- make_predictions(model_type = 9, experiment = 2, params = params_avg_m9, n_trials = 50000)

model_preds <- bind_rows(
  mutate(df_obs_summary, model = "obs"),
  mutate(m4_predictions_avg, model = "m4"),
  mutate(m6_predictions_avg, model = "m6"),
  mutate(m7_predictions_avg, model = "m7"),
  mutate(m8_predictions_avg, model = "m8"),
  mutate(m9_predictions_avg, model = "m9")
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
ggsave(plot = combined_plots, filename = 'exp2_qualitative_reviewer_averaged.jpeg')





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