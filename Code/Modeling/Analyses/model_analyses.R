

# Set-up -----------------------------------------------------------------------


rm(list = ls())

library(ggplot2)
library(ggpubr)
library(ggiraph)
library(rstatix)
library(dplyr)
source('Code/model_functions.R')
library(extrafont)
#font_import()
loadfonts(device = "win")

EXPERIMENT <- 1

figure_font <- 'Helvetica'

theme_set(
  theme_bw(base_size = 9) +
  theme(
    axis.title = element_text(size = 10, family = figure_font, face = "bold"),       
    axis.text = element_text(size = 9, family = figure_font, face = "bold"),   
    legend.title = element_text(size = 9, family = figure_font, face = "bold"),    
    legend.text = element_text(size = 9, family = figure_font, face = "bold"),
    legend.key.size = unit(0.4, "cm"),
    legend.spacing = unit(0.1, "cm"),
    legend.background = element_blank(),
    plot.title = element_text(size = 10.5, hjust = 0.5),
    panel.border = element_blank(),
    axis.line = element_line())
)

# Load data --------------------------------------------------------------------


df_observed <- read.csv(paste0('Data/Experiment_', EXPERIMENT, '/exp', EXPERIMENT, '_data_viable.csv'))
df_predicted_1 <- read.csv(paste0('Data/Experiment_', EXPERIMENT,'/Model_predictions/exp', EXPERIMENT, '_model1_predictions.csv'))
df_predicted_2 <- read.csv(paste0('Data/Experiment_', EXPERIMENT,'/Model_predictions/exp', EXPERIMENT, '_model2_predictions.csv'))
df_predicted_3 <- read.csv(paste0('Data/Experiment_', EXPERIMENT,'/Model_predictions/exp', EXPERIMENT, '_model3_predictions.csv'))
df_predicted_4 <- read.csv(paste0('Data/Experiment_', EXPERIMENT,'/Model_predictions/exp', EXPERIMENT, '_model4_predictions.csv'))
df_predicted_5 <- read.csv(paste0('Data/Experiment_', EXPERIMENT,'/Model_predictions/exp', EXPERIMENT, '_model5_predictions.csv'))
df_predicted_6 <- read.csv(paste0('Data/Experiment_', EXPERIMENT,'/Model_predictions/exp', EXPERIMENT, '_model6_predictions.csv'))
df_predicted_7 <- read.csv(paste0('Data/Experiment_', EXPERIMENT,'/Model_predictions/exp', EXPERIMENT, '_model7_predictions.csv'))
df_predicted_8 <- read.csv(paste0('Data/Experiment_', EXPERIMENT,'/Model_predictions/exp', EXPERIMENT, '_model8_predictions.csv'))
df_predicted_9 <- read.csv(paste0('Data/Experiment_', EXPERIMENT,'/Model_predictions/exp', EXPERIMENT, '_model9_predictions.csv'))

color_list <- c("1" = "#045275", "5" = "#7CCBA2", "6" = "#F0746E", "7" = "#7C1D6F")




p_rtconf_1 <- ggplot(data = df_observed[df_observed$cor == 1,], aes(x = rtconf)) +
  geom_histogram(aes(y = ..count.. / sum(..count..), fill = factor(cj, levels = c(1, 2))), binwidth = 0.1, alpha = 0.5, position = "identity") +
  geom_freqpoly(data = df_predicted_4[df_predicted_4$cor == 1,], aes(x = rtconf, y = ..count.. / sum(..count..), group = factor(cj, levels = c(1, 2)), color = factor(cj, levels = c(1, 2))), binwidth = 0.1, size = 1.5) + 
  scale_fill_manual(values = c("#fdb338", "#025196")) +  
  scale_color_manual(values = c("#fdb338", "#025196")) +  
  labs(x = "Confidence RT (s)", y = "Proportion", fill = "Observed", color = "Predicted", title = "Correct decision") + 
  xlim(c(-0.2, 3)) +
  theme(legend.position = c(0.8, 0.75)) 

ggplot(data = df_observed) +
  geom_histogram(aes(x = cj, y=..count../sum(..count..)), bins = 6) +
  geom_freqpoly(data = df_predicted_4, aes(x = cj, y=..count../sum(..count..)), bins = 6)



df_plot <- df_observed %>%
  group_by(sub, manipulation, cj, cor) %>%
  summarise(count = n(), .groups = 'drop') %>%
  group_by(sub, manipulation) %>%
  mutate(proportion = count / sum(count))

df_means <- df_plot %>%
  group_by(cj, cor) %>%
  summarise(mean_proportion = mean(proportion), .groups = 'drop')


df_plot_pred <- df_predicted_9 %>%
  group_by(sub, manipulation, cj, cor) %>%
  summarise(count = n(), .groups = 'drop') %>%
  group_by(sub, manipulation) %>%
  mutate(proportion = count / sum(count))
df_means_pred <- df_plot_pred %>%
  group_by(cj, cor) %>%
  summarise(mean_proportion = mean(proportion), .groups = 'drop')

# Create the jitter plot with means
a <- ggplot(df_plot) +
  geom_point(
    aes(x = as.factor(cj), y = proportion, color = as.factor(cor)),
    position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.5), 
    size = 3,
    alpha = 0.03
  ) +
  geom_point(
    data = df_means, 
    aes(x = as.factor(cj), y = mean_proportion, color = as.factor(cor)),
    position = position_dodge(width = 0.5),  
    size = 12,
    alpha = 1,
    shape = '-',
  ) +
  geom_point(
    data = df_means_pred, 
    aes(x = as.factor(cj), y = mean_proportion, group = as.factor(cor)),
    position = position_dodge(width = 0.5),  
    size = 3,
    alpha = 1,
    shape = 4,
    stroke = 1,
    fill = 'black'
  ) +
  ylim(c(0.0,0.3)) +
  scale_color_manual(values = c("0" = "#D81600", "1" = "#8FD694")) +  
  labs(x = "Confidence Judgment (cj)", y = "Proportion", color = "Correct (cor)") +
  theme(legend.position="none")






# For df_observations
means_df_observed <- df_observed %>%
  group_by(cj, cor, sub, manipulation) %>%
  summarize(mean_rtconf = mean(rtconf, na.rm = TRUE), .groups = 'drop')

# For df_predicted_4
means_df_predicted_9 <- df_predicted_9 %>%
  group_by(cj, cor, sub, manipulation) %>%
  summarize(mean_rtconf = mean(rtconf, na.rm = TRUE), .groups = 'drop')


# For df_observations
means_df_observed_cj_cor <- df_observed %>%
  group_by(cj, cor) %>%
  summarize(mean_rtconf = mean(rtconf, na.rm = TRUE), .groups = 'drop')

# For df_predicted_4
means_df_predicted_9_cj_cor <- df_predicted_9 %>%
  group_by(cj, cor) %>%
  summarize(mean_rtconf = mean(rtconf, na.rm = TRUE), .groups = 'drop')

b <- ggplot(means_df_observed) +
  geom_point(
    aes(x = as.factor(cj), y = mean_rtconf, color = as.factor(cor)),
    position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.5), 
    size = 3,
    alpha = 0.03
  ) +
  # Add means as solid points
  geom_point(
    data = means_df_observed_cj_cor, 
    aes(x = as.factor(cj), y = mean_rtconf, color = as.factor(cor)),
    position = position_dodge(width = 0.5),  # Only dodge without jitter for mean
    size = 12,
    alpha = 1,
    shape = '-',
  ) +
  geom_point(
    data = means_df_predicted_9_cj_cor, 
    aes(x = as.factor(cj), y = mean_rtconf, group = as.factor(cor)),
    position = position_dodge(width = 0.5),  # Only dodge without jitter for mean
    size = 3,
    alpha = 1,
    shape = 4,
    stroke = 1,
    fill = 'black'
  ) +
  ylim(c(0.0,2)) +
  scale_color_manual(values = c("0" = "#D81600", "1" = "#8FD694")) +  # Color mapping
  labs(x = "Confidence Judgment (cj)", y = "Confidence RT", color = "Correct (cor)") +
  theme(legend.position="none")







ggarrange(a, b)









#### !!!!!!!!!!test


subject = 20
manipulation = 'AccAcc'

plot <- ggplot(data = df_observed[df_observed$sub==subject & df_observed$manipulation==manipulation,], aes(x = cj, y = rtconf)) +
  geom_jitter(size = 2, alpha = 0.1, stroke = NA, width = 0.25) +
  stat_summary(data = df_predicted_4[df_predicted_4$sub==subject,], fun='mean', geom='line', size=2, colour=color_list[1]) +
  stat_summary(data = df_predicted_8[df_predicted_8$sub==subject,], fun='mean', geom='line', size=2, colour=color_list[2]) +
  stat_summary(data = df_predicted_6[df_predicted_6$sub==subject,], fun='mean', geom='line', size=2, colour=color_list[3]) +
  stat_summary(data = df_predicted_9[df_predicted_9$sub==subject,], fun='mean', geom='line', size=2, colour=color_list[4]) +
  stat_summary(fun='mean', geom='point', size=4, colour='black') +
  ylim(c(0,2)) +
  xlab('confidence rating') +
  ylab('confidence RT') +
  scale_x_continuous(breaks = unique(df_observed$cj)) 


ggsave(plot, )


# Separate model fit -----------------------------------------------------------

df_observed <- df_observed %>% mutate(rt_manipulation = ifelse(df_observed$manipulation %in% c("AccAcc", "AccFast"), 1, 0),
                            rtconf_manipulation = ifelse(df_observed$manipulation %in% c("AccAcc", "FastAcc"), 1, 0))

df_predicted_9 <- df_predicted_9 %>% mutate(rt_manipulation = ifelse(df_predicted_9$manipulation %in% c("AccAcc", "AccFast"), 1, 0),
                                            rtconf_manipulation = ifelse(df_predicted_9$manipulation %in% c("AccAcc", "FastAcc"), 1, 0))



df_obs_mean <- df_observed %>%
  group_by(sub, coherence, manipulation) %>% 
  summarise_each(funs(mean)) %>%
  group_by(coherence, manipulation) %>%
  mutate(rt_mean = mean(rt),
         rt_sd = sd(rt),
         rtconf_mean = mean(rtconf),
         rtconf_sd = sd(rtconf),
         accuracy_mean = mean(cor),
         accuracy_sd = sd(cor),
         cj_mean = mean(cj),
         cj_sd = sd(cj))


df_predictions_mean <- df_predicted_9 %>%
  mutate(cj = as.integer(cj)) %>%
  group_by(sub, coherence, manipulation) %>% 
  summarise_each(funs(mean)) %>%
  group_by(coherence, manipulation) %>%
  mutate(rt_mean = mean(rt),
         rt_sd = sd(rt),
         rtconf_mean = mean(rtconf),
         rtconf_sd = sd(rtconf),
         accuracy_mean = mean(cor),
         accuracy_sd = sd(cor),
         cj_mean = mean(cj),
         cj_sd = sd(cj))

manipulations <- c('0' = '"Make fast decisions"',
                   '1' = '"Make accurate\ndecisions"')


rt <- ggplot(data = df_obs_mean, aes(x = coherence, y = rt, color = as.factor(rtconf_manipulation))) +
  geom_point(size = 2.5, stroke = 1, shape = 16, alpha = 0.1, position = position_jitterdodge(jitter.width = 0.04, dodge.width = 0.07)) +
  stat_summary(geom = "point", size = 2.5, fun = "mean", position = position_dodge(width = 0.07)) +
  stat_summary(geom = "line", size = 1, fun = "mean", position = position_dodge(width = 0.07)) +
  geom_errorbar(aes(ymin = rt_mean - rt_sd / sqrt(40), ymax = rt_mean + rt_sd / sqrt(40), group = as.factor(rtconf_manipulation)), position = position_dodge(width = 0.07), size = 1, width = 0) +
  stat_summary(data = df_predictions_mean, aes(x = coherence, y = rt, group = as.factor(rtconf_manipulation)), color = 'black', geom = "point", size = 1.5, stroke = 1, shape = 4, fun = "mean", position = position_dodge(width = 0.07)) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1), breaks = c(0.1, 0.2, 0.4)) +
  scale_color_manual(values = c('#CA3C25', '#FFA630')) +
  ylab(label = 'Reaction Time (s)') +
  xlab(label = 'Coherence') +
  theme(axis.line = element_line(colour = 'black'),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = '#e0e0e0', size = 0.7),
        panel.grid.minor.x = element_blank(),
        panel.border = element_blank(),
        axis.ticks.length = unit(.2, 'cm'),
        panel.background = element_blank(),
        text = element_text(family = 'font', size = 12),
        axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 5, r = 0, b = 0, l = 0)),
        legend.position = 'none',
        strip.background = element_blank(),
        strip.text.x = element_text(size = 11),
        plot.margin=unit(c(.5,0.2,.5,0.2),"cm")) +
  facet_wrap(~rt_manipulation, labeller = as_labeller(manipulations))


rtconf <- ggplot(data = df_obs_mean, aes(x = coherence, y = rtconf, color = as.factor(rtconf_manipulation))) +
  geom_point(size = 2.5, stroke = 1, shape = 16, alpha = 0.1, position = position_jitterdodge(jitter.width = 0.04, dodge.width = 0.07)) +
  stat_summary(geom = "point", size = 2.5, fun = "mean", position = position_dodge(width = 0.07)) +
  stat_summary(geom = "line", size = 1, fun = "mean", position = position_dodge(width = 0.07)) +
  geom_errorbar(aes(ymin = rtconf_mean - rtconf_sd / sqrt(40), ymax = rtconf_mean + rtconf_sd / sqrt(40), group = as.factor(rtconf_manipulation)), position = position_dodge(width = 0.07), size = 1, width = 0) +
  stat_summary(data = df_predictions_mean, aes(x = coherence, y = rtconf, group = as.factor(rtconf_manipulation)), color = 'black', geom = "point", size = 1.5, stroke = 1, shape = 4, fun = "mean", position = position_dodge(width = 0.07)) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1), breaks = c(0.1, 0.2, 0.4)) +
  scale_color_manual(values = c('#CA3C25', '#FFA630')) +
  ylab(label = 'Confidence Reaction Time (s)') +
  xlab(label = 'Coherence') +
  theme(axis.line = element_line(colour = 'black'),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = '#e0e0e0', size = 0.7),
        panel.grid.minor.x = element_blank(),
        panel.border = element_blank(),
        axis.ticks.length = unit(.2, 'cm'),
        panel.background = element_blank(),
        text = element_text(family = 'font', size = 12),
        axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 5, r = 0, b = 0, l = 0)),
        legend.position = 'none',
        strip.background = element_blank(),
        strip.text.x = element_text(size = 11),
        plot.margin=unit(c(.5,0.2,.5,0.2),"cm")) +
  facet_wrap(~rt_manipulation, labeller = as_labeller(manipulations))

cor <- ggplot(data = df_obs_mean, aes(x = coherence, y = cor, color = as.factor(rtconf_manipulation))) +
  geom_point(size = 2.5, stroke = 1, shape = 16, alpha = 0.1, position = position_jitterdodge(jitter.width = 0.04, dodge.width = 0.07)) +
  stat_summary(geom = "point", size = 2.5, fun = "mean", position = position_dodge(width = 0.07)) +
  stat_summary(geom = "line", size = 1, fun = "mean", position = position_dodge(width = 0.07)) +
  geom_errorbar(aes(ymin = accuracy_mean - accuracy_sd / sqrt(40), ymax = accuracy_mean + accuracy_sd / sqrt(40), group = as.factor(rtconf_manipulation)), position = position_dodge(width = 0.07), size = 1, width = 0) +
  stat_summary(data = df_predictions_mean, aes(x = coherence, y = cor, group = as.factor(rtconf_manipulation)), color = 'black', geom = "point", size = 1.5, stroke = 1, shape = 4, fun = "mean", position = position_dodge(width = 0.07)) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1), breaks = c(0.1, 0.2, 0.4)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_color_manual(values = c('#CA3C25', '#FFA630')) +
  ylab(label = 'Accuracy') +
  xlab(label = 'Coherence') +
  theme(axis.line = element_line(colour = 'black'),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = '#e0e0e0', size = 0.7),
        panel.grid.minor.x = element_blank(),
        panel.border = element_blank(),
        axis.ticks.length = unit(.2, 'cm'),
        panel.background = element_blank(),
        text = element_text(family = 'font', size = 12),
        axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 5, r = 0, b = 0, l = 0)),
        legend.position = 'none',
        strip.background = element_blank(),
        strip.text.x = element_text(size = 11),
        plot.margin=unit(c(.5,0.2,.5,0.2),"cm")) +
  facet_wrap(~rt_manipulation, labeller = as_labeller(manipulations))

cj <- ggplot(data = df_obs_mean, aes(x = coherence, y = cj, color = as.factor(rtconf_manipulation))) +
  geom_point(size = 2.5, stroke = 1, shape = 16, alpha = 0.1, position = position_jitterdodge(jitter.width = 0.04, dodge.width = 0.07)) +
  stat_summary(geom = "point", size = 2.5, fun = "mean", position = position_dodge(width = 0.07)) +
  stat_summary(geom = "line", size = 1, fun = "mean", position = position_dodge(width = 0.07)) +
  geom_errorbar(aes(ymin = cj_mean - cj_sd / sqrt(40), ymax = cj_mean + cj_sd / sqrt(40), group = as.factor(rtconf_manipulation)), position = position_dodge(width = 0.07), size = 1, width = 0) +
  stat_summary(data = df_predictions_mean, aes(x = coherence, y = cj, group = as.factor(rtconf_manipulation)), color = 'black', geom = "point", size = 1.5, stroke = 1, shape = 4, fun = "mean", position = position_dodge(width = 0.07)) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1), breaks = c(0.1, 0.2, 0.4)) +
  scale_color_manual(values = c('#CA3C25', '#FFA630')) +
  ylab(label = 'Mean Confidence Judgement') +
  xlab(label = 'Coherence') +
  theme(axis.line = element_line(colour = 'black'),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = '#e0e0e0', size = 0.7),
        panel.grid.minor.x = element_blank(),
        panel.border = element_blank(),
        axis.ticks.length = unit(.2, 'cm'),
        panel.background = element_blank(),
        text = element_text(family = 'font', size = 12),
        axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 5, r = 0, b = 0, l = 0)),
        legend.position = 'none',
        strip.background = element_blank(),
        strip.text.x = element_text(size = 11),
        plot.margin=unit(c(.5,0.2,.5,0.2),"cm")) +
  facet_wrap(~rt_manipulation, labeller = as_labeller(manipulations))

ggarrange(rt, cor, rtconf, cj)

# Model fit confidence proportions ---------------------------------------------


SUB <- 1
MANIPULATION <- 'AccAcc'

p_rtconf <- if(EXPERIMENT == 1){

  
  
} else {
  
  mean_1 <- df_predicted_4 %>%
    filter(sub == SUB, manipulation == MANIPULATION) %>%  
    group_by(cj) %>%
    summarize(mean_rtconf = mean(rtconf, na.rm = TRUE), .groups = 'drop')
  mean_1$cj <- mean_1$cj + 1
  mean_5 <- df_predicted_5 %>%
    filter(sub == SUB, manipulation == MANIPULATION) %>%  
    group_by(cj) %>%
    summarize(mean_rtconf = mean(rtconf, na.rm = TRUE), .groups = 'drop')
  mean_5$cj <- mean_5$cj + 1
  mean_6 <- df_predicted_6 %>%
    filter(sub == SUB, manipulation == MANIPULATION) %>%  
    group_by(cj) %>%
    summarize(mean_rtconf = mean(rtconf, na.rm = TRUE), .groups = 'drop')
  mean_6$cj <- mean_6$cj + 1
  mean_7 <- df_predicted_7 %>%
    filter(sub == SUB, manipulation == MANIPULATION) %>%  
    group_by(cj) %>%
    summarize(mean_rtconf = mean(rtconf, na.rm = TRUE), .groups = 'drop')
  mean_7$cj <- mean_7$cj + 1
  
  # Plot with mean lines
  ggplot(data = df_observed[df_observed$sub == SUB & df_observed$manipulation == MANIPULATION,], aes(x = as.factor(cj), y = rtconf)) +
    geom_jitter(width = 0.3, alpha = 0.5, size = 2, shape = 16) +
    geom_line(data = mean_1, aes(x = cj, y = mean_rtconf), color = color_list[1], size = 2, linetype = "dashed") +
    geom_line(data = mean_5, aes(x = cj, y = mean_rtconf), color = color_list[2], size = 2, linetype = "dashed") +
    geom_line(data = mean_6, aes(x = cj, y = mean_rtconf), color = color_list[3], size = 2, linetype = "dashed") +
    geom_line(data = mean_7, aes(x = cj, y = mean_rtconf), color = color_list[4], size = 2, linetype = "dashed") +
    theme_bw()
}
            

df_observed <- read.csv(paste0('Data/Experiment_', exp, '/exp', exp, '_data_viable.csv'))
df_observed$model_type <- 'Observed'
df_predicted_1 <- read.csv(paste0('Data/Experiment_', exp,'/Model_predictions/exp', exp, '_model', model_1, '_predictions.csv'))
df_predicted_1$model_type <- model_1
df_predicted_2 <- read.csv(paste0('Data/Experiment_', exp,'/Model_predictions/exp', exp, '_model', model_2, '_predictions.csv'))
df_predicted_2$model_type <- model_2

df <- rbind(subset(df_observed, select = c('cj', 'model_type', 'sub')), subset(df_predicted_1, select = c('cj', 'model_type', 'sub')), subset(df_predicted_2, select = c('cj', 'model_type', 'sub')))
ggplot(data = df_observed, aes(x = cj)) +
  geom_bar()

library(dplyr)
df_proportions <- df[df$sub == 20,] %>%
  group_by(model_type, cj) %>%
  summarise(count = n()) %>%
  mutate(proportion = count / sum(count))

# Create the ggplot
ggplot(df_proportions, aes(x = cj, y = proportion, fill = model_type)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Proportion of cj by Model Type",
       x = "CJ",
       y = "Proportion") +
  theme_minimal()


# Model BIC --------------------------------------------------------------------

exp <- 2
df_observed <- read.csv(paste0('Data/Experiment_', exp, '/exp', exp, '_data_viable.csv'))

EXPERIMENT <- exp
bic_results <- NULL
#for (model in c(1,2,3,4,8,6,7)){
for (model in c(1,4,6,7,8,9)){

  df_predicted <- read.csv(paste0('Data/Experiment_', EXPERIMENT,'/Model_predictions/exp', EXPERIMENT, '_model', model, '_predictions.csv'))
  
  print(paste('Computing BIC - model', model))
  
  for (sub in 1:40){
    for (manipulation in c('AccAcc', 'AccFast', 'FastAcc', 'FastFast')){
      
      df_observed_subset <- df_observed[df_observed$sub == sub & df_observed$manipulation == manipulation,]
      df_predicted_subset <- df_predicted[df_predicted$sub == sub & df_predicted$manipulation == manipulation,]

      results_temp <- as.data.frame(t(unlist(compute_BIC(model_type = model, experiment = EXPERIMENT, df_observed = df_observed_subset, df_predicted = df_predicted_subset))))
      
      results_temp$model_type <- model
      results_temp$experiment <- EXPERIMENT
      results_temp$sub <- sub
      results_temp$manipulation <- manipulation
      
      bic_results <- rbind(bic_results, results_temp)

    }
  }
}
names(bic_results) <- c('BIC', 'chi2', 'n_parameters', 'n_trials', 'rt_chi2', 'rtconf_chi2', 'confidence_chi2', 'rt_chi2_outliers', 'rtconf_chi2_outliers', 'confidence_chi2_outliers', 'model_type', 'experiment', 'sub', 'manipulation')

# BIC summary
# take logs of mse's
bic_results$rt_chi2_log <- log(bic_results$rt_chi2)
bic_results$rtconf_chi2_log <- log(bic_results$rtconf_chi2)
bic_results$confidence_chi2_log <- log(bic_results$confidence_chi2)
aggregate(cbind(BIC, chi2, rt_chi2, rtconf_chi2, confidence_chi2, n_parameters, n_trials) ~ model_type + experiment, data = bic_results, FUN = mean)
aggregate(cbind(BIC, rt_chi2_log, rtconf_chi2_log, confidence_chi2_log) ~ model_type + experiment, data = bic_results, FUN = mean)

# BIC summary without empty cells
aggregate(cbind(BIC, chi2, rt_chi2, rtconf_chi2, confidence_chi2, n_parameters, n_trials) ~ model_type + experiment, data = bic_results[bic_results$rt_chi2_outliers == 0 & bic_results$rtconf_chi2_outliers == 0 & bic_results$confidence_chi2_outliers == 0,], FUN = mean)


# BIC figures ------------------------------------------------------------------

model_list <- if(EXPERIMENT == 1){
  c(1,5,6,7)
} else {
  c(4,5,6,7,9)
}

p_BIC <- ggplot(data = bic_results[bic_results$experiment == EXPERIMENT & bic_results$model_type %in% model_list,], aes(x = as.factor(model_type), color = as.factor(model_type), y = BIC)) +
  geom_jitter(aes(shape = rt_chi2_outliers > 0 | rtconf_chi2_outliers > 0 | confidence_chi2_outliers > 0), width = 0.3, alpha = 1, size = 2) + 
  stat_summary(fun = mean, geom = "errorbar", aes(ymin = ..y.., ymax = ..y..), width = 0.5, color = "black", size = 1) +
  labs(x = "Model type") +
  scale_shape_manual(values = c(16, 1)) +
  scale_color_manual(values = color_list) +
  theme(legend.position = "none")

p_rt_chi2 <- ggplot(data = bic_results[bic_results$experiment == EXPERIMENT & bic_results$model_type %in% model_list,], aes(x = as.factor(model_type), color = as.factor(model_type), y = log(rt_chi2))) +
  geom_jitter(aes(shape = rt_chi2_outliers > 0), width = 0.3, alpha = 1, size = 2) + 
  stat_summary(fun = mean, geom = "errorbar", aes(ymin = ..y.., ymax = ..y..), width = 0.5, color = "black", size = 1) +
  labs(x = "Model type", y = expression(log(RT ~ chi^2))) +
  scale_shape_manual(values = c(16, 1)) +
  scale_color_manual(values = color_list) +
  theme(legend.position = "none")

p_rtconf_chi2 <- ggplot(data = bic_results[bic_results$experiment == EXPERIMENT & bic_results$model_type %in% model_list,], aes(x = as.factor(model_type), color = as.factor(model_type), y = log(rtconf_chi2))) +
  geom_jitter(aes(shape = rtconf_chi2_outliers > 0), width = 0.3, alpha = 1, size = 2) + 
  stat_summary(fun = mean, geom = "errorbar", aes(ymin = ..y.., ymax = ..y..), width = 0.5, color = "black", size = 1) +
  labs(x = "Model type", y = expression(log(confidence ~ RT ~ chi^2))) +
  scale_shape_manual(values = c(16, 1)) +
  scale_color_manual(values = color_list) +
  theme(legend.position = "none")

p_confidence_chi2 <- ggplot(data = bic_results[bic_results$experiment == EXPERIMENT & bic_results$model_type %in% model_list,], aes(x = as.factor(model_type), color = as.factor(model_type), y = log(confidence_chi2))) +
  geom_jitter(aes(shape = confidence_chi2_outliers > 0), width = 0.3, alpha = 1, size = 2) + 
  stat_summary(fun = mean, geom = "errorbar", aes(ymin = ..y.., ymax = ..y..), width = 0.5, color = "black", size = 1) +
  labs(x = "Model type", y = expression(log(confidence ~ proportions ~ chi^2))) +
  scale_shape_manual(values = c(16, 1)) +
  scale_color_manual(values = color_list) +
  theme(legend.position = "none")


# cRT histograms ---------------------------------------------------------------


p_rtconf_0 <- ggplot(data = df_observed[df_observed$cor == 0,], aes(x = rtconf)) +
  geom_histogram(aes(y = ..count.. / sum(..count..), fill = factor(cj, levels = c(0, 1))), binwidth = 0.1, alpha = 0.5, position = "identity") +
  geom_freqpoly(data = df_predicted_1[df_predicted_1$cor == 0,], aes(x = rtconf, y = ..count.. / sum(..count..), group = factor(cj, levels = c(0, 1)), color = factor(cj, levels = c(0, 1))), binwidth = 0.1, size = 1.5) + 
  scale_fill_manual(values = c("#fdb338", "#025196"), labels = c('Low', 'High')) +  
  scale_color_manual(values = c("#fdb338", "#025196"), labels = c('Low', 'High')) +  
  labs(x = "Confidence RT (s)", y = "Proportion", fill = "Observed CJ", color = "Predicted CJ", title = "Error decision") + 
  xlim(c(-0.2, 3)) +
  theme(legend.position = c(0.8, 0.75)) 

p_rtconf_1 <- ggplot(data = df_observed[df_observed$cor == 1,], aes(x = rtconf)) +
  geom_histogram(aes(y = ..count.. / sum(..count..), fill = factor(cj, levels = c(0, 1))), binwidth = 0.1, alpha = 0.5, position = "identity") +
  geom_freqpoly(data = df_predicted_1[df_predicted_1$cor == 1,], aes(x = rtconf, y = ..count.. / sum(..count..), group = factor(cj, levels = c(0, 1)), color = factor(cj, levels = c(0, 1))), binwidth = 0.1, size = 1.5) + 
  scale_fill_manual(values = c("#fdb338", "#025196"), labels = c('Low', 'High')) +  
  scale_color_manual(values = c("#fdb338", "#025196"), labels = c('Low', 'High')) +  
  labs(x = "Confidence RT (s)", y = "Proportion", fill = "Observed CJ", color = "Predicted CJ", title = "Correct decision") + 
  xlim(c(-0.2, 3)) +
  theme(legend.position = c(0.8, 0.75)) 




ggplot(data = df_observed, aes(x = rt)) +
  geom_histogram(aes(y = ..count.. / sum(..count..), fill = factor(cor, levels = c(0, 1))), alpha = 0.5, position = "identity") +
  geom_freqpoly(data = df_predicted_1, aes(x = rt, y = ..count.. / sum(..count..), group = factor(cor), color = as.factor(cor))) 


ggplot(data = df_observed, aes(x = rt)) +
  geom_histogram(aes(fill = factor(cor, levels = c(0, 1))), alpha = 0.5, position = "identity") +
  geom_freqpoly(data = df_predicted_1, aes(x = rt, group = factor(cor), color = as.factor(cor)))

ggplot(data, aes(x = values, y = after_stat(density))) +
  geom_histogram(binwidth = 0.5, fill = "lightblue", color = "black", alpha = 0.5) +
  geom_freqpoly(binwidth = 0.5, color = "red", linewidth = 1)


plot <- ggarrange(
  ggarrange(
    ggarrange(p_rtconf_0, p_rtconf_1, align = 'hv'), 
    p_BIC, ncol = 2, widths = c(2, 1)),
  ggarrange(p_rt_chi2, p_rtconf_chi2, p_confidence_chi2, ncol = 3, align = 'hv'),
  nrow = 2, align = 'hv')

plot <- ggarrange(p_rtconf_0, p_rtconf_1)

ggarrange(p_rt_chi2, p_rtconf_chi2, p_confidence_chi2, ncol = 3)
plot <- ggarrange(p_BIC, p_rtconf_0, p_rtconf_1, p_rt_chi2, p_rtconf_chi2, p_confidence_chi2, ncol = 3, nrow = 2, align = 'hv')


plot <- ggarrange(p_BIC, p_rt_chi2, p_rtconf_chi2, p_confidence_chi2, ncol = 2, nrow = 2, align = 'hv')


ggsave(plot = plot, filename = 'test3.png')



ggsave("plot2.png", plot = plot, device = CairoPNG)
library(Cairo)


# ANOVA on best fitting model parameters ---------------------------------------


df_params <- read.csv(paste0('Data/Experiment_1/Model_parameters/exp1_model1_parameters.csv'))
df_params <- df_params %>% mutate(rt_manipulation = as.factor(ifelse(df_params$manipulation %in% c("AccAcc", "AccFast"), 1, 0)), rtconf_manipulation = as.factor(ifelse(df_params$manipulation %in% c("AccAcc", "FastAcc"), 1, 0)))

## Decision boundary (a) ----

# Outliers
plot_a <- ggplot(data = df_params) + 
  geom_point_interactive(aes(x = manipulation, y = a, data_id = sub, tooltip = sub))
girafe(ggobj = plot_a)

# Normality
ggqqplot(df_params, 'a', facet.by = c('rt_manipulation', 'rtconf_manipulation'))

# ANOVA
res.aov <- anova_test(
  data = df_params, dv = a, wid = sub,
  within = c(rt_manipulation, rtconf_manipulation),
  effect.size = 'pes'
)
get_anova_table(res.aov)

## Confidence boundary (a2) ----

# Outliers
plot_a2 <- ggplot(data = df_params) + 
  geom_point_interactive(aes(x = manipulation, y = a2, data_id = sub, tooltip = sub))
girafe(ggobj = plot_a2)

# Normality
ggqqplot(df_params, 'a2', facet.by = c('rt_manipulation', 'rtconf_manipulation'))

# ANOVA
res.aov <- anova_test(
  data = df_params, dv = a2, wid = sub,
  within = c(rt_manipulation, rtconf_manipulation),
  effect.size = 'pes'
)
get_anova_table(res.aov)

### Pairwise t-test
df_params %>% group_by(rt_manipulation) %>%
  t_test(a2 ~ rtconf_manipulation, paired = T)

## Confidence boundary slope (a2_slope) ----

# Outliers
plot_a2_slope <- ggplot(data = df_params) + 
  geom_point_interactive(aes(x = manipulation, y = a2_slope, data_id = sub, tooltip = sub))
girafe(ggobj = plot_a2_slope)

# Normality
ggqqplot(df_params, 'a2_slope', facet.by = c('rt_manipulation', 'rtconf_manipulation'))

# ANOVA
res.aov <- anova_test(
  data = df_params, dv = a2_slope, wid = sub,
  within = c(rt_manipulation, rtconf_manipulation),
  effect.size = 'pes'
)
get_anova_table(res.aov)

## Drift rate ~ motion coherence ----

df_params_v <- gather(df_params, coherence, drift_rate, v1:v3, factor_key=TRUE)

# Normality
ggqqplot(df_params_v, 'drift_rate', facet.by = c('rt_manipulation', 'coherence'))
ggqqplot(df_params_v, 'drift_rate', facet.by = c('rtconf_manipulation', 'coherence'))

# repeated measures ANOVA 
res.aov <- anova_test(
  data = df_params_v, dv = drift_rate, wid = sub,
  within = c(rt_manipulation, rtconf_manipulation, coherence),
  effect.size = 'pes'
)
get_anova_table(res.aov)


# Model parameters  ------------------------------------------------------------

df_params$manipulation_jitter <- jitter(unclass(as.factor(df_params$manipulation)), factor = 0.6)
df_params <- df_params %>% 
  mutate(rt_manipulation = as.factor(ifelse(manipulation %in% c("AccAcc", "AccFast"), 1, 0)),
         rtconf_manipulation = as.factor(ifelse(manipulation %in% c("AccAcc", "FastAcc"), 1, 0))) %>%
  group_by(manipulation) %>% 
  mutate(a_mean = mean(a),
         a_sd = sd(a),
         a2_mean = mean(a2),
         a2_sd = sd(a2),
         a2_slope_mean = mean(a2_slope),
         a2_slope_sd = sd(a2_slope))

(a <- ggplot(data = df_params, aes(x = strtoi(rt_manipulation), y = a, color = as.factor(rtconf_manipulation))) +
  geom_point(size = 2.5, stroke = 1, shape = 16, alpha = 0.2, position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.5)) +
  stat_summary(geom = "line", size = 1, fun = "mean", position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = a_mean - a_sd / sqrt(40), ymax = a_mean + a_sd / sqrt(40), group = as.factor(rtconf_manipulation)), position = position_dodge(width = 0.5), size = 1, width = 0) +
  stat_summary(geom = "point", size = 2.5, fun = "mean", position = position_dodge(width = 0.5)) +
  scale_x_continuous(labels = c('"Make fast\ndecisions"', '"Make accurate\n decisions"'),breaks = c(0, 1)) +
  scale_color_manual(values = c('#CA3C25', '#FFA630')) +
  ylab(label = 'Decision Boundary Separation') +
  xlab(label = '') +
  geom_line(stroke = 1, shape = 16, alpha = 0.2, position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.5), aes(group = interaction(rtconf_manipulation, sub))) +
  theme(axis.line = element_line(colour = 'black'),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = '#e0e0e0', size = 0.7),
        panel.grid.minor.x = element_blank(),
        panel.border = element_blank(),
        axis.ticks.length = unit(.2, 'cm'),
        panel.background = element_blank(),
        text = element_text(family = 'font', size = 12),
        axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 5, r = 0, b = 0, l = 0)),
        legend.position = 'none',
        strip.background = element_blank(),
        strip.text.x = element_text(size = 11),
        plot.margin=unit(c(.5,0.2,.5,0.2),"cm")))

(a <- ggplot(data = df_params, aes(x = strtoi(rt_manipulation), y = a, color = as.factor(rtconf_manipulation))) +
    geom_point(size = 2.5, stroke = 1, shape = 16, alpha = 0.2, position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.5)) +
    stat_summary(geom = "line", size = 1, fun = "mean", position = position_dodge(width = 0.5)) +
    geom_errorbar(aes(ymin = a_mean - a_sd / sqrt(40), ymax = a_mean + a_sd / sqrt(40), group = as.factor(rtconf_manipulation)), position = position_dodge(width = 0.5), size = 1, width = 0) +
    stat_summary(geom = "point", size = 2.5, fun = "mean", position = position_dodge(width = 0.5)) +
    scale_x_continuous(labels = c('"Make fast\ndecisions"', '"Make accurate\n decisions"'),breaks = c(0, 1)) +
    scale_color_manual(values = c('#CA3C25', '#FFA630')) +
    ylab(label = 'Decision Boundary Separation') +
    xlab(label = '') +
    geom_line(stroke = 1, shape = 16, alpha = 0.2, position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.5), aes(x = strtoi(rt_manipulation), y = a, color = as.factor(rtconf_manipulation), group = interaction(rtconf_manipulation, sub))) +
    theme(axis.line = element_line(colour = 'black'),
          panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_line(colour = '#e0e0e0', size = 0.7),
          panel.grid.minor.x = element_blank(),
          panel.border = element_blank(),
          axis.ticks.length = unit(.2, 'cm'),
          panel.background = element_blank(),
          text = element_text(family = 'font', size = 12),
          axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0)),
          axis.title.x = element_text(margin = margin(t = 5, r = 0, b = 0, l = 0)),
          legend.position = 'none',
          strip.background = element_blank(),
          strip.text.x = element_text(size = 11),
          plot.margin=unit(c(.5,0.2,.5,0.2),"cm")))
  



a2 <- ggplot(data = df_params, aes(x = strtoi(rt_manipulation), y = a2, color = as.factor(rtconf_manipulation))) +
  geom_point(size = 2.5, stroke = 1, shape = 16, alpha = 0.2, position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.5)) +
  stat_summary(geom = "point", size = 2.5, fun = "mean", position = position_dodge(width = 0.5)) +
  stat_summary(geom = "line", size = 1, fun = "mean", position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = a2_mean - a2_sd / sqrt(40), ymax = a2_mean + a2_sd / sqrt(40), group = as.factor(rtconf_manipulation)), position = position_dodge(width = 0.5), size = 1, width = 0) +
  scale_x_continuous(labels = c('"Make fast\ndecisions"', '"Make accurate\n decisions"'),breaks = c(0, 1)) +
  scale_color_manual(values = c('#CA3C25', '#FFA630')) +
  ylab(label = 'Confidence Boundary Separation') +
  xlab(label = '') +
  theme(axis.line = element_line(colour = 'black'),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = '#e0e0e0', size = 0.7),
        panel.grid.minor.x = element_blank(),
        panel.border = element_blank(),
        axis.ticks.length = unit(.2, 'cm'),
        panel.background = element_blank(),
        text = element_text(family = 'font', size = 12),
        axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 5, r = 0, b = 0, l = 0)),
        legend.position = 'none',
        strip.background = element_blank(),
        strip.text.x = element_text(size = 11),
        plot.margin=unit(c(.5,0.2,.5,0.2),"cm"))

a2_slope <- ggplot(data = df_params, aes(x = strtoi(rt_manipulation), y = a2_slope, color = as.factor(rtconf_manipulation))) +
  geom_errorbar(aes(ymin = a2_slope_mean - a2_slope_sd / sqrt(40), ymax = a2_slope_mean + a2_slope_sd / sqrt(40), group = as.factor(rtconf_manipulation)), position = position_dodge(width = 0.5), size = 1, width = 0) +
  geom_point(size = 2.5, stroke = 1, shape = 16, alpha = 0.2, position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.5)) +
  stat_summary(geom = "line", size = 1, fun = "mean", position = position_dodge(width = 0.5)) +
  stat_summary(geom = "point", size = 2.5, fun = "mean", position = position_dodge(width = 0.5)) +
  scale_x_continuous(labels = c('"Make fast\ndecisions"', '"Make accurate\n decisions"'),breaks = c(0, 1)) +
  scale_color_manual(values = c('#CA3C25', '#FFA630')) +
  ylab(label = 'Urgency') +
  xlab(label = '') +
  theme(axis.line = element_line(colour = 'black'),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = '#e0e0e0', size = 0.7),
        panel.grid.minor.x = element_blank(),
        panel.border = element_blank(),
        axis.ticks.length = unit(.2, 'cm'),
        panel.background = element_blank(),
        text = element_text(family = 'font', size = 12),
        axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 5, r = 0, b = 0, l = 0)),
        legend.position = 'none',
        strip.background = element_blank(),
        strip.text.x = element_text(size = 11),
        plot.margin=unit(c(.5,0.2,.5,0.2),"cm"))

plot <- ggarrange(a, a2, a2_slope, ncol = 3)
ggsave(plot = plot, filename = 'test3.png')







library(dplyr)
df_params <- read.csv(paste0('Data/Experiment_2/Model_parameters/exp2_model4_parameters.csv'))


manipulation <- 'FastAcc'

mean(df_params$a[df_params$manipulation==manipulation])
mean(df_params$ter[df_params$manipulation==manipulation])
mean(df_params$ter2[df_params$manipulation==manipulation])
mean(df_params$a2[df_params$manipulation==manipulation])
mean(df_params$starting_point_confidence[df_params$manipulation==manipulation])
mean(df_params$a2_slope_upper[df_params$manipulation==manipulation])
mean(df_params$a2_slope_lower[df_params$manipulation==manipulation])



df_params <- df_params %>% mutate(rt_manipulation = as.factor(ifelse(df_params$manipulation %in% c("AccAcc", "AccFast"), 1, 0)), rtconf_manipulation = as.factor(ifelse(df_params$manipulation %in% c("AccAcc", "FastAcc"), 1, 0)))
df_params$starting_point_confidence <- (df_params$starting_point_confidence -0.5) * 2

df_params$manipulation_jitter <- jitter(unclass(as.factor(df_params$manipulation)), factor = 0.6)
df_params <- df_params %>% 
  mutate(rt_manipulation = as.factor(ifelse(manipulation %in% c("AccAcc", "AccFast"), 1, 0)),
         rtconf_manipulation = as.factor(ifelse(manipulation %in% c("AccAcc", "FastAcc"), 1, 0))) %>%
  group_by(manipulation) %>% 
  mutate(a_mean = mean(a),
         a_sd = sd(a),
         a2_mean = mean(a2),
         a2_sd = sd(a2),
         starting_point_confidence_mean = mean(starting_point_confidence),
         starting_point_confidence_sd = sd(starting_point_confidence),
         a2_slope_upper_mean = mean(a2_slope_upper),
         a2_slope_upper_sd = sd(a2_slope_upper),
         a2_slope_lower_mean = mean(a2_slope_lower),
         a2_slope_lower_sd = sd(a2_slope_lower))


a <- ggplot(data = df_params, aes(x = strtoi(rt_manipulation), y = a, color = as.factor(rtconf_manipulation))) +
  geom_point(size = 2.5, stroke = 1, shape = 16, alpha = 0.2, position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.5)) +
  stat_summary(geom = "line", size = 1, fun = "mean", position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = a_mean - a_sd / sqrt(40), ymax = a_mean + a_sd / sqrt(40), group = as.factor(rtconf_manipulation)), position = position_dodge(width = 0.5), size = 1, width = 0) +
  stat_summary(geom = "point", size = 2.5, fun = "mean", position = position_dodge(width = 0.5)) +
  scale_x_continuous(labels = c('"Make fast\ndecisions"', '"Make accurate\n decisions"'),breaks = c(0, 1)) +
  scale_color_manual(values = c('#CA3C25', '#FFA630')) +
  ylab(label = 'Decision Boundary Separation') +
  xlab(label = '') +
  theme(axis.line = element_line(colour = 'black'),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = '#e0e0e0', size = 0.7),
        panel.grid.minor.x = element_blank(),
        panel.border = element_blank(),
        axis.ticks.length = unit(.2, 'cm'),
        panel.background = element_blank(),
        text = element_text(family = 'font', size = 12),
        axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 5, r = 0, b = 0, l = 0)),
        legend.position = 'none',
        strip.background = element_blank(),
        strip.text.x = element_text(size = 11),
        plot.margin=unit(c(.5,0.2,.5,0.2),"cm"))

a2 <- ggplot(data = df_params, aes(x = strtoi(rt_manipulation), y = a2, color = as.factor(rtconf_manipulation))) +
  geom_point(size = 2.5, stroke = 1, shape = 16, alpha = 0.2, position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.5)) +
  stat_summary(geom = "line", size = 1, fun = "mean", position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = a2_mean - a2_sd / sqrt(40), ymax = a2_mean + a2_sd / sqrt(40), group = as.factor(rtconf_manipulation)), position = position_dodge(width = 0.5), size = 1, width = 0) +
  stat_summary(geom = "point", size = 2.5, fun = "mean", position = position_dodge(width = 0.5)) +
  scale_x_continuous(labels = c('"Make fast\ndecisions"', '"Make accurate\n decisions"'),breaks = c(0, 1)) +
  scale_color_manual(values = c('#CA3C25', '#FFA630')) +
  ylab(label = 'Confidence Boundary Separation') +
  xlab(label = '') +
  theme(axis.line = element_line(colour = 'black'),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = '#e0e0e0', size = 0.7),
        panel.grid.minor.x = element_blank(),
        panel.border = element_blank(),
        axis.ticks.length = unit(.2, 'cm'),
        panel.background = element_blank(),
        text = element_text(family = 'font', size = 12),
        axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 5, r = 0, b = 0, l = 0)),
        legend.position = 'none',
        strip.background = element_blank(),
        strip.text.x = element_text(size = 11),
        plot.margin=unit(c(.5,0.2,.5,0.2),"cm"))

starting_point_confidence <- ggplot(data = df_params, aes(x = strtoi(rt_manipulation), y = starting_point_confidence, color = as.factor(rtconf_manipulation))) +
  geom_point(size = 2.5, stroke = 1, shape = 16, alpha = 0.2, position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.5)) +
  stat_summary(geom = "line", size = 1, fun = "mean", position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = starting_point_confidence_mean - starting_point_confidence_sd / sqrt(40), ymax = starting_point_confidence_mean + starting_point_confidence_sd / sqrt(40), group = as.factor(rtconf_manipulation)), position = position_dodge(width = 0.5), size = 1, width = 0) +
  stat_summary(geom = "point", size = 2.5, fun = "mean", position = position_dodge(width = 0.5)) +
  scale_x_continuous(labels = c('"Make fast\ndecisions"', '"Make accurate\n decisions"'),breaks = c(0, 1)) +
  scale_color_manual(values = c('#CA3C25', '#FFA630')) +
  ylab(label = 'Starting Point Confidence') +
  xlab(label = '') +
  theme(axis.line = element_line(colour = 'black'),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = '#e0e0e0', size = 0.7),
        panel.grid.minor.x = element_blank(),
        panel.border = element_blank(),
        axis.ticks.length = unit(.2, 'cm'),
        panel.background = element_blank(),
        text = element_text(family = 'font', size = 12),
        axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 5, r = 0, b = 0, l = 0)),
        legend.position = 'none',
        strip.background = element_blank(),
        strip.text.x = element_text(size = 11),
        plot.margin=unit(c(.5,0.2,.5,0.2),"cm"))

a2_slope_upper <- ggplot(data = df_params, aes(x = strtoi(rt_manipulation), y = a2_slope_upper, color = as.factor(rtconf_manipulation))) +
  geom_point(size = 2.5, stroke = 1, shape = 16, alpha = 0.2, position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.5)) +
  stat_summary(geom = "line", size = 1, fun = "mean", position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = a2_slope_upper_mean - a2_slope_upper_sd / sqrt(40), ymax = a2_slope_upper_mean + a2_slope_upper_sd / sqrt(40), group = as.factor(rtconf_manipulation)), position = position_dodge(width = 0.5), size = 1, width = 0) +
  stat_summary(geom = "point", size = 2.5, fun = "mean", position = position_dodge(width = 0.5)) +
  scale_x_continuous(labels = c('"Make fast\ndecisions"', '"Make accurate\n decisions"'),breaks = c(0, 1)) +
  scale_color_manual(values = c('#CA3C25', '#FFA630')) +
  ylab(label = 'Upper Confidence Urgency') +
  xlab(label = '') +
  theme(axis.line = element_line(colour = 'black'),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = '#e0e0e0', size = 0.7),
        panel.grid.minor.x = element_blank(),
        panel.border = element_blank(),
        axis.ticks.length = unit(.2, 'cm'),
        panel.background = element_blank(),
        text = element_text(family = 'font', size = 12),
        axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 5, r = 0, b = 0, l = 0)),
        legend.position = 'none',
        strip.background = element_blank(),
        strip.text.x = element_text(size = 11),
        plot.margin=unit(c(.5,0.2,.5,0.2),"cm"))

a2_slope_lower <- ggplot(data = df_params, aes(x = strtoi(rt_manipulation), y = a2_slope_lower, color = as.factor(rtconf_manipulation))) +
  geom_point(size = 2.5, stroke = 1, shape = 16, alpha = 0.2, position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.5)) +
  stat_summary(geom = "line", size = 1, fun = "mean", position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = a2_slope_lower_mean - a2_slope_lower_sd / sqrt(40), ymax = a2_slope_lower_mean + a2_slope_lower_sd / sqrt(40), group = as.factor(rtconf_manipulation)), position = position_dodge(width = 0.5), size = 1, width = 0) +
  stat_summary(geom = "point", size = 2.5, fun = "mean", position = position_dodge(width = 0.5)) +
  scale_x_continuous(labels = c('"Make fast\ndecisions"', '"Make accurate\n decisions"'),breaks = c(0, 1)) +
  scale_color_manual(values = c('#CA3C25', '#FFA630')) +
  ylab(label = 'Lower Confidence Urgency') +
  xlab(label = '') +
  theme(axis.line = element_line(colour = 'black'),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = '#e0e0e0', size = 0.7),
        panel.grid.minor.x = element_blank(),
        panel.border = element_blank(),
        axis.ticks.length = unit(.2, 'cm'),
        panel.background = element_blank(),
        text = element_text(family = 'font', size = 12),
        axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 5, r = 0, b = 0, l = 0)),
        legend.position = 'none',
        strip.background = element_blank(),
        strip.text.x = element_text(size = 11),
        plot.margin=unit(c(.5,0.2,.5,0.2),"cm"))

ggarrange(a, a2, starting_point_confidence, a2_slope_upper, a2_slope_lower)
