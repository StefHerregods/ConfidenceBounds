alpha = 0.15
size = 2




df_params <- read.csv(paste0('Data/Experiment_1/Model_parameters/exp1_model1_parameters.csv'))
df_params <- df_params %>% mutate(rt_manipulation = as.factor(ifelse(df_params$manipulation %in% c("AccAcc", "AccFast"), 1, 0)), rtconf_manipulation = as.factor(ifelse(df_params$manipulation %in% c("AccAcc", "FastAcc"), 1, 0)))


library(dplyr)

df_params <- df_params %>%
  mutate(manipulation_jitter = case_when(
    manipulation == "AccAcc"  ~ 1.15 + runif(n(), -0.2, 0.2),
    manipulation == "AccFast" ~ 1.85 + runif(n(), -0.2, 0.2),
    manipulation == "FastAcc" ~ 3.15 + runif(n(), -0.2, 0.2),
    manipulation == "FastFast"~ 3.85 + runif(n(), -0.2, 0.2)
  ))

library(dplyr)

df_means <- df_params %>%
  group_by(manipulation) %>%
  summarise(across(where(is.numeric), 
                   list(mean = ~mean(.x, na.rm = TRUE), 
                        sd = ~sd(.x, na.rm = TRUE))))
df_means <- df_means %>%
  mutate(manipulation_jitter = case_when(
    manipulation == "AccAcc"  ~ 1.15,
    manipulation == "AccFast" ~ 1.85,
    manipulation == "FastAcc" ~ 3.15,
    manipulation == "FastFast"~ 3.85
  ))
df_means <- df_means %>% mutate(rt_manipulation = as.factor(ifelse(df_means$manipulation %in% c("AccAcc", "AccFast"), 1, 0)), rtconf_manipulation = as.factor(ifelse(df_means$manipulation %in% c("AccAcc", "FastAcc"), 1, 0)))


library(ggplot2)


a <- ggplot(data = df_params, aes(x = manipulation_jitter, y = a, fill = as.factor(rtconf_manipulation)))  +
  geom_line(alpha = alpha, aes(group = interaction(rtconf_manipulation, sub), color = as.factor(rtconf_manipulation))) +
  geom_point(size = size, stroke = 1, shape = 21, alpha = alpha, color = 'white') +
  geom_errorbar(data = df_means, aes(ymin = a_mean - a_sd / sqrt(40), ymax = a_mean + a_sd / sqrt(40), x = manipulation_jitter, y = a_mean, color = rtconf_manipulation), size = 1, width = 0.05) +
  geom_line(data = df_means, size = 1, aes(x = manipulation_jitter, y = a_mean, group = rtconf_manipulation, color = rtconf_manipulation)) +
  scale_x_continuous(labels = c('"Make accurate\ndecisions"', '"Make fast\n decisions"'), breaks = c(1.5, 3.5)) +
  scale_fill_manual(values = c('#CA3C25', '#FFA630')) +
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


a2 <- ggplot(data = df_params, aes(x = manipulation_jitter, y = a2, fill = as.factor(rtconf_manipulation)))  +
  geom_line(alpha = alpha, aes(group = interaction(rtconf_manipulation, sub), color = as.factor(rtconf_manipulation))) +
  geom_point(size = size, stroke = 1, shape = 21, alpha = alpha, color = 'white') +
  geom_errorbar(data = df_means, aes(ymin = a2_mean - a2_sd / sqrt(40), ymax = a2_mean + a2_sd / sqrt(40), x = manipulation_jitter, y = a2_mean, color = rtconf_manipulation), size = 1, width = 0.05) +
  geom_line(data = df_means, size = 1, aes(x = manipulation_jitter, y = a2_mean, group = rtconf_manipulation, color = rtconf_manipulation)) +
  scale_x_continuous(labels = c('"Make accurate\ndecisions"', '"Make fast\n decisions"'), breaks = c(1.5, 3.5)) +
  scale_fill_manual(values = c('#CA3C25', '#FFA630')) +
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


a2_slope <- ggplot(data = df_params, aes(x = manipulation_jitter, y = a2_slope, fill = as.factor(rtconf_manipulation)))  +
  geom_line(alpha = alpha, aes(group = interaction(rtconf_manipulation, sub), color = as.factor(rtconf_manipulation))) +
  geom_point(size = size, stroke = 1, shape = 21, alpha = alpha, color = 'white') +
  geom_errorbar(data = df_means, aes(ymin = a2_slope_mean - a2_slope_sd / sqrt(40), ymax = a2_slope_mean + a2_slope_sd / sqrt(40), x = manipulation_jitter, y = a2_slope_mean, color = rtconf_manipulation), size = 1, width = 0.05) +
  geom_line(data = df_means, size = 1, aes(x = manipulation_jitter, y = a2_slope_mean, group = rtconf_manipulation, color = rtconf_manipulation)) +
  scale_x_continuous(labels = c('"Make accurate\ndecisions"', '"Make fast\n decisions"'), breaks = c(1.5, 3.5)) +
  scale_fill_manual(values = c('#CA3C25', '#FFA630')) +
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
ggsave(plot = plot, filename = 'exp1.png')





########--------------------------------------------------------






df_params <- read.csv(paste0('Data/Experiment_2/Model_parameters/exp2_model4_parameters.csv'))
df_params <- df_params %>% mutate(rt_manipulation = as.factor(ifelse(df_params$manipulation %in% c("AccAcc", "AccFast"), 1, 0)), rtconf_manipulation = as.factor(ifelse(df_params$manipulation %in% c("AccAcc", "FastAcc"), 1, 0)))


library(dplyr)

df_params <- df_params %>%
  mutate(manipulation_jitter = case_when(
    manipulation == "AccAcc"  ~ 1.15 + runif(n(), -0.2, 0.2),
    manipulation == "AccFast" ~ 1.85 + runif(n(), -0.2, 0.2),
    manipulation == "FastAcc" ~ 3.15 + runif(n(), -0.2, 0.2),
    manipulation == "FastFast"~ 3.85 + runif(n(), -0.2, 0.2)
  ))

library(dplyr)

df_means <- df_params %>%
  group_by(manipulation) %>%
  summarise(across(where(is.numeric), 
                   list(mean = ~mean(.x, na.rm = TRUE), 
                        sd = ~sd(.x, na.rm = TRUE))))
df_means <- df_means %>%
  mutate(manipulation_jitter = case_when(
    manipulation == "AccAcc"  ~ 1.15,
    manipulation == "AccFast" ~ 1.85,
    manipulation == "FastAcc" ~ 3.15,
    manipulation == "FastFast"~ 3.85
  ))
df_means <- df_means %>% mutate(rt_manipulation = as.factor(ifelse(df_means$manipulation %in% c("AccAcc", "AccFast"), 1, 0)), rtconf_manipulation = as.factor(ifelse(df_means$manipulation %in% c("AccAcc", "FastAcc"), 1, 0)))


library(ggplot2)


a <- ggplot(data = df_params, aes(x = manipulation_jitter, y = a, fill = as.factor(rtconf_manipulation)))  +
  geom_line(alpha = alpha, aes(group = interaction(rtconf_manipulation, sub), color = as.factor(rtconf_manipulation))) +
  geom_point(size = size, stroke = 1, shape = 21, alpha = alpha, color = 'white') +
  geom_errorbar(data = df_means, aes(ymin = a_mean - a_sd / sqrt(40), ymax = a_mean + a_sd / sqrt(40), x = manipulation_jitter, y = a_mean, color = rtconf_manipulation), size = 1, width = 0.05) +
  geom_line(data = df_means, size = 1, aes(x = manipulation_jitter, y = a_mean, group = rtconf_manipulation, color = rtconf_manipulation)) +
  scale_x_continuous(labels = c('"Make accurate\ndecisions"', '"Make fast\n decisions"'), breaks = c(1.5, 3.5)) +
  scale_fill_manual(values = c('#CA3C25', '#FFA630')) +
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


a2 <- ggplot(data = df_params, aes(x = manipulation_jitter, y = a2, fill = as.factor(rtconf_manipulation)))  +
  geom_line(alpha = alpha, aes(group = interaction(rtconf_manipulation, sub), color = as.factor(rtconf_manipulation))) +
  geom_point(size = size, stroke = 1, shape = 21, alpha = alpha, color = 'white') +
  geom_errorbar(data = df_means, aes(ymin = a2_mean - a2_sd / sqrt(40), ymax = a2_mean + a2_sd / sqrt(40), x = manipulation_jitter, y = a2_mean, color = rtconf_manipulation), size = 1, width = 0.05) +
  geom_line(data = df_means, size = 1, aes(x = manipulation_jitter, y = a2_mean, group = rtconf_manipulation, color = rtconf_manipulation)) +
  scale_x_continuous(labels = c('"Make accurate\ndecisions"', '"Make fast\n decisions"'), breaks = c(1.5, 3.5)) +
  scale_fill_manual(values = c('#CA3C25', '#FFA630')) +
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


a2_slope_upper <- ggplot(data = df_params, aes(x = manipulation_jitter, y = a2_slope_upper, fill = as.factor(rtconf_manipulation)))  +
  geom_line(alpha = alpha, aes(group = interaction(rtconf_manipulation, sub), color = as.factor(rtconf_manipulation))) +
  geom_point(size = size, stroke = 1, shape = 21, alpha = alpha, color = 'white') +
  geom_errorbar(data = df_means, aes(ymin = a2_slope_upper_mean - a2_slope_upper_sd / sqrt(40), ymax = a2_slope_upper_mean + a2_slope_upper_sd / sqrt(40), x = manipulation_jitter, y = a2_slope_upper_mean, color = rtconf_manipulation), size = 1, width = 0.05) +
  geom_line(data = df_means, size = 1, aes(x = manipulation_jitter, y = a2_slope_upper_mean, group = rtconf_manipulation, color = rtconf_manipulation)) +
  scale_x_continuous(labels = c('"Make accurate\ndecisions"', '"Make fast\n decisions"'), breaks = c(1.5, 3.5)) +
  scale_fill_manual(values = c('#CA3C25', '#FFA630')) +
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


a2_slope_lower <- ggplot(data = df_params, aes(x = manipulation_jitter, y = a2_slope_lower, fill = as.factor(rtconf_manipulation)))  +
  geom_line(alpha = alpha, aes(group = interaction(rtconf_manipulation, sub), color = as.factor(rtconf_manipulation))) +
  geom_point(size = size, stroke = 1, shape = 21, alpha = alpha, color = 'white') +
  geom_errorbar(data = df_means, aes(ymin = a2_slope_lower_mean - a2_slope_lower_sd / sqrt(40), ymax = a2_slope_lower_mean + a2_slope_lower_sd / sqrt(40), x = manipulation_jitter, y = a2_slope_lower_mean, color = rtconf_manipulation), size = 1, width = 0.05) +
  geom_line(data = df_means, size = 1, aes(x = manipulation_jitter, y = a2_slope_lower_mean, group = rtconf_manipulation, color = rtconf_manipulation)) +
  scale_x_continuous(labels = c('"Make accurate\ndecisions"', '"Make fast\n decisions"'), breaks = c(1.5, 3.5)) +
  scale_fill_manual(values = c('#CA3C25', '#FFA630')) +
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



vratio <- ggplot(data = df_params, aes(x = manipulation_jitter, y = starting_point_confidence, fill = as.factor(rtconf_manipulation)))  +
  geom_line(alpha = alpha, aes(group = interaction(rtconf_manipulation, sub), color = as.factor(rtconf_manipulation))) +
  geom_point(size = size, stroke = 1, shape = 21, alpha = alpha, color = 'white') +
  geom_errorbar(data = df_means, aes(ymin = starting_point_confidence_mean - starting_point_confidence_sd / sqrt(40), ymax = starting_point_confidence_mean + starting_point_confidence_sd / sqrt(40), x = manipulation_jitter, y = starting_point_confidence_mean, color = rtconf_manipulation), size = 1, width = 0.05) +
  geom_line(data = df_means, size = 1, aes(x = manipulation_jitter, y = starting_point_confidence_mean, group = rtconf_manipulation, color = rtconf_manipulation)) +
  scale_x_continuous(labels = c('"Make accurate\ndecisions"', '"Make fast\n decisions"'), breaks = c(1.5, 3.5)) +
  scale_fill_manual(values = c('#CA3C25', '#FFA630')) +
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

plot <- ggarrange(a, a2, vratio, a2_slope_upper, a2_slope_lower, ncol = 3, nrow = 2)
ggsave(plot = plot, filename = 'exp2.png')





### PROC -------------------------------------------------------
  
  
df <- read.csv(paste0('Data/Experiment_1/exp1_data_viable.csv'))
df <- df %>% mutate(rt_manipulation = as.factor(ifelse(df$manipulation %in% c("AccAcc", "AccFast"), 1, 0)), rtconf_manipulation = as.factor(ifelse(df$manipulation %in% c("AccAcc", "FastAcc"), 1, 0)))


  
subs <- unique(df$sub);N <- length(subs)
roc <- data.frame(matrix(NA,N,4));names(roc) <- unique(df$manipulation) 
for(i in 1:N){
  tempDat <- subset(df,sub==subs[i])
  for(c in unique(df$manipulation)){
    temp <- subset(tempDat,manipulation==c)
    roc[i,c] <- pROC::auc(as.numeric(temp$cor),as.numeric(temp$cj))
  }
}
roc$sub <- 1:nrow(roc)


roc_long <- reshape::melt(roc, id.vars = 'sub')
roc_long <- roc_long %>% mutate(rt_manipulation = as.factor(ifelse(roc_long$variable %in% c("AccAcc", "AccFast"), 1, 0)),
                                rtconf_manipulation = as.factor(ifelse(roc_long$variable %in% c("AccAcc", "FastAcc"), 1, 0))) %>%
  group_by(rt_manipulation, rtconf_manipulation) %>%
  mutate(roc_mean = mean(value),
         roc_sd = sd(value))
roc_long$manipulation <- roc_long$variable

library(dplyr)

roc_long <- roc_long %>%
  mutate(manipulation_jitter = case_when(
    manipulation == "AccAcc"  ~ 1.15 + runif(n(), -0.2, 0.2),
    manipulation == "AccFast" ~ 1.85 + runif(n(), -0.2, 0.2),
    manipulation == "FastAcc" ~ 3.15 + runif(n(), -0.2, 0.2),
    manipulation == "FastFast"~ 3.85 + runif(n(), -0.2, 0.2)
  ))

library(dplyr)

df_means <- roc_long %>%
  group_by(manipulation) %>%
  summarise(across(where(is.numeric), 
                   list(mean = ~mean(.x, na.rm = TRUE), 
                        sd = ~sd(.x, na.rm = TRUE))))
df_means <- df_means %>%
  mutate(manipulation_jitter = case_when(
    manipulation == "AccAcc"  ~ 1.15,
    manipulation == "AccFast" ~ 1.85,
    manipulation == "FastAcc" ~ 3.15,
    manipulation == "FastFast"~ 3.85
  ))
df_means <- df_means %>% mutate(rt_manipulation = as.factor(ifelse(df_means$manipulation %in% c("AccAcc", "AccFast"), 1, 0)), rtconf_manipulation = as.factor(ifelse(df_means$manipulation %in% c("AccAcc", "FastAcc"), 1, 0)))




roc <- ggplot(data = roc_long, aes(x = manipulation_jitter, y = value, fill = as.factor(rtconf_manipulation)))  +
  geom_line(alpha = alpha, aes(group = interaction(rtconf_manipulation, sub), color = as.factor(rtconf_manipulation))) +
  geom_point(size = size, stroke = 1, shape = 21, alpha = alpha, color = 'white') +
  geom_errorbar(data = df_means, aes(ymin = value_mean - value_sd / sqrt(40), ymax = value_mean + value_sd / sqrt(40), x = manipulation_jitter, y = roc_mean_mean, color = rtconf_manipulation), size = 1, width = 0.05) +
  geom_line(data = df_means, size = 1, aes(x = manipulation_jitter, y = value_mean, group = rtconf_manipulation, color = rtconf_manipulation)) +
  scale_x_continuous(labels = c('"Make accurate\ndecisions"', '"Make fast\n decisions"'), breaks = c(1.5, 3.5)) +
  scale_fill_manual(values = c('#CA3C25', '#FFA630')) +
  scale_color_manual(values = c('#CA3C25', '#FFA630')) +
  ylab(label = 'Type II AUC') +
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

ggsave(plot = roc, filename = 'exp1_roc.png')



