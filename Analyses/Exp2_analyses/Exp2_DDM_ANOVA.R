# Repeated measures ANOVA on the estimated DDM parameters of Exp2


# Set-up ----


## Load packages

library(rstatix)
library(tidyr)
library(dplyr)
library(ggpubr)
library(ggplot2)
library(ggiraph)


# Load data ----


df_DDM <- data.frame(matrix(ncol = 13, nrow = 40*4))
colnames(df_DDM) <- c('sub', 'manipulation', 'v1', 'v2', 'v3', 'a', 'ter', 'a2_upper', 'a2_lower', 'postdriftmod', 'a2_slope_upper', 'a2_slope_lower', 'ter2')
condLab <- c('FastFast', 'AccFast', 'AccAcc', 'FastAcc') 
j <- 1
for (sub_id in (1:40)){  
  for (cond in 1:4){
    file_name <- paste0('Data\\Experiment_2\\Parameter_estimation\\exp2_separated_2_results_sub_', sub_id, '_', condLab[cond], '.Rdata')
    load(file_name)
    df_DDM[j,] <- c(sub_id, condLab[cond], results$optim$bestmem[1], results$optim$bestmem[2], results$optim$bestmem[3], results$optim$bestmem[4], results$optim$bestmem[5], results$optim$bestmem[6], results$optim$bestmem[7], results$optim$bestmem[8], results$optim$bestmem[9], results$optim$bestmem[10], results$optim$bestmem[11])
    j <- j + 1
  }
}
df_DDM[3:13] <- lapply(df_DDM[3:13], as.numeric)



# Transform data ----


df_DDM <- df_DDM %>% mutate(rt_manipulation = as.factor(ifelse(df_DDM$manipulation %in% c("AccAcc", "AccFast"), 1, 0)),
                            rtconf_manipulation = as.factor(ifelse(df_DDM$manipulation %in% c("AccAcc", "FastAcc"), 1, 0)))


# Repeated measures ANOVA ----


## Decision bound (a) ----

### Outliers
plot_a <- ggplot(data = df_DDM) + 
  geom_point_interactive(aes(x = manipulation, y = a, data_id = sub, tooltip = sub))
girafe(ggobj = plot_a)

### Normality
ggqqplot(df_DDM, 'a', facet.by = c('rt_manipulation', 'rtconf_manipulation'))

### ANOVA
res.aov <- anova_test(
  data = df_DDM, dv = a, wid = sub,
  within = c(rt_manipulation, rtconf_manipulation),
  effect.size = 'pes'
)
get_anova_table(res.aov)

### Pairwise t-test
df_DDM %>% group_by(rtconf_manipulation) %>%
  t_test(a ~ rt_manipulation, paired = T)

## Confidence bound (a2_upper) ----

### Outliers
plot_a2_upper <- ggplot(data = df_DDM) + 
  geom_point_interactive(aes(x = manipulation, y = a2_upper, data_id = sub, tooltip = sub))
girafe(ggobj = plot_a2_upper)

### Normality
ggqqplot(df_DDM, 'a2_upper', facet.by = c('rt_manipulation', 'rtconf_manipulation'))

### ANOVA
res.aov <- anova_test(
  data = df_DDM, dv = a2_upper, wid = sub,
  within = c(rt_manipulation, rtconf_manipulation),
  effect.size = 'pes'
)
get_anova_table(res.aov)


## Confidence bound (a2_lower) ----

### Outliers
plot_a2_lower <- ggplot(data = df_DDM) + 
  geom_point_interactive(aes(x = manipulation, y = a2_lower, data_id = sub, tooltip = sub))
girafe(ggobj = plot_a2_lower)

### Normality
ggqqplot(df_DDM, 'a2_lower', facet.by = c('rt_manipulation', 'rtconf_manipulation'))

### ANOVA
res.aov <- anova_test(
  data = df_DDM, dv = a2_lower, wid = sub,
  within = c(rt_manipulation, rtconf_manipulation),
  effect.size = 'pes'
)
get_anova_table(res.aov)


## Urgency (a2_slope_upper) ----

### Outliers
plot_a2_slope_upper <- ggplot(data = df_DDM) + 
  geom_point_interactive(aes(x = manipulation, y = a2_slope_upper, data_id = sub, tooltip = sub))
girafe(ggobj = plot_a2_slope_upper)

### Normality
ggqqplot(df_DDM, 'a2_slope_upper', facet.by = c('rt_manipulation', 'rtconf_manipulation'))

### ANOVA
res.aov <- anova_test(
  data = df_DDM, dv = a2_slope_upper, wid = sub,
  within = c(rt_manipulation, rtconf_manipulation),
  effect.size = 'pes'
)
get_anova_table(res.aov)

## Urgency (a2_slope_lower) ----

### Outliers
plot_a2_slope_lower <- ggplot(data = df_DDM) + 
  geom_point_interactive(aes(x = manipulation, y = a2_slope_lower, data_id = sub, tooltip = sub))
girafe(ggobj = plot_a2_slope_lower)

### Normality
ggqqplot(df_DDM, 'a2_slope_lower', facet.by = c('rt_manipulation', 'rtconf_manipulation'))

### ANOVA
res.aov <- anova_test(
  data = df_DDM, dv = a2_slope_lower, wid = sub,
  within = c(rt_manipulation, rtconf_manipulation),
  effect.size = 'pes'
)
get_anova_table(res.aov)

