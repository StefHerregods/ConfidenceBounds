
# Set-up ----


## Load packages

library(rstatix)
library(tidyr)
library(dplyr)
library(ggpubr)
library(ggplot2)
library(ggiraph)
library(WRS2)

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


## Confidence bound (a2) ----

### Outliers
plot_a2 <- ggplot(data = df_DDM) + 
  geom_point_interactive(aes(x = manipulation, y = a2, data_id = sub, tooltip = sub)) 
girafe(ggobj = plot_a2) 

### Normality
ggqqplot(df_DDM, 'a2', facet.by = c('rt_manipulation', 'rtconf_manipulation'))

### repeated measures ANOVA 
res.aov <- anova_test(
  data = df_DDM, dv = a2, wid = sub,
  within = c(rt_manipulation, rtconf_manipulation),
  effect.size = 'pes'
)
get_anova_table(res.aov)

## Urgency (a2_slope) ----

### Outliers
plot_a2_slope <- ggplot(data = df_DDM) + 
  geom_point_interactive(aes(x = manipulation, y = a2_slope, data_id = sub, tooltip = sub))
girafe(ggobj = plot_a2_slope)
df_DDM_2 <- df_DDM[df_DDM$a2_slope < 5,]

### Normality
ggqqplot(df_DDM_2, 'a2_slope', facet.by = c('rt_manipulation', 'rtconf_manipulation'))

### ANOVA
res.aov <- anova_test(
  data = df_DDM_2, dv = a2_slope, wid = sub,
  within = c(rt_manipulation, rtconf_manipulation),
  effect.size = 'pes'
)
get_anova_table(res.aov)





