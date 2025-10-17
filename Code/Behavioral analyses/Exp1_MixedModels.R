# Behavioral analysis
# Script contains the mixed effect models code used to analyze RT's, confidence RT's, accuracy and CJ


# Set-up ----


rm(list = ls())

## Activating packages 

library(lme4)
library(dplyr)
library(ggplot2)
library(grid)
library(car)
library(effects)
library(optimx)
library(dfoptim)
library(ggResidpanel)
library(arm)
library(splines)

## VIF function
## https://stackoverflow.com/questions/26633483/collinearity-after-accounting-for-random-mixed-effects

vif.lme <- function (fit) {  
  # adapted from rms::vif
  v <- vcov(fit)
  nam <- names(fixef(fit))
  # exclude intercepts
  ns <- sum(1 * (nam == "Intercept" | nam == "(Intercept)"))
  if (ns > 0) {
    v <- v[-(1:ns), -(1:ns), drop = FALSE]
    nam <- nam[-(1:ns)] }
  d <- diag(v)^0.5
  v <- diag(solve(v/(d %o% d)))
  names(v) <- nam
  v }

## write to excel function

write.excel <- function(x,row.names=FALSE,col.names=FALSE,...) {
  write.table(x,"clipboard",sep="\t",row.names=row.names,col.names=col.names,...)
}

## Read data

df <- read.csv(file = "Data\\Experiment_1\\Exp1_data_viable.csv")

## Transform variables into factors

df <- df %>% mutate(rt_manipulation = as.factor(ifelse(df$manipulation %in% c("AccAcc", "AccFast"), 1, 0)),
                    rtconf_manipulation = as.factor(ifelse(df$manipulation %in% c("AccAcc", "FastAcc"), 1, 0)),
                    coherence = as.factor(coherence),
                    cor_numeric = cor,
                    cor = as.factor(cor),
                    cj = as.factor(cj))


# decision RT ----


## Based on correct data only

df_correct <- subset(df, cor == 1)
df_incorrect <- subset(df, cor == 0)

## Log transform

df_correct$rt_log <- log(df_correct$rt)
df_correct$rt_log <- scale(df_correct$rt_log, scale = FALSE)

## Are random slopes necessary? 

plot1 <- ggplot(df_correct, aes(x = as.factor(rt_manipulation), y = rt, group = sub)) +
  stat_smooth(geom='line', alpha=1, se=FALSE) +
  theme_minimal()
plot2 <- ggplot(df_correct, aes(x = as.factor(rtconf_manipulation), y = rt, group = sub)) +
  stat_smooth(geom='line', alpha=1, se=FALSE) +
  theme_minimal()
plot3 <- ggplot(df_correct, aes(x = coherence, y = rt, group = sub)) +
  stat_smooth(geom='line', alpha=1, se=FALSE) +
  theme_minimal()

grid.newpage()
pushViewport(viewport(layout=grid.layout(1,3)))
vplayout <- function(x,y){
  viewport(layout.pos.row=x, layout.pos.col=y)}
print(plot1, vp=vplayout(1,1)); print(plot2, vp=vplayout(1,2)); print(plot3, vp=vplayout(1,3))

## Model comparisons (through likelihood ratio tests)

RT_1 <- lmer(rt ~ 1 + rt_manipulation * rtconf_manipulation * coherence + (1|sub), 
             data = df_correct, control = lmerControl(optimizer = "bobyqa"))
RT_2 <- lmer(rt ~ 1 + rt_manipulation * rtconf_manipulation * coherence + (1 + rt_manipulation|sub), 
             data = df_correct, control = lmerControl(optimizer = "bobyqa"))
RT_3 <- lmer(rt ~ 1 + rt_manipulation * rtconf_manipulation * coherence + (1 + rtconf_manipulation|sub), 
             data = df_correct, control = lmerControl(optimizer = "bobyqa"))
RT_4 <- lmer(rt ~ 1 + rt_manipulation * rtconf_manipulation * coherence + (1 + coherence|sub), 
             data = df_correct, control = lmerControl(optimizer = "bobyqa"))  

anova(RT_1, RT_2)  # Significant; BIC 36263
anova(RT_1, RT_3)  # Significant; BIC 37418
anova(RT_1, RT_4)  # Significant; BIC 37342

RT_5 <- lmer(rt ~ 1 + rt_manipulation * rtconf_manipulation * coherence + (1 + rt_manipulation + coherence|sub), 
             data = df_correct, control = lmerControl(optimizer = "bobyqa"))

anova(RT_2, RT_5)  # Significant

RT_6 <- lmer(rt ~ 1 + rt_manipulation * rtconf_manipulation * coherence + (1 + rt_manipulation + coherence + rtconf_manipulation|sub), 
             data = df_correct, control = lmerControl(optimizer = "bobyqa"))

anova(RT_5, RT_6)  # Significant

RT_7 <- lmer(rt ~ 1 + rt_manipulation * rtconf_manipulation * coherence + (1 + rt_manipulation * coherence + rtconf_manipulation|sub), 
             data = df_correct, control = lmerControl(optimizer = "bobyqa"))  # boundary (singular) fit
RT_8 <- lmer(rt ~ 1 + rt_manipulation * rtconf_manipulation * coherence + (1 + rt_manipulation + coherence * rtconf_manipulation|sub), 
             data = df_correct, control = lmerControl(optimizer = "bobyqa"))  # boundary (singular) fit
RT_9 <- lmer(rt ~ 1 + rt_manipulation * rtconf_manipulation * coherence + (1 + rt_manipulation * rtconf_manipulation + coherence|sub), 
             data = df_correct, control = lmerControl(optimizer = "bobyqa"))  

## Model assumptions

### (1) Linearity
### Categorical/dummy coded predictors -> assumption met by definition
### (2) Homogeneity of variance: violated -> log transform RT
resid_panel(RT_9)
### (3) Normality assumption: violated

## Model selection with log transformed rt

RT_7 <- lmer(rt_log ~ 1 + rt_manipulation * rtconf_manipulation * coherence + (1|sub), 
             data = df_correct, control = lmerControl(optimizer = "bobyqa"))
RT_8 <- lmer(rt_log ~ 1 + rt_manipulation * rtconf_manipulation * coherence + (1 + rt_manipulation|sub), 
             data = df_correct, control = lmerControl(optimizer = "bobyqa"))
RT_9 <- lmer(rt_log ~ 1 + rt_manipulation * rtconf_manipulation * coherence + (1 + rtconf_manipulation|sub), 
             data = df_correct, control = lmerControl(optimizer = "bobyqa"))
RT_10 <- lmer(rt_log ~ 1 + rt_manipulation * rtconf_manipulation * coherence + (1 + coherence|sub), 
              data = df_correct, control = lmerControl(optimizer = "bobyqa"))  # Boundary (singular) fit

anova(RT_7, RT_8)  # Significant; BIC 23320
anova(RT_7, RT_9)  # Significant; BIC 24573

RT_11 <- lmer(rt_log ~ 1 + rt_manipulation * rtconf_manipulation * coherence + (1 + rt_manipulation + rtconf_manipulation|sub), 
              data = df_correct, control = lmerControl(optimizer = "bobyqa"))

anova(RT_8, RT_11)  # Significant

RT_12 <- lmer(rt_log ~ 1 + rt_manipulation * rtconf_manipulation * coherence + (1 + rt_manipulation * rtconf_manipulation|sub), 
              data = df_correct, control = lmerControl(optimizer = "bobyqa")) 

anova(RT_11, RT_12)  # Significant

## Model assumptions

### (1) Linearity
### Categorical/dummy coded predictors -> assumption met by definition
### (2) Homogeneity of variance
resid_panel(RT_12)
### (3) Normality assumption
resid_panel(RT_12)
### (4) Multicollinearity - VIF
vif.lme(RT_12)

## Model interpretation

Anova(RT_12)  # For p-values
summary(RT_12)  # For estimates
confint(RT_12, method = 'boot', parm = 'beta_')

## Further analysis interactions

contrast.matrix <- rbind("rt_manipulation1:rtconf_manipulation1" = c(1, 1, 0, 1, 1, 1, 0, 0, 0, 0, 0, 0),
                         "rt_manipulation1:coherence0.4" = c(1, 1, 0, 1, 1, 0, 0, 1, 0, 0, 0, 0),
                         "rt_manipulation1:coherence0.4" = c(1, 1, 0, 1, 1, 1, 0, 1, 0, 0, 0, 0))
summary(multcomp::glht(RT_12, linfct = contrast.matrix))


# Confidence rating RT ----


## Are random slopes necessary? 

plot1 <- ggplot(df_correct, aes(x = as.factor(rt_manipulation), y = rtconf, group = sub)) +
  stat_smooth(geom='line', alpha=1, se=FALSE) +
  theme_minimal()
plot2 <- ggplot(df_correct, aes(x = as.factor(rtconf_manipulation), y = rtconf, group = sub)) +
  stat_smooth(geom='line', alpha=1, se=FALSE) +
  theme_minimal()
plot3 <- ggplot(df_correct, aes(x = coherence, y = rtconf, group = sub)) +
  stat_smooth(geom='line', alpha=1, se=FALSE) +
  theme_minimal()

grid.newpage()
pushViewport(viewport(layout=grid.layout(1,3)))
print(plot1, vp=vplayout(1,1))
print(plot2, vp=vplayout(1,2))
print(plot3, vp=vplayout(1,3))

## Log transform

df_correct$rtconf_log <- log(df_correct$rtconf)

df_correct$rtconf_log <- scale(df_correct$rtconf_log, scale = FALSE) 

## Model comparisons (through likelihood ratio tests)

RTconf_1 <- lmer(rtconf_log ~ 1 + rt_manipulation * rtconf_manipulation * coherence + (1|sub), 
                 data = df_correct, control = lmerControl(optimizer = "bobyqa"))
RTconf_2 <- lmer(rtconf_log ~ 1 + rt_manipulation * rtconf_manipulation * coherence + (1 + rt_manipulation|sub), 
                 data = df_correct, control = lmerControl(optimizer = "bobyqa"))
RTconf_3 <- lmer(rtconf_log ~ 1 + rt_manipulation * rtconf_manipulation * coherence + (1 + rtconf_manipulation|sub), 
                 data = df_correct, control = lmerControl(optimizer = "bobyqa")) 
RTconf_4 <- lmer(rtconf_log ~ 1 + rt_manipulation * rtconf_manipulation * coherence + (1 + coherence|sub), 
                 data = df_correct, control = lmerControl(optimizer = "bobyqa"))  # Boundary (singular) fit

anova(RTconf_1, RTconf_2)  # Significant; BIC 47560
anova(RTconf_1, RTconf_3)  # Significant; BIC 44066

RTconf_5 <- lmer(rtconf_log ~ 1 + rt_manipulation * rtconf_manipulation * coherence + (1 + rt_manipulation + rtconf_manipulation|sub), 
                 data = df_correct, control = lmerControl(optimizer = "bobyqa"))

anova(RTconf_3, RTconf_5)  # Significant

RTconf_6 <- lmer(rtconf_log ~ 1 + rt_manipulation * rtconf_manipulation * coherence + (1 + rt_manipulation * rtconf_manipulation|sub), 
                 data = df_correct, control = lmerControl(optimizer = "bobyqa"))

anova(RTconf_5, RTconf_6) # Significant

## Model assumptions

### (1) Linearity
### Categorical/dummy coded predictors -> assumption met by definition
### (2) Homogeneity of variance: assumption not met 
resid_panel(RTconf_6)
### (3) Normality (of errors) assumption
resid_panel(RTconf_6)
### (4) VIF
vif.lme(RTconf_6)

## Model interpretation

Anova(RTconf_6)
summary(RTconf_6)
confint(RTconf_6, method = 'boot', parm = 'beta_')


# Accuracy ----


## Are random slopes necessary? 

plot1 <- ggplot(df, aes(x = as.factor(rt_manipulation), y = cor, group = sub)) +
  stat_smooth(geom='line', alpha=1, se=FALSE) +
  theme_minimal()
plot2 <- ggplot(df, aes(x = as.factor(rtconf_manipulation), y = cor, group = sub)) +
  stat_smooth(geom='line', alpha=1, se=FALSE) +
  theme_minimal()
plot3 <- ggplot(df, aes(x = coherence, y = cor, group = sub)) +
  stat_smooth(geom='line', alpha=1, se=FALSE) +
  theme_minimal()

grid.newpage()
pushViewport(viewport(layout=grid.layout(1,3)))
print(plot1, vp=vplayout(1,1))
print(plot2, vp=vplayout(1,2))
print(plot3, vp=vplayout(1,3))

## Model comparisons (through likelihood ratio tests)

Cor_1 <- glmer(cor ~ 1 + rt_manipulation * rtconf_manipulation * coherence + (1|sub), 
               data = df, family = binomial, control = glmerControl(optimizer = "bobyqa"))
Cor_2 <- glmer(cor ~ 1 + rt_manipulation * rtconf_manipulation * coherence + (1 + rt_manipulation|sub), 
               data = df, family = binomial, control = glmerControl(optimizer = "bobyqa"))
Cor_3 <- glmer(cor ~ 1 + rt_manipulation * rtconf_manipulation * coherence + (1 + rtconf_manipulation|sub), 
               data = df, family = binomial, control = glmerControl(optimizer = "bobyqa"))  
Cor_4 <- glmer(cor ~ 1 + rt_manipulation * rtconf_manipulation * coherence + (1 + coherence|sub), 
               data = df, family = binomial, control = glmerControl(optimizer = "bobyqa"))  

anova(Cor_1, Cor_2)  # Significant; BIC 32237
anova(Cor_1, Cor_3)  # Not significant
anova(Cor_1, Cor_4)  # Significant; BIC 32078 (additionally: lower AIC, higher logLik than Cor_2)

Cor_5 <- glmer(cor ~ 1 + rt_manipulation * rtconf_manipulation * coherence + (1 + rt_manipulation + coherence|sub), 
                  data = df, family = binomial, control = glmerControl(optimizer = "bobyqa"))  # Boundary (singular) fit

## Model assumptions

### (1) Independence of errors
# Gray lines indicate plus and minus 2 standard-error bounds (around 95% of residuals)

binnedplot(fitted(Cor_4), 
           residuals(Cor_4, type = "response"), 
           nclass = NULL, 
           xlab = "Expected Values", 
           ylab = "Average residual", 
           main = "Binned residual plot", 
           cex.pts = 0.8, 
           col.pts = 1, 
           col.int = "gray")
### (2) Linearity (no continuous variables)
### (3) Absence of multicollinearity
vif.lme(Cor_4)

## Model interpretation

Anova(Cor_4)
summary(Cor_4)
confint(Cor_4, method = 'boot', parm = 'beta_')


# Confidence judgements ----


## Are random slopes necessary? 

plot1 <- ggplot(df_correct, aes(x = as.factor(rt_manipulation), y = cj, group = sub)) +
  stat_smooth(geom='line', alpha=1, se=FALSE) +
  theme_minimal()
plot2 <- ggplot(df_correct, aes(x = as.factor(rtconf_manipulation), y = cj, group = sub)) +
  stat_smooth(geom='line', alpha=1, se=FALSE) +
  theme_minimal()
plot3 <- ggplot(df_correct, aes(x = coherence, y = cj, group = sub)) +
  stat_smooth(geom='line', alpha=1, se=FALSE) +
  theme_minimal()

grid.newpage()
pushViewport(viewport(layout=grid.layout(1,3)))
print(plot1, vp=vplayout(1,1))
print(plot2, vp=vplayout(1,2))
print(plot3, vp=vplayout(1,3))

## Model comparisons (through likelihood ratio tests)

cj_1 <- glmer(cj ~ 1 + rt_manipulation * rtconf_manipulation * coherence + (1|sub), 
               data = df_correct, family = binomial, control = glmerControl(optimizer = "bobyqa"))
cj_2 <- glmer(cj ~ 1 + rt_manipulation * rtconf_manipulation * coherence + (1 + rt_manipulation|sub), 
               data = df_correct, family = binomial, control = glmerControl(optimizer = "bobyqa"))
cj_3 <- glmer(cj ~ 1 + rt_manipulation * rtconf_manipulation * coherence + (1 + rtconf_manipulation|sub), 
               data = df_correct, family = binomial, control = glmerControl(optimizer = "bobyqa"))  
cj_4 <- glmer(cj ~ 1 + rt_manipulation * rtconf_manipulation * coherence + (1 + coherence|sub), 
               data = df_correct, family = binomial, control = glmerControl(optimizer = "bobyqa"))  

anova(cj_1, cj_2)  # Significant; BIC 24211
anova(cj_1, cj_3)  # Significant; BIC 24248
anova(cj_1, cj_4)  # Significant; BIC 24089

cj_5 <- glmer(cj ~ 1 + rt_manipulation * rtconf_manipulation * coherence + (1 + coherence + rt_manipulation|sub), 
               data = df_correct, family = binomial, control = glmerControl(optimizer = "bobyqa"))

anova(cj_4, cj_5)  # Significant

cj_6 <- glmer(cj ~ 1 + rt_manipulation * rtconf_manipulation * coherence + (1 + coherence + rt_manipulation + rtconf_manipulation|sub), 
              data = df_correct, family = binomial, control = glmerControl(optimizer = "bobyqa"))

anova(cj_5, cj_6)  # Significant

cj_7 <- glmer(cj ~ 1 + rt_manipulation * rtconf_manipulation * coherence + (1 + coherence * rt_manipulation + rtconf_manipulation|sub), 
              data = df_correct, family = binomial, control = glmerControl(optimizer = "bobyqa"))  # Boundary (singular) fit
cj_8 <- glmer(cj ~ 1 + rt_manipulation * rtconf_manipulation * coherence + (1 + coherence + rt_manipulation * rtconf_manipulation|sub), 
              data = df_correct, family = binomial, control = glmerControl(optimizer = "bobyqa"))  # Boundary (singular) fit
cj_9 <- glmer(cj ~ 1 + rt_manipulation * rtconf_manipulation * coherence + (1 + coherence * rtconf_manipulation + rt_manipulation|sub), 
              data = df_correct, family = binomial, control = glmerControl(optimizer = "bobyqa"))  # Boundary (singular) fit

## Model assumptions

### (1) Independence of errors
### Gray lines indicate plus and minus 2 standard-error bounds (around 95% of residuals)
binnedplot(fitted(cj_6), 
           residuals(cj_6, type = "response"), 
           nclass = NULL, 
           xlab = "Expected Values", 
           ylab = "Average residual", 
           main = "Binned residual plot", 
           cex.pts = 0.8, 
           col.pts = 1, 
           col.int = "gray")
### (2) Linearity (no continuous variables)
### (3) Absence of multicollinearity
vif.lme(cj_6)

## Model interpretation

Anova(cj_6)
summary(cj_6)
confint(cj_6, method = 'boot', parm = 'beta_')
