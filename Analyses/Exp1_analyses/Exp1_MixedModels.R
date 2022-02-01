# Internship project: ConfidenceBounds (2021-2022)
# Script contains the mixed effect models code used to check hypothesis (a), (b) and 
# (c) of experiment 1.


# Activating packages

library(lme4)
library(dplyr)
library(ggplot2)
library(grid)
library(car)
library(effects)

# Setting working directory

setwd('C:\\Users\\herre\\Desktop\\Internship\\Results\\Exp1_Results')

df <- read.csv(file = "Exp1_data_viable.csv")

# Transform variables into factors

df$rt_manipulation <- ifelse(df$manipulation %in% c("AccAcc", "AccFast"), 1, 0)
df$rt_manipulation <- as.factor(df$rt_manipulation)
df$rtconf_manipulation <- ifelse(df$manipulation %in% c("AccAcc", "FastAcc"), 1, 0)
df$rtconf_manipulation <- as.factor(df$rtconf_manipulation)
df$coherence <- as.factor(df$coherence)


### Effect of manipulations on decision RT ###


# Based on correct data only

df_correct <- subset(df, cor == 1)

# Are random slopes necessary? -> Yes (but less for coherence)

plot1 <- ggplot(df_correct, aes(x = as.factor(rt_manipulation), y = rt, group = sub)) +
  stat_smooth(geom='line', alpha=1, se=FALSE, method = 'lm') +
  theme_minimal()
plot2 <- ggplot(df_correct, aes(x = as.factor(rtconf_manipulation), y = rt, group = sub)) +
  stat_smooth(geom='line', alpha=1, se=FALSE, method = 'lm') +
  theme_minimal()
plot3 <- ggplot(df_correct, aes(x = coherence, y = rt, group = sub)) +
  stat_smooth(geom='line', alpha=1, se=FALSE) +
  theme_minimal()

grid.newpage()
pushViewport(viewport(layout=grid.layout(1,3)))
vplayout <- function(x,y){
  viewport(layout.pos.row=x, layout.pos.col=y)}
print(plot1, vp=vplayout(1,1)); print(plot2, vp=vplayout(1,2)); print(plot3, vp=vplayout(1,3))

# Model comparisons (through likelihood ratio tests)

RT_1 <- lmer(rt ~ 1 + rt_manipulation * rtconf_manipulation * coherence + (1|sub), data = df_correct)
RT_2 <- lmer(rt ~ 1 + rt_manipulation * rtconf_manipulation * coherence + (1 + rt_manipulation|sub), data = df_correct)
RT_3 <- lmer(rt ~ 1 + rt_manipulation * rtconf_manipulation * coherence + (1 + rtconf_manipulation|sub), data = df_correct)
RT_4 <- lmer(rt ~ 1 + rt_manipulation * rtconf_manipulation * coherence + (1 + coherence|sub), data = df_correct)  # Fails to converge

anova(RT_1, RT_2)  # Significant
anova(RT_1, RT_3)  # Significant

RT_5 <- lmer(rt ~ 1 + rt_manipulation * rtconf_manipulation * coherence + (1 + rt_manipulation + rtconf_manipulation|sub), data = df_correct)

anova(RT_2, RT_5)  # Significant
anova(RT_3, RT_5)  # Significant


RT_6 <- lmer(rt ~ 1 + rt_manipulation * rtconf_manipulation * coherence + (1 + rt_manipulation * rtconf_manipulation|sub), data = df_correct)

anova(RT_5, RT_6)  # Significant

# Model assumptions -> violated -> log transform rt

plot(RT_6)
RT_6_resid <- resid(RT_6)
RT_6_fit <- fitted(RT_6)
qqnorm(RT_6_resid)
qqline(RT_6_resid)
df_temp <- data.frame(cbind(RT_6_fit, RT_6_resid))
ggplot(df_temp, aes(x = RT_6_fit, y = RT_6_resid)) + geom_point() + geom_smooth(se = F) + geom_hline(aes(yintercept=0))

# Log transform

df_correct$rt_log <- log(df_correct$rt)

df_correct$rt_log <- scale(df_correct$rt_log, scale = FALSE) 

# Model selection with log transformed rt

RT_7 <- lmer(rt_log ~ 1 + rt_manipulation * rtconf_manipulation * coherence + (1|sub), data = df_correct)
RT_8 <- lmer(rt_log ~ 1 + rt_manipulation * rtconf_manipulation * coherence + (1 + rt_manipulation|sub), data = df_correct)
RT_9 <- lmer(rt_log ~ 1 + rt_manipulation * rtconf_manipulation * coherence + (1 + rtconf_manipulation|sub), data = df_correct)
RT_10 <- lmer(rt_log ~ 1 + rt_manipulation * rtconf_manipulation * coherence + (1 + coherence|sub), data = df_correct)  # Singular fit

anova(RT_7, RT_8)  # Significant
anova(RT_7, RT_9)  # Significant

RT_11 <- lmer(rt_log ~ 1 + rt_manipulation * rtconf_manipulation * coherence + (1 + rt_manipulation + rtconf_manipulation|sub), data = df_correct)

anova(RT_8, RT_11)  # Significant
anova(RT_9, RT_11)  # Significant

RT_12 <- lmer(rt_log ~ 1 + rt_manipulation * rtconf_manipulation * coherence + (1 + rt_manipulation * rtconf_manipulation|sub), data = df_correct)

anova(RT_11, RT_12)  # Significant

# Model assumptions

plot(RT_12)
RT_12_resid <- resid(RT_12)
RT_12_fit <- fitted(RT_12)
qqnorm(RT_12_resid)
qqline(RT_12_resid)
df_temp <- data.frame(cbind(RT_12_fit, RT_12_resid))
ggplot(df_temp, aes(x = RT_12_fit, y = RT_12_resid)) + geom_point() + geom_smooth(se = F) + geom_hline(aes(yintercept=0))

# Multicollinearity - VIF

# https://stackoverflow.com/questions/26633483/collinearity-after-accounting-for-random-mixed-effects
vif.lme <- function (fit) {  
  ## adapted from rms::vif
  v <- vcov(fit)
  nam <- names(fixef(fit))
  ## exclude intercepts
  ns <- sum(1 * (nam == "Intercept" | nam == "(Intercept)"))
  if (ns > 0) {
    v <- v[-(1:ns), -(1:ns), drop = FALSE]
    nam <- nam[-(1:ns)] }
  d <- diag(v)^0.5
  v <- diag(solve(v/(d %o% d)))
  names(v) <- nam
  v }

vif.lme(RT_12)

# Model interpretation

anova(RT_12)
confint(RT_12)

plot(effect('rt_manipulation',RT_12))
plot(effect('coherence',RT_12))
plot(effect('rt_manipulation:coherence',RT_12))
plot(effect('rt_manipulation:rtconf_manipulation',RT_12))

data.frame(effect('rt_manipulation:coherence',RT_12))
fixef(RT_12)
ranef(RT_9)


### Effect of manipulations on confidence rating RT ###


# Are random slopes necessary? 

plot1 <- ggplot(df_correct, aes(x = as.factor(rt_manipulation), y = rtconf, group = sub)) +
  stat_smooth(geom='line', alpha=1, se=FALSE, method = 'lm') +
  theme_minimal()
plot2 <- ggplot(df_correct, aes(x = as.factor(rtconf_manipulation), y = rtconf, group = sub)) +
  stat_smooth(geom='line', alpha=1, se=FALSE, method = 'lm') +
  theme_minimal()
plot3 <- ggplot(df_correct, aes(x = coherence, y = rtconf, group = sub)) +
  stat_smooth(geom='line', alpha=1, se=FALSE, method = 'lm') +
  theme_minimal()

grid.newpage()
pushViewport(viewport(layout=grid.layout(1,3)))
vplayout <- function(x,y){
  viewport(layout.pos.row=x, layout.pos.col=y)}
print(plot1, vp=vplayout(1,1))
print(plot2, vp=vplayout(1,2))
print(plot3, vp=vplayout(1,3))

# Model comparisons (through likelihood ratio tests)

RTconf_1 <- lmer(rtconf ~ 1 + rt_manipulation * rtconf_manipulation * coherence + (1|sub), data = df_correct)
RTconf_2 <- lmer(rtconf ~ 1 + rt_manipulation * rtconf_manipulation * coherence + (1 + rt_manipulation|sub), data = df_correct)
RTconf_3 <- lmer(rtconf ~ 1 + rt_manipulation * rtconf_manipulation * coherence + (1 + rtconf_manipulation|sub), data = df_correct)
RTconf_4 <- lmer(rtconf ~ 1 + rt_manipulation * rtconf_manipulation * coherence + (1 + coherence|sub), data = df_correct)  # Fails to converge

anova(RT_1, RT_2)  # Significant
anova(RT_1, RT_3)  # Significant



# Model assumptions

plot(RTconf_12)
RTconf_12_resid <- resid(RTconf_12)
RTconf_12_fit <- fitted(RTconf_12)
qqnorm(RTconf_12_resid)
qqline(RTconf_12_resid)
df_temp <- data.frame(cbind(RTconf_12_fit, RTconf_12_resid))
ggplot(df_temp, aes(x = RTconf_12_fit, y = RTconf_12_resid)) + geom_point() + geom_smooth(se = F) + geom_hline(aes(yintercept=0))












### Effect of manipulations on accuracy ###










#(1) The linearity assumption
# & (2) The homogeneity assumption; use Levene test to check this out!
Plot.Model <- plot(RT_2) #creates a fitted vs residual plot
Plot.Model ## you want an even spread around zero.
## for the homogeneity assumption you can test it with:
leveneTest(residuals(RT_2) ~ df$sub)

leveneTest(model1resid ~ df$sub)

#(3) The residuals should be normally distributed

model1resid <- resid(RT_2)
model1fit <- fitted(RT_2)
qqnorm(model1resid)
qqline(model1resid)

# you can log transform it or look for an appropriate model
df$rt.log <- log(df$rt)
df$rt.scaled <- scale(df$rt)






#anova(modelA_3, modelA_4)

#modelA_5 <- lmer(rt ~ 1 + rt_manipulation * rtconf_manipulation + rt_manipulation * coherence + (1 + rt_manipulation + rtconf_manipulation + coherence|sub), REML = FALSE, data = df)  # Adding manipulations
#summary(modelA_5)
#confint(modelA_5)

#anova(modelA_4, modelA_5

# Assumptions

modelA_4_resid <- resid(modelA_4)
modelA_4_fit <- fitted(modelA_4)
qqnorm(modelA_4_resid)
qqline(modelA_4_resid)
df_temp <- data.frame(cbind(modelA_4_fit, modelA_4_resid))
ggplot(df_temp, aes(x = modelA_4_fit, y = modelA_4_resid)) + geom_point() + geom_smooth(se = F) + geom_hline(aes(yintercept=0))


# (b) Faster confidence reaction times and less accurate confidence ratings 
# (i.e., confidence ratings that are less precise in the prediction of accuracy)
# when participants are asked to give quick confidence ratings 
# (vs think carefully about their ratings), 
# with no effect on decision accuracy and decision reaction times.



