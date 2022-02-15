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
library(optimx)
library(dfoptim)


# VIF function

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

# Setting working directory

setwd('C:\\Users\\herre\\Desktop\\Internship\\Results\\Exp1_Results')

df <- read.csv(file = "Exp1_data_viable.csv")

# Transform variables into factors

df$rt_manipulation <- ifelse(df$manipulation %in% c("AccAcc", "AccFast"), 1, 0)
df$rt_manipulation <- as.factor(df$rt_manipulation)
df$rtconf_manipulation <- ifelse(df$manipulation %in% c("AccAcc", "FastAcc"), 1, 0)
df$rtconf_manipulation <- as.factor(df$rtconf_manipulation)
df$coherence <- as.factor(df$coherence)
df$cor <- as.factor(df$cor)


### Effect of manipulations on decision RT ###


# Based on correct data only

df_correct <- subset(df, cor == 1)

# Are random slopes necessary? -> Yes (but less for coherence)

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

# Model comparisons (through likelihood ratio tests)

RT_1 <- lmer(rt ~ 1 + rt_manipulation * rtconf_manipulation * coherence + (1|sub), 
             data = df_correct, control = lmerControl(optimizer = "bobyqa"))
RT_2 <- lmer(rt ~ 1 + rt_manipulation * rtconf_manipulation * coherence + (1 + rt_manipulation|sub), 
             data = df_correct, control = lmerControl(optimizer = "bobyqa"))
RT_3 <- lmer(rt ~ 1 + rt_manipulation * rtconf_manipulation * coherence + (1 + rtconf_manipulation|sub), 
             data = df_correct, control = lmerControl(optimizer = "bobyqa"))
RT_4 <- lmer(rt ~ 1 + rt_manipulation * rtconf_manipulation * coherence + (1 + coherence|sub), 
             data = df_correct, control = lmerControl(optimizer = "bobyqa"))  

anova(RT_1, RT_2)  # Significant; BIC 36388
anova(RT_1, RT_3)  # Significant; BIC 37541
anova(RT_1, RT_4)  # Significant; BIC 37470

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
             data = df_correct, control = lmerControl(optimizer = "bobyqa"))  # boundary (singular) fit

# Model assumptions -> violated -> log transform RT

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

RT_7 <- lmer(rt_log ~ 1 + rt_manipulation * rtconf_manipulation * coherence + (1|sub), 
             data = df_correct, control = lmerControl(optimizer = "bobyqa"))
RT_8 <- lmer(rt_log ~ 1 + rt_manipulation * rtconf_manipulation * coherence + (1 + rt_manipulation|sub), 
             data = df_correct, control = lmerControl(optimizer = "bobyqa"))
RT_9 <- lmer(rt_log ~ 1 + rt_manipulation * rtconf_manipulation * coherence + (1 + rtconf_manipulation|sub), 
             data = df_correct, control = lmerControl(optimizer = "bobyqa"))
RT_10 <- lmer(rt_log ~ 1 + rt_manipulation * rtconf_manipulation * coherence + (1 + coherence|sub), 
              data = df_correct, control = lmerControl(optimizer = "bobyqa"))  # Boundary (singular) fit

allFit_RT_10 <- allFit(RT_10)
is.OK <- sapply(allFit_RT_10, is, "merMod")
allFit_RT_10.OK <- allFit_RT_10[is.OK]
lapply(allFit_RT_10.OK, function(x) x@optinfo$conv$lme4$messages)
summary(allFit_RT_10)  # Singular fit for nearly all optimizers

anova(RT_7, RT_8)  # Significant; BIC 24711
anova(RT_7, RT_9)  # Significant; BIC 25857

RT_11 <- lmer(rt_log ~ 1 + rt_manipulation * rtconf_manipulation * coherence + (1 + rt_manipulation + rtconf_manipulation|sub), 
              data = df_correct, control = lmerControl(optimizer = "bobyqa"))

anova(RT_8, RT_11)  # Significant

RT_12 <- lmer(rt_log ~ 1 + rt_manipulation * rtconf_manipulation * coherence + (1 + rt_manipulation * rtconf_manipulation|sub), 
              data = df_correct, control = lmerControl(optimizer = "bobyqa")) 

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

vif.lme(RT_12)

# Model interpretation

Anova(RT_12)
confint(RT_12)

plot(effect('rt_manipulation', RT_12))
plot(effect('rtconf_manipulation', RT_12))  # Not significant
plot(effect('coherence', RT_12))
plot(effect('rt_manipulation:coherence', RT_12))
plot(effect('rt_manipulation:rtconf_manipulation', RT_12))

#data.frame(effect('rt_manipulation:coherence', RT_12))
fixef(RT_12)
ranef(RT_12)


### Effect of manipulations on confidence rating RT ###


# Are random slopes necessary? 

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
vplayout <- function(x,y){
  viewport(layout.pos.row=x, layout.pos.col=y)}
print(plot1, vp=vplayout(1,1))
print(plot2, vp=vplayout(1,2))
print(plot3, vp=vplayout(1,3))

# Log transform

df_correct$rtconf_log <- log(df_correct$rtconf)

df_correct$rtconf_log <- scale(df_correct$rtconf_log, scale = FALSE) 

# Model comparisons (through likelihood ratio tests)

RTconf_1 <- lmer(rtconf_log ~ 1 + rt_manipulation * rtconf_manipulation * coherence + (1|sub), 
                 data = df_correct, control = lmerControl(optimizer = "bobyqa"))
RTconf_2 <- lmer(rtconf_log ~ 1 + rt_manipulation * rtconf_manipulation * coherence + (1 + rt_manipulation|sub), 
                 data = df_correct, control = lmerControl(optimizer = "bobyqa"))
RTconf_3 <- lmer(rtconf_log ~ 1 + rt_manipulation * rtconf_manipulation * coherence + (1 + rtconf_manipulation|sub), 
                 data = df_correct, control = lmerControl(optimizer = "bobyqa")) 
RTconf_4 <- lmer(rtconf_log ~ 1 + rt_manipulation * rtconf_manipulation * coherence + (1 + coherence|sub), 
                 data = df_correct, control = lmerControl(optimizer = "bobyqa"))  # Boundary (singular) fit

allFit_RTconf_4 <- allFit(RTconf_4)
is.OK <- sapply(allFit_RTconf_4, is, "merMod")
allFit_RTconf_4.OK <- allFit_RTconf_4[is.OK]
lapply(allFit_RTconf_4.OK, function(x) x@optinfo$conv$lme4$messages)
summary(allFit_RTconf_4)  # Boundary (singular) fit for nearly all optimizers

anova(RTconf_1, RTconf_2)  # Significant; BIC 47547
anova(RTconf_1, RTconf_3)  # Significant; BIC 44147

RTconf_5 <- lmer(rtconf_log ~ 1 + rt_manipulation * rtconf_manipulation * coherence + (1 + rt_manipulation + rtconf_manipulation|sub), 
                 data = df_correct, control = lmerControl(optimizer = "bobyqa"))

anova(RTconf_3, RTconf_5)  # Significant

RTconf_6 <- lmer(rtconf_log ~ 1 + rt_manipulation * rtconf_manipulation * coherence + (1 + rt_manipulation * rtconf_manipulation|sub), 
                 data = df_correct, control = lmerControl(optimizer = "bobyqa"))

anova(RTconf_5, RTconf_6) # Significant

# Model assumptions

plot(RTconf_6)
RTconf_6_resid <- resid(RTconf_6)
RTconf_6_fit <- fitted(RTconf_6)
qqnorm(RTconf_6_resid)
qqline(RTconf_6_resid)
df_temp <- data.frame(cbind(RTconf_6_fit, RTconf_6_resid))
ggplot(df_temp, aes(x = RTconf_6_fit, y = RTconf_6_resid)) + geom_point() + geom_smooth(se = F) + geom_hline(aes(yintercept=0))

# Multicollinearity - VIF

vif.lme(RTconf_6)

# Model interpretation

anova(RTconf_6)
confint(RTconf_6)

plot(effect('rt_manipulation', RTconf_6))  # Not significant
plot(effect('rtconf_manipulation', RTconf_6)) 
plot(effect('coherence', RTconf_6))
plot(effect('rt_manipulation:rtconf_manipulation', RTconf_6))

#data.frame(effect('rt_manipulation:coherence', RTconf_6))
fixef(RTconf_6)
ranef(RTconf_6)


### Effect of manipulations on accuracy ###


# Are random slopes necessary? 

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
vplayout <- function(x,y){
  viewport(layout.pos.row=x, layout.pos.col=y)}
print(plot1, vp=vplayout(1,1))
print(plot2, vp=vplayout(1,2))
print(plot3, vp=vplayout(1,3))

# Model comparisons (through likelihood ratio tests)

Cor_1 <- glmer(cor ~ 1 + rt_manipulation * rtconf_manipulation * coherence + (1|sub), 
               data = df, family = binomial, control = glmerControl(optimizer = "bobyqa"))
Cor_2 <- glmer(cor ~ 1 + rt_manipulation * rtconf_manipulation * coherence + (1 + rt_manipulation|sub), 
               data = df, family = binomial, control = glmerControl(optimizer = "bobyqa"))
Cor_3 <- glmer(cor ~ 1 + rt_manipulation * rtconf_manipulation * coherence + (1 + rtconf_manipulation|sub), 
               data = df, family = binomial, control = glmerControl(optimizer = "bobyqa"))  
Cor_4 <- glmer(cor ~ 1 + rt_manipulation * rtconf_manipulation * coherence + (1 + coherence|sub), 
               data = df, family = binomial, control = glmerControl(optimizer = "bobyqa"))  # Boundary (singular) fit

allFit_Cor_4 <- allFit(Cor_4)
is.OK <- sapply(allFit_Cor_4, is, "merMod")
allFit_Cor_4.OK <- allFit_Cor_4[is.OK]
lapply(allFit_Cor_4.OK, function(x) x@optinfo$conv$lme4$messages)
summary(allFit_Cor_4)  # Boundary (singular) fit for nearly all optimizers

anova(Cor_1, Cor_2)  # Significant
anova(Cor_1, Cor_3)  # Not significant

Cor_5 <- glmer(cor ~ 1 + rt_manipulation * rtconf_manipulation * coherence + (1 + rt_manipulation + rtconf_manipulation|sub), 
                  data = df, family = binomial, control = glmerControl(optimizer = "bobyqa"))  # Fails to converge

anova(Cor_2, Cor_5)  # Not significant

# Model assumptions

plot(Cor_2)
Cor_4_resid <- resid(Cor_4)
Cor_4_fit <- fitted(Cor_4)
qqnorm(Cor_4_resid)
qqline(Cor_4_resid)
df_temp <- data.frame(cbind(Cor_4_fit, Cor_4_resid))
ggplot(df_temp, aes(x = Cor_4_fit, y = Cor_4_resid)) + geom_point() + geom_smooth(se = F) + geom_hline(aes(yintercept=0))

vif.lme(Cor_2)
  


Anova(Cor_2)
confint(Cor_2)











library(aod)
library(ggpubr)
library(arm)
library(car)
library(ggplot2)
library(ggResidpanel)
library(lme4)
library(tidyr)

### ASSUMPTIONS MULTILEVEL LOGISTIC REGRESSION

#independence of errors -> plot residuals over time
#linearity between log odds and continuous predictors
#absence of multicollinearity -> VIF
#lack of strongly influential outliers

# SO NO homoscedasticity nor normally distributed residuals

# In this example 'm_conf_slopes' is our mixed model




# INDEPENDENCE OF ERRORS
## This means that when you plot your residuals there shouldn't be a trend (just a straigth line)
## Otherwise there is still some uncaptured variance, causing the errors to be dependent (predictable)

# for linear mixed models you can just look at the left upper panel but with logistic regression this will look odd 
resid_panel(Cor_4) 

# That's why another approach is needed using binned residuals
# If most of the dots fall within the area, then it's okay
binnedplot(fitted(Cor_4), 
           residuals(Cor_4, type = "response"), 
           nclass = NULL, 
           xlab = "Expected Values", 
           ylab = "Average residual", 
           main = "Binned residual plot", 
           cex.pts = 0.8, 
           col.pts = 1, 
           col.int = "gray")



# CHECK LINEAR RELATIONSHIP LOGODDS AND PREDICTORS

## APPROACH 1
probabilities <- predict(Cor_4, type = "response")

# only CONTINUOUS variables
mydata <- data.frame(df$rt_manipulation, df$rtconf_manipulation, df$coherence)
colnames(mydata) <- c("rt_manipulation", "rtconf_manipulation", "coherence")
predictors <- colnames(mydata)
# Bind the logit and tidying the data for plot
mydata <- mydata %>%
  mutate(logit = log(probabilities/(1-probabilities))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)

# You should see a more or less linear relation
ggplot(mydata, aes(logit,predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess",se=FALSE) + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y")

## APPROACH 2

# Check linearity between logodds and continuous predictors with Box Tidwell transformation
# Just add interaction between predictor and ln(predictor) in your model
# If you have negative values, then make then all positive (just add constant to all the values to make then positive)
# If not significant, linearity is okay!

df_boxtidwell <- df_excluded

# ln only possible for values > 0 so transform variables
df_boxtidwell$prev_conf_scaled_bt <- 1 + df_boxtidwell$prev_conf_scaled + abs(min(df_boxtidwell$prev_conf_scaled))
df_boxtidwell$evidence_scaled_bt <- 1 + df_boxtidwell$evidence_scaled + abs(min(df_boxtidwell$evidence_scaled)) 
df_boxtidwell$prev_evidence_scaled_bt <- 1 + df_boxtidwell$prev_evidence_scaled + abs(min(df_boxtidwell$prev_evidence_scaled))

m_boxtidwell <- glmer(data=df_boxtidwell, resp ~ 
                        
                        evidence_scaled_bt +
                        prev_evidence_scaled_bt +
                        prev_resp * prev_conf_scaled_bt  +
                        
                        evidence_scaled_bt:log(evidence_scaled_bt) +
                        prev_evidence_scaled_bt:log(prev_evidence_scaled_bt) +
                        prev_conf_scaled_bt:log(prev_conf_scaled_bt) +
                        
                        (1|sub),
                      family = binomial,control=glmerControl(optimizer='bobyqa',optCtrl = list(maxfun=10000000)))

Anova(m_boxtidwell)

# ABSENCE OF MULTICOLLINEARITY
# Just check VIF



