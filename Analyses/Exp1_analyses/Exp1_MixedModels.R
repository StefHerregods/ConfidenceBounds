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

RT_1 <- lmer(rt ~ 1 + rt_manipulation * rtconf_manipulation * coherence + (1|sub), data = df_correct)
RT_2 <- lmer(rt ~ 1 + rt_manipulation * rtconf_manipulation * coherence + (1 + rt_manipulation|sub), data = df_correct)
RT_3 <- lmer(rt ~ 1 + rt_manipulation * rtconf_manipulation * coherence + (1 + rtconf_manipulation|sub), data = df_correct)
RT_4 <- lmer(rt ~ 1 + rt_manipulation * rtconf_manipulation * coherence + (1 + coherence|sub), data = df_correct)  # Fails to converge


allFit(show.meth.tab=TRUE)
allFit_RT_4 <- allFit(RT_12)
summary <- summary(allFit_RT_4)
summary$which.OK


is.OK <- sapply(allFit_RT_4, is, "merMod")
allFit_RT_4.OK <- allFit_RT_4[is.OK]
lapply(allFit_RT_4.OK,function(x) x@optinfo$conv$lme4$messages)





anova(RT_1, RT_2)  # Significant
anova(RT_1, RT_3)  # Significant

RT_5 <- lmer(rt ~ 1 + rt_manipulation * rtconf_manipulation * coherence + (1 + rt_manipulation + rtconf_manipulation|sub), data = df_correct)

anova(RT_2, RT_5)  # Significant

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
RT_10 <- lmer(rt_log ~ 1 + rt_manipulation * rtconf_manipulation * coherence + (1 + coherence|sub), data = df_correct)  

anova(RT_7, RT_8)  # Significant
anova(RT_7, RT_9)  # Significant
anova(RT_7, RT_10)  # Significant

RT_11 <- lmer(rt_log ~ 1 + rt_manipulation * rtconf_manipulation * coherence + (1 + rt_manipulation + coherence|sub), data = df_correct)

anova(RT_8, RT_11)  # Significant

RT_12 <- lmer(rt_log ~ 1 + rt_manipulation * rtconf_manipulation * coherence + (1 + rt_manipulation + rtconf_manipulation + coherence|sub), data = df_correct)  # Singular fit

RT_13 <- lmer(rt_log ~ 1 + rt_manipulation * rtconf_manipulation * coherence + (1 + rt_manipulation * coherence|sub), data = df_correct)  # Singular fit

# Model assumptions

plot(RT_11)
RT_11_resid <- resid(RT_11)
RT_11_fit <- fitted(RT_11)
qqnorm(RT_11_resid)
qqline(RT_11_resid)
df_temp <- data.frame(cbind(RT_11_fit, RT_11_resid))
ggplot(df_temp, aes(x = RT_11_fit, y = RT_11_.resid)) + geom_point() + geom_smooth(se = F) + geom_hline(aes(yintercept=0))

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

vif.lme(RT_11)

# Model interpretation

anova(RT_11)
confint(RT_11)

plot(effect('rt_manipulation', RT_11))
plot(effect('rtconf_manipulation', RT_11))
plot(effect('coherence', RT_11))
plot(effect('rt_manipulation:coherence', RT_11))
plot(effect('rt_manipulation:rtconf_manipulation', RT_11))

data.frame(effect('rt_manipulation:coherence', RT_11))
fixef(RT_11)
ranef(RT_11)


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

RTconf_1 <- lmer(rtconf_log ~ 1 + rt_manipulation * rtconf_manipulation * coherence + (1|sub), data = df_correct)
RTconf_2 <- lmer(rtconf_log ~ 1 + rt_manipulation * rtconf_manipulation * coherence + (1 + rt_manipulation|sub), data = df_correct)
RTconf_3 <- lmer(rtconf_log ~ 1 + rt_manipulation * rtconf_manipulation * coherence + (1 + rtconf_manipulation|sub), data = df_correct) 
RTconf_4 <- lmer(rtconf_log ~ 1 + rt_manipulation * rtconf_manipulation * coherence + (1 + coherence|sub), data = df_correct)

anova(RT_1, RT_2)  # Significant
anova(RT_1, RT_3)  # Significant
anova(RT_1, RT_4)  # Significant

RTconf_5 <- lmer(rtconf_log ~ 1 + rt_manipulation * rtconf_manipulation * coherence + (1 + rt_manipulation + coherence|sub), data = df_correct)


# TO DO !!!!

# Model assumptions

plot(RTconf_12)
RTconf_12_resid <- resid(RTconf_12)
RTconf_12_fit <- fitted(RTconf_12)
qqnorm(RTconf_12_resid)
qqline(RTconf_12_resid)
df_temp <- data.frame(cbind(RTconf_12_fit, RTconf_12_resid))
ggplot(df_temp, aes(x = RTconf_12_fit, y = RTconf_12_resid)) + geom_point() + geom_smooth(se = F) + geom_hline(aes(yintercept=0))













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

# Transform accuracy into factor

df$cor <- as.factor(df$cor)

# Model comparisons (through likelihood ratio tests)

Cor_1 <- glmer(cor ~ 1 + rt_manipulation * rtconf_manipulation * coherence + (1|sub), data = df, family = binomial)
Cor_2 <- glmer(cor ~ 1 + rt_manipulation * rtconf_manipulation * coherence + (1 + rt_manipulation|sub), data = df, family = binomial)
Cor_3 <- glmer(cor ~ 1 + rt_manipulation * rtconf_manipulation * coherence + (1 + rtconf_manipulation|sub), data = df, family = binomial)  
Cor_4 <- glmer(cor ~ 1 + rt_manipulation * rtconf_manipulation * coherence + (1 + coherence|sub), data = df, family = binomial) 

anova(Cor_1, Cor_2)  # Significant
anova(Cor_1, Cor_3)  # Not significant
anova(Cor_1, Cor_4)  # Significant

RTconf_5 <- glmer(cor ~ 1 + rt_manipulation * rtconf_manipulation * coherence + (1 + coherence + rt_manipulation|sub), data = df, family = binomial)  # Fails to converge

anova(Cor_2, Cor_4)  # Significant; better BIC, AIC, log likelihood for Cor_4

# Model assumptions

plot(Cor_4)
Cor_4_resid <- resid(Cor_4)
Cor_4_fit <- fitted(Cor_4)
qqnorm(Cor_4_resid)
qqline(Cor_4_resid)
df_temp <- data.frame(cbind(Cor_4_fit, Cor_4_resid))
ggplot(df_temp, aes(x = Cor_4_fit, y = Cor_4_resid)) + geom_point() + geom_smooth(se = F) + geom_hline(aes(yintercept=0))

vif.lme(Cor_4)
  















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
resid_panel(m_conf_slopes) 

# That's why another approach is needed using binned residuals
# If most of the dots fall within the area, then it's okay
binnedplot(fitted(m_conf_slopes), 
           residuals(m_conf_slopes, type = "response"), 
           nclass = NULL, 
           xlab = "Expected Values", 
           ylab = "Average residual", 
           main = "Binned residual plot", 
           cex.pts = 0.8, 
           col.pts = 1, 
           col.int = "gray")



# CHECK LINEAR RELATIONSHIP LOGODDS AND PREDICTORS

## APPROACH 1
probabilities <- predict(m_conf_slopes, type = "response")

# only CONTINUOUS variables
mydata <- data.frame(df_excluded$evidence_scaled,df_excluded$prev_evidence_scaled,df_excluded$prev_conf_scaled)
colnames(mydata) <- c("evidence","prev_evidence","prev_conf")
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



