# Internship project: ConfidenceBounds (2021-2022)
# Script contains the mixed effect models code used to check hypothesis (a), (b) and 
# (c) of experiment 1.

# Questions:
# normalize RT? log of rt?
# REML = FALSE or TRUE
# logistic for binary outcome variables
# coherence as 1, 2, 3?


# Activating packages

library(lme4)
library(lmerTest)
library(dplyr)
library(ggplot2)
library(grid)

# Setting working directory

setwd('C:\\Users\\herre\\Desktop\\Internship\\Results\\Exp1_Results')

# Loading data

df <- read.csv(file = "Exp1_data_viable.csv")


### Effect of manipulations on decision RT ###


# Only use data with correct decisions

df_correct <- subset(df, cor == 1)

# Are random slopes necessary?

plot1 <- ggplot(df_correct, aes(x = as.factor(rt_manipulation), y = rt, group = sub)) +
  stat_smooth(geom='line', alpha=1, se=FALSE, method = 'lm') +
  theme_minimal()
plot2 <- ggplot(df_correct, aes(x = as.factor(rtconf_manipulation), y = rt, group = sub)) +
  stat_smooth(geom='line', alpha=1, se=FALSE, method = 'lm') +
  theme_minimal()
plot3 <- ggplot(df_correct, aes(x = coherence, y = rt, group = sub)) +
  stat_smooth(geom='line', alpha=1, se=FALSE, method = 'lm') +
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
RT_4 <- lmer(rt ~ 1 + rt_manipulation * rtconf_manipulation * coherence + (1 + coherence|sub), data = df_correct)

anova(RT_1, RT_2) # significant
anova(RT_1, RT_3) # significant
anova(RT_1, RT_4) # significant

RT_5 <- lmer(rt ~ 1 + rt_manipulation * rtconf_manipulation * coherence + (1 + rt_manipulation + rtconf_manipulation|sub), data = df_correct)

anova(RT_2, RT_5) # significant
anova(RT_3, RT_5) # significant

RT_6 <- lmer(rt ~ 1 + rt_manipulation * rtconf_manipulation * coherence + (1 + rtconf_manipulation + coherence|sub), data = df_correct)

anova(RT_3, RT_6) # significant
anova(RT_4, RT_6) # significant

RT_7 <- lmer(rt ~ 1 + rt_manipulation * rtconf_manipulation * coherence + (1 + coherence + rt_manipulation|sub), data = df_correct)

anova(RT_4, RT_7) # significant
anova(RT_2, RT_7) # significant

RT_8 <- lmer(rt ~ 1 + rt_manipulation * rtconf_manipulation * coherence + (1 + coherence + rt_manipulation + rtconf_manipulation|sub), data = df_correct)

anova(RT_5, RT_8) # significant
anova(RT_5, RT_8) # significant
anova(RT_5, RT_8) # significant


# Any model including all 3 random slopes + random slop for an interaction fails to converge
# The model below converges, but does not include coherence
RT_9 <- lmer(rt ~ 1 + rt_manipulation * rtconf_manipulation * coherence + (1 + rt_manipulation * rtconf_manipulation|sub), data = df_correct)

anova(RT_8, RT_9)   # Equally complex model -> no p-value (0 degrees of freedom)
                    # RT_8 better AIC (lower), BIC (lower), log-likelihood (higher)





Anova(RT_1) # binary variables (look for literature)
anova(RT_1) # continuous variables (look for literature)
anova(RT_base, RT_1)

shapiro.test(RT_2)



### Effect of manipulations on confidence rating RT ###

# Which data set to use?

# Are random slopes necessary? !!!based on correct data only?!

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


### Effect of manipulations on ??? ###







RT_2 <- lmer(rt ~ 1 + rt_manipulation * rtconf_manipulation * coherence + (1|sub), data = df)

RT_base <- lmer(rt ~ 1 + rt_manipulation * rtconf_manipulation * coherence + (1 + rt_manipulation + rtconf_manipulation + coherence|sub), data = df)
RT_base <- lmer(rt.log ~ 1 + rt_manipulation * rtconf_manipulation * coherence + (1 + rt_manipulation + rtconf_manipulation + coherence|sub), data = df)
RT_base <- lmer(rt.scaled ~ 1 + rt_manipulation * rtconf_manipulation * coherence + (1 + rt_manipulation + rtconf_manipulation + coherence|sub), data = df)

RT_base <- glm(rt ~ 1 + rt_manipulation * rtconf_manipulation * coherence + (1 + rt_manipulation * rtconf_manipulation * coherence|sub), family=Gamma	(link = "inverse"), data=df)


df$cor.factor <- as.factor(df$cor)
RT_base <- glm(cor.factor ~ 1 + rt_manipulation * rtconf_manipulation * coherence + (1 + rt_manipulation + rtconf_manipulation + coherence|sub), family = binomial	(link = "logit"), data = df)




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




# Effect on decision reaction time

modelA_1 <- lmer(rt ~ 1 + (1|sub), REML = FALSE, data = df)  # Intercept only model
summary(modelA_1)
confint(modelA_1)

modelA_2 <- lmer(rt ~ 1 + rt_manipulation + rtconf_manipulation + coherence + (1|sub), REML = FALSE, data = df)  # Adding manipulations
summary(modelA_2)
confint(modelA_2)

anova(modelA_1, modelA_2)

modelA_3 <- lmer(rt ~ 1 + rt_manipulation * rtconf_manipulation  * coherence + (1|sub), REML = FALSE, data = df)  # Adding manipulations
summary(modelA_3)
confint(modelA_3)
anova(modelA_3)

anova(modelA_2, modelA_3)

modelA_4 <- lmer(rt ~ 1 + rt_manipulation * rtconf_manipulation + rt_manipulation * coherence + (1|sub), REML = FALSE, data = df)  # Adding manipulations
summary(modelA_4)
confint(modelA_4)

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


# Exploration

# Random slopes necessary?
df %>%
  ggplot(aes(x = rtconf_manipulation, y = rtconf)) +
  geom_point() +
  geom_point(stat = "summary", fun = "mean", colour = 'red', size = 3) +
  facet_wrap(vars(sub))

# Effect on confidence rating reaction time

modelB_1 <- lmer(rtconf ~ 1 + (1|sub), REML = FALSE, data = df)  # Intercept only model
summary(modelB_1)
confint(modelB_1)

modelB_2 <- lmer(rtconf ~ 1 + rt_manipulation + rtconf_manipulation + coherence + (1|sub), REML = FALSE, data = df)  # Adding manipulations
summary(modelB_2)
confint(modelB_2)

anova(modelB_1, modelB_2)

modelB_3 <- lmer(rtconf ~ 1 + rt_manipulation * rtconf_manipulation * coherence + (1|sub), REML = FALSE, data = df)  # Adding manipulations
summary(modelB_3)
confint(modelB_3)

anova(modelB_2, modelB_3)

modelB_4 <- lmer(rtconf ~ 1 + rt_manipulation + rtconf_manipulation + (1 + rt_manipulation + rtconf_manipulation|sub), REML = FALSE, data = df)  # Adding manipulations
summary(modelB_4)
confint(modelB_4)

anova(modelB_2, modelB_4)


# (c) Faster and more accurate decisions, and faster and more accurate 
# confidence ratings in easier trials than in more difficult trials.
