# Internship project: ConfidenceBounds (2021-2022)
# Script contains the mixed models code used to check hypothesis (a), (b) and 
# (c) of experiment 1.


# Activating packages

library(lme4)


# Loading data

setwd('C:\\Users\\herre\\OneDrive\\Bureaublad\\Internship\\Results\\Exp1_results_tot')
df <- NULL
for (i in 1:40){
  file_name <- paste0("DotsTask_sub",i,".csv",collapse="")
  if (file.exists(file_name)){
    data_temp <- read.csv(file=file_name)
    data_temp$subject <- i
    df <- rbind(data_temp,df)
  }
}

# Remove training trials

df <- df[df$block > 3,] 

# Remove too slow trials

df <- df[df$slow_trial == 0,]

# !!! Remove too slow confidence rating trials !!!

df <- df[df$rtconf < 10,]

# Separating decision and confidence rating manipulations

for (i in 1:nrow(df)){
  if (df$manipulation[i] %in% c("AccAcc", "AccFast")){
    df$rt_manipulation[i] <- "Acc"
  } else {
    df$rt_manipulation[i] <- "Fast"
  }
  if (df$manipulation[i] %in% c("AccAcc", "FastAcc")){
    df$rtconf_manipulation[i] <- "Acc"
  } else {
    df$rtconf_manipulation[i] <- "Fast"
  }  
}


# Hypothesis (a)
# Faster decision reaction times and lower accuracy when participants are 
# asked to respond quickly (vs accurately), 
# without an effect on confidence reaction times and confidence ratings.


# Exploration

# Random slopes necessary?
df %>%
  ggplot(aes(x = rt_manipulation, y = rt)) +
  geom_point() +
  geom_point(stat = "summary", fun = "mean", colour = 'red', size = 3) +
  facet_wrap(vars(sub))

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
