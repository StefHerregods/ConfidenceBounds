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

# Effect on decision reaction time

modelA_1 <- lmer(rt ~ 1 + (1|sub), REML = FALSE, data = df)  # Intercept only model
summary(modelA_1)
confint(modelA_1)

modelA_2 <- lmer(rt ~ 1 + rt_manipulation + rtconf_manipulation + (1|sub), REML = FALSE, data = df)  # Adding manipulations
summary(modelA_2)
confint(modelA_2)

anova(modelA_1, modelA_2)

modelA_3 <- lmer(rt ~ 1 + rt_manipulation + rtconf_manipulation + (1 + rt_manipulation + rtconf_manipulation|sub), REML = FALSE, data = df)  # Adding manipulations
summary(modelA_3)
confint(modelA_3)


# (b) Faster confidence reaction times and less accurate confidence ratings 
# (i.e., confidence ratings that are less precise in the prediction of accuracy)
# when participants are asked to give quick confidence ratings 
# (vs think carefully about their ratings), 
# with no effect on decision accuracy and decision reaction times.


# (c) Faster and more accurate decisions, and faster and more accurate 
# confidence ratings in easier trials than in more difficult trials.