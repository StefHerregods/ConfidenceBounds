# Internship project: ConfidenceBounds (2021-2022)
# Script contains the mixed effect models code used to analyze RT's, confidence RT's and accuracy

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

# https://stackoverflow.com/questions/26633483/collinearity-after-accounting-for-random-mixed-effects
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

## 

write.excel <- function(x,row.names=FALSE,col.names=FALSE,...) {
  write.table(x,"clipboard",sep="\t",row.names=row.names,col.names=col.names,...)
}

## Setting working directory
if(Sys.info()['user']=="u0136938"){
  setwd('C:/Users/u0136938/OneDrive - KU Leuven/Documents/Projecten/Stef - sato op confidence/ConfidenceBounds/Data')
}else{
  setwd('C:\\Users\\herre\\Desktop\\Internship\\Results\\Exp1_Results')
}
  
df <- read.csv(file = "Exp1_data_viable.csv")

## Transform variables into factors

df <- df %>% mutate(rt_manipulation = as.factor(ifelse(df$manipulation %in% c("AccAcc", "AccFast"), 1, 0)),
                    rtconf_manipulation = as.factor(ifelse(df$manipulation %in% c("AccAcc", "FastAcc"), 1, 0)),
                    coherence = as.factor(coherence),
                    cor_numeric = cor,
                    cor = as.factor(cor),
                    cj = as.factor(cj))


# Decision RT ----


## Based on correct data only

df_correct <- subset(df, cor == 1)
df_incorrect <- subset(df, cor == 0)

## Log transform

df_correct$rt_log <- log(df_correct$rt)
df_correct$rt_log <- scale(df_correct$rt_log, scale = FALSE)

## Are random slopes necessary? -> Yes (but less for coherence)

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

### Multicollinearity - VIF

vif.lme(RT_12)

## Model interpretation

Anova(RT_12)  # For p-values
summary(RT_12)  # For estimates
confint(RT_12, method = 'boot', parm = 'beta_')

plot(effect('rt_manipulation', RT_12))
plot(effect('rtconf_manipulation', RT_12)) 
plot(effect('coherence', RT_12))
plot(effect('rt_manipulation:coherence', RT_12))
temp <- data.frame(effect('rt_manipulation:coherence', RT_12))
temp$fit[temp$rt_manipulation==1]-temp$fit[temp$rt_manipulation==0]
plot(effect('rt_manipulation:rtconf_manipulation', RT_12))

## Further analysis interactions

contrast.matrix <- rbind("rt_manipulation1:rtconf_manipulation1" = c(1, 1, 0, 1, 1, 1, 0, 0, 0, 0, 0, 0),
                         "rt_manipulation1:coherence0.4" = c(1, 1, 0, 1, 1, 0, 0, 1, 0, 0, 0, 0),
                         "rt_manipulation1:coherence0.4" = c(1, 1, 0, 1, 1, 1, 0, 1, 0, 0, 0, 0))
summary(multcomp::glht(RT_12, linfct = contrast.matrix))

#data.frame(effect('rt_manipulation:coherence', RT_12))
fixef(RT_12)
ranef(RT_12)



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

plot(effect('rt_manipulation', RTconf_6))  
plot(effect('rtconf_manipulation', RTconf_6)) 
plot(effect('coherence', RTconf_6))
plot(effect('rt_manipulation:rtconf_manipulation', RTconf_6))

#data.frame(effect('rt_manipulation:coherence', RTconf_6))
fixef(RTconf_6)
ranef(RTconf_6)


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

plot(effect('rt_manipulation', cj_6)) 
plot(effect('rtconf_manipulation', cj_6))   
plot(effect('coherence', cj_6))
plot(effect('rt_manipulation:coherence', cj_6))
data.frame(effect('rt_manipulation:coherence', cj_6))
data.frame(effect('rt_manipulation:rtconf_manipulation', cj_6)) 


#Confidence resolution via type II AUC
subs <- unique(df$sub);N <- length(subs)
roc <- data.frame(matrix(NA,N,4));names(roc) <- unique(df$manipulation) 
for(i in 1:N){
  tempDat <- subset(df,sub==subs[i])
  for(c in unique(df$manipulation)){
    temp <- subset(tempDat,manipulation==c)
    roc[i,c] <- pROC::auc(as.numeric(temp$cor),as.numeric(temp$cj))
  }
}

#code for error bars
error.bar <- function(x, y, upper, lower=upper, length=0,...){
  if(length(x) != length(y) | length(y) !=length(lower) | length(lower) != length(upper))
    stop("vectors must be same length")
  arrows(x,y+upper, x, y-lower, angle=90, code=3, length=length, ...)
}
#plot
locX <- t(jitter(matrix(rep(1:2,N),2,N),.5)) #jitter for individual points
plot(colMeans(roc),frame=F,cex.lab=1.5,type='n',ylim=range(roc),xlim=c(.5,2.5),ylab="Type II AUC",xlab="Choice SAT",xaxt='n');axis(1,at=1:2,labels=c("Fast decision","Accurate Decisions"))
for(i in 1:N) points(locX[i,]-.15,roc[i,1:2],pch=21,bg=rgb(202/255,60/255,37/255,.3),col="white",cex=1.5)
for(i in 1:N) points(locX[i,]+.15,roc[i,3:4],pch=21,bg=rgb(255/255,166/255,48/255,.3),col="white",cex=1.5)
lines(1:2-.15,colMeans(roc)[1:2],type='b',lty=2,col="white",bg=rgb(202/255,60/255,37/255,1),pch=21,lwd=2,cex=2)
lines(1:2+.15,colMeans(roc)[3:4],type='b',lty=2,col="white",bg=rgb(255/255,166/255,48/255,1),pch=21,lwd=2,cex=2)
error.bar(c(1:2-.15,1:2+.15),colMeans(roc),sapply(roc,sd)/sqrt(N),col=c(rep(rgb(202/255,60/255,37/255,1),2),rep(rgb(255/255,166/255,48/255,1),2)),lwd=2)

roc_long <- reshape::melt(roc)
roc_long$sub <- rep(1:N,4)
roc_long$choice <- "acc"
roc_long$choice[roc_long$variable=="FastFast"] <- "fast"
roc_long$choice[roc_long$variable=="FastAcc"] <- "fast"
roc_long$conf <- "acc"
roc_long$conf[roc_long$variable=="FastFast"] <- "fast"
roc_long$conf[roc_long$variable=="AccFast"] <- "fast"

roc_long$conf <- factor(roc_long$conf)
roc_long$choice <- factor(roc_long$choice)

res.aov <- rstatix::anova_test(
  data = roc_long, dv = value, wid = sub,
  within = c(choice,conf)
)
res.aov











#Confidence resolution via type II AUC
subs <- unique(df$sub);N <- length(subs)
roc <- data.frame(matrix(NA,N,4));names(roc) <- unique(df$manipulation) 
for(i in 1:N){
  tempDat <- subset(df,sub==subs[i])
  for(c in unique(df$manipulation)){
    temp <- subset(tempDat,manipulation==c)
    roc[i,c] <- pROC::auc(as.numeric(temp$cor),as.numeric(temp$cj))
  }
}
roc_long <- reshape::melt(roc)
roc_long <- roc_long %>% mutate(rt_manipulation = as.factor(ifelse(roc_long$variable %in% c("AccAcc", "AccFast"), 1, 0)),
                                rtconf_manipulation = as.factor(ifelse(roc_long$variable %in% c("AccAcc", "FastAcc"), 1, 0))) %>%
  group_by(rt_manipulation, rtconf_manipulation) %>%
  mutate(roc_mean = mean(value),
         roc_sd = sd(value))






roc_plot <- ggplot(data = roc_long, aes(x = strtoi(rt_manipulation), y = value, color = as.factor(rtconf_manipulation))) +
  geom_errorbar(aes(ymin = roc_mean - roc_sd / sqrt(40), ymax = roc_mean + roc_sd / sqrt(40), group = as.factor(rtconf_manipulation)), position = position_dodge(width = 0.5), size = 1, width = 0) +
  geom_point(size = 2.5, stroke = 1, shape = 16, alpha = 0.2, position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.5)) +
  stat_summary(geom = "line", size = 1, fun = "mean", position = position_dodge(width = 0.5)) +
  stat_summary(geom = "point", size = 2.5, fun = "mean", position = position_dodge(width = 0.5)) +
  scale_x_continuous(labels = c('"Make fast\ndecisions"', '"Make accurate\n decisions"'),breaks = c(0, 1)) +
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

ggsave(filename = 'test.png',
       plot = roc_plot,
       device = 'png',
       width = 9,
       height = 7,
       units = 'cm')


roc_long <- reshape::melt(roc)
roc_long$sub <- rep(1:N,4)
roc_long$choice <- "acc"
roc_long$choice[roc_long$variable=="FastFast"] <- "fast"
roc_long$choice[roc_long$variable=="FastAcc"] <- "fast"
roc_long$conf <- "acc"
roc_long$conf[roc_long$variable=="FastFast"] <- "fast"
roc_long$conf[roc_long$variable=="AccFast"] <- "fast"

roc_long$conf <- factor(roc_long$conf)
roc_long$choice <- factor(roc_long$choice)

res.aov <- rstatix::anova_test(
  data = roc_long, dv = value, wid = sub,
  within = c(choice,conf)
)
res.aov







