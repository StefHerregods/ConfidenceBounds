
# Activating packages

library(Rcpp)
library(ggplot2)

# Set-up

rm(list=ls())
sourceCpp("C:\\Users\\herre\\OneDrive\\Documenten\\GitHub\\ConfidenceBounds\\Analyses\\Exp1_analyses\\DDM_confidence_bounds.cpp") 


# Constant variables

ntrials <- 50000 # Number of trials
sigma <- 1 # Within-trial noise
dt <- 0.01 # Precision; dt=.001 is better (but only when finished because slow)
a <- 1 # Upper bound (lower = 0) 
z <- 0.5 # Starting point (0.5 == middle, no bias)

# Manipulated variables

v <- c(0, 0.5, 1, 1.5, 2)     # Drift rate; i
a <- c(1)                     # Upper bound (lower = 0); j
a2 <- seq(0.1, 3, 0.1)  # Confidence bound; k
ter <- c(0.1)                 # Non-decision time; l
ter2 <- c(0)                  # Second non-decision time; m
a2_slope <- c(0)              # Urgency; n
postdriftmod <- c(1)          # Multiplied with v to calculate post-decisional drift rate

### Simulate data ###

predictions <- NULL
simulation <- 0
simulation_tot <- length(v)*length(a)*length(a2)*length(ter)*length(ter2)*length(a2_slope)*length(postdriftmod)
print(paste0('Number of simulations: ', simulation_tot, ' (with ', ntrials, ' trials per simulation)'))

for (i in 1:length(v)){
  for (j in 1:length(a)){
    for (k in 1:length(a2)){
      for (l in 1:length(ter)){
        for (m in 1:length(ter2)){
          for (n in 1:length(a2_slope)){
            for (o in 1:length(postdriftmod)){
                
              predictions_temp <- data.frame(DDM_confidence_bounds(v = v[i], a = a[j], ter = ter[l], z = z, ntrials = ntrials, s = sigma, dt = dt, a2 = a2[k], postdriftmod = postdriftmod[o], a2_slope = a2_slope[n], ter2 = ter2[m]))
              predictions_temp$v <- v[i]
              predictions_temp$a <- a[j]
              predictions_temp$a2 <- a2[k]
              predictions_temp$ter <- ter[l]
              predictions_temp$ter2 <- ter2[m]
              predictions_temp$a2_slope <- a2_slope[n]
              predictions_temp$postdriftmod <- postdriftmod[o]
              predictions <- rbind(predictions, predictions_temp)
              
              simulation <- simulation + 1
              print(paste0(simulation, ' / ', simulation_tot))
        
            }
          }
        }
      }
    }  
  }
}

names(predictions) <- c('rt', 'resp', 'cor', 'evidence_2', 'rtfull', 'rtconf', 'cj', 'v', 'a', 'a2', 'ter', 'ter2', 'a2_slope', 'postdriftmod')


### Plots ###

a2_mean <- predictions %>%
  group_by(a2) %>% 
  summarise_each(funs(mean))

ggplot(data = a2_mean, aes(x = a2, y = cj)) +
  geom_line(size = 1) +
  theme_bw()














#empty dataframe
data <- data.frame('rt' = numeric(0),    # Create empty data frame
                   'resp' = numeric(0),
                   'cor' = numeric(0),
                   'evidence2' = numeric(0),
                   'rt2' = numeric(0),
                   'cj' = numeric(0),
                   'rtconfidence' = numeric(0),
                   'ConfBound' = numeric(0),
                   'drift' = numeric(0),
                   'a2' = numeric(0))

for(sub in 1:N){
  
  #random variables
  ter <- runif(1, .2, .6) #variation in non decision time
  vratio <- runif(1, 0, 1.5) #variation in vratio (i.e., individual differences in metacognition)
  
  for (i in 1:length(drift)){
    for (j in 1:length(a2)){
      df <- data.frame(DDM_confidence_boundaries(v=drift[i], a=bound, ter=ter, z=z, ntrials=samples, s=sigma, dt=dt, postdriftmod=vratio, a2=a2[j]))
      df$drift <- drift[i]
      df$a2 <- a2[j]
      names(df) <- c('rt','resp','cor','evidence2','rt2','cj','rtconfidence','ConfBound', 'drift', 'a2') #ConfBound == 1 for upper bound
      data <- rbind(data, data.frame(df))
    }
  }
  print(sub)
} 


#plots
bound <- data.frame('drift' = numeric(0),
                    'a2' = numeric(0),
                    'p_upper' = numeric(0))
k <- 1
for (i in drift){
  for (j in a2){
    df <- subset(data, drift==i & a2==j)
    par(mfrow=c(3,3))
    
    #creating reaction time plots
    #hist(df$rt[df$resp==1],col=rgb(0,1,0,.5),main="",xlab="RT")
    #hist(df$rt[df$resp==-1],col=rgb(1,0,0,.5),add=T)
    hist(df$rt2[df$resp==1],col=rgb(0,1,0,.5),main="",xlab="RT2", xlim = c(0,3))
    hist(df$rt2[df$resp==-1],col=rgb(1,0,0,.5),add=T, xlim = c(0,3))
    
    #calculations for line plot
    bound <- rbind(bound, table(df$ConfBound)[2]/(table(df$ConfBound)[1]+table(df$ConfBound)[2]))
    bound$drift[k] <- i
    bound$a2[k] <- j
    k <- k + 1
  }
}
names(bound) <- c('p_upper', 'drift', 'a2')

data2 <- merge(data, bound, by=c("drift","a2"))
data2Cor0 <- data2[data2$cor==0,]
data2Cor1 <- data2[data2$cor==1,]

# Basic line plot with points
cbp1 <- c("#999999", "#E69F00", "#56B4E9", "#009E73",
          "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
#after wrong decision
ggplot(data=data2Cor0, aes(x=drift, y=p_upper, group=a2)) + 
  xlab('drift rate') + ylab('proportion of trials crossing correct confidence bound') +
  xlim(-0.001,2) +
  ylim(0.49,1) +
  geom_line(aes(color=as.factor(a2))) +
  geom_point(aes(color=as.factor(a2))) +
  theme_classic() +
  labs(colour="a2") + 
  scale_colour_manual(values=cbp1)
#after correct decision
ggplot(data=data2Cor1, aes(x=drift, y=p_upper, group=a2)) +
  xlab('drift rate') + ylab('proportion of trials crossing correct confidence bound') +
  xlim(-0.001,2) +
  ylim(0.49,1) +
  geom_line(aes(color=as.factor(a2))) +
  geom_point(aes(color=as.factor(a2))) +
  theme_classic() +
  labs(colour="a2") + 
  scale_colour_manual(values=cbp1)










### manipulating decision bound x confidence bound

#activating packages
library(Rcpp)
library(ggplot2)

#set-up
rm(list=ls())
setwd("C:/Users/herre/Desktop/Internship/Simulations")
sourceCpp("DDM_confidence_boundaries.cpp")

#Constant variables
N <-100 #N participants (100)
samples <- 500 #ntrials per participant, per condition
sigma <- 1 #within-trial noise
dt <- 0.001 #precision; dt=.001 is better (but only when finished because slow)
z <- 0.5 #starting point (0.5 == middle, no bias)
drift <- 1

#manipulated variables
a <- c(0.5,1,1.5,2,2.5) #drift rate
a2 <- c(0.5,1,1.5,2,2.5) #confidence bound 

#empty dataframe
data <- data.frame('rt' = numeric(0),    # Create empty data frame
                   'resp' = numeric(0),
                   'cor' = numeric(0),
                   'evidence2' = numeric(0),
                   'rt2' = numeric(0),
                   'cj' = numeric(0),
                   'rtconfidence' = numeric(0),
                   'ConfBound' = numeric(0),
                   'drift' = numeric(0),
                   'a2' = numeric(0))

for(sub in 1:N){
  
  #random variables
  ter <- runif(1, .2, .6) #variation in non decision time
  vratio <- runif(1, 0, 1.5) #variation in vratio (i.e., individual differences in metacognition)
  
  for (i in 1:length(a)){
    for (j in 1:length(a2)){
      df <- data.frame(DDM_confidence_boundaries(v=drift, a=a[i], ter=ter, z=z, ntrials=samples, s=sigma, dt=dt, postdriftmod=vratio, a2=a2[j]))
      df$a <- a[i]
      df$a2 <- a2[j]
      names(df) <- c('rt','resp','cor','evidence2','rt2','cj','rtconfidence','ConfBound', 'a', 'a2') #ConfBound == 1 for upper bound
      data <- rbind(data, data.frame(df))
    }
  }
  print(sub)
} 

#plots
bound <- data.frame('a' = numeric(0),
                    'a2' = numeric(0),
                    'p_upper' = numeric(0))
k <- 1
for (i in a){
  for (j in a2){
    df <- subset(data, a==i & a2==j)
    par(mfrow=c(3,3))
    
    #creating reaction time plots
    #hist(df$rt[df$resp==1],col=rgb(0,1,0,.5),main="",xlab="RT")
    #hist(df$rt[df$resp==-1],col=rgb(1,0,0,.5),add=T)
    hist(df$rt2[df$resp==1],col=rgb(0,1,0,.5),main="",xlab="RT2", xlim = c(0,3))
    hist(df$rt2[df$resp==-1],col=rgb(1,0,0,.5),add=T, xlim = c(0,3))
    
    #calculations for line plot
    bound <- rbind(bound, table(df$ConfBound)[2]/(table(df$ConfBound)[1]+table(df$ConfBound)[2]))
    bound$a[k] <- i
    bound$a2[k] <- j
    bound 
    k <- k + 1
  }
}
names(bound) <- c('p_upper', 'a', 'a2')

data2 <- merge(data, bound, by=c("a","a2"))
data2Cor0 <- data2[data2$cor==0,]
data2Cor1 <- data2[data2$cor==1,]

# Basic line plot with points
cbp1 <- c("#999999", "#E69F00", "#56B4E9", "#009E73",
          "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
ggplot(data=data2Cor0, aes(x=a, y=p_upper, group=a2)) +
  xlab('a (decision bound)') + ylab('proportion of trials crossing correct confidence bound') +
  ylim(0.5,1) +
  geom_line(aes(color=as.factor(a2))) +
  geom_point(aes(color=as.factor(a2))) +
  theme_classic() +
  labs(colour="a2 (confidence bound)") + 
  scale_colour_manual(values=cbp1)
ggplot(data=data2Cor1, aes(x=a, y=p_upper, group=a2)) +
  xlab('a (decision bound)') + ylab('proportion of trials crossing correct confidence bound') +
  ylim(0.5,1) +
  geom_line(aes(color=as.factor(a2))) +
  geom_point(aes(color=as.factor(a2))) +
  theme_classic() +
  labs(colour="a2 (confidence bound)") + 
  scale_colour_manual(values=cbp1)
