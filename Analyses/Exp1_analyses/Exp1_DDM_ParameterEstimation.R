# Internship project 2021-2022
# Script estimates DDM parameters of DDM with confidence bounds
# Input: decision reaction times, decisions, confidence rating reaction times, binary confidence rating
# Based on https://github.com/kdesende/dynamic_influences_on_static_measures/blob/main/3A_experiment1_sato
  # Desender, K., Vermeylen, L., Verguts, T. (2021)


rm(list=ls())

# Setting working directory

setwd('C:\\Users\\herre\\Desktop\\Internship\\Results\\Exp1_Results')

# Load packages

library(Rcpp)  # To source, compile and run C++ functions
library(DEoptim)  # Optimization algorithm
library(RcppZiggurat)  # Random number generator (normal distribution) 

# Give R access to the DDM simulation function in C++

sourceCpp("C:\\Users\\herre\\OneDrive\\Documenten\\GitHub\\ConfidenceBounds\\Analyses\\Exp1_analyses\\DDM_confidence_bounds.cpp") 

# Variable settings

z = 0.5  # Starting point (accuracy-coded dataset -> 0.5)
ntrials = 1000  # Number of decision-making simulations per observation
sigma = 1  # Within-trial noise
dt = 0.001  # Precision

itermax = 50  # Number of DeOptim iterations

# for observations: use 'metacognition', not cj?

# ?

chi_square_optim <- function(params, observations, returnFit){  
  
  # Generate predictions
  
  names(params) <- c('v', 'a', 'ter', 'a2', 'postdriftmod')
  predictions <<- data.frame(DDM_confidence_bounds(v = params['v'], a = params['a'], ter = params['ter'], z = z, ntrials = ntrials, s = sigma, dt = dt, a2 = params['a2'], postdriftmod = params['postdriftmod']))
  names(predictions) <- c('rt', 'resp', 'cor', 'raw_evidence2', 'rtfull', 'confidence', 'rtconf', 'cj')
  #predictions$evidence2 <- predictions$raw_evidence2 #???
  
  # Linear scaling of confidence (from continuous to binary)
  
  predictions$cj <- (predictions$cj + params['add_mean']) / params['add_sd']
  predictions$cj <- ifelse(predictions$cj > 0, 1, 0)  # Transform to binary variable (0-1)
  
  # Separate predictions according to the response
  
  c_predicted <- predictions[predictions$cor == 1,]
  e_predicted <- predictions[predictions$cor == 0,]
  
  # Sort prediction RTs 
  
  c_predicted_rt <- sort(c_predicted$rt)
  e_predicted_rt <- sort(e_predicted$rt)
  
  # Sort prediction confidence RTs
  
  c_predicted_rtconf <- sort(c_predicted$rtconf)
  e_predicted_rtconf <- sort(e_predicted$rtconf)  
  
  # If we're only simulating data: return the predictions
  
  if(returnFit==0){ 
    return(predictions[,c('rt','cor','cj')])
    
  # If we are fitting the model: compare these predictions to the observations 
  
  }else{ 
  
    
    ### 1 - Decision RT comparison ###
    
    
    # Separate observations into correct and error trials
    
    c_observed <- observations[observations$cor == 1,]
    e_observed <- observations[observations$cor == 0,]
    
    # Get the quantile RTs on the "observed data" for correct and error distributions separately (for quantiles .1, .3, .5, .7, .9)
    
    c_quantiles <- quantile(c_observed$rt, probs = c(.1,.3,.5,.7,.9), names = FALSE)
    e_quantiles <- quantile(e_observed$rt, probs = c(.1,.3,.5,.7,.9), names = FALSE)
    
    # To combine correct and incorrect trials, we scale the expected interquantile probability by the proportion of correct and incorrect respectively
    
    prop_obs_c <- dim(c_observed)[1] / dim(observations)[1]
    prop_obs_e <- dim(e_observed)[1] / dim(observations)[1]
    
    c_obs_proportion = prop_obs_c * c(.1, .2, .2, .2, .2, .1)
    e_obs_proportion = prop_obs_e * c(.1, .2, .2, .2, .2, .1)
    obs_props <- c(c_obs_proportion,e_obs_proportion)
    
    # Calculate proportion of responses that fall between the observed quantiles when applied to the predicted data 
    #!!!(scale by N?)
    
    c_pred_proportion <- c(
      sum(c_predicted_rt <= c_quantiles[1]),
      sum(c_predicted_rt <= c_quantiles[2]) - sum(c_predicted_rt <= c_quantiles[1]),
      sum(c_predicted_rt <= c_quantiles[3]) - sum(c_predicted_rt <= c_quantiles[2]),
      sum(c_predicted_rt <= c_quantiles[4]) - sum(c_predicted_rt <= c_quantiles[3]),
      sum(c_predicted_rt <= c_quantiles[5]) - sum(c_predicted_rt <= c_quantiles[4]),
      sum(c_predicted_rt > c_quantiles[5])
    ) / dim(predictions)[1]
    
    e_pred_proportion <- c(
      sum(e_predicted_rt <= e_quantiles[1]),
      sum(e_predicted_rt <= e_quantiles[2]) - sum(e_predicted_rt <= e_quantiles[1]),
      sum(e_predicted_rt <= e_quantiles[3]) - sum(e_predicted_rt <= e_quantiles[2]),
      sum(e_predicted_rt <= e_quantiles[4]) - sum(e_predicted_rt <= e_quantiles[3]),
      sum(e_predicted_rt <= e_quantiles[5]) - sum(e_predicted_rt <= e_quantiles[4]),
      sum(e_predicted_rt > e_quantiles[5])
    ) / dim(predictions)[1]
    
    pred_props_rt <- c(c_pred_proportion, e_pred_proportion)
    
    # Avoid zeros in the the data (because of division by predictions for chi square statistic) -> set to small number
    
    pred_props_rt[pred_props_rt == 0] <- .0000001
    
    
    ### 2 - Confidence rating RT comparison ###
    
    
    # Get the quantile confidence RTs on the "observed data" for correct and error distributions separately (for quantiles .1, .3, .5, .7, .9)
    
    c_quantiles <- quantile(c_observed$rtconf, probs = c(.1,.3,.5,.7,.9), names = FALSE)
    e_quantiles <- quantile(e_observed$rtconf, probs = c(.1,.3,.5,.7,.9), names = FALSE)
    
    # Calculate proportion of responses that fall between the observed quantiles when applied to the predicted data 
    #!!!(scale by N?)
    
    c_pred_proportion <- c(
      sum(c_predicted_rtconf <= c_quantiles[1]),
      sum(c_predicted_rtconf <= c_quantiles[2]) - sum(c_predicted_rtconf <= c_quantiles[1]),
      sum(c_predicted_rtconf <= c_quantiles[3]) - sum(c_predicted_rtconf <= c_quantiles[2]),
      sum(c_predicted_rtconf <= c_quantiles[4]) - sum(c_predicted_rtconf <= c_quantiles[3]),
      sum(c_predicted_rtconf <= c_quantiles[5]) - sum(c_predicted_rtconf <= c_quantiles[4]),
      sum(c_predicted_rtconf > c_quantiles[5])
    ) / dim(predictions)[1]
    
    e_pred_proportion <- c(
      sum(e_predicted_rtconf <= e_quantiles[1]),
      sum(e_predicted_rtconf <= e_quantiles[2]) - sum(e_predicted_rtconf <= e_quantiles[1]),
      sum(e_predicted_rtconf <= e_quantiles[3]) - sum(e_predicted_rtconf <= e_quantiles[2]),
      sum(e_predicted_rtconf <= e_quantiles[4]) - sum(e_predicted_rtconf <= e_quantiles[3]),
      sum(e_predicted_rtconf <= e_quantiles[5]) - sum(e_predicted_rtconf <= e_quantiles[4]),
      sum(e_predicted_rtconf > e_quantiles[5])
    ) / dim(predictions)[1]
    
    pred_props_rtconf <- c(c_pred_proportion, e_pred_proportion)
    
    # Avoid zeros in the the data (because of division by predictions for chi square statistic) -> set to small number
    
    pred_props_rtconf[pred_props_rtconf == 0] <- .0000001
    
    
    
    # Now, do the same for confidence  #!!!!!
    
    # obs_props_cj <- c(sum(c_observed$cj == 0),
    #                   sum(c_observed$cj == 1)
    # )/length(observations$cj)
    
    # To make the next step easier, lets sort the predictions for correct and errors
    
    # c_predicted_cj <- sort(c_predicted$cj)
    # e_predicted_cj <- sort(e_predicted$cj)
    
    # Calculate proportion of responses that fall between the observed quantiles when applied to the predicted data 
    #(scale by N?)
    
    # pred_props_cj <- c(
    #   sum(c_predicted_cj == 1),
    #   sum(c_predicted_cj == 2),
    #   sum(c_predicted_cj == 3),
    #   sum(c_predicted_cj == 4),
    #   sum(c_predicted_cj == 5),
    #   sum(c_predicted_cj == 6),
    #   sum(e_predicted_cj == 1),
    #   sum(e_predicted_cj == 2),
    #   sum(e_predicted_cj == 3),
    #   sum(e_predicted_cj == 4),
    #   sum(e_predicted_cj == 5),
    #   sum(e_predicted_cj == 6)
    # ) / dim(predictions)[1]    
    
    # Avoid zeros in the the data (because of division by predictions for chi square statistic) -> set to small number
   
    # pred_props_cj[pred_props_cj==0] <- .0000001
    
    # Combine the quantiles for RTs and cj
    obs_props <- c(obs_props, obs_props) #, obs_props_cj)
    pred_props <- c(pred_props_rt, pred_props_rtconf) #, pred_props_cj)
    
    # calculate chi square
    chiSquare = sum( ( (obs_props - pred_props) ^ 2) / pred_props )
    
    #Return chiSquare
    return(chiSquare)
    
  }
}

# Load data

df <- read.csv(file = "Exp1_data_viable.csv")
subs <- unique(df$sub)
N<-length(subs)

# Create empty matrices

v_matrix <- matrix(NA, N, 4)  #!!! change to higher than 4 for more conditions
a_matrix <- matrix(NA, N, 4)
ter_matrix <- matrix(NA, N, 4)
a2_matrix <- matrix(NA, N, 4)
postdriftmod_matrix <- matrix(NA, N, 4)

# Optimize (extended) DDM parameters 

for(i in 1:N){  # For each participant separately
   
  print(paste('Running participant', subs[i], 'from', N))
  condLab <- unique(df$manipulation)  #!!! Change to all forms of manipulations
  tempAll <- subset(df, sub == subs[i])
  
  for(c in 1:4){  # For each condition separately !!! Change to higher for more conditions
    
    tempDat <- subset(tempAll, manipulation == condLab[c])
    tempDat <- tempDat[,c('rt', 'cor', 'resp', 'cj', 'manipulation')]
    
    # Load existing individual results if these already exist
    # TODO
    
    # Optimization function
    
    optimal_params <- DEoptim(chi_square_optim, # function to optimize
                              lower = c(0, .5, 0, .5,   0),  # lowest possible values for v, a, ter, a2, postdriftmod
                              upper = c(3,  4, 1,  4, 2.5), 
                              observations = tempDat, returnFit = 1, control=c(itermax = itermax)) # observed data is a parameter for the ks function we pass
    
    results <- summary(optimal_params)
    
    # Save individual results
    
    save(results, file=paste0('Parameter_estimation\\results_sub_',i,'_',condLab[c],'.Rdata'))
    
    # Add results 
    
    v_matrix[i,c] <- results$optim$bestmem[1]  #!!! change to higher than 2 for more conditions
    a_matrix[i,c] <- results$optim$bestmem[2]
    ter_matrix[i,c] <- results$optim$bestmem[3]
    a2_matrix[i,c] <- results$optim$bestmem[4]
    postdriftmod_matrix[i,c] <- results$optim$bestmem[5]
    
  }
}

# Plot ???

jpeg(file="exp_1_sato_results.jpg", res=600, width=400*4*(350/72), height=400*(350/72))
par(mfrow=c(1,4))
plot(colMeans(v_matrix),frame=F,cex.lab=1.5,ylim=c(0,5),xlim=c(.8,4.2),ylab="Drift rate",xlab="Manipulation",xaxt='n')
axis(1,1:4,c("1", "1", "3", "4"))
for(i in 1:N) lines(1:4,v_matrix[i,1:4],type='b',lty=2,col='grey',pch=19)
points(colMeans(v_matrix),type='b',lwd=5);error.bar(1:4,colMeans(v_matrix),colSds(as.matrix(v_matrix))/sqrt(N),length=0,lwd=3)

plot(colMeans(a_matrix),frame=F,cex.lab=1.5,ylim=c(.5,4),xlim=c(.8,4.2),ylab="Decision boundary",xlab="Instruction condition",xaxt='n');axis(1,1:4,c("Accuracy","Speed"))
for(i in 1:N) lines(1:2,a_matrix[i,1:2],type='b',lty=2,col='grey',pch=19)
points(colMeans(a_matrix),type='b',lwd=5);error.bar(1:2,colMeans(a_matrix),colSds(as.matrix(a_matrix))/sqrt(N),length=0,lwd=3)
plot(colMeans(mratio_sato),frame=F,cex.lab=1.5,ylim=c(-.5,2.5),xlim=c(.8,2.2),ylab="M-ratio",xlab="Instruction condition",xaxt='n');axis(1,1:2,c("Accuracy","Speed"))
for(i in 1:N) lines(1:2,mratio_sato[i,1:2],type='b',lty=2,col='grey',pch=19)
points(colMeans(mratio_sato),type='b',lwd=5);error.bar(1:2,colMeans(mratio_sato),colSds(as.matrix(mratio_sato))/sqrt(N),length=0,lwd=3)
plot(colMeans(post_drift_sato),frame=F,cex.lab=1.5,ylim=c(0,2.5),xlim=c(.8,2.2),ylab="v-ratio",xlab="Instruction condition",xaxt='n');axis(1,1:2,c("Accuracy","Speed"))
for(i in 1:N) lines(1:2,post_drift_sato[i,1:2],type='b',lty=2,col='grey',pch=19)
points(colMeans(post_drift_sato),type='b',lwd=5);error.bar(1:2,colMeans(post_drift_sato),colSds(as.matrix(post_drift_sato))/sqrt(N),length=0,lwd=3)
dev.off()




write.table(v_matrix, file='drift.txt')
write.table(a_matrix, file='bound.txt')
write.table(ter_matrix, file='ter.txt')

v_matrix <- read.table('drift.txt')
a_matrix <- read.table('bound.txt')

