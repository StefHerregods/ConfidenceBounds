#Generates random DDM with confidence bounds figure
#Upper boundary is always seen as the correct choice

library(ggplot2)

#set-up
rm(list=ls())
setwd("C:/Users/herre/Desktop/Internship/Simulations")

#variables
sigma <- 1 #within-trial noise
dt <- 0.005 #precision
a <- 1 #upper bound (lower <- 0) 
z <- 0.5 #starting point (0.5 == middle, no bias) 
v <- 0.5 #drift rate
a2 <- 0.5 #confidence bound
ter <- 0.1 #non-decision time 
vratio <- 1 #metacognition

#fixed variables
acc <- -1
t <- 0
evidence <- a * z
v_post <- v * vratio

#Create empty lists
rt <- NULL
evidence_list <- NULL

rt <- append(rt, t)
evidence_list <- append(evidence_list, evidence)

#non-decision time
while (t <= ter) {
  t <- t + dt
  rt <- append(rt, t)
  evidence_list <- append(evidence_list, evidence)
}




#decision process
while (acc == -1){
  
  t <- t + dt
  evidence <- evidence + v * dt + sigma * sqrt(dt) * rnorm(1,0,1)

  if (evidence >= a){
    resp <- 1
    evidence <- a
    acc <- 1
  } else if (evidence <= 0){
    resp <- -1
    evidence <- 0
    acc <- 0
  }
  
  rt <- append(rt, t)
  evidence_list <- append(evidence_list, evidence)
  
}
decisionTime <- t

#confidence process
if (resp == 1){
  while ((evidence < a + a2/2) && (evidence > a - a2/2)){
    
    t = t + dt
    rt <- append(rt, t)
    evidence = evidence + v_post * dt + sigma * sqrt(dt) * rnorm(1,0,1)
    evidence_list <- append(evidence_list, evidence)
  }
  upper_bound <- a + a2/2
  lower_bound <- a - a2/2
  if (evidence > a + a2/2){
    evidence_list <- head(evidence_list, -1)
    evidence_list <- append(evidence_list, a + a2/2)
  } else if (evidence < a - a2/2){
    evidence_list <- head(evidence_list, -1)
    evidence_list <- append(evidence_list, a - a2/2)
  }
} else if (resp == -1){
  while ((evidence < a2/2) && (evidence > -a2/2)){
    
    t = t + dt
    rt <- append(rt, t)
    evidence = evidence + v_post * dt + sigma * sqrt(dt) * rnorm(1,0,1)
    evidence_list <- append(evidence_list, evidence)
  }
  upper_bound <- a2/2
  lower_bound <- - a2/2
  if (evidence > a2/2){
    evidence_list <- head(evidence_list, -1)
    evidence_list <- append(evidence_list, a2/2)
  } else if (evidence < a2/2){
    evidence_list <- head(evidence_list, -1)
    evidence_list <- append(evidence_list, - a2/2)
  }
}



data <- data.frame(rt, evidence_list)
ggplot(data = data, aes(x = rt, y = evidence_list)) +
  geom_rect(xmin = 0, xmax = ter, ymin = a / 3, ymax = 2 * a / 3, fill = "grey", alpha = 0.01) +
  geom_line(size = 1.1) +
  geom_segment(aes(x = 0, xend = t * 1.06, y = a / 2, yend = a / 2), size = 0.7, arrow = arrow(length = unit(0.2,"cm"))) +
  geom_segment(aes(x = 0, xend = t * 1.05, y = a, yend = a), linetype = 5) +
  geom_segment(aes(x = 0, xend = t * 1.05, y = 0, yend = 0), linetype = 5) +
  geom_segment(aes(x = decisionTime, xend = t * 1.05, y = upper_bound, yend = upper_bound)) +
  geom_segment(aes(x = decisionTime, xend = t * 1.05, y = lower_bound, yend = lower_bound)) +
  geom_segment(aes(x = 0, xend = 0, y = 0, yend = a), size = 0.7) +
  theme_void()
  
  

#  geom_text(x = -0.01, y = a, label = "a", fontface = 3) +
#  geom_text(x = -0.01, y = 0, label = "0") +
#  geom_text(y = 9 * a / 20, x = 9.8 * t / 10, label = "time", size = 6, fontface = 1) +
#  geom_text(y = 9 * a / 20, x = ter / 2, label = "T", size = 6, fontface = 3) +
#  geom_text(y = 8.9 * a / 20, x = 1.1 * ter / 2, label = "er", size = 4, fontface = 3)

  
  
  