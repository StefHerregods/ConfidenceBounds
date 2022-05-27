#Generates random DDM with confidence bounds figure
#Upper boundary is always seen as the correct choice

library(ggplot2)
library(gganimate)
library(gifski)
library(ggthemes)

#set-up
rm(list=ls())
setwd("C:/Users/herre/Desktop/Internship/Simulations")
df <- NULL

#variables
sigma <- 1 #within-trial noise
dt <- 0.001 #precision
a <- 3 #upper bound (lower <- 0) 
z <- 0.5 #starting point (0.5 == middle, no bias) 
v <- 0.8 #drift rate
a2 <- 2 #confidence bound
ter <- 0.01 #non-decision time 
vratio <- 1 #metacognition

#fixed variables
acc <- -1
t <- 0
evidence <- a * z
v_post <- v * vratio

for (n in 1:1){
  
  #Create empty lists
  rt <- NULL
  evidence_list <- NULL
  t <- 0
  evidence <- a * z
  acc <- -1
  
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
  

  df_temp <- data.frame(rt, evidence_list, resp, evidence, acc)
  #df_temp <- cbind(rownames(df_temp), data.frame(df_temp), n)
  #colnames(df_temp)[1] <- "index"
  #df_temp$index <- as.numeric(df_temp$index)
  
  df <- rbind(df, df_temp)
  
}


df <- cbind(rownames(df), data.frame(df), n)

a <- ggplot(df, aes(as.numeric(rownames(df)),evidence_list)) + geom_line(group = 1) +
  theme_void()
ggsave(a, filename = 'test.png', bg = 'transparent', height = 2.2, width = 6)



# Pause timer
for (i in 1:50){
  rt <- append(rt, rt[length(rt)])
  evidence_list <- append(evidence_list, evidence_list[length(evidence_list)])
}

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

df <- data.frame(rt, evidence_list, resp, evidence, acc)
df <- cbind(rownames(df), data.frame(df), n)
colnames(df)[1] <- "index"
df$index <- as.numeric(df$index)

# Basic DDM

t <- max(df$rt)

gif <- ggplot(data = df, aes(x = rt)) +
  geom_line(size = 0.6, aes(y = evidence_list, group = as.factor(n))) +
  geom_segment(aes(x = 0, xend = t * 1.06, y = a / 2, yend = a / 2), size = 0.7, arrow = arrow(length = unit(0.2,"cm"))) +
  geom_segment(aes(x = 0, xend = t * 1.05, y = a, yend = a), linetype = 5) +
  geom_segment(aes(x = 0, xend = t * 1.05, y = 0, yend = 0), linetype = 5) +
  geom_segment(aes(x = 0, xend = 0, y = 0, yend = a), size = 0.7) +
  theme_void() +
  transition_reveal(index) +
  theme(panel.background = element_rect(fill = '#F1F2EB'))

animate(gif, nframes = 100, height = 700, width = 1000, bg = 'transparent')

df2 <- filter(df, df$evidence_list == 1 | df$evidence_list == 0)
hist <- ggplot(data = df2, aes(x = rt)) +
  geom_histogram(fill = NA, alpha = 0.6, position = 'identity', color = 'black', size = 0.05, bins = 15) +
  theme_void() +
  geom_smooth(aes(y = ..density..)) +
  theme(rect = element_rect(fill = "transparent")) +
  facet_wrap(~ evidence_list)

ggsave(a, filename = 'density.png', bg = 'transparent', height = 0.9, width = 25)

a <- ggplot() +
  geom_density(data = df2, aes(x = rt)) +
  facet_wrap(~ evidence_list) +
  theme_void() +
  xlim(c(-0.1, 1.2))

# Extended DDM

gif <- ggplot(data = df, aes(x = rt, y = evidence_list)) +
  geom_line(size = 1) +
  geom_segment(aes(x = 0, xend = t * 1.06, y = a / 2, yend = a / 2), size = 0.7, arrow = arrow(length = unit(0.2,"cm"))) +
  geom_segment(aes(x = 0, xend = t * 1.05, y = a, yend = a), linetype = 5) +
  geom_segment(aes(x = 0, xend = t * 1.05, y = 0, yend = 0), linetype = 5) +
  geom_segment(aes(x = decisionTime, xend = t * 1.05, y = upper_bound, yend = upper_bound)) +
  geom_segment(aes(x = decisionTime, xend = t * 1.05, y = lower_bound, yend = lower_bound)) +
  geom_segment(aes(x = 0, xend = 0, y = 0, yend = a), size = 0.7) +
  theme_void() +
  #transition_reveal(index) +
  theme(panel.background = element_rect(fill = '#F5F2EE'))

animate(gif, nframes = 100, height = 700, width = 1000, bg = 'transparent')
  
anim_save('test', animation = gif, height = 700, width = 1000, bg = 'transparent')
  


options(gganimate.dev_args = list(bg = 'transparent'))


#geom_text(x = -0.01, y = a, label = "a", fontface = 3) +
#geom_text(x = -0.01, y = 0, label = "0") +
#geom_text(y = 9 * a / 20, x = 9.8 * t / 10, label = "time", size = 6, fontface = 1) +
#geom_text(y = 9 * a / 20, x = ter / 2, label = "T", size = 6, fontface = 3) +
#geom_text(y = 8.9 * a / 20, x = 1.1 * ter / 2, label = "er", size = 4, fontface = 3)

  
  
  