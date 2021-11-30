# November 2021
# Script contains exploratory graphs
# Used to check for irregularities in the data


# Setting working directory

setwd("C:\\Users\\herre\\Desktop\\Internship\\Results\\Exp2_Results") 

# Loading data

data_full <- read.csv(file="Exp2_data_full.csv")
colnames(data_full)[1] <- gsub('^...','',colnames(data_full)[1])
data_viable <- read.csv(file="Exp2_data_viable.csv")
