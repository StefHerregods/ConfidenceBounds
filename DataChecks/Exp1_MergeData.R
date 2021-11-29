# 29 November 2021
# Merging all data + adding batch-number

# Data information 
Batch = 1  # (change to the correct batch-number)

# Setting working directory

setwd('C:\\Users\\herre\\OneDrive\\Bureaublad\\Internship\\Results\\Exp1_results_temp_4')

# Read main file

# Loop through all files to be added 
for (i in 1:40){
  
  file_name <- paste0("DotsTask_sub",i,".csv",collapse="")
  if (file.exists(file_name)){
    
    data_temp <- read.csv(file=file_name)
    data_temp$subject <- i
    
    data_full <- rbind(data_temp,data_full)
  }
}