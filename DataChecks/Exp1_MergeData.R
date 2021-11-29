# 29 November 2021
# Merging all experiment 1 data + adding batch-number


# Setting working directory

setwd("C:\\Users\\herre\\Desktop\\Internship\\Results\\Exp1_Results")

# Initialize data_full

data_full <- NULL

# Loop through all batch folders

for (j in 1:9){
  
  # Loop through all files to be added 
  
  for (i in 1:40){
    
    file_name <- paste0("Exp1_Batch",j,"\\DotsTask_sub",i,".csv",collapse="")
    if (file.exists(file_name)){
      
      data_temp <- read.csv(file=file_name)
      data_temp$batch <- j
      data_full <- rbind(data_temp,data_full)
      
    }
  }
}

# Save full data set

write.csv(data_full,"Exp1_data_full.csv",row.names=FALSE)

