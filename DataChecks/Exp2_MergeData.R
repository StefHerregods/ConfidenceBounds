# Merging all experiment 2 data + adding batch-number

# Initialize data_full

data_full <- NULL

# Loop through all batch folders

for (j in 1:20){
  
  # Loop through all files to be added 
  
  for (i in 1:40){
    
    file_name <- paste0("Data\\Experiment_2\\Exp2_Batch",j,"\\DotsTask_sub",i,".csv",collapse="")
    if (file.exists(file_name)){
      
      data_temp <- read.csv(file=file_name)
      data_temp$batch <- j
      data_full <- rbind(data_temp,data_full)
      
    }
  }
}

# Save full data set

write.csv(data_full,"Exp2_data_full.csv",row.names=FALSE)
