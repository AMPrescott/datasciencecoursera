corr <- function(directory, threshold = 0){
  #Calculate correlations between nitrate and sulfate at sites that have complete observations
  # greater than the threshold value
  
  #create a data table that has list of sites in directory with counts complete observations
  threshTest <- complete(directory)
  
  #Initialize a numeric vector for storing correlation scores
  corScores <- as.numeric(c())
  
  #Test each site to see if it exceeds the threshold.  If it does, read in file.  Calculate 
  #correlation scores and row append to corScores
  for(site in seq_len(length(threshTest[,2]))){
    if(threshTest[site,2]>threshold){
      #Create a fileID and read in values from file into a data matrix
      if(threshTest[site,1]<10){
        fileID <-  paste0(directory,"/00",threshTest[site,1],".csv")
      }
      else if(threshTest[site,1]<100){
        fileID <-  paste0(directory,"/0",threshTest[site,1],".csv")
      }
      else{
        fileID <-  paste0(directory,"/",threshTest[site,1],".csv")
      }
      #Open file and perform correlation calculations between 'sulfate' and 'nitrate values
      siteData <- data.matrix(read.csv(fileID))
      corScores <- c(corScores, cor(siteData[complete.cases(siteData),2], siteData[complete.cases(siteData),3]))
    }
  }
  corScores
}
  