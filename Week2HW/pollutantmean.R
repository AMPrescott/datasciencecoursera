#This function calculates the mean for the files from specdata by combining sites
#referenced id and averaging over available values for the specified pollutant.  
#The files are located in the location indicated by directory
pollutantmean <- function(directory, pollutant, id = 1:332) {
  #Initialize vector for storing file names
  fileID <- c(rep('x',length(id)))
  
  #If values listed in vector id are less than 100, add leading zeros to match 
  #file name formats and concatonate with directory.  Open and merge the files in a 
  #single data frame
  for(site in seq_len(length(id))){
    
    if(id[site]<10){
      fileID[site] <-  paste0(directory,"00",id[site],".csv")
    }
    else if(id[site]<100){
      fileID[site] <-  paste0(directory,"0",id[site],".csv")
    }
    else{
      fileID[site] <-  paste0(directory,id[site],".csv")
    }
    
    #Open first file into allSitesData data frame
    if(site == 1){
      allSitesData <- data.frame(read.csv(fileID[site]))
    }
    #If first file is already read, read file into temporary data frame and merge with allSitesData
    else{
      tempData <- data.frame(read.csv(fileID[site]))
      allSitesDate <- merge(allSitesData, tempData, all = TRUE)
    }
  }
  
  #Calculate the combined site mean of the specified pollutant column
  comSiteMean <- mean(allSitesData[[pollutant]], na.rm = TRUE)
  
}