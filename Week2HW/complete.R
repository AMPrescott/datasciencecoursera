complete <- function(directory, id = 1:332) {
  #This function reads in a directory of files and reports the number of complete
  #observations recorded in each file
  
  #Initialize vector for storing file names
  fileID <- c(rep('x',length(id)))
  #Create a character vector of filenames to open
    for(site in seq_len(length(id))){
      if(id[site]<10){
        fileID[site] <-  paste0(directory,"/00",id[site],".csv")
      }
      else if(id[site]<100){
        fileID[site] <-  paste0(directory,"/0",id[site],".csv")
      }
      else{
        fileID[site] <-  paste0(directory,"/",id[site],".csv")
      }
    }
  #Initialize a data frame with fileID and counts of complete observations
  #Begin with observations numbers equal to 1
  completeObs <-  data.frame(ID = id, numCompObs = rep(0,length(id)))
  
  #Open file into data table and check for number of complete observations
  for(site in seq_len(length(id))){
    #Read in values from one file and test each observation for completeness
    test <-  complete.cases(data.matrix(read.csv(fileID[site])))
    #Initialize a counter to zero that will be increment for every complete observation
    compCount <- 0
    #loop through test and check for true values
    for(check in seq_len(length(test))){
      if(test[check]){
        compCount <- compCount + 1
      }
    }
    #update completeObs[site,2] to the value indicated in compCount
    completeObs[site,2] <- compCount
  }
  completeObs
}