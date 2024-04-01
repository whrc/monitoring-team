library(dplyr)


#getcsdata 
# a simple function to import a data file from
# a CS datalogger with correct column headings

getcsdata <- function(dataPath, skip = 3) {
  
  fileColumns <- read.csv(file = dataPath, skip = 1)
  fileData <- read.csv(file = dataPath, skip = skip, na.strings = c("NAN","NaN",7999))
  colnames(fileData) <- colnames(fileColumns)
  
  # if (cleanNA) {
  #   timeCol <- fileData$TIMESTAMP
  #   
  #   fileData <- fileData[,2:length(fileData)]
  #   fileData <- fileData %>%
  #     mutate_all(~na_if(., 'NaN'))
  #   fileData <- fileData %>%
  #     mutate_all(~na_if(., 'NAN'))
  #   fileData <- fileData %>%
  #     mutate_all(~na_if(., '7999'))
  #   
  #   fileData$TIMESTAMP <- timeCol
  # }
  
  fileData$TIMESTAMP <- as.POSIXct(fileData$TIMESTAMP, format="%Y-%m-%d %H:%M:%S")
  
  fileData[,2:length(fileData)] <- sapply(fileData[,2:length(fileData)], as.numeric)
  
  return(fileData)
  
}

#agdata <- getcsdata("~/Desktop/LoggerNet/B2_Ag_PV_CR1000_Table1.dat")







#getsummarydata
# a simple function to import summary data 
# output from LiCor GHG package
# "C:/Users/dtrangmoe/Documents/R Practice/ScottyCreek R Practice/summaries"

getsummarydata <- function(dataPath, days = 8) {
  
  #get names
  summaryFileNamesLong <- list.files(dataPath, pattern="*.txt", full.names=TRUE)
  summaryDays <- c((length(summaryFileNamesLong)-days):length(summaryFileNamesLong))
  summaryFileNames <- summaryFileNamesLong[summaryDays]
  
  #read in all data files
  summaryFiles <- lapply(summaryFileNames, read.csv, sep = "\t", skip = 1, na.strings = c("NAN","NaN",7999))
  summaryFileTotal <- bind_rows(summaryFiles)
  
  #get/apply column namesge
  summaryCols <- read.csv(summaryFileNames[[1]], sep = "\t")
  colnames(summaryFileTotal) <- colnames(summaryCols)
  
  #force data to be numeric
  summaryFileTotal[,5:length(summaryFileTotal)] <- sapply(summaryFileTotal[,5:length(summaryFileTotal)], as.numeric)
  
  # #clean "NaN" to NA
  # if (cleanNA) {
  #   summaryFileTotal <- summaryFileTotal %>%
  #     mutate_all(~na_if(., 'NaN'))
  #   summaryFileTotal <- summaryFileTotal %>%
  #     mutate_all(~na_if(., 7999))
  # }
  
  
  return(summaryFileTotal)
  
}


