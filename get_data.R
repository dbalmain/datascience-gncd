getData <- function() {
  url <- 'https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip'
  zipFile <- 'data.zip'
  dataDir <- 'UCI HAR Dataset'
  if (!file.exists(dataDir)) {
    if (!file.exists(zipFile)) {
      download.file(url, zipFile, method='curl')            
    }
    unzip(zipFile)
  }
}