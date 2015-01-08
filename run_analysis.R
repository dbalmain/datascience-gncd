library(dplyr)
library(tidyr)

runAnalysis <- function() {
  
  # utility function to create data path
  dataPath <- function(path) {
    file.path('UCI HAR Dataset', path)
  }
  
  # 1.
  # Merge the training and the test sets to create one data set.
  xTrain <- read.table(dataPath('train/X_train.txt'))
  xTest  <- read.table(dataPath( 'test/X_test.txt' ))
  data <- rbind(xTrain, xTest)
  
  # load the features
  features <- read.table(dataPath('features.txt'))
  names(features) <- c('index', 'name')
  
  # name the data columns
  names(data) <- features$name

  # 2.
  # Extract only the measurements on the mean and standard deviation for each measurement. 
  data <- data[, grep('(mean|std)\\(', features$name)]

  # 3.
  # Use descriptive activity names to name the activities in the data set
  # Also add the subject ids
  activityLabels <- read.table(dataPath('activity_labels.txt'))
  activityIds <- rbind(read.table(dataPath('train/y_train.txt')),
                       read.table(dataPath( 'test/y_test.txt')))
  activities <- activityLabels[activityIds[,1],2]
  
  subjects <- rbind(read.table(dataPath('train/subject_train.txt')),
                    read.table(dataPath( 'test/subject_test.txt')))[, 1]

  # add the Activity and Subject columns to our data table
  data <- cbind(Activity = activities, Subject = subjects, data)

  # 4.
  # Appropriately label the data set with descriptive variable names. 
  # - an initial 't' indicates a time variable
  # - an initial 'f' indicates a frequency variable
  # - let's remove the '()'
  # - also, let's remove the BodyBody's. I think this is a mistake in the data.
  names <- names(data)
  names <- gsub('^t', 'Time', names)
  names <- gsub('^f', 'Freq', names)
  names <- gsub('\\(\\)', '', names)
  names <- gsub('BodyBody', 'Body', names)
  names(data) <- names
  
  # 5.
  # From the data set in step 4, creates a second, independent tidy data set
  # with the average of each variable for each activity and each subject.
  tidy <- data %>% group_by(Activity, Subject) %>% summarise_each(funs(mean))
  names(tidy)[-c(1,2)] <- paste0("Mean", names(tidy)[-c(1,2)])
  
  write.table(tidy, 'tidy.txt', row.names=FALSE)
}