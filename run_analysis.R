#Peer Assessment for course "Getting and Cleaning Data"

#Assignment description:

#The purpose of this project is to demonstrate your ability to collect, work with, and clean a data set.
#The goal is to prepare tidy data that can be used for later analysis. You will be graded by your peers
#on a series of yes/no questions related to the project. You will be required to submit: 1) a tidy data
#set as described below, 2) a link to a Github repository with your script for performing the analysis,
#and 3) a code book that describes the variables, the data, and any transformations or work that you
#performed to clean up the data called CodeBook.md. You should also include a README.md in the repo with
#your scripts. This repo explains how all of the scripts work and how they are connected.  

#One of the most exciting areas in all of data science right now is wearable computing - see for example
#this article . Companies like Fitbit, Nike, and Jawbone Up are racing to develop the most advanced
#algorithms to attract new users. The data linked to from the course website represent data collected
#from the accelerometers from the Samsung Galaxy S smartphone. A full description is available at the
#site where the data was obtained: 
  
#  http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones 

#Here are the data for the project: 
  
#  https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 

#You should create one R script called run_analysis.R that does the following. 
#Merges the training and the test sets to create one data set.
#Extracts only the measurements on the mean and standard deviation for each measurement. 
#Uses descriptive activity names to name the activities in the data set
#Appropriately labels the data set with descriptive activity names. 
#Creates a second, independent tidy data set with the average of each variable for each activity and
#each subject. 

#read data from provided directory and dataset file name
readData <- function(directory, filename) {
  # read the column names
  filepath <- paste("./", directory, "/", "features.txt", sep="")
  data_cols <- read.table(filepath, header=F, as.is=T, col.names=c("MeasureID", "MeasureName"))
  
  # read y data
  filepath <- paste("./", directory, "/", filename, "/y_", filename, ".txt", sep="")
  y_data <- read.table(filepath, header=F, col.names=c("ActivityID"))
  
  # read x data
  filepath <- paste("./", directory, "/", filename, "/X_", filename, ".txt", sep="")
  x_data <- read.table(filepath, header=F, col.names=data_cols$MeasureName)
  
  # read subject data
  filepath <- paste("./", directory, "/", filename, "/subject_", filename, ".txt", sep="")
  subject_data <- read.table(filepath, header=F, col.names=c("SubjectID"))  
  
  ## extract the data from the merged data where the column names are mean OR std
  mean_columns <- grep(".*mean\\(\\)", data_cols$MeasureName)
  std_columns <- grep(".*std\\(\\)", data_cols$MeasureName)
  
  ## put both mean and std columns into single vector
  mean_std_data_cols <- c(mean_columns, std_columns)
  
  ## sort the vector 
  mean_std_data_cols <- sort(mean_std_data_cols)
  
  # subset the data (done early to save memory)
  x_data <- x_data[,mean_std_data_cols]
  
  # append the activity id and subject id columns
  x_data$ActivityID <- y_data$ActivityID
  x_data$SubjectID <- subject_data$SubjectID
  
  # return the data
  x_data
}

#Merge data from test and train sets
mergeData <- function(directory) {
  testData <- readData(directory, "test")
  trainData <- readData(directory, "train")
  data <- rbind(testData, trainData)
  col_names <- colnames(data)
  col_names <- gsub("\\.+mean\\.+", col_names, replacement="Mean")
  col_names <- gsub("\\.+std\\.+",  col_names, replacement="Std")
  colnames(data) <- col_names
  
  labels_filename <- paste("./", directory, "/", "activity_labels.txt", sep="")
  activity_labels <- read.table(labels_filename, col.names=c("ActivityID", "ActivityName"), header=FALSE, as.is=TRUE)
  activity_labels$ActivityName <- as.factor(activity_labels$ActivityName)
  data <- merge(data, activity_labels)

  data
}

#create tidy data set
createDataset <- function(data) {
  library(reshape2)
  
  # melt the dataset
  id_vars = c("ActivityID", "ActivityName", "SubjectID")
  mvars = setdiff(colnames(data), id_vars)
  melted_data <- melt(data, id=id_vars, measure.vars=mvars)
  
  # result
  dcast(melted_data, ActivityName + SubjectID ~ variable, mean)    
}

print("Creating tidy dataset...")

#set data dir
dataDirectory <- "UCI HAR Dataset"
#create tidy data set
data <- createDataset(mergeData(dataDirectory))
#write it to the file
write.table(data, "tidyset.txt")

print("Saved data to tidyset.txt.")