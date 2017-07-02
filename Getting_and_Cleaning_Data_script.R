#0. Download the zip file and unzip it

if (!require("data.table")) {
  install.packages("data.table")
}

library(data.table)
fileurl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
setwd("~/Documents/eLearning/R")
if (!file.exists('./UCI_HAR_Dataset.zip')){
  download.file(fileurl,"./UCI_HAR_Dataset.zip", mode = "wb")
  unzip("UCI_HAR_Dataset.zip", exdir = './')
}

setwd("~/Documents/eLearning/R/UCI HAR Dataset")
list.files()


#1. Merges the training and the test sets to create one data set
X_train <- read.table('./UCI HAR Dataset/train/X_train.txt')
y_train <- read.csv('./UCI HAR Dataset/train/y_train.txt', header = FALSE, sep = ' ')
subject_train <- read.csv('./UCI HAR Dataset/train/subject_train.txt',header = FALSE, sep = ' ')

features <- read.csv('./UCI HAR Dataset/features.txt', header = FALSE, sep = ' ') #Features contains all calnames
features <- as.character(features[,2]) #turn the dataset into text values before use them as dataset colnames

train <-  data.frame(subject_train, y_train, X_train) #Merge the 3 trains datasets
colnames(train) <- c(c('subject', 'activity'), features)

X_test <- read.table('./UCI HAR Dataset/test/X_test.txt')
y_test <- read.csv('./UCI HAR Dataset/test/y_test.txt', header = FALSE, sep = ' ')
subject_test <- read.csv('./UCI HAR Dataset/test/subject_test.txt', header = FALSE, sep = ' ')

test <-  data.frame(subject_test, y_test, X_test) #Merge the 3 tests datasets
colnames(test) <- c(c('subject', 'activity'), features)

DFmerge <- rbind(train, test)

#2. Extracts only the measurements on the mean and standard deviation for each measurement
extract <- grep('mean|std', features) #grep columns with mean or standard
DFmerge_extract <- DFmerge[,c(1,2,extract)]

#3. Uses descriptive activity names to name the activities in the data set
activity_labels <- read.table('./UCI HAR Dataset/activity_labels.txt', header = FALSE)
activity_labels <- as.character(activity_labels[,2])
DFmerge_extract$activity <- activity_labels[DFmerge_extract$activity] #Add activity labels to DFmerge_extract

#4. Appropriately labels the data set with descriptive variable names. 
new_var_names <- colnames(DFmerge_extract)
new_var_names <- gsub("[(][)]", "", new_var_names)
new_var_names <- gsub("^t", "TimeDomain_", new_var_names)
new_var_names <- gsub("^f", "FrequencyDomain_", new_var_names)
new_var_names <- gsub("Acc", "Accelerometer", new_var_names)
new_var_names <- gsub("Gyro", "Gyroscope", new_var_names)
new_var_names <- gsub("Mag", "Magnitude", new_var_names)
new_var_names <- gsub("-std-", "_StandardDeviation_", new_var_names)
names(DFmerge_extract) <- new_var_names


#5.From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
tidy_dataset <- aggregate(DFmerge_extract[,3:81], by = list(activity = DFmerge_extract$activity, subject = DFmerge_extract$subject),FUN = mean)
