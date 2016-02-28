## Setting working directory
setwd("./G&C_data_w4_quiz/Course_project")

##loading libraries
library(plyr)
library(dplyr)
library(reshape2)

## URL for download
fileUrl<-"https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"

## Downloadig file
download.file(fileUrl, destfile="./UCI_HAR_Dataset.zip", method ="curl")

##Unzipping archive file
unzip("UCI_HAR_Dataset.zip")

# Feature & Activity Names
featureNames  <- read.table("./UCI_HAR_Dataset/features.txt", stringsAsFactors=FALSE)[[2]]
activityLabels <- read.table("./UCI_HAR_Dataset/activity_labels.txt", col.names=c("activityID", "activity"))

# Load training data
xTrain <- read.table("./UCI_HAR_Dataset/train/X_train.txt")
yTrain <- read.table("./UCI_HAR_Dataset/train/y_train.txt", col.names=c("activityID"))
subjTrain <- read.table("./UCI_HAR_Dataset/train/subject_train.txt", col.names=c("subjectID"))

# Load test data
xTest <- read.table("./UCI_HAR_Dataset/test/X_test.txt")
yTest <- read.table("./UCI_HAR_Dataset/test/y_test.txt", col.names=c("activityID"))
subjTest <- read.table("./UCI_HAR_Dataset/test/subject_test.txt", col.names=c("subjectID"))

# Setting names for xTest and xTrain
colnames(xTest) <- featureNames
colnames(xTrain) <- featureNames

#1 Merges the training and the test sets to create one data set.
# Creating the final training set by merging yTrain, subjectTrain, and xTrain
trainingData = cbind(subjTrain,yTrain,xTrain);

# Creating the final test set by merging the xTest, yTest and subjectTest data
testData = cbind(subjTest,yTest,xTest);


# Merge training and test data
merged_Data2 = rbind(trainingData,testData);

## adding SubjectID and Activity to featureNames
featureNames<-names(merged_Data2)

#2 Extracts only the measurements on the mean and standard deviation for each measurement.
Std_Mean_index <- grepl("mean\\()|std\\()", featureNames)

# re-adding subjectID and activity columns
Std_Mean_index[1:2] <- TRUE
Std_Mean_Names <- featureNames[Std_Mean_index]

merged_Data_sm2 <- merged_Data2[,Std_Mean_Names]

#3 Uses descriptive activity names to name the activities in the data set
merged_Data_sm2$activityID <- activityLabels[merged_Data_sm2$activityID, 2]

# Rename 2nd column from ActivityID to Activity
names(merged_Data_sm2)[2]<-"Activity"

#4 Appropriately labels the data set with descriptive variable names.
names(merged_Data_sm2) <- gsub("\\()","",names(merged_Data_sm2))
names(merged_Data_sm2) <- sub("^f", "Frequency", names(merged_Data_sm2))
names(merged_Data_sm2) <- sub("^t", "Time", names(merged_Data_sm2))
names(merged_Data_sm2) <- sub("Acc", "Acceleration", names(merged_Data_sm2))
names(merged_Data_sm2) <- sub("Mag", "Magnitude", names(merged_Data_sm2))
names(merged_Data_sm2) <- sub("Gyro", "Gyroscope", names(merged_Data_sm2))
names(merged_Data_sm2) <- sub("-mean\\(\\)", "Mean", names(merged_Data_sm2))
names(merged_Data_sm2) <- sub("-std\\(\\)", "StandardDeviation", names(merged_Data_sm2))

#5 From the data set in step 4, creates a second, independent tidy data set with 
##the average of each variable for each activity and each subject.
  
tidy_Data <- merged_Data_sm2 %>%
        group_by(subjectID, Activity) %>%
        summarise_each(funs(mean))

# Writing the final Tidy Data Set to file
write.table(tidy_Data, file="tidy_Data.txt", row.name=FALSE,sep='\t')




