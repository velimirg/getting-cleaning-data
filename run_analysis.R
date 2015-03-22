library(data.table)
library(dplyr)

featureNames <- read.table("UCI HAR Dataset/features.txt")
activityLabels <- read.table("UCI HAR Dataset/activity_labels.txt", header=FALSE)

#read train and test data
subjectTrain <- read.table("UCI HAR Dataset/train/subject_train.txt", header=FALSE)
activityTrain <- read.table("UCI HAR Dataset/train/y_train.txt", header=FALSE)
featuresTrain <- read.table("UCI HAR Dataset/train/X_train.txt", header=FALSE)
subjectTest <- read.table("UCI HAR Dataset/test/subject_test.txt", header=FALSE)
activityTest <- read.table("UCI HAR Dataset/test/y_test.txt", header=FALSE)
featuresTest <- read.table("UCI HAR Dataset/test/X_test.txt", header=FALSE)

#PART 1

#merge train and test datasets
subject <- rbind(subjectTrain, subjectTest)
activity <- rbind(activityTrain, activityTest)
features <- rbind(featuresTrain, featuresTest)

#name the columns from the features file in variable featureNames
colnames(features) <- t(featureNames[2])

#Add activity and subject to features
colnames(activity) <- "Activity"
colnames(subject) <- "Subject"
completeData <- cbind(features,activity,subject)

#PART 2
#extract measurements on the mean and standard deviation for each measurement
columnsWithMeanSTD <- grep(".*Mean.*|.*Std.*", names(completeData), ignore.case=TRUE)

#add activity and subject columns
requiredColumns <- c(columnsWithMeanSTD, 562, 563)

#check number of variables
dim(completeData)
extractedData <- completeData[,requiredColumns]

#check number ofvariables in extracted data
dim(extractedData)

#PART 3 

extractedData$Activity <- as.character(extractedData$Activity)
for (i in 1:6) {
  extractedData$Activity[extractedData$Activity == i] <- as.character(activityLabels[i,2])
}

#activity variable as a factor
extractedData$Activity <- as.factor(extractedData$Activity)

#PART 4

#check variable names
names(extractedData)

names(extractedData)<-gsub("Acc", "Accelerometer", names(extractedData))
names(extractedData)<-gsub("Gyro", "Gyroscope", names(extractedData))
names(extractedData)<-gsub("BodyBody", "Body", names(extractedData))
names(extractedData)<-gsub("Mag", "Magnitude", names(extractedData))
names(extractedData)<-gsub("^t", "Time", names(extractedData))
names(extractedData)<-gsub("^f", "Frequency", names(extractedData))
names(extractedData)<-gsub("tBody", "TimeBody", names(extractedData))
names(extractedData)<-gsub("-mean()", "Mean", names(extractedData), ignore.case = TRUE)
names(extractedData)<-gsub("-std()", "STD", names(extractedData), ignore.case = TRUE)
names(extractedData)<-gsub("-freq()", "Frequency", names(extractedData), ignore.case = TRUE)
names(extractedData)<-gsub("angle", "Angle", names(extractedData))
names(extractedData)<-gsub("gravity", "Gravity", names(extractedData))

#check new variable names
names(extractedData)

#PART 5

#subject variable as a factor
extractedData$Subject <- as.factor(extractedData$Subject)
extractedData <- data.table(extractedData)

#Create a tidy data  with average for each activity and subject
tidyData <- aggregate(. ~Subject + Activity, extractedData, mean)

#order according to subject and activity
tidyData <- tidyData[order(tidyData$Subject,tidyData$Activity),]

#Write tidy data into a text file
write.table(tidyData, file = "tidy_data.txt", row.names = FALSE)

