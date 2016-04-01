##########################################################################
# 0. Load Required Packages                                              #
##########################################################################
require(data.table)
require(dplyr)

##########################################################################
# 1. Load the files from 
# https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 
# and add labels
##########################################################################

features <- read.table("./UCI HAR Dataset/features.txt")[,2]

xTest <- read.table("./UCI HAR Dataset/test/X_test.txt")
xTrain <- read.table("./UCI HAR Dataset/train/X_train.txt")

names(xTest) <- features
names(xTrain) <- features

yTest <- read.table("./UCI HAR Dataset/test/y_test.txt")
yTrain <- read.table("./UCI HAR Dataset/train/y_train.txt")

names(yTest) <- "activity"
names(yTrain) <- "activity"

subjectTest <- read.table("./UCI HAR Dataset/test/subject_test.txt")
subjectTrain <- read.table("./UCI HAR Dataset/train/subject_train.txt")

names(subjectTest) <- "subjectID"
names(subjectTrain) <- "subjectID"

##########################################################################
# 2. Combine the data into one table                                     #
##########################################################################

testData <- cbind(subjectTest, yTest, xTest)

trainData <- cbind(subjectTrain, yTrain, xTrain)

allData <- rbind(testData, trainData)

##########################################################################
# 3. Extracts only the measurement on the mean and standard deviation for#
#    each measurement                                                    #
##########################################################################

filteredFeatures <- grepl("mean|std", features)
filteredFeatures[1:2] <- TRUE
allData <- allData[,filteredFeatures]

##########################################################################
# 4. Give the Activity a more descriptive label                          #
##########################################################################

activityLabels <- read.table("./UCI HAR Dataset/activity_labels.txt")[,2]

allData$activity <- factor(allData$activity, labels=activityLabels)

##########################################################################
# 5. Appropriately label the data set with descriptive variable names    #
##########################################################################

names(allData) <- gsub("^t", "time", names(allData))
names(allData) <- gsub("^f", "frequency", names(allData))
names(allData) <- gsub("Acc", "Accelerometer", names(allData))
names(allData) <- gsub("Gyro", "Gyroscope", names(allData))
names(allData) <- gsub("Mag", "Magnitude", names(allData))
names(allData) <- gsub("BodyBody", "Body", names(allData))
names(allData) <- gsub("\\(\\)", "", names(allData))
names(allData) <- gsub(",", "-", names(allData))
names(allData) <- gsub("-", "_", names(allData))

##########################################################################
# 6. Create a second, independent tidy data set with the average of each #
#    variable for each activity and each subject                         #
##########################################################################

tidyData <- aggregate(. ~subjectID + activity, allData, mean)
tidyData <- tidyData[order(tidyData$subjectID,tidyData$activity),]
write.table(tidyData, file = "./UCI HAR Dataset/Tidy.txt", row.names = FALSE)

