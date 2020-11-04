###1.Merges the training and the test sets to create one data set.

##1.1load package may be needed
library(dplyr)
library(data.table)

##1.2Download and read dataset
fileurl = 'https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip'
if (!file.exists('./UCI HAR Dataset.zip')){
  download.file(fileurl,'./UCI HAR Dataset.zip', mode = 'wb')
  unzip("UCI HAR Dataset.zip", exdir = getwd())
}

features <- read.table("UCI HAR Dataset/features.txt")
activities <- read.table("UCI HAR Dataset/activity_labels.txt")
subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt")
x_test <- read.table("UCI HAR Dataset/test/X_test.txt")
y_test <- read.table("UCI HAR Dataset/test/y_test.txt")
subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt")
x_train <- read.table("UCI HAR Dataset/train/X_train.txt")
y_train <- read.table("UCI HAR Dataset/train/y_train.txt")

##1.3combine x, y(activities), subject respectively
x<-rbind(x_test,x_train)
y<-rbind(y_test,y_train)
subject<-rbind(subject_test,subject_train)

##1.4naming the columns
colnames(x) <- features[,2]
colnames(y) <- "Activity"
colnames(subject) <- "Subject"

##1.5merge
total_data <- cbind(subject,y,x)

###2.Extracts only the measurements on the mean and standard deviation for each measurement.
TidyData <- select(total_data,Subject,Activity, contains("mean"), contains("std"))

###3.Uses descriptive activity names to name the activities in the data set.
TidyData$Activity <- activities[TidyData$Activity, 2]

###4.Appropriately labels the data set with descriptive variable names.
#Acc can be replaced with Accelerometer
#Gyro can be replaced with Gyroscope
#BodyBody can be replaced with Body
#Mag can be replaced with Magnitude
#Character f can be replaced with Frequency
#Character t can be replaced with Time

names(TidyData)<-gsub("Acc", "Accelerometer", names(TidyData))
names(TidyData)<-gsub("Gyro", "Gyroscope", names(TidyData))
names(TidyData)<-gsub("Mag", "Magnitude", names(TidyData))
names(TidyData)<-gsub("BodyBody", "Body", names(TidyData))
names(TidyData)<-gsub("^t", "Time", names(TidyData))
names(TidyData)<-gsub("^f", "Frequency", names(TidyData))
names(TidyData)<-gsub("tBody", "TimeBody", names(TidyData))
names(TidyData)<-gsub("-mean()", "Mean", names(TidyData), ignore.case = TRUE)
names(TidyData)<-gsub("-std()", "STD", names(TidyData), ignore.case = TRUE)
names(TidyData)<-gsub("-freq()", "Frequency", names(TidyData), ignore.case = TRUE)
names(TidyData)<-gsub("angle", "Angle", names(TidyData))
names(TidyData)<-gsub("gravity", "Gravity", names(TidyData))

###From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

FinalData <- aggregate(TidyData[,3:88], by = list(activity = TidyData$Activity, subject = TidyData$Subject),FUN = mean)
write.table(FinalData, "FinalData.txt", row.name=FALSE)
