########################################### Getting and Cleaning Data Course Project ########################################
# 
# This is the last assignemnt project for Coursera Getting and Cleaning Data.
# In this project I will follow the bellow tasks to create a tidy dataset file
# 1- Merges the training and the test sets to create one data set.
# 2- Extracts only the measurements on the mean and standard deviation for each measurement.
# 3- Uses descriptive activity names to name the activities in the data set
# 4- Appropriately labels the data set with descriptive variable names.
# 5- From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject

###############################################################################################################################
#
# check work directory and assign the right work directory
getwd()
#setwd("C:/Users/Username/Documents/Data Scientist/Getting and Cleaning Data/MyProject")

############ Task 0 ############################################################################################################

# check if data directory exist, if not create one and download the file from the repository into the work directory and then unzip the folder
# https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip
# get the zip folder and unzip it in the work directory
if(!file.exists("./data")){dir.create("./data")}
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileUrl,destfile ="./data/UCI HAR Dataset.zip" )

#Once it is certain zip file is downloded let unzip it by using the unizip command in R

if (!file.exists("./data/UCI HAR Dataset")) {
  unzip(zipfile = "./data/UCI HAR Dataset.zip", exdir = "./data")
}




############ Task 1 Merges the training and the test sets to create one data set. ##############################################
#
# read the traning set,Training labels and the subject_train data respectevly into x_train, y_train, subject_train table variables
x_train <- read.table("./data/UCI HAR Dataset/train/X_train.txt")
y_train <- read.table("./data/UCI HAR Dataset/train/y_train.txt")
subject_train <- read.table("./data/UCI HAR Dataset/train/subject_train.txt")



# Read the test data set, labels and  subject respectevely into  X_test.txt, y_test.txt and subject_test
x_test <- read.table("./data/UCI HAR Dataset/test/X_test.txt")
y_test <- read.table("./data/UCI HAR Dataset/test/y_test.txt")
subject_test <- read.table("./data/UCI HAR Dataset/test/subject_test.txt")


#Read the list of all features data
features <- read.table("./data/UCI HAR Dataset/features.txt", as.is = TRUE)

# Read the activity_labels.txt which links the class labels with their activity name
activity_labels <- read.table("./data/UCI HAR Dataset/activity_labels.txt")



# Merged train dataset, test dataset and then merged all of them together
allMerged <- rbind(
trainMerged <- cbind(subject_train,x_train,y_train),
testMerged <- cbind(subject_test,x_test,y_test)
)


# assign colunm name
colnames(activity_labels) <- c('activityId', 'activityType')
colnames(allMerged) <- c("subjectId",features[,2], "activityId")


############ Task 2 Extracts only the measurements on the mean and standard deviation for each measurement.################

allMerged<- allMerged[,grepl("subjectId|activityId|mean|std", colnames(allMerged))]



############ Task 3 Uses descriptive activity names to name the activities in the data set#################################

#Getting the name of the activity instead 
allMerged$activityId <- factor(allMerged$activityId, levels = activity_labels[, 1], labels = activity_labels[, 2])


############ Task4 Appropriately labels the data set with descriptive variable names. #####################################
#Put allMerged columns name in a variable

allMegedValidcolNames  <- colnames(allMerged); 



# Cleaning up the variable names

  allMegedValidallMegedValidcolNames<- gsub("\\()","",allMegedValidcolNames)
  allMegedValidcolNames <- gsub("-std$","StdDev",allMegedValidcolNames)
  allMegedValidcolNames <-gsub("-mean","Mean",allMegedValidcolNames)
  allMegedValidcolNames <- gsub("^(t)","time",allMegedValidcolNames)
  allMegedValidcolNames <- gsub("^(f)","freq",allMegedValidcolNames)
  allMegedValidcolNames <-gsub("([Gg]ravity)","Gravity",allMegedValidcolNames)
  allMegedValidcolNames <- gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",allMegedValidcolNames)
  allMegedValidcolNames <- gsub("[Gg]yro","Gyro",allMegedValidcolNames)
  allMegedValidcolNames <- gsub("AccMag","AccMagnitude",allMegedValidcolNames)
  allMegedValidcolNames <- gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",allMegedValidcolNames)
  allMegedValidcolNames <- gsub("JerkMag","JerkMagnitude",allMegedValidcolNames)
  allMegedValidcolNames <- gsub("GyroMag","GyroMagnitude",allMegedValidcolNames)

 
  # reassigning valid column name 
colnames(allMerged) <- allMegedValidcolNames


############ Task 5 From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject
#calculating average for eachgorup by  acitivity and subject
myTidyData <-  aggregate(.~subjectId + activityId, allMerged, mean)
myTidyData <- myTidyData[order(myTidyData$subjectId,myTidyData$activityId),]

# write output into a file
write.table(myTidyData, "tidyData", row.names = FALSE)
