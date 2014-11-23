# Getting and Cleaning Data Course Project

# 1.Merges the training and the test sets to create one data set.
# 2.Extracts only the measurements on the mean and standard deviation for each measurement. 
# 3.Uses descriptive activity names to name the activities in the data set
# 4.Appropriately labels the data set with descriptive variable names. 
# 5.From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

# 1. Read data and merge in three datasets.

# train data
datatrainX<- read.table("../UCI HAR Dataset/train/X_train.txt")
datatrainY <- read.table("../UCI HAR Dataset/train/y_train.txt")
datatrainsubject <- read.table("../UCI HAR Dataset/train/subject_train.txt")

# test data
datatestX <- read.table("../UCI HAR Dataset/test/X_test.txt")
datatestY <- read.table("../UCI HAR Dataset/test/y_test.txt")
datatestsubject <- read.table("../UCI HAR Dataset/test/subject_test.txt")

# merge data
datamergeX <- rbind(datatrainX, datatestX)
datamergeY <- rbind(datatrainY, datatestY)
datamergesubject <- rbind(datatrainsubject, datatestsubject)




# 2. load features.txt to find mean and std labels

features <- read.table("../UCI HAR Dataset/features.txt")
data_mean <- grep("-mean\\(\\)|-std\\(\\)", features[, 2])
datamergeX <- datamergeX[, data_mean]

names(datamergeX) <- features[data_mean, 2]
names(datamergeX) <- gsub("\\(|\\)", "", names(datamergeX))
names(datamergeX) <- tolower(names(datamergeX))

# 3. load activity_labels.tx with labels

labels <- read.table("../UCI HAR Dataset/activity_labels.txt")
labels[, 2] = gsub("_", "", tolower(as.character(labels[, 2])))
datamergeY[,1] = labels[datamergeY[,1], 2]
names(datamergeY) <- "activity"

# 4. Merge and labeled data

names(datamergesubject) <- "subject"
datalabeled<- cbind(datamergesubject, datamergeY, datamergeX)
write.table(datalabeled, "labeled_data.txt")

# 5. find subjects,activities and calculate average of each variable of them 

uniqueSubjects = unique(datamergesubject)[,1]
numSubjects = length(unique(datamergesubject)[,1])
numLabels = length(labels[,1])
numCols = dim(datalabeled)[2]
result = datalabeled[1:(numSubjects*numLabels), ]

i = 1
for (j in 1:numSubjects) {
  for (k in 1:numLabels) {
    result[i, 1] = uniqueSubjects[j]
    result[i, 2] = labels[k, 2]
    tmp <- datalabeled[datalabeled$subject==j & datalabeled$activity==labels[k, 2], ]
    result[i, 3:numCols] <- colMeans(tmp[, 3:numCols])
    i = i+1
  }
}
write.table(result, "averages_data.txt")
