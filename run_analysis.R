
#0 Download and unzip data set

if(!file.exists("./data")){dir.create("./data")}
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileUrl,destfile="./data/Dataset.zip",method="curl")

unzip(zipfile="./data/Dataset.zip",exdir="./data")

path_rf <- file.path("./data" , "UCI HAR Dataset")
files<-list.files(path_rf, recursive=TRUE)
files

#


#1 Merge training and test sets

#1.1 Read files

#a Read training tables

x_train <- read.table("./data/UCI HAR Dataset/train/X_train.txt")
y_train <- read.table("./data/UCI HAR Dataset/train/y_train.txt")
subject_train <- read.table("./data/UCI HAR Dataset/train/subject_train.txt")

#b Read testing tables
x_test <- read.table("./data/UCI HAR Dataset/test/X_test.txt")
y_test <- read.table("./data/UCI HAR Dataset/test/y_test.txt")
subject_test <- read.table("./data/UCI HAR Dataset/test/subject_test.txt")

#c Read features vector
features <- read.table('./data/UCI HAR Dataset/features.txt')

#d Read activity labels
activityLabels <- read.table('./data/UCI HAR Dataset/activity_labels.txt')

#1.2 Assign column names

colnames(x_train) <- features[,2] 
colnames(y_train) <-"activityId"
colnames(subject_train) <- "subjectId"

colnames(x_test) <- features[,2] 
colnames(y_test) <- "activityId"
colnames(subject_test) <- "subjectId"

colnames(activityLabels) <- c('activityId','activityType')

#1.3 Merge all data into a single set
train <- cbind(y_train, subject_train, x_train)
test <- cbind(y_test, subject_test, x_test)
merged <- rbind(train, test)

#2. Extract only the measurements on the mean and standard deviation for each measurement

#2.1 Read column names
colNames <- colnames(merged)

#2.2 Create vector for defining ID, mean and standard deviation
id_mean_and_std <- (grepl("activityId" , colNames) | 
                     grepl("subjectId" , colNames) | 
                     grepl("mean.." , colNames) | 
                     grepl("std.." , colNames) 
)


#2.3 Create subset of data with ID, mean and standard deviation
subset <- merged[ , id_mean_and_std == TRUE]

#3. Use descriptive activity names to name the activities in the data set
labeledDataSet <- merge(subset, activityLabels, by='activityId', all.x=TRUE)

#4. Appropriately labels the data set with descriptive variable names

#4a. Clean names

cnames<-names(labeledDataSet)
cnames<-gsub(pattern = "-", replacement = "_", x= cnames)
cnames<-gsub(pattern = "\\(", replacement = "", x= cnames)
cnames<-gsub(pattern = "\\)", replacement = "", x= cnames)

colnames(labeledDataSet)<-cnames

#4b. Assigned cleaned names to subset
labeledDataSet<- select(labeledDataSet, subjectId, activityId, activityType, tBodyAcc_mean_X:fBodyBodyGyroJerkMag_meanFreq)


#5. Create a second, independent tidy data set with the average of each variable for each activity and each subject

#3.1 Make second tidy data set
TidyData <- aggregate(. ~subjectId + activityId, labeledDataSet[,-3], mean)
TidyData <- merge(TidyData, activityLabels, by='activityId', all.x=TRUE)
TidyData<- select(TidyData, subjectId, activityType, tBodyAcc_mean_X:fBodyBodyGyroJerkMag_meanFreq)
TidyData <- TidyData[order(TidyData$subjectId, TidyData$activityType),]

#5.a. Write to csv file

write.csv(TidyData, "TidyDataSet.csv", row.names=FALSE)



