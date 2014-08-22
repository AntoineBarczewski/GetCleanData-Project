#note that the file adresses are written for windows

#we load the plyr package
library("plyr")

#The file is supposed unzipped and store in the current directory with the following name
dataFolder = "UCI HAR Dataset\\"

#Here is a function to get the path to each piece of data contained in the file
getFilePath <- function(filepath) {
  paste(dataFolder, filepath, sep="")
}

#We read all the data required
XTrain <- read.table(getFilePath("train\\X_train.txt"))
XTest <- read.table(getFilePath("test\\X_test.txt"))
YTrain <- read.table(getFilePath("train\\y_train.txt"))
YTest <- read.table(getFilePath("test\\y_test.txt"))
subjectTrain <- read.table(getFilePath("train\\subject_train.txt"))
subjectTest <- read.table(getFilePath("test\\subject_test.txt"))
features <- read.table(getFilePath("features.txt"))
activityLabels <- read.table(getFilePath("activity_labels.txt"))

#For each data type, we merge the train and test data
XData<-rbind(XTrain,XTest)
YData<-rbind(YTrain,YTest)
SubjectData<-rbind(subjectTrain,subjectTest)

#We spot the positions of the measurement we need, here std and mean
meanOrStdPosition<-grep("mean|std",features[,2])

#We extract the right features names and the corresponding data
rightFeatures<-features[meanOrStdPosition,2]
XData<-XData[,meanOrStdPosition]

#We name the data with the right features names (we take off the "()")
rightFeatures<-gsub("\\(\\)","",rightFeatures)
names(XData)<-rightFeatures

#We get the name of the activity for every observation from activity labels
YData[,1]<-activityLabels[YData[,1],2]

#we name the columns dedicated to activities and subjects
names(YData)<-"ActivityLabel"
names(SubjectData)<-"SubjectID"

#we merge the dataframes
tidyData<-cbind(SubjectData,YData,XData)

#we print the tidy data
write.table(tidyData, "tidy_data.txt", row.name=FALSE)

#we spot the two columns to remove from the calculation asked for the second data table
excludedColumns = which(names(tidyData) %in% c("subjectnumber", "activity"))

#we calculate the mean of each column per subject and per activity
result <- ddply(tidyData, .(SubjectID, activityLabels), .fun=function(x){ colMeans(x[,-excludedColumns]) })

#we print the second data table
write.table(result, "average_data.txt", row.name=FALSE)