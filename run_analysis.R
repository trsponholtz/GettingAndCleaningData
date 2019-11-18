#load dplyr library
library(dplyr)
library(plyr)


#read in data files
testfile <- "C:/Users/Todd/Desktop/CourseraCourses/JhuDataScience/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/test/X_test.txt"
trainfile <- "C:/Users/Todd/Desktop/CourseraCourses/JhuDataScience/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/train/X_train.txt"

test <- read.table(testfile)

train <- read.table(trainfile)

#read in activity files and replace numeric labels with descriptive 
testactivity <- "C:/Users/Todd/Desktop/CourseraCourses/JhuDataScience/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/test/y_test.txt"
trainactivity <- "C:/Users/Todd/Desktop/CourseraCourses/JhuDataScience/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/train/y_train.txt"

acttest <- read.table(testactivity)
acttest <- case_when( acttest[1]==1~"Walking",
                      acttest[1]==2~"Ascending Stairs",
                      acttest[1]==3~"Descending Stairs",
                      acttest[1]==4~"Sitting",
                      acttest[1]==5~"Standing",
                      acttest[1]==6~"Laying"
                      )

acttrain <-read.table(trainactivity)
acttrain <- case_when( acttrain[1]==1~"Walking",
                      acttrain[1]==2~"Ascending Stairs",
                      acttrain[1]==3~"Descending Stairs",
                      acttrain[1]==4~"Sitting",
                      acttrain[1]==5~"Standing",
                      acttrain[1]==6~"Laying"
)

#read in subject labels
subtestfile <- "C:/Users/Todd/Desktop/CourseraCourses/JhuDataScience/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/test/subject_test.txt"
subtrainfile <- "C:/Users/Todd/Desktop/CourseraCourses/JhuDataScience/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/train/subject_train.txt"

subtest <- read.table(subtestfile)

subtrain <- read.table(subtrainfile)


#add variable labels
labelfile <- "C:/Users/Todd/Desktop/CourseraCourses/JhuDataScience/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/features.txt"
labels <- read.table(labelfile)
labels <- as.character(labels[,2])

test <- setNames(test,labels)

train <- setNames(train,labels)


#restrict datasets to mean and standard deviation variables for all measurements
test2 <- test[,grepl("mean",colnames(test)) | grepl("std",colnames(test))]

train2 <- train[,grepl("mean",colnames(train)) | grepl("std",colnames(train))]


#add subject labels
train2['subjectID'] <-subtrain
test2['subjectID'] <- subtest


#add activity labels
train2['activity'] <- acttrain
test2['activity'] <- acttest

#add set variable, identifying if data is from training or testing dataset
train2['set'] <- "train"
test2['set'] <- "test"


#merge datasets
cleaned <- rbind(train2,test2)

#remove "-" and "()" from variable names

cleanedLabels <- gsub("-","",names(cleaned)) 

cleanedLabels <- gsub("\\()","", cleanedLabels)


#relabel variables with cleaned versions
cleaned <- setNames(cleaned,cleanedLabels)


#reorder variables and change names
cleaned2 <- select(cleaned, subjectID, set, activity, 
                        MeanBodyAccelerationXAxis = tBodyAccmeanX, 
                        StDeviationBodyAccelerationXAxis = tBodyAccstdX,
                        MeanBodyAccelerationYAxis = tBodyAccmeanY,
                        StDeviationBodyAccelerationYAxis = tBodyAccstdY,
                        MeanBodyAccelerationZAxis = tBodyAccmeanZ,
                        StDeviationBodyAccelerationZAxis = tBodyAccstdZ,
                   
                        MeanGravityAccelerationXAxis = tGravityAccmeanX, 
                        StDeviationGravityAccelerationXAxis = tGravityAccstdX,
                        MeanGravityAccelerationYAxis = tGravityAccmeanY,
                        StDeviationGravityAccelerationYAxis = tGravityAccstdY,
                        MeanGravityAccelerationZAxis = tGravityAccmeanZ,
                        StDeviationGravityAccelerationZAxis = tGravityAccstdZ,
                   
                        MeanBodyJerkXAxis=tBodyAccJerkmeanX,
                        StDeviationBodyJerkXAxis=tBodyAccJerkstdX,
                        MeanBodyJerkYAxis=tBodyAccJerkmeanY,
                        StDeviationBodyJerkYAxis=tBodyAccJerkstdY,
                        MeanBodyJerkZAxis=tBodyAccJerkmeanZ,
                        StDeviationBodyJerkZAxis=tBodyAccJerkstdZ,
                   
                        MeanBodyGyroXAxis = tBodyGyromeanX, 
                        StDeviationBodyGyroXAxis = tBodyGyrostdX,
                        MeanBodyGyroYAxis = tBodyGyromeanY, 
                        StDeviationBodyGyroYAxis = tBodyGyrostdY,
                        MeanBodyGyroZAxis = tBodyGyromeanZ, 
                        StDeviationBodyGyroZAxis = tBodyGyrostdZ,
                   
                        MeanGyroJerkXAxis = tBodyGyroJerkmeanX,
                        StDeviationGyroJerkXAxis = tBodyGyroJerkstdX,
                        MeanGyroJerkYAxis = tBodyGyroJerkmeanY,
                        StDeviationGyroJerkYAxis = tBodyGyroJerkstdY,
                        MeanGyroJerkZAxis = tBodyGyroJerkmeanZ,
                        StDeviationGyroJerkZAxis = tBodyGyroJerkstdZ,
                   
                        MeanMagnitudeBodyAcceleration = tBodyAccMagmean,
                        StDeviationMagnitudeBodyAcceleration = tBodyAccMagstd,
                        MeanMagnitudeBodyGyro = tBodyGyroMagmean,
                        StDeviationMagnitudeBodyGyro = tBodyGyroMagstd,
                        MeanMagnitudeGravityAcceleration = tGravityAccMagmean,
                        StdMagnitudeGravityAccereration = tGravityAccMagstd,
                        
                        MeanMagnitudeBodyJerk = tBodyAccJerkMagmean,
                        StDeviationMagnitudeBodyJerk = tBodyAccJerkMagstd,
                   
                        MeanMagnitudeBodyGyroJerk = tBodyGyroJerkMagmean,
                        StDeviationMagnitudeBodyGyroJerk = tBodyGyroJerkMagstd,
                   
                        FourierMeanBodyAccelerationXAxis = fBodyAccmeanX,
                        FourierStDeviationBodyAccelerationXAxis = fBodyAccstdX,
                        FourierMeanBodyAccelerationYAxis = fBodyAccmeanY,
                        FourierStDeviationBodyAccelerationYAxis = fBodyAccstdY,
                        FourierMeanBodyAccelerationZAxis = fBodyAccmeanZ,
                        FourierStDeviationBodyAccelerationZAxis = fBodyAccstdZ,
                           
                        FourierMeanBodyJerkXAxis = fBodyAccJerkmeanX,
                        FourierStDeviationBodyJerkXAxis = fBodyAccJerkstdX,
                        FourierMeanBodyJerkYAxis = fBodyAccJerkmeanY,
                        FourierStDeviationBodyJerkYAxis = fBodyAccJerkstdY,
                        FourierMeanBodyJerkZAxis = fBodyAccJerkmeanZ,
                        FourierStDeviationBodyJerkZAxis = fBodyAccJerkstdZ,
                   
                        FourierMeanBodyJerkFrequencyXAxis = fBodyAccJerkmeanFreqX,
                        FourierMeanBodyJerkFrequencyYAxis = fBodyAccJerkmeanFreqY,
                        FourierMeanBodyJerkFrequencyZAxis = fBodyAccJerkmeanFreqZ,
                   
                        FourierMeanBodyGyroJerkFrequencyXAxis = fBodyGyromeanFreqX,
                        FourierMeanBodyGyroJerkFrequencyYAxis = fBodyGyromeanFreqY,
                        FourierMeanBodyGyroJerkFrequencyZAxis = fBodyGyromeanFreqZ,
                   
                        FourierMeanMagnitudeBodyAcceleration = fBodyBodyGyroMagmean,
                        FourierStDeviationMagnitudeBodyAcceleration = fBodyBodyGyroMagstd,
                        FourierMeanMagnitudeBodyGyroJerkFrequency = fBodyBodyGyroMagmeanFreq,
                   
                        FourierMeanMagnitudeBodyGyroJerk = fBodyBodyGyroJerkMagmean,
                        FourierStDeviationMagnitudeBodyGyroJerk = fBodyBodyGyroJerkMagstd,
                        FourierMeanMangnitudeBodyGyroFreq = fBodyBodyGyroJerkMagmeanFreq
)


#save dataset to text
write.table(cleaned2,file="C:/Users/Todd/Desktop/CourseraCourses/JhuDataScience/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/finalDataSet.txt",
            row.name=FALSE)

#create dataset with means of all variables by subject and activity
cleaned3 <- group_by(cleaned2,subjectID,activity)
cleanSummary <- summarize(cleaned3,
                          
                          MeanBodyAccelerationXAxis = mean,
                          StDeviationBodyAccelerationXAxis = mean(StDeviationBodyAccelerationXAxis),
                          MeanBodyAccelerationYAxis = mean(MeanBodyAccelerationYAxis),
                          StDeviationBodyAccelerationYAxis = mean(StDeviationBodyAccelerationYAxis),
                          MeanBodyAccelerationZAxis = mean(MeanBodyAccelerationZAxis),
                          StDeviationBodyAccelerationZAxis = mean(StDeviationBodyAccelerationZAxis),
                          
                          MeanGravityAccelerationXAxis = mean(MeanGravityAccelerationXAxis), 
                          StDeviationGravityAccelerationXAxis = mean(StDeviationGravityAccelerationXAxis),
                          MeanGravityAccelerationYAxis = mean(MeanGravityAccelerationYAxis),
                          StDeviationGravityAccelerationYAxis = mean(StDeviationGravityAccelerationYAxis),
                          MeanGravityAccelerationZAxis = mean(MeanGravityAccelerationZAxis),
                          StDeviationGravityAccelerationZAxis = mean(StDeviationGravityAccelerationZAxis),
                          
                          MeanBodyJerkXAxis=mean(MeanBodyJerkXAxis),
                          StDeviationBodyJerkXAxis=mean(StDeviationBodyJerkXAxis),
                          MeanBodyJerkYAxis=mean(MeanBodyJerkYAxis),
                          StDeviationBodyJerkYAxis=mean(StDeviationBodyJerkYAxis),
                          MeanBodyJerkZAxis=mean(MeanBodyJerkZAxis),
                          StDeviationBodyJerkZAxis=mean(StDeviationBodyJerkZAxis),
                          
                          MeanBodyGyroXAxis = mean(MeanBodyGyroXAxis), 
                          StDeviationBodyGyroXAxis = mean(StDeviationBodyGyroXAxis),
                          MeanBodyGyroYAxis = mean(MeanBodyGyroYAxis), 
                          StDeviationBodyGyroYAxis = mean(StDeviationBodyGyroYAxis),
                          MeanBodyGyroZAxis = mean(MeanBodyGyroZAxis), 
                          StDeviationBodyGyroZAxis = mean(StDeviationBodyGyroZAxis),
                          
                          MeanGyroJerkXAxis = mean(MeanGyroJerkXAxis),
                          StDeviationGyroJerkXAxis = mean(StDeviationGyroJerkXAxis),
                          MeanGyroJerkYAxis = mean(MeanGyroJerkYAxis),
                          StDeviationGyroJerkYAxis = mean(StDeviationGyroJerkYAxis),
                          MeanGyroJerkZAxis = mean(MeanGyroJerkZAxis),
                          StDeviationGyroJerkZAxis = mean(StDeviationGyroJerkZAxis),
                          
                          MeanMagnitudeBodyAcceleration = mean(MeanMagnitudeBodyAcceleration),
                          StDeviationMagnitudeBodyAcceleration = mean(StDeviationMagnitudeBodyAcceleration),
                          MeanMagnitudeBodyGyro = mean(MeanMagnitudeBodyGyro),
                          StDeviationMagnitudeBodyGyro = mean(StDeviationMagnitudeBodyGyro),
                          MeanMagnitudeGravityAcceleration = mean(MeanMagnitudeGravityAcceleration),
                          StdMagnitudeGravityAccereration = mean(StdMagnitudeGravityAccereration),
                          
                          MeanMagnitudeBodyJerk = mean(MeanMagnitudeBodyJerk),
                          StDeviationMagnitudeBodyJerk = mean(StDeviationMagnitudeBodyJerk),
                          
                          MeanMagnitudeBodyGyroJerk = mean(MeanMagnitudeBodyGyroJerk),
                          StDeviationMagnitudeBodyGyroJerk = mean(StDeviationMagnitudeBodyGyroJerk),
                          
                          FourierMeanBodyAccelerationXAxis = mean(FourierMeanBodyAccelerationXAxis),
                          FourierStDeviationBodyAccelerationXAxis = mean(FourierStDeviationBodyAccelerationXAxis),
                          FourierMeanBodyAccelerationYAxis = mean(FourierMeanBodyAccelerationYAxis),
                          FourierStDeviationBodyAccelerationYAxis = mean( FourierStDeviationBodyAccelerationYAxis),
                          FourierMeanBodyAccelerationZAxis = mean(FourierMeanBodyAccelerationZAxis),
                          FourierStDeviationBodyAccelerationZAxis = mean(FourierStDeviationBodyAccelerationZAxis),
                          
                          FourierMeanBodyJerkXAxis = mean(FourierMeanBodyJerkXAxis),
                          FourierStDeviationBodyJerkXAxis = mean(FourierStDeviationBodyJerkXAxis),
                          FourierMeanBodyJerkYAxis = mean(FourierMeanBodyJerkYAxis),
                          FourierStDeviationBodyJerkYAxis = mean(FourierStDeviationBodyJerkYAxis),
                          FourierMeanBodyJerkZAxis = mean(FourierMeanBodyJerkZAxis),
                          FourierStDeviationBodyJerkZAxis = mean( FourierStDeviationBodyJerkZAxis),
                          
                          FourierMeanBodyJerkFrequencyXAxis = mean(FourierMeanBodyJerkFrequencyXAxis),
                          FourierMeanBodyJerkFrequencyYAxis = mean(FourierMeanBodyJerkFrequencyYAxis),
                          FourierMeanBodyJerkFrequencyZAxis = mean(FourierMeanBodyJerkFrequencyZAxis),
                          
                          FourierMeanBodyGyroJerkFrequencyXAxis = mean(FourierMeanBodyGyroJerkFrequencyXAxis),
                          FourierMeanBodyGyroJerkFrequencyYAxis = mean(FourierMeanBodyGyroJerkFrequencyYAxis),
                          FourierMeanBodyGyroJerkFrequencyZAxis = mean(FourierMeanBodyGyroJerkFrequencyZAxis),
                          
                          FourierMeanMagnitudeBodyAcceleration = mean(FourierMeanMagnitudeBodyAcceleration),
                          FourierStDeviationMagnitudeBodyAcceleration = mean(FourierStDeviationMagnitudeBodyAcceleration),
                          FourierMeanMagnitudeBodyGyroJerkFrequency = mean(FourierMeanMagnitudeBodyGyroJerkFrequency),
                          
                          FourierMeanMagnitudeBodyGyroJerk = mean(FourierMeanMagnitudeBodyGyroJerk),
                          FourierStDeviationMagnitudeBodyGyroJerk = mean(FourierStDeviationMagnitudeBodyGyroJerk),
                          FourierMeanMangnitudeBodyGyroFreq = mean(FourierMeanMangnitudeBodyGyroFreq))

#write summary data to txt file
write.table(cleaned2,file="C:/Users/Todd/Desktop/CourseraCourses/JhuDataScience/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/finalSummaryData.txt",
            row.name=FALSE)
                        