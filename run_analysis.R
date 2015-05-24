## This script is the programming assignment for the get-data-01 
## Getting & Cleaning Data course.
## PURPOSE: This script merges the training and test sets to create one 
## data set. It then extracts only the measurements on the mean and 
## standard deviation for each measurement. The results are tidy data. 
## Descriptive activity names are used to name the activities in the 
## data set, and the data set is appropriately labeled 
## with descriptive variable names. This script makes two passes through 
## the data; from the initial data set, a second, independent tidy data set 
## is created with the average of each variable for each activity 
## and each subject.

## The original data came from: 
## http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones
## This data has previously been downloaded into a local file & unzipped
library(utils)
library(dplyr)

## DATA TRANSFORMATION 1 - Merges train & test data sets.
## Also removes feature number (1-561) while reading data
## from files into data tables. Made the decision to order the training data 
## first since it is the larger of the two data sets (train-70% & test-30%)

        ## Establishes a data frame & reads feature titles into data frame 
        ## from the feature text file
        dataFeature <- data.frame()
        dataFeature <- read.table ("features.txt")
        
        dataActivity <- data.frame()
        dataActivity2 <- data.frame()
        dataActivity <-read.table ("./y_train.txt")
        dataActivity2 <- read.table("./y_test.txt")
        dataActivity <- rbind(dataActivity, dataActivity2)
        
        subject <- data.frame()
        subject <-read.table ("./subject_train.txt")

        subject2 <- data.frame()
        subject2 <-read.table ("./subject_test.txt")
        subject <- rbind(subject, subject2)
        subject <- cbind(subject, dataActivity)
        colnames(subject) <- c("subject", "activity")
        
        ## Establishes three data frames, then reads data into two of them 
        ## from each of the two text files, X_train and X_test, then row binds 
        ## the train and test into the third data frame, dataTotal. 
        ## This approach keeps the train and test data easily available in 
        ## their own data frames, if needed.
        dataTrain <- data.frame()
        dataTest <- data.frame()
        dataTotal <- data.frame()
                
        ## Reads train & test data into data frames from files
        dataTrain <- read.table("./X_train.txt")

        ## Adds the subject names to the train data set as row titles
        ## This doesn't work as titles since rownames must be unique.     
        dataTest <- read.table("./X_test.txt")
 
        ## Moves train data into the "top" of the total data frame, then 
        ## appends test data after the train data
        dataTotal <- rbind(dataTrain, dataTest)
        
        ## Adds the feature names to the total data set as column titles
        colnames(dataTotal) <- dataFeature [ , 2]
        


## DATA TRANSFORMATION 2 - Retains only means & standard deviation columns,
## i.e. removes variables that are not of interest. 
        
        ## Subset the data table to return only the desired variables 
        ## (feature names) using grepl (logical grep) of regular expression 
        ## "mean|std" to match the desired variables in the column names 
        dataTotal <- dataTotal[ , grepl("mean|std", names(dataTotal))]
        dataTotal <- cbind(subject, dataTotal)

        

## DATA TRANSFORMATION 3 - Applies descriptive activity names to the 
## activities in the data set. Is there a more elegant way to do this?

dataTotal$activity <- gsub("1", "walking", dataTotal$activity, fixed=TRUE)
dataTotal$activity <- gsub("2", "walkingupstairs", dataTotal$activity, fixed=TRUE)
dataTotal$activity <- gsub("3", "walkingdownstairs", dataTotal$activity, fixed=TRUE)
dataTotal$activity <- gsub("4", "sitting", dataTotal$activity, fixed=TRUE)
dataTotal$activity <- gsub("5", "standing", dataTotal$activity, fixed=TRUE)
dataTotal$activity <- gsub("6", "lying", dataTotal$activity, fixed=TRUE)



## DATA TRANSFORMATION 4 - Creates more analyzable variable names & labels by 
## making lower case and removing punctuation marks "-" and "()".

        ## Removes punctuation marks & make variable names lower case
        ## Uses fixed=TRUE so don't have to apply escape characters
        names(dataTotal) <- tolower(names(dataTotal))
        names(dataTotal) <- gsub("-", "", names(dataTotal), fixed=TRUE)
        names(dataTotal) <- gsub("()", "", names(dataTotal), fixed=TRUE)

## mean, std=standard deviation, t=time, f=frequency, x,y,z = 3-dim coordinates.

     

## DATA TRANSFORMATION 5 - Groups the data by subject and then by activity
## and presents the mean of each feature measurement within those groupings.
## Apparently, the British spelling of "summarise" must be used.
        ## Suggested by Luke question & update Dec 21 '14  on Stackflow 
        ## and by course discussion forums, esp Gregory D. Horne post
        dataTidy <- data.frame()
        dataTidy <- group_by(dataTotal, subject, activity) %>% summarise_each(
                        funs(mean), tbodyaccmeanx:fbodybodygyrojerkmagmeanfreq)


## Write the tidy data set to file. Note: Selected text file export since all 
## the other files in this assignment were text.
write.table(dataTidy, file="tidy-data.txt", row.name=FALSE)


## for testing - 
print(head(dataTidy, n=20))
print(tail(dataTidy, n=20))
