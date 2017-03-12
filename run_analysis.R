##' A full description is available at the site where the data was obtained:
##' http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones
##' 
##' Here are the data for the project:
##' https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip
##' 
##' You should create one R script called run_analysis.R that does the following.
##' 
##' 1. Merges the training and the test sets to create one data set.
##' 2. Extracts only the measurements on the mean and standard deviation for each measurement.
##' 3. Uses descriptive activity names to name the activities in the data set
##' 4. Appropriately labels the data set with descriptive variable names.
##' 5. From the data set in step 4, creates a second, independent tidy data set with 
##' the average of each variable for each activity and each subject.

##' Install required packages
#install.packages("dplyr")
library(dplyr)
library(tidyr)

##' Create necessary variables
mydestfile <- "dataset.zip"
myfileurl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
mydestdir <- "data"
mydestsubdir <- "UCI HAR Dataset"

myfeaturesfile <- "features.txt"
myactivitylabelsfile <- "activity_labels.txt"

writeDestDir = "outputdata"
writeDestTidyDsFilename = "HARtidydataset.csv"
writeDestAverageDsFilename = "HARaveragesdataset.csv"

##' This function downloads the raw data from the url location, places it in the destdir/subdestdir folder
##' (which will be created if needed), and stores it as destfile.
##' If destfile already exists, no download will take place, unless forcedownload is TRUE.
##' Finally, the destfile will be unzipped.
##' forcedownload - if TRUE, the data from the url will be downloaded even if a local copy already exists
downloadData <- function(url, destfile, destdir, forcedownload = FALSE) {
    ##' Create the data directory and download the data
    if(!file.exists(destdir)) {
        dir.create(destdir)
    }
    
    ##' If there is already a copy of the raw data in the destdir, don't download,
    ##' unless forcedownload = true
    if(any(!file.exists(file.path(destfile)), forcedownload)) {
        
        download.file(myfileurl,destfile = destfile, mode="wb")
        dateDownloaded <- date()
    }
    
    ##' Move to the data directory and unzip the data
    ##' Any file with the same name that is already in destdir WILL be overwritten
    if(any(!file.exists(file.path(destfile)), forcedownload)) {
        setwd(destdir)
        unzip(file.path("..",destfile), overwrite = TRUE)
        
        ##' Lastly, move back up
        setwd(file.path(".."))    
    }
}

loadsubjectids <- function(type, destdir, destsubdir) {
    setwd(file.path(destdir,destsubdir,type))
    
    rawsubjectDf <- read.table(file = paste("subject_",type,".txt", sep = ""), header = FALSE, 
                               colClasses = "integer", stringsAsFactors = FALSE)
    subjectIdsTibble <- tbl_df(rawsubjectDf)
    rm(rawsubjectDf)
    
    subjectIdsTibble <- subjectIdsTibble %>%
        rename(subjectid = V1)
    
    ##' Move back to original working directory
    setwd(file.path("..","..",".."))
    subjectIdsTibble
}

loadactivityids <- function(type, destdir, destsubdir) {
    setwd(file.path(destdir,destsubdir,type))
    
    rawactivityDf <- read.table(file = paste("y_",type,".txt", sep = ""), header = FALSE, 
                                colClasses = "integer", stringsAsFactors = FALSE)
    activityTibble <- tbl_df(rawactivityDf)
    rm(rawactivityDf)
    
    activityTibble <- activityTibble %>%
        rename(activityid = V1)
    
    ##' Move back to original working directory
    setwd(file.path("..","..",".."))
    activityTibble
}

getfeaturesdata <- function(type = "train", destdir, destsubdir, featurelabels) {
    setwd(file.path(destdir,destsubdir,type))
    
    rawfeaturesdatadf <- read.table(file = paste("X_",type,".txt", sep = ""), header = FALSE, 
                                    colClasses = "numeric", stringsAsFactors = FALSE)
    fdTibble <- tbl_df(rawfeaturesdatadf)
    rm(rawfeaturesdatadf)
    
    fnames <- c(featurelabels$featurename)
    colnames(fdTibble) <- fnames
    
    ##' Drop any column that doesn't contain mean() or std().
    fdTibble <- select(fdTibble, matches("mean\\(\\)|std\\(\\)"))
    
    ##' Move back to original working directory
    setwd(file.path("..","..",".."))
    
    fdTibble
}

getTibble <- function(type = "train", destdir, destsubdir, featureLabels, activityLabels) {
    
    ##' Load the subject ids in a tibble
    subjectids <- loadsubjectids(type, destdir, destsubdir)
    theTibble <- tbl_df(subjectids)
    rm(subjectids)
    
    ##' Load the activity ids and colbind to the tibble
    activityids <- loadactivityids(type, destdir, destsubdir)
    theTibble <- tbl_df(cbind.data.frame(theTibble, activityids))
    rm(activityids)
    
    ##' Load the dataset
    theTibble <- tbl_df(cbind.data.frame(theTibble,getfeaturesdata(type, destdir, destsubdir, featureLabels)))
    theTibble
}

##' Load the feature labels and select mean and standard deviation for each measurement
##' mean(): Mean value
##' std(): Standard deviation
##' Returns a tibble with two columns and one row per feature:
##' 1. id - The col id
##' 2. featurename - The feature name
loadFeatureLabels <- function(destdir,destsubdir) {
    
    setwd(file.path(destdir,destsubdir))
    
    rawfeaturesDf <- read.table(file = myfeaturesfile, header = FALSE, 
                                colClasses = "character", stringsAsFactors = FALSE)
    featuresTibble <- tbl_df(rawfeaturesDf)
    rm(rawfeaturesDf)
    
    ##' Check for unique featurenames
    #fnames <- c(featuresTibble$V2)
    #print("Number of unique featurenames = length(unique(fnames)) =")
    #print(length(unique(fnames)))
    #print("Add row id + ### to featurenames to make them unique.")
    
    featuresTibble <- featuresTibble %>%
        mutate(V2 = paste(V1,"###",V2, sep="")) %>%
        rename(id = V1, featurename = V2)
    
    setwd(file.path("..",".."))
    featuresTibble
}

loadActivityLabels <- function(destdir,destsubdir) {
    ##' Load the activity labels
    setwd(file.path(destdir,destsubdir))
    
    rawactivitylabelsDf <- read.table(file = myactivitylabelsfile, header = FALSE, 
                                      colClasses = "character", stringsAsFactors = FALSE)
    activityLabelsTibble <- tbl_df(rawactivitylabelsDf)
    rm(rawactivitylabelsDf)
    
    ##' Clean the data
    ##' Make id an int
    ##' Rename col names
    activityLabelsTibble <- activityLabelsTibble %>%
        rename(id = V1, activity = V2) %>%
        mutate(id = as.integer(id)) 
    
    setwd(file.path("..",".."))
    activityLabelsTibble
}

##' This function writes a dataframe df in csv format to the destdir path
##' (which will be created if needed), with the name passed in destfilename.
##' If destfilename already exists, it will be overwritten if overwritefile is TRUE.
writeCsvData <- function(df, destdir, destfilename, overwritefile = FALSE) {
    ##' Create the destination directory if needed
    if(!file.exists(destdir)) {
        dir.create(destdir)
    }
    
    ##' If destfilename already exists, only overwrite if overwritefile is TRUE
    if(any(!file.exists(file.path(c(destdir, destfilename)), overwritefile))) {
        setwd(file.path(destdir))
        write.csv(df, file = destfilename, row.names = FALSE)
        setwd(file.path(".."))
    }
}

##' Start
##' Download the raw data from the url
downloadData(myfileurl, mydestfile, mydestdir, forcedownload = FALSE)

##' Load the selected features
featureLabels <- loadFeatureLabels(destdir = mydestdir, destsubdir = mydestsubdir)
#featureLabels

##' Load the activity labels
activityLabels <- loadActivityLabels(destdir = mydestdir, destsubdir = mydestsubdir)
#activityLabels

##' Take a look at the feature and activity labels
str(featureLabels)
activityLabels

##' Get the train and test data sets as tibbles (two identically formatted data tibbles)
trainTibble <- getTibble(destdir = mydestdir, destsubdir = mydestsubdir, type = "train", featureLabels, activityLabels)
testTibble <- getTibble(destdir = mydestdir, destsubdir = mydestsubdir, type = "test", featureLabels, activityLabels)
rm(featureLabels)

##' Merge the train and test tibbles
tidyTibble <- tbl_df(rbind.data.frame(trainTibble,testTibble))
rm(trainTibble)
rm(testTibble)

##' Clean up the data:
##' Look for missing and strange values
##' 1. Remove leading indexes + ### in colnames
#print("Check that colnames are still unique if the helping rowid+### are removed.")
#print("Number of colnames =")
#print(length(colnames(tidyTibble)))

cleanedcolnames <- sub("^[0-9]+###", "", colnames(tidyTibble))
#print("Structure of cleaned colnames:")
#str(cleanedcolnames)
#print("Number of unique cleaned colnames =")
#print(length(unique(cleanedcolnames)))
#print("OK to clean colnames; cleaning...")
colnames(tidyTibble) <- cleanedcolnames
rm(cleanedcolnames)
#print("Structure of cleaned tibble's colnames:")
#print(str(colnames(tidyTibble)))

##' 3. Uses descriptive activity names to name the activities in the data set
tidyTibble <- merge(tidyTibble, activityLabels, by.x = "activityid", by.y = "id",
                    sort = FALSE)
tidyTibble <- tidyTibble %>%
    select(activity, everything()) %>%
    select(-activityid)
#first remove underscores from activity values
#These will also be made lowercase later when factorising them, more efficiently to do it there once per factor
tidyTibble$activity <- sub("_","",tidyTibble$activity) 
rm(activityLabels)

##' 2. Split col on std/mean, xyz
tidyTibble <- tidyTibble %>% gather(tmpfeature, value, -(activity:subjectid))
#str(tidyTibble)

tidyTibble <- tidyTibble %>% separate(col = tmpfeature, into = c("signaldomain", "rest"), 
                                      sep = c(1), convert = TRUE, remove = FALSE) %>% #separate out the leading f/t
    separate(col = rest, into = c("feature", "rest"), sep = "-", extra = "merge") %>% #separate out the part before the first -
    separate(col = rest, into = c("measure", "dimension"), sep = "\\(", remove = FALSE, fill = "right") %>% #separate the mean/std and dimension, leaving blanks as ')' not to lose them
    select(-rest) #drop the rest column
tidyTibble

tidyTibble <- tbl_df(tidyTibble)

##' Set the non-axial values to NA
nabool <- tidyTibble$dimension == ")"
tidyTibble[nabool,"dimension"] <- NA
rm(nabool)

##' Clean up the axial names (from '-)X' to 'X')
tidyTibble$dimension <- sub("\\)-","",tidyTibble$dimension)
str(tidyTibble$dimension)

##' Separate the remaining parts of the feature values
table(tidyTibble$feature,tidyTibble$signaldomain)

##' First, there are three feature values that seems to have the leading 'Body' doubled, clean those ('BodyBodyAccJerkMag' etc)
tidyTibble$feature <- sub("BodyBody", "Body", tidyTibble$feature)
table(tidyTibble$feature,tidyTibble$signaldomain)

##' Now, separate out the leading Body|Gravity
##' Introduce a - to simplify separation
tidyTibble$feature <- sub("Body", "Body-", tidyTibble$feature)
tidyTibble$feature <- sub("Gravity", "Gravity-", tidyTibble$feature)
#table(tidyTibble$feature,tidyTibble$signaldomain)
tidyTibble <- separate(tidyTibble, col = feature, into = c("accelerationtype","featurerest"), sep = "-", remove = TRUE)
table(tidyTibble$accelerationtype,tidyTibble$featurerest)

##' Then, split up the remainders - on Acc/Gyro, Jerk/NA, Mag/NA
##'  - (Acc AccJerk AccJerkMag AccMag   Gyro GyroJerk GyroJerkMag GyroMag)
tidyTibble$featurerest <- sub("Acc", "Acc-", tidyTibble$featurerest)
tidyTibble$featurerest <- sub("Gyro", "Gyro-", tidyTibble$featurerest)
tidyTibble <- separate(tidyTibble, col = featurerest, into = c("method", "featurerest2"), 
                       sep = "-", remove = TRUE, fill = "right")
table(tidyTibble$method, tidyTibble$featurerest2, useNA = "always")

#Check for missing values
table(!grepl("(.+)", tidyTibble$featurerest2), tidyTibble$featurerest2, useNA = "always")
#Set these to NA
tidyTibble[!grepl("(.+)", tidyTibble$featurerest2), "featurerest2"] <- NA
table(tidyTibble$method, tidyTibble$featurerest2, useNA = "always")

##' Remaining rest (featurerest2): Jerk JerkMag    Mag   <NA>
tidyTibble$featurerest2 <- sub("Mag","aaa-Mag", tidyTibble$featurerest2)
tidyTibble <- separate(tidyTibble, col = featurerest2, into = c("jerk","magnitude"), sep = "-",
                       remove = FALSE, fill = "right")
table(tidyTibble$tmpfeature, tidyTibble$featurerest2, useNA = "always")
table(tidyTibble$magnitude, tidyTibble$jerk, useNA = "always")

##' Remaining rest (jerk): aaa   Jerk Jerkaaa   <NA>
tidyTibble[grepl("^aaa", tidyTibble$jerk), "jerk"] <- NA
tidyTibble$jerk <- sub("Jerkaaa", "jerk", tidyTibble$jerk)
tidyTibble$jerk <- sub("Jerk", "jerk", tidyTibble$jerk)
tidyTibble$magnitude <- sub("Mag", "mag", tidyTibble$magnitude)
table(tidyTibble$magnitude, tidyTibble$jerk, useNA = "always")

##' Turn the jerk and magnitude variables into booleans
tidyTibble <- tidyTibble %>% mutate(isjerk = ifelse(is.na(jerk), FALSE, TRUE)) %>%
    mutate(ismagnitude = ifelse(is.na(magnitude), FALSE, TRUE)) %>%
    select(-jerk, -magnitude) %>% 
    select(-tmpfeature, -featurerest2) # Remove waste cols

##' Cast all relevant variables as factors (should this have been done earlier?), 
##' converting values to lowercase in the process

tidyTibble$activity <- factor(tidyTibble$activity, ordered = FALSE, levels = unique(tidyTibble$activity), 
                              labels = tolower(unique(tidyTibble$activity)))
tidyTibble$activity <- addNA(tidyTibble$activity, ifany = TRUE)


tidyTibble$signaldomain <- factor(tidyTibble$signaldomain, ordered = FALSE)
tidyTibble$signaldomain <- addNA(tidyTibble$signaldomain, ifany = TRUE)

tidyTibble$accelerationtype <- factor(tidyTibble$accelerationtype, ordered = FALSE, 
                                      levels = unique(tidyTibble$accelerationtype), 
                                      labels = tolower(unique(tidyTibble$accelerationtype)))
tidyTibble$accelerationtype <- addNA(tidyTibble$accelerationtype, ifany = TRUE)

tidyTibble$method <- factor(tidyTibble$method, ordered = FALSE, 
                            levels = unique(tidyTibble$method), 
                            labels = tolower(unique(tidyTibble$method)))
tidyTibble$method <- addNA(tidyTibble$method, ifany = TRUE)

tidyTibble$measure <- factor(tidyTibble$measure, ordered = FALSE)
tidyTibble$measure <- addNA(tidyTibble$measure, ifany = TRUE)

tidyTibble$dimension <- factor(tidyTibble$dimension, ordered = FALSE, 
                               levels = c("X", "Y", "Z"), 
                               labels = tolower(c("X", "Y", "Z")))
tidyTibble$dimension <- addNA(tidyTibble$dimension, ifany = TRUE)

##' Convert subjectid to character so not to confuse it to be a value
tidyTibble$subjectid <- as.character(tidyTibble$subjectid)

##' Put value last
tidyTibble <- tidyTibble[,c(1:7, 9:10,8)]
tidyTibble

##' Look at the tidyTibble
#summary(tidyTibble)

##' 5. From the data set in step 4, creates a second, independent tidy data set with 
##' the average of each variable for each activity and each subject.

averages <- tidyTibble
averages <- group_by(averages, activity, subjectid, signaldomain, accelerationtype, 
                     method, measure, dimension, isjerk, ismagnitude) %>%
    summarize(avg = mean(value))
#str(averages)
#For example, the averaged mean and standard frequency gyro jerk values for subject 4 doing activity 'sitting', are:
filter(averages, subjectid == "4", activity == "sitting", signaldomain =="f", isjerk == TRUE, method == "gyro") %>%
    select(avg, accelerationtype, measure, dimension, ismagnitude, everything())

##' Print some info on the datasets
print(str(tidyTibble))
print(unique(tidyTibble$activity))

##' Return the datasets by dumping to file
writeCsvData(df = tidyTibble, destdir = writeDestDir, destfile = writeDestTidyDsFilename, overwritefile = TRUE)
writeCsvData(df = averages, destdir = writeDestDir, destfile = writeDestAverageDsFilename, overwritefile = TRUE)