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

##' Create some variables
mydestfile <- "dataset.zip"
myfileurl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
mydestdir <- "data"
mydestsubdir <- "UCI HAR Dataset"

myfeaturesfile <- "features.txt"
myactivitylabelsfile <- "activity_labels.txt"

downloadData <- function(url, destfile, destdir, destsubdir) {
    ##' Create the data directory and download the data
    if(!file.exists(destdir)) {
        dir.create(destdir)
    }
    download.file(myfileurl,destfile = destfile, mode="wb")
    dateDownloaded <- date()
    
    ##' Move to the data directory and unzip the data
    setwd(destdir)
    unzip(file.path("..",destfile), overwrite = FALSE) #Change to overwrite = TRUE to replace files
    
    ##' Lastly, move back up
    setwd(file.path(".."))
}

getTibble <- function(type = "train") {
    #temp - replace with proper code
    library(datasets)
    data(mtcars)
    df <- tbl_df(mtcars)
    df
}

loadFeatureLabels <- function(destdir,destsubdir) {
    ##' Load the features and select mean and standard deviation for each measurement
    ##' mean(): Mean value
    ##' std(): Standard deviation
    ##' Returns a tibble with four columns:
    ##' 1. The row id
    ##' 2. The feature name
    ##' 3. isMean - TRUE if this feature is a mean value
    ##' 4. isStDev - TRUE if this feature is a standard deviation value
    
    setwd(file.path(destdir,destsubdir))
    
        rawfeaturesDf <- read.table(file = myfeaturesfile, header = FALSE, colClasses = "character", stringsAsFactors = FALSE)
        featuresTibble <- tbl_df(rawfeaturesDf)
        rm(rawfeaturesDf)
        
        featureMeanBool <- grepl("[Mm]ean", featuresTibble$V2)
        featureStDevBool <- grepl("[Ss]td", featuresTibble$V2)
        
        featuresTibble <- featuresTibble %>%
            mutate(isMean = featureMeanBool, isStDev = featureStDevBool) %>%
            rename(id = V1, featurename = V2)
        
        print(featuresTibble)
     
    setwd(file.path("..",".."))
    featuresTibble
}

loadActivityLabels <- function(destdir,destsubdir) {
    ##' Load the activity labels
    
    setwd(file.path(destdir,destsubdir))
    
        rawactivitylabelsDf <- read.table(file = myactivitylabelsfile, header = FALSE, colClasses = "character", stringsAsFactors = FALSE)
        activityLabelsTibble <- tbl_df(rawactivitylabelsDf)
        rm(rawactivitylabelsDf)
    
    setwd(file.path("..",".."))
    activityLabelsTibble
}

downloadData(myfileurl, mydestfile, mydestdir, mydestsubdir)

##' Load the selected features
features <- loadFeatureLabels(destdir = mydestdir, destsubdir = mydestsubdir)

##' Load the activity labels
activityLabels <- loadActivityLabels(destdir = mydestdir, destsubdir = mydestsubdir)

##' Get the train and test data sets as tibbles
trainTibble <- getTibble("train")
testTibble <- getTibble("test")

##' Merge the train and test tibbles
##' etc

