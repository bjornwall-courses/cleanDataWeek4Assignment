
---
title: "README"
author: "bjornwall"
date: "28 February 2017"
output: html_document
---

##Getting and Cleaning Data - Week 4 assignment
This repo contains the week 4 assignment of the Getting and Cleaning Data course.

It builds on the Human Activity Recognition Using Smartphones Dataset, version 1.0, created by Jorge L. Reyes-Ortiz, Davide Anguita, Alessandro Ghio, Luca Oneto.

A full description is available at the site where the data was obtained:
http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones

Here are the data for the project:
https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip

> You should create one R script called run_analysis.R that does the following.
> 
> 1. Merges the training and the test sets to create one data set.
> 2. Extracts only the measurements on the mean and standard deviation for each measurement.
> 3. Uses descriptive activity names to name the activities in the data set
> 4. Appropriately labels the data set with descriptive variable names.
> 5. From the data set in step 4, creates a second, independent tidy data set with 
> the average of each variable for each activity and each subject.

#The modified dataset includes the following files:
1. Two tidy datasets (in the output data folder):
    a. HARtidydataset.csv - the tidy dataset
    b. HARaveragesdataset.csv - the dataset holding averaged values from the tidy dataset
2. The R script run_analysis.R that creates the tidy datasets from the raw data
3. The codebook Codebook.Rmd that describes the variables
4. This README.md file

#How the tidy datasets were obtained from the raw data

Downloading and unzipping the data from the url, the following files are used:
a. activity_labels.txt: the key value pairs of the six activities
b. featurs.txt: the 561 feature labels

For each test set (train, test):
c. subject_train/test.txt: the subject ids per observation
d. y_train/test.txt: the activity ids per observation
e. X_train/test.txt: the feature values per observation

The remaining files are not used, as they are raw data used to create the above files.