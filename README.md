"Getting and Cleaning Data"
===

This is my project for the "Getting and Cleaning Data" Coursera course.


# Project Tasks

The project task is to start with this dataset: https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip

And clean it to make it a [tidy dataset](http://www.jstatsoft.org/v59/i10/).

Specifically, these are the instructions:

> You should create one R script called run_analysis.R that does the following. 
> 
> 1. Merges the training and the test sets to create one data set.>
> 2. Extracts only the measurements on the mean and standard deviation for each measurement. 
> 3. Uses descriptive activity names to name the activities in the data set
> 4. Appropriately labels the data set with descriptive variable names. 
> 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

#run_analysis script

On this repository you will find a script called `run_analysis.R` which reads the data from the folder `UCL HAR Dataset`, cleans it up and writes a tidy dataset to the file `tidyData.txt`. The script assumes the folder is present in the working directory and that the directory structure matches the one provided in original zip file.

Check the [run_analysis.R](run_analysis.R) file to see how the data is processed. The comments should help know what is done at each step.

Check the [CodeBook.md](CodeBook.md) file for explanation on the variable names, meaning and how they are summarized.