# Getting and Cleaning Data - Project - by Zelite
# Raw data has been unzipped and is in folder rawdata

#libraries needed
library(dplyr)
#library(stringr)

folders <- paste0("./rawdata/", c("test", "train"))

features <- read.table("./rawdata/features.txt", row.names = 1, 
                       stringsAsFactors = FALSE)

activities <- read.table(file = "rawdata/activity_labels.txt")

readAndMerge <- function(folder){
  #this function reads the .txt files in a folder and merges them
  #in this way we dont need to repeat the code for both the test 
  #and train dataset
  files <- list.files(folder, pattern = ".txt$", full.names = TRUE)
  files <- sapply(files, read.table, stringsAsFactors = FALSE, 
                  simplify = FALSE, USE.NAMES = TRUE)
  result <- data.frame(files)
  
  #add col with "set" of data: test or train?
  result$set <- basename(folder)
  result
}

#reading both test and train datasets; the result is a list
all.data <- lapply(folders, readAndMerge)

#preparing the names for the columns of the data.frames
namesForCols <- c("subject", 
                  make.names(names = features[[1]], unique = TRUE),
                  "activity", "set") 

#change the names in the data.frames in the list before binding them
all.data <- lapply(all.data, function(x) {names(x) <- namesForCols; x})

#binding the dfs togethers
all.data <- rbind_all( all.data)

#Now we can start manipulating our joint df

#lets select the right cols
filt.data <- select(all.data, subject, activity, set, matches("mean|std")) %>% 
  mutate(activity=factor(x = activity, levels = activities$V1, labels = activities$V2))

