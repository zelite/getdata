# Getting and Cleaning Data - Project - by Zelite
# Raw data has been unzipped on the working directory

#libraries needed
library(dplyr)
library(tidyr)
library(stringr)


#Helper functions------------------------------------------------------
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

#function to clean a bit more the names. Will be used later on the final
#stages of the subsetting the function removes repeated dots, removes dots from
#end of the variable, replaces f and t from the start of a variable name with
#Frequency and Time, and replaces Acc and Gyro by Accelerometer and Gyroscope
cleanNamesMore <- function(name){
  patterns <- c( "\\.+" , "\\.$" , "^f" , "^t","Acc","Gyro" ) #first two matches repeated dots and dots in end of name
  replacements <- c(".", "",  "Frequency", "Time", "Accelerometer", "Gyroscope")
  
  new.name <- name
  for(i in seq_along(patterns)){
    new.name <- str_replace_all(string = new.name, pattern = patterns[[i]], replacements[[i]])
  }
  new.name  
}

#Reading of data---------------------------------

folders <- paste0("./UCI HAR Dataset/", c("test", "train"))

features <- read.table("./UCI HAR Dataset/features.txt", row.names = 1, 
                       stringsAsFactors = FALSE)

activities <- read.table(file = "UCI HAR Dataset/activity_labels.txt")



#reading both test and train datasets; the result is a list
all.data <- lapply(folders, readAndMerge)

#preparing the names for the columns of the data.frames
#make.names ensures the names are proper R variables and are unique (important for processing with dplyr)
namesForCols <- c("subject", 
                  make.names(names = features[[1]], unique = TRUE),
                  "activity", "set") 




#change the names in the data.frames in the list before binding them
all.data <- lapply(all.data, function(x) {names(x) <- namesForCols; x})

#binding the dfs togethers
all.data <- rbind_all( all.data)

#Cleaning and Processing------------------------------
#Now we can start manipulating our joint df

#lets select the right cols
#1. select cols of mean, std and meanFreq. There are other features with "mean" in the name
#such as angle(X,gravityMean), but I will not consider them. In my interpretation, this is a function
#applied to a a mean, which does not seem to be what is intended.
#2. convert the activities to factors with proper labels
#3. Convert to a long format using 'gather' from package tidyr
#4. applies the cleanNamesMore function to the variable names
#5. summarises the variables by taking the average for each subject, activity and original variable.
filt.data <- select(all.data, subject, activity, set, matches("mean|std|meanFreq", ignore.case = FALSE)) %>% 
  mutate(activity=factor(x = activity, levels = activities$V1, labels = activities$V2)) %>%
  gather(key = "variable", value = "value", -subject, -activity, -set) %>%
  mutate(variable = cleanNamesMore(variable)) %>%
  group_by(subject, activity, variable) %>% summarise(average=mean(value))

#Writes the resulting data set into the file "tidyData.txt" that will appear in the working directory
write.table(filt.data, file = "tidyData.txt", row.names = FALSE)
