run_analysis <- function()
  {
  library(dplyr)


##Read Test
  Xtest <- read.table("C3/UCI HAR Dataset/test/X_test.txt")
  ytest <- read.table("C3/UCI HAR Dataset/test/Y_test.txt")
  Subjtest <- read.table("C3/UCI HAR Dataset/test/subject_test.txt")

## Read Train
  Xtrain <- read.table("C3/UCI HAR Dataset/train/X_train.txt")
  ytrain <- read.table("C3/UCI HAR Dataset/train/Y_train.txt")
  Subjtrain <- read.table("C3/UCI HAR Dataset/train/subject_train.txt")

##Read variables and activity labels
  variables <- read.table("C3/UCI HAR Dataset/features.txt")
  activity_label <- read.table("C3/UCI HAR Dataset/activity_labels.txt")

## Rename Columns with variables names and change it to valid names
  names(Xtest) <- variables[,2]
  valid_column_names <- make.names(names=names(Xtest), unique=TRUE, allow_ = TRUE)
  names(Xtest) <- valid_column_names
  
  names(Xtrain) <- variables[,2]
  valid_column_names <- make.names(names=names(Xtrain), unique=TRUE, allow_ = TRUE)
  names(Xtrain) <- valid_column_names
  
  colnames(Subjtest) <- "Subject"
  colnames(Subjtrain) <- "Subject"
  

## apply label names to Y table
  ytest <- merge(ytest, activity_label, by.y = "V1")
  
  ytrain <- merge(ytrain, activity_label, by.y = "V1")
  
  
## Select mean and standard deviation values from tables
  xtest_mean <- select(Xtest, contains(".mean."))
  xtest_std <- select(Xtest, contains(".std."))
  
  xtrain_mean <- select(Xtrain, contains(".mean."))
  xtrain_std <- select(Xtrain, contains(".std."))


## Create complete Test and Train tables
  Activity <- ytest$V2
  Test <- cbind(Subjtest,Activity,xtest_mean,xtest_std)
  
  Activity <- ytrain$V2
  Train <- cbind(Subjtrain,Activity,xtrain_mean,xtrain_std)
  
  
## Merge two data table
  Tidy_Data <- merge(Test,Train, all = T)
  
## create the average data set
  Average <- Tidy_Data %>% aggregate(list(activity = Tidy_Data $Activity, subject = Tidy_Data $Subject), mean) %>% arrange(activity, subject) %>% select(-Subject, -Activity)

  
}