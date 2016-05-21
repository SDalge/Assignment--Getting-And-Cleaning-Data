## Steps from Script to analyse datasets and generate tidy data and average tidy data

### Step 1 - Read Test datasets

      
      "MEASUREMENTS"

         Xtest <- read.table("C3/UCI HAR Dataset/test/X_test.txt") 
  
      "ACTIVITIES"

          ytest <- read.table("C3/UCI HAR Dataset/test/Y_test.txt") 

      "SUBJECTS"  
      
          Subjtest <- read.table("C3/UCI HAR Dataset/test/subject_test.txt")
  


### Step 2 - Read Train datasets

      "MEASUREMENTS"
     
         Xtrain <- read.table("C3/UCI HAR Dataset/train/X_train.txt")
  
      "ACTIVITIES"
      
        ytrain <- read.table("C3/UCI HAR Dataset/train/Y_train.txt") 
  
      "SUBJECTS"  
      
        Subjtrain <- read.table("C3/UCI HAR Dataset/train/subject_train.txt")
  
  

### Step 3 - Read variables names and activity labels

       variables <- read.table("C3/UCI HAR Dataset/features.txt")
  
        activity_label <- read.table("C3/UCI HAR Dataset/activity_labels.txt")


### Step 4 - Rename Columns with variables names and change it to valid names

        names(Xtest) <- variables[,2]
  
        valid_column_names <- make.names(names=names(Xtest), unique=TRUE, allow_ = TRUE)
  
        names(Xtest) <- valid_column_names
  
        names(Xtrain) <- variables[,2]
  
        valid_column_names <- make.names(names=names(Xtrain), unique=TRUE, allow_ = TRUE)
  
        names(Xtrain) <- valid_column_names
  
        colnames(Subjtest) <- "Subject"
  
        colnames(Subjtrain) <- "Subject"
  

### Step 5 - Apply label names to Y table

        ytest <- merge(ytest, activity_label, by.y = "V1")
  
        ytrain <- merge(ytrain, activity_label, by.y = "V1")
  
  
### Step 6 - Select mean and standard deviation values from tables and store in *_mean / *_std data tables.

      xtest_mean <- select(Xtest, contains(".mean."))
  
      xtest_std <- select(Xtest, contains(".std."))
  
      xtrain_mean <- select(Xtrain, contains(".mean."))
      
      xtrain_std <- select(Xtrain, contains(".std."))


### Step 7 - Create complete Test and Train tables

        Activity <- ytest$V2      
      
      "TEST DATA TABLE WITH COMPLETE DATA FROM TEST DATA SET WITH SUBJECTS AND ACTIVITIES"
      
        Test <- cbind(Subjtest,Activity,xtest_mean,xtest_std)   
      
  
      Activity <- ytrain$V2
      
      "TRAIN DATA TABLE WITH COMPLETE DATA FROM TRAIN DATA SET WITH SUBJECTS AND ACTIVITIES"
      
        Train <- cbind(Subjtrain,Activity,xtrain_mean,xtrain_std)
  
  
### Step 8 - Merge two data table

    "TIDY_DATA WITH MERGED TRAIN AND TEST DATASETS"
    
        Tidy_Data <- merge(Test,Train, all = T)
  
  
### Step 9 - Create the Average data set 

      Average <- Tidy_Data %>% aggregate(list(activity = Tidy_Data $Activity, subject = Tidy_Data $Subject), mean) %>% arrange(activity, subject) %>% select(-Subject, -Activity)
