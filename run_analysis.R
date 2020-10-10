# Reading data

    # reading the selected few of the train and test data sets
        randomly_partitioned_test_data_set <- read.table("./UCI HAR Dataset/test/X_test.txt")
        randomly_partitioned_train_data_set <- read.table("./UCI HAR Dataset/train/X_train.txt")
        
    # reading training and test labels
        randomly_partitioned_test_data_set_labels <- read.table("./UCI HAR Dataset/test/y_test.txt")
        randomly_partitioned_train_data_set_labels <- read.table("./UCI HAR Dataset/train/y_train.txt")
        
    # reading subjects' information for the test and train data sets into R
        randomly_partitioned_test_data_set_subjects <- read.table("./UCI HAR Dataset/test/subject_test.txt")
        randomly_partitioned_train_data_set_subjects <- read.table("./UCI HAR Dataset/train/subject_train.txt")
        
    # reading activity labels into R
        activity_labels <- read.table( "./UCI HAR Dataset/activity_labels.txt")
    
    # reading the list of features into R
        all_features <- read.table("./UCI HAR Dataset/features.txt")

        
# creating a large data frame for train and test data sets each
    Test<-data.frame(randomly_partitioned_test_data_set_subjects, randomly_partitioned_test_data_set_labels, randomly_partitioned_test_data_set)
    
    #setting the column heads for the above data frame
        names_test_train <- c("Subject Number", "Activity Number", all_features[,2])
        names(Test) <- names_test_train
    Train<-data.frame(randomly_partitioned_train_data_set_subjects, randomly_partitioned_train_data_set_labels, randomly_partitioned_train_data_set)
    
    #setting the column heads for the above data frame
        names(Train) <- names_test_train
        
# merging tables
# 1. Merges the training and the test sets to create one data set.
    tidy_data<-bind_rows(Train, Test)
    
# extracting required data
#2.  Extracts only the measurements on the mean and standard deviation for each measurement. 
    library(dplyr)
    # extracting a subset of the tidy data which contains "mean()" in the column name
    mean_data_column_names <- grep("mean()", names(tidy_data))
    Extracted_data_subset_1 <- select(tidy_data, mean_data_column_names)
    # extracting a subset of the tidy data which contains "std()" in the column name
    std_data_column_names <- grep("std()", names(tidy_data))
    Extracted_data_subset_2 <- select(tidy_data, std_data_column_names)
    #Extracting the subject and activity columns in the tidy_data set
    Extracted_data_subset_3 <- select(tidy_data, Subject Number, Activity Number)
    #combining the extracted datasets to form a larger dataset of the required values
    Extracted_data <- cbind(Extracted_data_subset_3,Extracted_data_subset_1,Extracted_data_subset_2)
    
# naming columns
# 3. Uses descriptive activity names to name the activities in the data set
    
    l <- 1
    for (i in Extracted_data[[2]]) {
          for (j in activity_labels[[1]]){
            if ( i ==j ) {
                Extracted_data[[2]][l] <- activity_labels[[2]][j]
                l <- l +1
            }
          }
    }
    
    
# 4. Appropriately labels the data set with descriptive variable names. 
    names_to_be_changed <- names(Extracted_data)
    
    function_to_change_name <- function(data_frame, item_to_change, item_to_replace_with){
            k <- 1
            changed_names <- c()
            for (i in data_frame) {
                  changed_names[k] <- gsub(item_to_change,item_to_replace_with,data_frame[k])
                  k <- k+1
                  }
            return(changed_names)
     }
    changed_names <- function_to_change_name(names_to_be_changed, "^t", "TimeDomain")
    changed_names <- function_to_change_name(changed_names, "^f", "FeatureDomain")  
    changed_names <- function_to_change_name(changed_names, "Acc", "Acceleration")
    changed_names <- function_to_change_name(changed_names, "mean", "Mean")
    changed_names <- function_to_change_name(changed_names, "\\-", "")
    changed_names <- function_to_change_name(changed_names, "\\(\\)", "")
    changed_names <- function_to_change_name(changed_names, "X$", "AlongXaxis")
    changed_names <- function_to_change_name(changed_names, "Y$", "AlongYaxis")
    changed_names <- function_to_change_name(changed_names, "Z$", "AlongZaxis")
    changed_names <- function_to_change_name(changed_names, "Gyro", "GyroscopicMotion")
    changed_names <- function_to_change_name(changed_names, "BodyBody", "Body")
    changed_names <- function_to_change_name(changed_names, "Mag", "Magnitude")
    changed_names <- function_to_change_name(changed_names, "Freq", "Frequency")
    changed_names <- function_to_change_name(changed_names, "std", "StandardDeviation")
    changed_names[2] <- "Activity Name"
  
    names(Extracted_data) <- changed_names
    
# creating an independent dataset
# 5. a second, independent tidy data set with the average of each variable for each activity and each subject.
    
    split_data_based_on_subject_number <- split(Extracted_data, Extracted_data$`Subject Number`)
    w <- 1
    second_tidy_data_set <- Extracted_data[0,]
    for (k in 1:30){
        #print(n)
        for (i in split_data_based_on_subject_number[k]){
            split_data_based_on_activity <- split(i,(i[2]))
            for (s in split_data_based_on_activity){
                x <- (s[,3:81])
                z <- colMeans(x)
                s[1,3:81] <- z
                second_tidy_data_set[w,] <- s[1,]
                w <- w+1
            }
        }  
    }
    
    for ( g in 3:81){
        
        changed_names[g] <- paste("AverageOf",changed_names[g],sep = "")
    }
    names(second_tidy_data_set) <- changed_names
    