# ---------------------------------------------------------------------------- #
#
# This work is done as a requirment of Cousera "Getting and Cleaning" course.
#
# Author: Saranga Wijeratne
#
# ---------------------------------------------------------------------------- #


library(dplyr)
library(tidyr)


# Download necessary data file if its not in local system
if (!file.exists("./data/Dataset.zip")){
  download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip", "./data/Dataset.zip", method="curl")
  unzip("./data/Dataset.zip", "./data/")
}

# ----------------------------------------------------------------------------
# Step 1 - Merges the training and the test sets to create one data set.
# ----------------------------------------------------------------------------

# 1. Read in features.txt and activity_labels.txt before reading test and train data, because some column names can be assigned to test and train from these two files.

activity<-read.table("./data/UCI HAR Dataset/activity_labels.txt", header=F, col.names = c("activityno", "activity"))
features<-read.table("./data/UCI HAR Dataset/features.txt", header=F, col.names = c("featureno", "feature"))

# 2. Import test dataset

read_test_data<-read.table("./data/UCI HAR Dataset/test/X_test.txt", header = F, sep="", col.names = features$feature)
read_test_lables<-read.table("./data/UCI HAR Dataset/test/y_test.txt", header = F, sep="", col.names = "activityno")
read_test_subject<-read.table("./data/UCI HAR Dataset/test/subject_test.txt", header = F, sep="", col.names = "subject")

test_data<-cbind(read_test_subject, read_test_lables, read_test_data)

# 3. Import train dataset

read_train_data<-read.table("./data/UCI HAR Dataset/train/X_train.txt", header = F, sep="", col.names = features$feature)
read_train_lables<-read.table("./data/UCI HAR Dataset/train/y_train.txt", header = F, sep="", col.names = "activityno")
read_train_subject<-read.table("./data/UCI HAR Dataset/train/subject_train.txt", header = F, sep="", col.names = "subject")

train_data<-cbind(read_train_subject, read_train_lables, read_train_data)

# 4. Merge train and test datasets

all_data<-rbind(test_data, train_data)

# ---------------------------------------------------------------------------------------------------
# Step 2 - Extracts only the measurements on the mean and standard deviation for each measurement.
# ---------------------------------------------------------------------------------------------------

filtered_all_data<-select(all_data, contains("mean"), contains("std") , -contains("meanFreq"))

# ---------------------------------------------------------------------------------------------------
# Step 3 - Uses descriptive activity names to name the activities in the data set
# ---------------------------------------------------------------------------------------------------

add_desc_activityname_data_all<-merge(activity, all_data, by="activityno")

# ---------------------------------------------------------------------------------------------------
# Step 4 - Appropriately labels the data set with descriptive variable names.
# ---------------------------------------------------------------------------------------------------

all_headers<-names(add_desc_activityname_data_all)

# Remove parentheses
all_headers<-gsub("[[:space:]]", "", all_headers)
all_headers<-gsub("\\.", "", all_headers)
# Replace "Acc", Coeff and "Mag" with "acceleration", coefficient and "magnitude"
all_headers<-gsub("Acc", "-acceleration", all_headers)
all_headers<-gsub("Mag", "-magnitude", all_headers)
all_headers<-gsub("Coeff", "-coefficient", all_headers)
#Replace "f" and "t" at the begining with "time" and "frequency"
all_headers<-gsub("^t", "time", all_headers)
all_headers<-gsub("^f", "frequency", all_headers)
#Convert everything to lower case
all_headers<-tolower(all_headers)

names(add_desc_activityname_data_all)<-all_headers

# ---------------------------------------------------------------------------------------------------
# Step 5 - From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
# ---------------------------------------------------------------------------------------------------
# Group data by each activity each subject

tidy_data<-add_desc_activityname_data_all %>% group_by(activity, subject) %>% summarise_each(funs(mean),  -activity,-subject, -activityno) %>% gather(measurment, mean, -activity, -subject)

#Write New tidy dataset to a file
write.csv(tidy_data, "tidy_data.csv", row.names = F)
