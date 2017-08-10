# Getting and CLeaning Data Course Project

This was done as a requirement for the Cousera "Getting and Cleaning Data" course. 

## Assignment Input Data

 The data used in this project is data collected from the accelerometers from the Samsung Galaxy S smartphone. A full description is available at the site where the data was obtained:

http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones

## Assignment Requirements

1. Merges the training and the test sets to create one data set.
2. Extracts only the measurements on the mean and standard deviation for each measurement.
3. Uses descriptive activity names to name the activities in the data set
4. Appropriately labels the data set with descriptive variable names.
5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

## Data Processing Steps

For more information,  read the [CodeBook.md](CodeBook.md).


## Code Usage

"run_analysis.R" will read, merge, clean and write input data to "tidy_data.csv" file.  More in [CodeBook.md](CodeBook.md)

In bash terminal,

```bash
RScript run_analysis.R
```

In side R,

```R
source("run_analysis.R")
```

## Output 

Output file name: tidy_data.csv

