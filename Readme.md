Readme
================

Getting and Cleaning Data Project
=================================

An analysis of accelerometer data generated from an experiment with Samsung Galaxy S smartphones.

Contents
--------

*CodeBook.md* - Describes the experiment data and summarized data.  
*run\_analysis.R* - R script to merge, clean-up, transform, and summarize the experiment data.  
*tidy\_Data.txt* - Output file generated by the run\_analysis.R script.  
*README.md* - This file.  

Overview
--------

30 volunteers performed 6 different activities while wearing a smartphone. The smartphone captured various data about their movements.

During the data processing we used following files:

-   features.txt: Names of the 561 features. activity\_labels.txt: Names and IDs for each of the 6 activities.

-   X\_train.txt: 7352 observations of the 561 features, for 21 of the 30 volunteers.

-   subject\_train.txt: A vector of 7352 integers, denoting the ID of the volunteer related to each of the observations in X\_train.txt.
-   y\_train.txt: A vector of 7352 integers, denoting the ID of the activity related to each of the observations in X\_train.txt.

-   X\_test.txt: 2947 observations of the 561 features, for 9 of the 30 volunteers.

-   subject\_test.txt: A vector of 2947 integers, denoting the ID of the volunteer related to each of the observations in X\_test.txt.
-   y\_test.txt: A vector of 2947 integers, denoting the ID of the activity related to each of the observations in X\_test.txt.

This analysis was performed using only the files above, and did not use the raw signal data. Therefore, the data files in the "Inertial Signals" folders were ignored.

Processing steps
----------------

-   All of the relevant data files were read into data frames, appropriate column headers were added, and the training and test sets were combined into a single data set using **rbind()** and **cbind()** functions.

-   All feature columns were removed that did not contain the exact string "mean()" or "std()" using **grepl()** function. Subsequently,we obtained 66 feature columns and 2 leading columns: subjectID and Activity.

-   The activity column was converted from a integer to a factor, using labels describing the activities (in the file *activity\_labels.txt*).

-   Then we renamed column names of the data set with descriptive variable names using functions **sub()** and **gsub()**. (We replaced *f* to *Frequency*, *t* to *Time*, *Acc* to *Acceleration*, *std()* to *StandardDeviation* etc.)

-   A tidy data set was created containing the mean of each feature for each subject and each activity. For this transformation we used **dplyr** package and its cascade operations and finctions **summarise\_each()**. Thus, each subject has 6 rows in the tidy data set (one row for each activity), and each row contains the mean value for each of the 66 features for that subject/activity combination. Since there are 30 subjects, there are a total of 180 rows.

-   The tidy data set was output to a *TXT* file *tidy\_Data.txt* using **write.table()** function.
