Code Book
================

Getting and Cleaning Data Project
=================================

Description
-----------

Additional information about the variables, data and transformations used in the course project for the Getting and Cleaning Data course.

Source Data
-----------

A full description of the initial data used in this project can be found at the site where the data was obtained:[The UCI Repository](http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones)

The source data for this project can be found here: [Source data](https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip)

Abstract
--------

Human Activity Recognition database built from the recordings of 30 subjects performing activities of daily living (ADL) while carrying a waist-mounted smartphone with embedded inertial sensors.

### Description of abbreviations of measurements

1.  Body = related to body movement.
2.  Gravity = acceleration of gravity
3.  Accelertion = accelerometer measurement
4.  Gyroscope = gyroscopic measurements
5.  Jerk = sudden movement acceleration
6.  Magnitude = magnitude of movement
7.  Mean and SD are calculated for each subject for each activity for each mean and SD measurements.

The units given are g’s for the accelerometer and rad/sec for the gyro and g/sec and rad/sec/sec for the corresponding jerks.

These signals were used to estimate variables of the feature vector for each pattern: ‘-XYZ’ is used to denote 3-axial signals in the X, Y and Z directions. They total 33 measurements including the 3 dimensions - the X,Y, and Z axes.

### The set of variables that were estimated from these signals are:

mean(): Mean value
std(): Standard deviation

### Subject ID and Activity type

First two columns of the Data Set represents Subject ID and Activity type. Subject ID is a number of volunteer and is in the range 1:30. Activity type is a one of the 6th activity, performed by volunteer:
+ LAYING
+ SITTING
+ STANDING
+ WALKING
+ WALKING\_DOWNSTAIRS
+ WALKING\_UPSTAIRS

Variables
---------

After the processing and transformation the final Tidy Dataset consist of the following variables:

-   subjectID : int 1 1 1 1 1 1 2 2 2 2 ...
-   Activity : Factor w/ 6 levels "LAYING","SITTING",..: 1 2 3 4 5 6 1 2 3 4 ...
-   TimeBodyAccelerationMean-X : num 0.222 0.261 0.279 0.277 0.289 ...
-   TimeBodyAccelerationMean-Y : num -0.04051 -0.00131 -0.01614 -0.01738 -0.00992 ...
-   TimeBodyAccelerationMean-Z : num -0.113 -0.105 -0.111 -0.111 -0.108 ...
-   TimeBodyAccelerationStandardDeviation-X : num -0.928 -0.977 -0.996 -0.284 0.03 ...
-   TimeBodyAccelerationStandardDeviation-Y : num -0.8368 -0.9226 -0.9732 0.1145 -0.0319 ...
-   TimeBodyAccelerationStandardDeviation-Z : num -0.826 -0.94 -0.98 -0.26 -0.23 ...
-   TimeGravityAccelerationMean-X : num -0.249 0.832 0.943 0.935 0.932 ...
-   TimeGravityAccelerationMean-Y : num 0.706 0.204 -0.273 -0.282 -0.267 ...
-   TimeGravityAccelerationMean-Z : num 0.4458 0.332 0.0135 -0.0681 -0.0621 ...
-   TimeGravityAccelerationStandardDeviation-X : num -0.897 -0.968 -0.994 -0.977 -0.951 ...
-   TimeGravityAccelerationStandardDeviation-Y : num -0.908 -0.936 -0.981 -0.971 -0.937 ...
-   TimeGravityAccelerationStandardDeviation-Z : num -0.852 -0.949 -0.976 -0.948 -0.896 ...
-   TimeBodyAccelerationJerkMean-X : num 0.0811 0.0775 0.0754 0.074 0.0542 ...
-   TimeBodyAccelerationJerkMean-Y : num 0.003838 -0.000619 0.007976 0.028272 0.02965 ...
-   TimeBodyAccelerationJerkMean-Z : num 0.01083 -0.00337 -0.00369 -0.00417 -0.01097 ...
-   TimeBodyAccelerationJerkStandardDeviation-X : num -0.9585 -0.9864 -0.9946 -0.1136 -0.0123 ...
-   TimeBodyAccelerationJerkStandardDeviation-Y : num -0.924 -0.981 -0.986 0.067 -0.102 ...
-   TimeBodyAccelerationJerkStandardDeviation-Z : num -0.955 -0.988 -0.992 -0.503 -0.346 ...
-   TimeBodyGyroscopeMean-X : num -0.0166 -0.0454 -0.024 -0.0418 -0.0351 ...
-   TimeBodyGyroscopeMean-Y : num -0.0645 -0.0919 -0.0594 -0.0695 -0.0909 ...
-   TimeBodyGyroscopeMean-Z : num 0.1487 0.0629 0.0748 0.0849 0.0901 ...
-   TimeBodyGyroscopeStandardDeviation-X : num -0.874 -0.977 -0.987 -0.474 -0.458 ...
-   TimeBodyGyroscopeStandardDeviation-Y : num -0.9511 -0.9665 -0.9877 -0.0546 -0.1263 ...
-   TimeBodyGyroscopeStandardDeviation-Z : num -0.908 -0.941 -0.981 -0.344 -0.125 ...
-   TimeBodyGyroscopeJerkMean-X : num -0.1073 -0.0937 -0.0996 -0.09 -0.074 ...
-   TimeBodyGyroscopeJerkMean-Y : num -0.0415 -0.0402 -0.0441 -0.0398 -0.044 ...
-   TimeBodyGyroscopeJerkMean-Z : num -0.0741 -0.0467 -0.049 -0.0461 -0.027 ...
-   TimeBodyGyroscopeJerkStandardDeviation-X : num -0.919 -0.992 -0.993 -0.207 -0.487 ...
-   TimeBodyGyroscopeJerkStandardDeviation-Y : num -0.968 -0.99 -0.995 -0.304 -0.239 ...
-   TimeBodyGyroscopeJerkStandardDeviation-Z : num -0.958 -0.988 -0.992 -0.404 -0.269 ...
-   TimeBodyAccelerationMagnitudeMean : num -0.8419 -0.9485 -0.9843 -0.137 0.0272 ...
-   TimeBodyAccelerationMagnitudeStandardDeviation : num -0.7951 -0.9271 -0.9819 -0.2197 0.0199 ...
-   TimeGravityAccelerationMagnitudeMean : num -0.8419 -0.9485 -0.9843 -0.137 0.0272 ...
-   TimeGravityAccelerationMagnitudeStandardDeviation : num -0.7951 -0.9271 -0.9819 -0.2197 0.0199 ...
-   TimeBodyAccelerationJerkMagnitudeMean : num -0.9544 -0.9874 -0.9924 -0.1414 -0.0894 ...
-   TimeBodyAccelerationJerkMagnitudeStandardDeviation : num -0.9282 -0.9841 -0.9931 -0.0745 -0.0258 ...
-   TimeBodyGyroscopeMagnitudeMean : num -0.8748 -0.9309 -0.9765 -0.161 -0.0757 ...
-   TimeBodyGyroscopeMagnitudeStandardDeviation : num -0.819 -0.935 -0.979 -0.187 -0.226 ...
-   TimeBodyGyroscopeJerkMagnitudeMean : num -0.963 -0.992 -0.995 -0.299 -0.295 ...
-   TimeBodyGyroscopeJerkMagnitudeStandardDeviation : num -0.936 -0.988 -0.995 -0.325 -0.307 ...
-   FrequencyBodyAccelerationMean-X : num -0.9391 -0.9796 -0.9952 -0.2028 0.0382 ...
-   FrequencyBodyAccelerationMean-Y : num -0.86707 -0.94408 -0.97707 0.08971 0.00155 ...
-   FrequencyBodyAccelerationMean-Z : num -0.883 -0.959 -0.985 -0.332 -0.226 ...
-   FrequencyBodyAccelerationStandardDeviation-X : num -0.9244 -0.9764 -0.996 -0.3191 0.0243 ...
-   FrequencyBodyAccelerationStandardDeviation-Y : num -0.834 -0.917 -0.972 0.056 -0.113 ...
-   FrequencyBodyAccelerationStandardDeviation-Z : num -0.813 -0.934 -0.978 -0.28 -0.298 ...
-   FrequencyBodyAccelerationJerkMean-X : num -0.9571 -0.9866 -0.9946 -0.1705 -0.0277 ...
-   FrequencyBodyAccelerationJerkMean-Y : num -0.9225 -0.9816 -0.9854 -0.0352 -0.1287 ...
-   FrequencyBodyAccelerationJerkMean-Z : num -0.948 -0.986 -0.991 -0.469 -0.288 ...
-   FrequencyBodyAccelerationJerkStandardDeviation-X : num -0.9642 -0.9875 -0.9951 -0.1336 -0.0863 ...
-   FrequencyBodyAccelerationJerkStandardDeviation-Y : num -0.932 -0.983 -0.987 0.107 -0.135 ...
-   FrequencyBodyAccelerationJerkStandardDeviation-Z : num -0.961 -0.988 -0.992 -0.535 -0.402 ...
-   FrequencyBodyGyroscopeMean-X : num -0.85 -0.976 -0.986 -0.339 -0.352 ...
-   FrequencyBodyGyroscopeMean-Y : num -0.9522 -0.9758 -0.989 -0.1031 -0.0557 ...
-   FrequencyBodyGyroscopeMean-Z : num -0.9093 -0.9513 -0.9808 -0.2559 -0.0319 ...
-   FrequencyBodyGyroscopeStandardDeviation-X : num -0.882 -0.978 -0.987 -0.517 -0.495 ...
-   FrequencyBodyGyroscopeStandardDeviation-Y : num -0.9512 -0.9623 -0.9871 -0.0335 -0.1814 ...
-   FrequencyBodyGyroscopeStandardDeviation-Z : num -0.917 -0.944 -0.982 -0.437 -0.238 ...
-   FrequencyBodyAccelerationMagnitudeMean : num -0.8618 -0.9478 -0.9854 -0.1286 0.0966 ...
-   FrequencyBodyAccelerationMagnitudeStandardDeviation : num -0.798 -0.928 -0.982 -0.398 -0.187 ...
-   FrequencyBodyBodyAccelerationJerkMagnitudeMean : num -0.9333 -0.9853 -0.9925 -0.0571 0.0262 ...
-   FrequencyBodyBodyAccelerationJerkMagnitudeStandardDeviation: num -0.922 -0.982 -0.993 -0.103 -0.104 ...
-   FrequencyBodyBodyGyroscopeMagnitudeMean : num -0.862 -0.958 -0.985 -0.199 -0.186 ...
-   FrequencyBodyBodyGyroscopeMagnitudeStandardDeviation : num -0.824 -0.932 -0.978 -0.321 -0.398 ...
-   FrequencyBodyBodyGyroscopeJerkMagnitudeMean : num -0.942 -0.99 -0.995 -0.319 -0.282 ...
-   FrequencyBodyBodyGyroscopeJerkMagnitudeStandardDeviation : num -0.933 -0.987 -0.995 -0.382 -0.392 ...
