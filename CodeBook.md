CodeBook: Getting and Clean Data Course Project
================================================================================

The experiments have been carried out with a group of 30 volunteers within an
age bracket of 19-48 years. Each person performed six activities (walking,
walking upstairs, walking downstairs, sitting, standing, laying) wearing a
smartphone (Samsung Galaxy S II) on the waist. Using its embedded accelerometer
and gyroscope, the experimenters captured 3-axial linear acceleration and
3-axial angular velocity at a constant rate of 50Hz. The experiments were
video-recorded to label the data manually.

The sensor signals (accelerometer and gyroscope) were pre-processed by applying
noise filters and then sampled in fixed-width sliding windows of 2.56 sec and
50% overlap (128 readings/window). The sensor acceleration signal, which has
gravitational and body motion components, was separated using a Butterworth
low-pass filter into body acceleration and gravity. The gravitational force is
assumed to have only low frequency components, therefore a filter with 0.3 Hz
cutoff frequency was used. From each window, a vector of features was obtained
by calculating variables from the time and frequency domains.

Subsequently, the body linear acceleration and angular velocity were derived in
time to obtain Jerk signals. Also the magnitude of these three-dimensional
signals were calculated using the Euclidean norm.

Key variables: Activity and Subject
--------------------------------------------------------------------------------

* Activity: The activity being performed for the observation. One of;
  1. Walking
  2. Walking Upstairs
  3. Walking Downstairs
  4. Sitting
  5. Standing
  6. Laying
* Subject: a unique subject identifier

Measurement Means
--------------------------------------------------------------------------------

All other variables are means of the measurement for each activity and subject.
This is signified by the first "Mean" in the name.

The next part of the name is either "Time" or "Freq" indicating that the
measurement was taken in either the Time domain or the Frequency domain.

The next part of the name indicates whether the measurement was the subject's
"Body" acceleration or gravitational "Gravity" acceleration. 

"Acc" is a measurment of acceleration and "Gyro" is a measurement of torque.

"Jerk" variables are measurements of Jerk signals and "Mag" refers to the
magnitude of the signals calculated using the Euclidean norm.

Finally, the '-mean' and '-std' are the mean and standard deviation of the
measurement of 2.56 second sliding windows of 128 measurements at 50Hz. '-X',
'-Y' and '-Z' indicate the axis of the measurement.

All values are floating point numbers.

  - MeanTimeBodyAcc-mean-X
  - MeanTimeBodyAcc-mean-Y
  - MeanTimeBodyAcc-mean-Z
  - MeanTimeBodyAcc-std-X
  - MeanTimeBodyAcc-std-Y
  - MeanTimeBodyAcc-std-Z
  - MeanTimeGravityAcc-mean-X
  - MeanTimeGravityAcc-mean-Y
  - MeanTimeGravityAcc-mean-Z
  - MeanTimeGravityAcc-std-X
  - MeanTimeGravityAcc-std-Y
  - MeanTimeGravityAcc-std-Z
  - MeanTimeBodyAccJerk-mean-X
  - MeanTimeBodyAccJerk-mean-Y
  - MeanTimeBodyAccJerk-mean-Z
  - MeanTimeBodyAccJerk-std-X
  - MeanTimeBodyAccJerk-std-Y
  - MeanTimeBodyAccJerk-std-Z
  - MeanTimeBodyGyro-mean-X
  - MeanTimeBodyGyro-mean-Y
  - MeanTimeBodyGyro-mean-Z
  - MeanTimeBodyGyro-std-X
  - MeanTimeBodyGyro-std-Y
  - MeanTimeBodyGyro-std-Z
  - MeanTimeBodyGyroJerk-mean-X
  - MeanTimeBodyGyroJerk-mean-Y
  - MeanTimeBodyGyroJerk-mean-Z
  - MeanTimeBodyGyroJerk-std-X
  - MeanTimeBodyGyroJerk-std-Y
  - MeanTimeBodyGyroJerk-std-Z
  - MeanTimeBodyAccMag-mean
  - MeanTimeBodyAccMag-std
  - MeanTimeGravityAccMag-mean
  - MeanTimeGravityAccMag-std
  - MeanTimeBodyAccJerkMag-mean
  - MeanTimeBodyAccJerkMag-std
  - MeanTimeBodyGyroMag-mean
  - MeanTimeBodyGyroMag-std
  - MeanTimeBodyGyroJerkMag-mean
  - MeanTimeBodyGyroJerkMag-std
  - MeanFreqBodyAcc-mean-X
  - MeanFreqBodyAcc-mean-Y
  - MeanFreqBodyAcc-mean-Z
  - MeanFreqBodyAcc-std-X
  - MeanFreqBodyAcc-std-Y
  - MeanFreqBodyAcc-std-Z
  - MeanFreqBodyAccJerk-mean-X
  - MeanFreqBodyAccJerk-mean-Y
  - MeanFreqBodyAccJerk-mean-Z
  - MeanFreqBodyAccJerk-std-X
  - MeanFreqBodyAccJerk-std-Y
  - MeanFreqBodyAccJerk-std-Z
  - MeanFreqBodyGyro-mean-X
  - MeanFreqBodyGyro-mean-Y
  - MeanFreqBodyGyro-mean-Z
  - MeanFreqBodyGyro-std-X
  - MeanFreqBodyGyro-std-Y
  - MeanFreqBodyGyro-std-Z
  - MeanFreqBodyAccMag-mean
  - MeanFreqBodyAccMag-std
  - MeanFreqBodyAccJerkMag-mean
  - MeanFreqBodyAccJerkMag-std
  - MeanFreqBodyGyroMag-mean
  - MeanFreqBodyGyroMag-std
  - MeanFreqBodyGyroJerkMag-mean
  - MeanFreqBodyGyroJerkMag-std
