# Welcome to the Code Book

## Fields
* subject - The person 's ID
* activity - The label of the activity performed when the corresponding measurements were taken
    
## Activity Labels

 *    WALKING (value 1)
 *    WALKING_UPSTAIRS (value 2)
 *    WALKING_DOWNSTAIRS (value 3)
 *    SITTING (value 4)
 *    STANDING (value 5)
 *    LAYING (value 6)
    
## Columns names (features and measurements)

*  [1] "subject"                                                    
*  [2] "activity"                                                   
*  [3] "subject.1"                                                  
*  [4] "activity.1"                                                 
* [5] "TimeDomain_BodyAccelerometer-mean-X"                        
*  [6] "TimeDomain_BodyAccelerometer-mean-Y"                        
*  [7] "TimeDomain_BodyAccelerometer-mean-Z"                        
*  [8] "TimeDomain_BodyAccelerometer_StandardDeviation_X"           
*  [9] "TimeDomain_BodyAccelerometer-correlation-X,Z"               
* [10] "TimeDomain_BodyAccelerometer-correlation-Y,Z"               
* [11] "TimeDomain_GravityAccelerometer-mean-X"                     
* [12] "TimeDomain_GravityAccelerometer-mean-Y"                     
* [13] "TimeDomain_GravityAccelerometer-mean-Z"                     
* [14] "TimeDomain_GravityAccelerometer_StandardDeviation_X"        
* [15] "TimeDomain_GravityAccelerometer-correlation-X,Z"            
* [16] "TimeDomain_GravityAccelerometer-correlation-Y,Z"            
* [17] "TimeDomain_BodyAccelerometerJerk-mean-X"                    
* [18] "TimeDomain_BodyAccelerometerJerk-mean-Y"                    
* [19] "TimeDomain_BodyAccelerometerJerk-mean-Z"                    
* [20] "TimeDomain_BodyAccelerometerJerk_StandardDeviation_X"       
* [21] "TimeDomain_BodyAccelerometerJerk-correlation-X,Z"           
* [22] "TimeDomain_BodyAccelerometerJerk-correlation-Y,Z"           
* [23] "TimeDomain_BodyGyroscope-mean-X"                            
* [24] "TimeDomain_BodyGyroscope-mean-Y"                            
* [25] "TimeDomain_BodyGyroscope-mean-Z"                            
* [26] "TimeDomain_BodyGyroscope_StandardDeviation_X"               
* [27] "TimeDomain_BodyGyroscope-correlation-X,Z"                   
* [28] "TimeDomain_BodyGyroscope-correlation-Y,Z"                   
* [29] "TimeDomain_BodyGyroscopeJerk-mean-X"                        
* [30] "TimeDomain_BodyGyroscopeJerk-mean-Y"                        
* [31] "TimeDomain_BodyGyroscopeJerk-mean-Z"                        
* [32] "TimeDomain_BodyGyroscopeJerk_StandardDeviation_X"           
* [33] "TimeDomain_BodyGyroscopeJerk-correlation-X,Z"               
* [34] "TimeDomain_BodyGyroscopeJerk-correlation-Y,Z"               
* [35] "TimeDomain_BodyAccelerometerMagnitude-arCoeff3"             
* [36] "TimeDomain_BodyAccelerometerMagnitude-arCoeff4"             
* [37] "TimeDomain_GravityAccelerometerMagnitude-arCoeff3"          
* [38] "TimeDomain_GravityAccelerometerMagnitude-arCoeff4"          
* [39] "TimeDomain_BodyAccelerometerJerkMagnitude-arCoeff3"         
* [40] "TimeDomain_BodyAccelerometerJerkMagnitude-arCoeff4"         
* [41] "TimeDomain_BodyGyroscopeMagnitude-arCoeff3"                 
* [42] "TimeDomain_BodyGyroscopeMagnitude-arCoeff4"                 
* [43] "TimeDomain_BodyGyroscopeJerkMagnitude-arCoeff3"             
* [44] "TimeDomain_BodyGyroscopeJerkMagnitude-arCoeff4"             
* [45] "FrequencyDomain_BodyAccelerometer-mean-X"                   
* [46] "FrequencyDomain_BodyAccelerometer-mean-Y"                   
* [47] "FrequencyDomain_BodyAccelerometer-mean-Z"                   
* [48] "FrequencyDomain_BodyAccelerometer_StandardDeviation_X"      
* [49] "FrequencyDomain_BodyAccelerometer-maxInds-Y"                
* [50] "FrequencyDomain_BodyAccelerometer-maxInds-Z"                
* [51] "FrequencyDomain_BodyAccelerometer-meanFreq-X"               
* [52] "FrequencyDomain_BodyAccelerometer-bandsEnergy-1,24"         
* [53] "FrequencyDomain_BodyAccelerometer-bandsEnergy-25,48"        
* [54] "FrequencyDomain_BodyAccelerometerJerk-mean-X"               
* [55] "FrequencyDomain_BodyAccelerometerJerk-mean-Y"               
* [56] "FrequencyDomain_BodyAccelerometerJerk-mean-Z"               
* [57] "FrequencyDomain_BodyAccelerometerJerk_StandardDeviation_X"  
* [58] "FrequencyDomain_BodyAccelerometerJerk-maxInds-Y"            
* [59] "FrequencyDomain_BodyAccelerometerJerk-maxInds-Z"            
* [60] "FrequencyDomain_BodyAccelerometerJerk-meanFreq-X"           
* [61] "FrequencyDomain_BodyAccelerometerJerk-bandsEnergy-1,24"     
* [62] "FrequencyDomain_BodyAccelerometerJerk-bandsEnergy-25,48"    
* [63] "FrequencyDomain_BodyGyroscope-mean-X"                       
* [64] "FrequencyDomain_BodyGyroscope-mean-Y"                       
* [65] "FrequencyDomain_BodyGyroscope-mean-Z"                       
* [66] "FrequencyDomain_BodyGyroscope_StandardDeviation_X"          
* [67] "FrequencyDomain_BodyGyroscope-maxInds-Y"                    
* [68] "FrequencyDomain_BodyGyroscope-maxInds-Z"                    
* [69] "FrequencyDomain_BodyGyroscope-meanFreq-X"                   
* [70] "FrequencyDomain_BodyGyroscope-bandsEnergy-1,24"             
* [71] "FrequencyDomain_BodyGyroscope-bandsEnergy-25,48"            
* [72] "FrequencyDomain_BodyAccelerometerMagnitude-entropy"         
* [73] "FrequencyDomain_BodyAccelerometerMagnitude-skewness"        
* [74] "FrequencyDomain_BodyAccelerometerMagnitude-kurtosis"        
* [75] "FrequencyDomain_BodyBodyAccelerometerJerkMagnitude-entropy" 
* [76] "FrequencyDomain_BodyBodyAccelerometerJerkMagnitude-skewness"
* [77] "FrequencyDomain_BodyBodyAccelerometerJerkMagnitude-kurtosis"
* [78] "FrequencyDomain_BodyBodyGyroscopeMagnitude-entropy"         
* [79] "FrequencyDomain_BodyBodyGyroscopeMagnitude-skewness"        
* [80] "FrequencyDomain_BodyBodyGyroscopeMagnitude-kurtosis"        
* [81] "FrequencyDomain_BodyBodyGyroscopeJerkMagnitude-entropy" 


## Description of measurements

*    Body = related to body movement.
*    Gravity = acceleration of gravity
*    Accelerometer = accelerometer measurement
*    Gyroscope = gyroscopic measurements
*    Jerk = sudden movement acceleration
*    Magnitude = magnitude of movement
*    mean and SD are calculated for each subject for each activity for each mean and SD measurements.

