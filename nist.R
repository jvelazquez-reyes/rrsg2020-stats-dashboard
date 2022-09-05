get_reference_NIST_values <- function(serial_number){
  "
  Get the reference T1 values for the CaliberMRI/NIST values of the T1 plate at 20C.
  
  The CaliberMRI/NIST quantitative MRI phantom has two versions, version 1 is
  for serial numbers <0042 and version 2 is for serial numbers >=0042.
  
  T1 values are in ms.
  The values for version 1 are from: https://app.box.com/s/sqeuvq6uqbgx8ixa6mcp37nbrcpndwwm
  The values for version 2 are from an internal email with CaliberMRI, which they stated will be released
  in a new technical specifications document on their website near the end of August 2020.
  "
  if (serial_number<42){
    refT1 <- matrix(0, 1, 14)
    refT1[1,1] = 1989
    refT1[2,1] = 1454
    refT1[3,1] = 984.1
    refT1[4,1] = 709
    refT1[5,1] = 496
    refT1[6,1] = 351.5
    refT1[7,1] = 247.13
    refT1[8,1] = 175.3
    refT1[9,1] = 125.9
    refT1[10,1] = 89.0
    refT1[11,1] = 62.7
    refT1[12,1] = 44.53
    refT1[13,1] = 30.84
    refT1[14,1] = 21.719
    return(refT1)
  } else{
    refT1 <- matrix(0, 1, 14)
    refT1[1,1] = 1883.97
    refT1[2,1] = 1330.16
    refT1[3,1] = 987.27
    refT1[4,1] = 690.08
    refT1[5,1] = 484.97
    refT1[6,1] = 341.58
    refT1[7,1] = 240.86
    refT1[8,1] = 174.95
    refT1[9,1] = 121.08
    refT1[10,1] = 85.75
    refT1[11,1] = 60.21
    refT1[12,1] = 42.89
    refT1[13,1] = 30.40
    refT1[14,1] = 21.44
    return(refT1)
  }
}

get_NIST_ids <- function(){
  ids <- c('T1 - NIST sphere 1',
           'T1 - NIST sphere 2',
           'T1 - NIST sphere 3',
           'T1 - NIST sphere 4',
           'T1 - NIST sphere 5',
           'T1 - NIST sphere 6',
           'T1 - NIST sphere 7',
           'T1 - NIST sphere 8',
           'T1 - NIST sphere 9',
           'T1 - NIST sphere 10',
           'T1 - NIST sphere 11',
           'T1 - NIST sphere 12',
           'T1 - NIST sphere 13',
           'T1 - NIST sphere 14')
  return(ids)
}

temperature_correction <- function(input_temperature,serial_number,interpolation="cubic"){
  "
  NIST phantom temperaure correction tool
    
    INPUT ARGUMENTS:
        input_temperature - Temperature (°C) at which the phantom measurements were taken
        serial_number - Phantom serial number
        interpolation - 'quadratic' (default), 'cubic' and 'cubic-spline':
            'quadratic' - A low order polynomial was used to fit a log-log representation of the data
            'cubic' and 'cubic-spline' were used on the original data, no transformations applied

    OUTPUT:
        Array of temperature-corrected T1 values

    EXAMPLE:
        temperature_correction(20,42) = array([1883.97, 1330.16,  987.27,  690.08,  484.97,  341.58,  240.86,
                                               174.95,  121.08,   85.75,   60.21,   42.89,   30.4 ,   21.44])
        In the example above, the input temperature was 20°C, the phantom serial number was 42, and the quadratic
        polynomial was used to interpolate the data in a log-log representation.
 
        temperature_correction(18,40,'cubic-spline') = array([1830.34, 1317.71,  963.56,  686.88,  482.91,  341.53,
                                                          241.84,  175.94,  121.79,   86.41,   60.7 ,   43.24,   30.65,   21.62])
        In the last example, the input temperature was 18°C, the phantom serial number was 40, and a cubic-spline
        was used to interpolate the data in the original representation.
        
    NOTE:
        The polynomials of the fits for phantom serial numbers >= 42 are also used for serial numbers < 42,
        assuming that the T1 values will vary with temperature along the curve for either phantom version.
        The output T1 values for serial numbers > 42 are normalized to the reference T1 values of the phantom (SN<42).
  "
  # Dictionary of data
  list_14 <- c(21.94,21.62,21.44,21.28,21.26,21.31)
  names(list_14) <- c("16","18","20","22","24","26")
  list_13 <- c(31.05,30.65,30.40,30.27,30.25,30.31)
  names(list_13) <- c("16","18","20","22","24","26")
  list_12 <- c(43.79,43.24,42.89,42.72,42.70,42.80)
  names(list_12) <- c("16","18","20","22","24","26")
  list_11 <- c(61.49,60.70,60.21,59.97,60.00,60.17)
  names(list_11) <- c("16","18","20","22","24","26")
  list_10 <- c(87.47,86.41,85.75,85.03,85.01,85.28)
  names(list_10) <- c("16","18","20","22","24","26")
  list_9 <- c(122.99,121.79,121.08,120.80,120.90,121.34)
  names(list_9) <- c("16","18","20","22","24","26")
  list_8 <- c(177.68,175.94,174.95,174.59,174.78,175.48)
  names(list_8) <- c("16","18","20","22","24","26")
  list_7 <- c(243.77,241.84,240.86,240.75,241.31,242.45)
  names(list_7) <- c("16","18","20","22","24","26")
  list_6 <- c(343.00,341.53,341.58,342.58,344.23,346.67)
  names(list_6) <- c("16","18","20","22","24","26")
  list_5 <- c(483.91,482.91,484.97,486.92,490.24,494.55)
  names(list_5) <- c("16","18","20","22","24","26")
  list_4 <- c(675.07,686.88,690.08,695.01,701.06,709.4)
  names(list_4) <- c("16","18","20","22","24","26")
  list_3 <- c(950.71,963.56,987.27,1000.81,1015.70,1030.78)
  names(list_3) <- c("16","18","20","22","24","26")
  list_2 <- c(1274.07,1317.71,1330.16,1355.29,1367.79,1395.9)
  names(list_2) <- c("16","18","20","22","24","26")
  list_1 <- c(1766.68,1830.34,1883.97,1937.34,1987.50,2066.95)
  names(list_1) <- c("16","18","20","22","24","26")
  phantom_v2 <- list(list_14,list_13,list_12,list_11,list_10,list_9,list_8,
                     list_7,list_6,list_5,list_4,list_3,list_2,list_1)
  names(phantom_v2) <- c("14","13","12","11","10","9","8","7","6","5","4","3","2","1")
  
  ##Get keys and values of dictionary to construct a 2D array##
  #Get dictionary keys as lists
  sphereID = as.numeric(names(phantom_v2))
  temperature = as.numeric(names(phantom_v2[["1"]]))
  
  #Define array (data) to store data for interpolation
  data <- matrix(0, length(temperature), length(sphereID)+1)
  #Fill data array
  row = 0
  for (i in temperature){
    row = row + 1
    data[row,1] = i
    for (j in sphereID){
      T1_value = as.numeric(phantom_v2[[as.character(j)]][as.character(i)]);
      data[row,j+1] = T1_value;
    }
  }
  
  ##Code for Temperature Correction: Interpolation##
  #Define output arrays     
  estimatedT1 = matrix(0, length(sphereID), length(input_temperature))
  
  outputArray = matrix(0, length(sphereID), length(input_temperature))
  
  #Interpolation
  for (k in 1:length(sphereID)){
    for (l in 1:length(input_temperature)){
      if (interpolation=='cubic'){
        #Cubic        
        cubic = stats::splinefun(data[,1], data[,k+1], method = "fmm")
        estimatedT1[k,l] = cubic(input_temperature[l])
      } else if (interpolation=='cubic-spline'){
        #Cubic Spline
        cubicSpline = stats::splinefun(data[,0], data[,k+1], method = "hyman")
        estimatedT1[k,l] = cubicSpline(input_temperature[l])
      } else {
        print('Invalid interpolation (choose from "cubic-spline" (default), "cubic"')
        return(NULL)
      }
    }
  }
    
  #Returning the array with temperature-corrected T1 values
  if (serial_number>=42){
    outputArray = estimatedT1
    return(outputArray)
  } else if (serial_number<42){
    outputArray = estimatedT1*(get_reference_NIST_values(41)/get_reference_NIST_values(42))
    return(outputArray)
  } else{
    print('Invalid serial number.')
    return(NULL)
  }
}