linear_mixed_effects_model <- function(sites){
  ##Data in long format
  data <- read.csv(paste(path_to_CSVfile, "3T_NIST_T1maps_database.csv", sep = ""))
  data[] <- gsub("[][]", "", as.matrix(data))
  
  listSpheres = list()
  list2append = list()
  count <- 1
  for (i in sites){
    phantomTemperature = as.numeric(data[i,"phantom.temperature"])
    phantomVersion = as.numeric(data[i,"phantom.version"])
    if (phantomVersion<42){
      refT1 = temperature_correction(phantomTemperature,phantomVersion)
    } else {
      refT1 = temperature_correction(phantomTemperature,phantomVersion)
    }
    
    id = data[i,"id"]
    dType = data[i,"Data.type"]
    vendor = data[i,"MRI.vendor"]
    version = data[i,"MRI.version"]
    
    for (j in seq(1,14)){
      dataSphere = gsub("\\. ","",data[i,j+grep("^T1...NIST.sphere.1$", colnames(data))-1])
      dataSphere = as.matrix(as.numeric(unlist(strsplit(dataSphere," "))))
      dataSphere = dataSphere[!is.na(dataSphere)]
      
      sid <- as.matrix(rep(id,length(dataSphere)))
      sphere <- as.matrix(rep(j,length(dataSphere)))
      t1ref <- as.matrix(rep(refT1[j],length(dataSphere)))
      dataType <- as.matrix(rep(dType,length(dataSphere)))
      MRIvendor <- as.matrix(rep(vendor,length(dataSphere)))
      MRIversion <- as.matrix(rep(version,length(dataSphere)))
      
      dataLong <- data.frame(sid, sphere, t1ref, dataSphere, dataType, MRIvendor, MRIversion)
      
      #Long format data frame
      if (count==1){
        longTmp = rbind(data.frame(), dataLong)
        count = count + 1
      }
      else{
        lmeData = rbind(longTmp, dataLong)
        longTmp <- lmeData
      }
    }
  }
  
  firstLME <- lmer(dataSphere ~ t1ref + MRIversion + (1|sid), data = lmeData)
  returnLME <- list("dataLME" = lmeData)
  return(returnLME)
}