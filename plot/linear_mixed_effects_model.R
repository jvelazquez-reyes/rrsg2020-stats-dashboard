linear_mixed_effects_model <- function(sites){
  ##Data in long format
  data <- read.csv(paste(path_to_CSVfile, "3T_NIST_T1maps_database.csv", sep = ""))
  data[] <- gsub("[][]", "", as.matrix(data))
  
  submission <- 1:40
  listSpheres = list()
  list2append = list()
  count <- 1
  for (i in submission){
    for (j in seq(1,14)){
      dataSphere = gsub("\\. ","",data[i,j+grep("^T1...NIST.sphere.1$", colnames(data))-1])
      dataSphere = as.matrix(as.numeric(unlist(strsplit(dataSphere," "))))
      dataSphere = dataSphere[!is.na(dataSphere)]
      
      phantomTemperature = as.numeric(data[j,"phantom.temperature"])
      phantomVersion = as.numeric(data[j,"phantom.version"])
      id = data[i,"id"]
      sid <- as.matrix(rep(id,length(dataSphere)))
      sphere <- as.matrix(j,length(dataSphere))
      
      dataLong <- data.frame(sid, sphere, dataSphere)
      
      #Long format data frame
      if (count==1){
        longTmp = rbind(data.frame(), dataLong)
      }
      else{
        lmeData = rbind(longTmp, dataLong)
        longTmp <- lmeData
      }
      count = count + 1
    }
  }
  return(lmeData)
}