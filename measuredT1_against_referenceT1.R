measuredT1_against_referenceT1 <- function(scans){
  meanSites <- data.frame()
  stdSites <- data.frame()
  mseSites <- data.frame()
  rmseSites <- data.frame()
  data4icc <- data.frame()
  correlations <- data.frame(Site=as.numeric(), R=as.numeric(), Lin=as.numeric())
  correlations2 <- data.frame(R=as.numeric(), Lin=as.numeric())
  
  firstSphere = whitelist$whitelists$`NIST spheres`$whitelist[1]
  lastSphere = tail(whitelist$whitelists$`NIST spheres`$whitelist,1)
  for (j in seq(1,length(scans))){
    rowIndex = match(scans[j],as.numeric(data[,"id"]))
    phantomTemperature = as.numeric(data[rowIndex,"phantom.temperature"])
    phantomVersion = as.numeric(data[rowIndex,"phantom.version"])
    refT1 = temperature_correction(phantomTemperature,phantomVersion)
    
    for (k in seq(firstSphere,lastSphere)){
      measuredT1 = as.numeric(unlist(listSpheres[[rowIndex]][k]))
      meanSites[k,j] = mean(measuredT1)
      stdSites[k,j] = sd(measuredT1)
      rmseSites[k,j] = rmse(measuredT1, rep(refT1[k],length(measuredT1)))
      
      #Data for the ICC calculation
      data4icc[k,j] = abs(meanSites[k,j] - refT1[k])*100/refT1[k]
    }
    
    sid <- as.matrix(rep(as.character(scans[j]),lastSphere))
    sph <- as.matrix(firstSphere:lastSphere)
    
    dumIndSite = intersect(scans[j],labelSidSite[,1])
    indFiltSite = match(dumIndSite,labelSidSite[,1])
    ID_Site <- as.matrix(rep(labelSidSite[indFiltSite,2],lastSphere))
    
    dumIndVendor = intersect(scans[j],labelSidVendor[,1])
    indFiltVendor = match(dumIndVendor,labelSidVendor[,1])
    ID_Vendor <- as.matrix(rep(labelSidVendor[indFiltVendor,2],lastSphere))
    
    #Bland-Altman analysis
    measValue <- meanSites[,j]
    reference <- refT1[firstSphere:lastSphere]
    difference <- measValue - reference
    average <- (measValue + reference)/2
    perc_difference <- difference*100/average
    BA2plot <- data.frame(sid, ID_Site, ID_Vendor, sph, measValue, reference, difference, perc_difference, average)
    
    #STD
    stdValues <- stdSites[,j]
    std2plot <- data.frame(sid, ID_Site, ID_Vendor, sph, reference, stdValues)
    
    #RMSE
    rmseValues <- rmseSites[,j]
    rmse2plot <- data.frame(sid, ID_Site, ID_Vendor, sph, reference, rmseValues)
    
    #Long format data frame
    if (j==1){
      stdTmp = rbind(data.frame(), std2plot)
      rmseTmp = rbind(data.frame(), rmse2plot)
      BATmp = rbind(data.frame(), BA2plot)
      
      if (length(scans)==1){
        stdData = stdTmp
        rmseData = rmseTmp
        BAData = BATmp
      }
    }
    else{
      stdData = rbind(stdTmp, std2plot)
      rmseData = rbind(rmseTmp, rmse2plot)
      BAData = rbind(BATmp, BA2plot)
      stdTmp <- stdData
      rmseTmp <- rmseData
      BATmp <- BAData
    }
    
    data2coef <- data.frame(measValue, reference)
    #Pearson correlation coefficient
    Pearson_test = cor(data2coef)
    
    #Lin's concordance correlation coefficient
    Lin_test = CCC(data2coef[,1], data2coef[,2])
    
    correlations[j,1] = scans[j]
    correlations[j,2] = Pearson_test[1,2]
    correlations[j,3] = Lin_test[[1]][1]
  }
  
  #ICC(2,1): It reflects the variation between 2 or more raters who measure the same group of subjects.
  icc_test = icc(data4icc, model = "twoway", type = "agreement")
  Pearson_test2 = cor(BAData$measValue, BAData$reference)
  Lin_test2 = CCC(BAData$measValue, BAData$reference)
  correlations2[1,1] = Pearson_test2
  correlations2[1,2] = Lin_test2[[1]][1]
  #correlations2[1,3] = icc_test[7]
  colnames(correlations2) = c("Pearson","Lin")
  
  returnStats <- list("Correlation_coefficients" = correlations,
                      "BAData" = BAData,
                      "stdData" = stdData,
                      "rmseData" = rmseData,
                      "corr_coef_site" = correlations2,
                      "test" = data4icc)
  return(returnStats)
  
}