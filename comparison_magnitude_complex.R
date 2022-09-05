comparison_magnitude_complex <- function(cases){
  pValues = data.frame()
  meanMag = data.frame()
  meanComp = data.frame()
  diff_Mag_Comp <- data.frame()
  diff_Perc_Mag_Comp <- data.frame()
  avg_MagComp <- data.frame()
  corr_per_sphere <- data.frame(Sphere=as.integer(), R=as.numeric())
  spheres = 1:14
  flag = 1
  cnt <- 1
  for (j in seq(1,length(cases))){
    id = as.numeric(data[cases[j],"id"])
    for (k in spheres){
      #phantomTemperature = as.numeric(data[cases[j],"phantom.temperature"])
      #phantomVersion = as.numeric(data[cases[j],"phantom.version"])
      refT1 = temperature_correction(20,42)
      
      magData = as.numeric(unlist(listSpheres[[cases[j]]][k]))
      compData = as.numeric(unlist(listSpheres[[cases[j]+1]][k]))
      
      ##DATA FOR LONG DATAFRAME FORMAT
      sid_long <- as.matrix(rep(id,length(magData)))
      sph_long <- as.matrix(rep(k,length(magData)))
      refT1_long <- as.matrix(rep(refT1[k],length(magData)))
      average = (magData + compData)/2
      difference = magData - compData
      perc_difference = 100*difference/average
      
      data_MagComp_long <- data.frame(sid_long, sph_long, refT1_long, magData, compData, difference, perc_difference, average)
      
      if (j==1 && flag==1){
        dataTmp_long = rbind(data.frame(), data_MagComp_long)
        flag = 0
      }
      else{
        dataComparison_long = rbind(dataTmp_long, data_MagComp_long)
        dataTmp_long <- dataComparison_long
      }

      ##DIFERENCE BETWEEN MAGNITUDE AND COMPLEX
      meanMag[k,j] = mean(magData)
      meanComp[k,j] = mean(compData)
      
      avg_MagComp[k,j] = (mean(magData) + mean(compData))/2
      diff_Mag_Comp[k,j] = mean(magData) - mean(compData)
      diff_Perc_Mag_Comp[k,j] = 100*diff_Mag_Comp[k,j]/avg_MagComp[k,j]
      
      ##STATISTICAL TESTS (COMPARE MEANS)
      #Test for normality of data
      magnitudeNormTest = shapiro.test(magData)
      complexNormTest = shapiro.test(compData)
      #Test for equal variances
      eqVarTest = bartlett.test(list(magData,compData))
      #t-test with equal variances, unequal variances and non-parametric test (normality test failure)
      if (magnitudeNormTest[2]>0.05 && complexNormTest[2]>0.05 && eqVarTest[3]>0.05){
        tTest = t.test(magData,compData,var.equal = TRUE)
        pValues[cnt,k] = tTest[3]
      } else if (magnitudeNormTest[2]>0.05 && complexNormTest[2]>0.05 && eqVarTest[3]<0.05){
        tTest = t.test(magData,compData,var.equal = FALSE)
        pValues[cnt,k] = tTest[3]
      } else if (magnitudeNormTest[2]<0.05 || complexNormTest[2]<0.05) {
        wTest = wilcox.test(magData,compData, paired = FALSE)
        pValues[cnt,k] = wTest[3]
      }
    }
    
    #id = data[cases[j],"id"]
    sid <- as.matrix(rep(id,14))
    sph <- as.matrix(1:14)
    t1 <- as.matrix(refT1)
    
    ##CORRELATION ANALYSIS
    corrTest <- cor.test(meanMag[,j], meanComp[,j], method = 'pearson')
    linTest <- CCC(meanMag[,j], meanComp[,j])
    Pearson_test <- data.frame(id, corrTest$estimate, linTest[[1]][1])
    
    #DIFFERENCE AND PERCENTAGE DIFFERENCE
    data_Mag_Comp <- data.frame(sid, sph, t1, diff_Mag_Comp[,j], diff_Perc_Mag_Comp[,j], meanMag[,j], meanComp[,j], avg_MagComp[,j])
    corr_Mag_Comp <- data.frame(sid, sph, t1, meanMag[,j], meanComp[,j])
    
    if (j==1){
      dataTmp = rbind(data.frame(), data_Mag_Comp)
      corrTmp = rbind(data.frame(), corr_Mag_Comp)
      PearsonTmp = rbind(data.frame(), Pearson_test)
      if (length(cases)==1){
        dataComparison = dataTmp
        dataCorrelation = corrTmp
        dataPearson = PearsonTmp
      }
    }
    else{
      dataComparison = rbind(dataTmp, data_Mag_Comp)
      dataCorrelation = rbind(corrTmp, corr_Mag_Comp)
      dataPearson = rbind(PearsonTmp, Pearson_test)
      dataTmp <- dataComparison
      corrTmp <- dataCorrelation
      PearsonTmp <- dataPearson
    }
  }
  
  colnames(dataComparison) <- c('sid', 'sph', 'refT1', 'diff', 'percDiff', 'Magnitude', 'Complex', 'average')
  colnames(dataCorrelation) <- c('sid', 'sph', 'refT1', 'Magnitude', 'Complex')
  colnames(dataPearson) <- c('Site', 'Pearson', 'Lin')
  
  #Correlation coefficients per sphere
  for (ii in seq(1,length(spheres))){
    data_per_sphere = subset(dataComparison_long, sph_long == spheres[ii])
    corr_per_sphere[ii,1] = spheres[ii]
    corr_per_sphere[ii,2] = cor(data_per_sphere$magData,data_per_sphere$compData)
  }
  
  returnComparison <- list("dataMagComp" = dataComparison,
                           "dataCorr" = dataCorrelation,
                           "PearsonCorr" = dataPearson,
                           "PearsonCorrSphere" = dataComparison_long,
                           "pValues" = pValues)
  
  return(returnComparison)
}