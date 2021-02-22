comparison_magnitude_complex <- function(cases,listSpheres){
  pValues = data.frame()
  diff_Mag_Comp <- data.frame()
  diff_Perc_Mag_Comp <- data.frame()
  cnt <- 1
  for (j in seq(1,length(cases))){
    for (k in seq(1,14)){
      magData = as.numeric(unlist(listSpheres[[cases[j]]][k]))
      compData = as.numeric(unlist(listSpheres[[cases[j]+1]][k]))
      
      ##DIFERENCE BETWEEN MAGNITUDE AND COMPLEX
      diff_Mag_Comp[k,j] = mean(magData) - mean(compData)
      diff_Perc_Mag_Comp[k,j] = 100*(mean(magData) - mean(compData))/mean(magData)
      
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
    
    id = data[j,"id"]
    sid <- as.matrix(rep(id,14))
    sph <- as.matrix(1:14)
    #stdValues <- stdSites[,j]
    data_Mag_Comp <- data.frame(sid, sph, diff_Mag_Comp[,j], diff_Perc_Mag_Comp[,j])
    
    if (j==1){
      dataTmp = rbind(data.frame(), data_Mag_Comp)
    }
    else{
      dataComparison = rbind(dataTmp, data_Mag_Comp)
      dataTmp <- dataComparison
    }
    cnt = cnt + 1
  }
  
  colnames(dataComparison) <- c('sid', 'sph', 'diff', 'percDiff')
  returnComparison <- list("dataMagComp" = dataComparison,
                      "pValues" = pValues)
  
  #test = magData[[25]][1] - compData[[25]][1]
  
  return(returnComparison)
}