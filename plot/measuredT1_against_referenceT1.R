measuredT1_against_referenceT1 <- function(scans){
  meanSites <- data.frame()
  stdSites <- data.frame()
  mseSites <- data.frame()
  rmseSites <- data.frame()
  correlations <- data.frame(Site=as.numeric(), ICC=as.numeric(), R=as.numeric(), Lin=as.numeric())
  dispersionList = list()
  BAList = list()
  stdList = list()
  rmseList = list()
  test <- data.frame()
  for (j in scans){
    data2plot <- data.frame()
    std2plot <- data.frame()
    rmse2plot <- data.frame()
    data2coef <- data.frame()
    
    
    
    phantomTemperature = as.numeric(data[j,"phantom.temperature"])
    phantomVersion = as.numeric(data[j,"phantom.version"])
    id = data[j,"id"]
    
    if (phantomVersion<42){
      refTemp = temperature_correction(phantomTemperature,phantomVersion)
    } else {
      refTemp = temperature_correction(phantomTemperature,phantomVersion)
    }
    
    for (k in seq(1,14)){
      measuredT1 = as.numeric(unlist(listSpheres[[j]][k]))
      meanSites[k,j] = mean(measuredT1)
      stdSites[k,j] = sd(measuredT1)
      rmseSites[k,j] = rmse(measuredT1, rep(refTemp[k],length(measuredT1)))
    }
    
    sid <- as.matrix(rep(id,14))
    sph <- as.matrix(1:14)
    
    #Bland-Altman analysis
    measValue <- meanSites[,j]
    reference <- refTemp
    difference <- measValue - reference
    average <- (measValue + reference)/2
    BA2plot <- data.frame(sid, sph, measValue, reference, difference, average)
    
    #STD

    stdValues <- stdSites[,j]
    std2plot <- data.frame(sid, sph, refTemp, stdValues)
    
    #RMSE
    rmseValues <- rmseSites[,j]
    rmse2plot <- data.frame(sid, sph, refTemp, rmseValues)
    
    #Long format data frame
    if (j==scans[1]){
      stdTmp = rbind(data.frame(), std2plot)
      rmseTmp = rbind(data.frame(), rmse2plot)
      BATmp = rbind(data.frame(), BA2plot)
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
    #ICC(1,1): It reflects the variation between 2 or more raters who measure the same group of subjects.
    icc_test = icc(data2coef, model = "oneway", type = "agreement")
    
    #Pearson correlation coefficient
    Pearson_test = cor(data2coef)
    
    #Lin's concordance correlation coefficient
    Lin_test = epi.ccc(data2coef[,1], data2coef[,2])
    
    correlations[j,1] = id
    correlations[j,2] = icc_test[7]
    correlations[j,3] = Pearson_test[1,2]
    correlations[j,4] = Lin_test[[1]][1]
    
    #PLOTS
    #Dispersion
    p = ggplot(data = data2plot, mapping = aes(x = reference, y = measValue)) +
      geom_point(color = "black", size = 1.5) +
      labs(x = "Reference T1 value (ms)", y = "Measured T1 value (ms)") +
      geom_smooth(method = "lm", se = TRUE, color = "red", lwd = 0.5) +
      geom_abline(intercept = 0, slope = 1, lwd = 0.7, col = "blue") +
      theme(axis.line = element_line(colour = "black"), 
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(), 
            panel.border = element_blank(), 
            panel.background = element_blank()) +
      theme_bw() + theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
                         axis.title = element_text(size = 12),
                         axis.text = element_text(size = 12))
    dispersionList[[j]] = ggplotly(p)
    
    #Bland-Altman
    p <- ggplot(data = data2plot, aes(x = average, y = difference)) +
      geom_point(pch = 1, size = 1.5, col = "black") +
      labs(title = paste("Site ID:", id, sep = ""), x = "Average T1 (ms)", 
           y = "Measured - Reference") +
      geom_smooth(method = "lm", se = TRUE, fill = "lightgrey", lwd = 0.1, lty = 5) +
      ylim(mean(data2plot$difference) - 4 * sd(data2plot$difference), 
           mean(data2plot$difference) + 4 * sd(data2plot$difference)) +
      # Línea de bias
      geom_hline(yintercept = mean(data2plot$difference), lwd = 1) +
      # Línea en y=0
      geom_hline(yintercept = 0, lty = 3, col = "grey30") +
      # Limits of Agreement
      geom_hline(yintercept = mean(data2plot$difference) + 
                   1.96 * sd(data2plot$difference), 
                 lty = 2, col = "firebrick") +
      geom_hline(yintercept = mean(data2plot$difference) - 
                   1.96 * sd(data2plot$difference), 
                 lty = 2, col = "firebrick") +
      theme(panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank()) +
      geom_text(label = "Bias", x = 2000, y = 30, size = 3, 
                colour = "black") +
      geom_text(label = "+1.96SD", x = 2000, y = 190, size = 3, 
                colour = "firebrick") +
      geom_text(label = "-1.96SD", x = 2000, y = -110, size = 3, 
                colour = "firebrick") +
      theme_bw() + theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
                         axis.title = element_text(size = 12),
                         axis.text = element_text(size = 12))
    BAList[[j]] = ggplotly(p)
    
    #STD
    p = ggplot(data = std2plot, aes(x = refTemp, y = stdValues)) +
      geom_point(color = "black", size = 1.5) +
      labs(title = paste("Site ID:", id, sep = ""), x = "Reference T1 (ms)", y = "SD (ms)") +
      theme(axis.line = element_line(colour = "black"), 
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(), 
            panel.border = element_blank(), 
            panel.background = element_blank()) +
      theme_bw() + theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
                         axis.title = element_text(size = 12),
                         axis.text = element_text(size = 12))
    stdList[[j]] = ggplotly(p)
    
    #RMSE
    #p = ggplot(data = rmse2plot, aes(x = refTemp, y = rmseValues)) +
    #  geom_point(color = "black", size = 1.5) +
    #  labs(title = paste("Site ID:", id, sep = ""), x = "Reference T1 (ms)", y = "RMSE (ms)") +
    #  theme(axis.line = element_line(colour = "black"), 
    #        panel.grid.major = element_blank(), 
    #        panel.grid.minor = element_blank(), 
    #        panel.border = element_blank(), 
    #        panel.background = element_blank()) +
    #  theme_bw() + theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    #                     axis.title = element_text(size = 12),
    #                     axis.text = element_text(size = 12))
    #rmseList[[j]] = ggplotly(p)
  }
  returnStats <- list("Correlation_coefficients" = correlations,
                     "BAData" = BAData,
                     "stdData" = stdData,
                     "rmseData" = rmseData)
  return(returnStats)
  
}