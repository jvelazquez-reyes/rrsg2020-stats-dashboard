comparison_across_sites <- function(site){
  meanSite = data.frame()
  sdSite = data.frame()
  matSpheres = matrix(0)
  corr_per_sphere <- data.frame(Sphere=as.integer(), Pearson=as.numeric())
  #spheres = whitelist$whitelists$`NIST spheres`$whitelist
  spheres = 1:14
  for (j in seq(1,length(site))){
    flag=1
    numSpheres = 1
    id = site[j]
    #Find SID_Site
    dumIndSite = intersect(site[j],labelSidSite[,1])
    indFiltSite = match(dumIndSite,labelSidSite[,1])
    for (k in spheres){
      rowIndex = match(site[j],as.numeric(data[,"id"]))
      siteData = as.numeric(unlist(listSpheres[[rowIndex]][k]))
      
      phantomTemperature = as.numeric(data[rowIndex,"phantom.temperature"])
      phantomVersion = as.numeric(data[rowIndex,"phantom.version"])
      refT1 = temperature_correction(phantomTemperature,phantomVersion)
      
      #For non-voxelwise
      meanSite[numSpheres,j] = mean(siteData)
      if (length(site)>1){
        sdSite[numSpheres,j] = sd(siteData)
      }
      else{
        sdSite[numSpheres,j] = 0
      }
      
      #Pixelwise
      sid_long <- as.matrix(rep(id,length(siteData)))
      ID_Site_long <- as.matrix(rep(labelSidSite[indFiltSite,2],length(siteData)))
      sph_long <- as.matrix(rep(k,length(siteData)))
      t1_long <- as.matrix(rep(refT1[c(spheres)][numSpheres],length(siteData)))
      ac_error <- 100*abs(siteData-t1_long)/t1_long
      
      data_Site_long <- data.frame(sid_long, ID_Site_long, sph_long, t1_long, siteData, ac_error)
      
      if (j==1 && flag==1){
        dataTmp_long = rbind(data.frame(), data_Site_long)
        flag=0
        if (length(site)==1){
          dataSite2plot_long = data_Site_long
        }
      }
      else{
        dataSite2plot_long = rbind(dataTmp_long,data_Site_long)
        dataTmp_long <- dataSite2plot_long
      }
      
      #matSpheres[,j,numSpheres] = siteData
      ##STATISTICAL TESTS (COMPARE MEANS)
      #subset(RefVSMeas$BAData, sph == 1)
      numSpheres = numSpheres + 1
    }
    
    #Non-voxelwise
    sid <- as.matrix(rep(id,length(spheres)))
    sph <- as.matrix(c(spheres))
    t1 <- as.matrix(refT1[c(spheres)])
    ID_Site <- as.matrix(rep(labelSidSite[indFiltSite,2],length(spheres)))
    
    data_Site <- data.frame(sid, ID_Site, sph, t1, meanSite[,j], sdSite[,j])
    
    if (j==1){
      dataTmp = rbind(data.frame(), data_Site)
      if (length(site)==1){
        dataSite2plot = data_Site
      }
    }
    else{
      dataSite2plot = rbind(dataTmp, data_Site)
      dataTmp <- dataSite2plot
    }
  }
  
  #Correlation coefficients per sphere, pixelwise
  for (ii in seq(1,length(spheres))){
    data_per_sphere = subset(dataSite2plot_long, sph_long == spheres[ii])
    corr_per_sphere[ii,1] = spheres[ii]
    corr_per_sphere[ii,2] = signif(cor(data_per_sphere$t1_long,data_per_sphere$siteData),3)
  }
  
  ##ONE-WAY ANOVA##
  multComparisons <- list()
  if (length(site)>2){
    for (j in seq(1,14)){
      flag = 1
      anovaGer <- data.frame(T1=numeric(),group=numeric())
      
      for (k in site){
        rowIndex = match(k,as.numeric(data[,"id"]))
        if (flag==1){
          firstIndex = 0
          lastIndex = 0
        }
        sample = as.numeric(unlist(listSpheres[[rowIndex]][j]))
        lastIndex = length(sample)
        anovaGer[(1+firstIndex):(firstIndex+lastIndex),1] = sample
        anovaGer[(1+firstIndex):(firstIndex+lastIndex),2] = rep(k,length(sample))
        
        firstIndex = firstIndex + length(sample)
        flag = 0
      }
      anovaGer$group <- as.factor(anovaGer$group)
      res.aov <- aov(T1 ~ group, data = anovaGer)
      multComparisons[j] = TukeyHSD(res.aov)
    }
  }
  
  colnames(dataSite2plot) <- c('sid', 'ID_Site', 'Sphere', 'refT1', 'Mean', 'Std')
  
  returnComparison <- list("dataSite" = dataSite2plot,
                           "dataSite_long" = dataSite2plot_long,
                           "corrSph_across_sites" = corr_per_sphere,
                           "ANOVA" = multComparisons)
  
  return(returnComparison)
}
