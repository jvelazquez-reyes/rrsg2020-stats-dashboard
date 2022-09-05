comparison_NISTHuman_nist <- function(sitesNP,dualSub_Sites){
  meanSite = data.frame()
  sdSite = data.frame()
  szSample = data.frame()
  spheres = 1:14
  refT1 = temperature_correction(20,42)
  for (j in seq(1,length(sitesNP))){
    flag=1
    
    id = data[sitesNP[j],"id"]

    cnt_sphere = 1
    for (k in spheres){
      siteData = as.numeric(unlist(listSpheres[[sitesNP[j]]][spheres[cnt_sphere]]))
      
      #For non-voxelwise
      meanSite[cnt_sphere,j] = mean(siteData)
      sdSite[cnt_sphere,j] = sd(siteData)
      szSample[cnt_sphere,j] = length(siteData)
      
      #Voxelwise
      sid_long <- as.matrix(rep(id,length(siteData)))
      t1ROI_long <- as.matrix(rep(refT1[c(spheres)][cnt_sphere],length(siteData)))
      NPHuman_long <- as.matrix(rep("NIst phantom"),length(siteData))
      
      data_Site_long <- data.frame(sid_long, t1ROI_long, siteData)
      
      if (j==1 && flag==1){
        dataTmp_long = rbind(data.frame(), data_Site_long)
        flag=0
        if (length(sitesNP)==1){
          dataSite2plot_long = data_Site_long
        }
      }
      else{
        dataSite2plot_long = rbind(dataTmp_long,data_Site_long)
        dataTmp_long <- dataSite2plot_long
      }
      
      cnt_sphere = cnt_sphere + 1
    }
    
    #Non-voxelwise
    sid_name <- as.matrix(rep(dualSub_Sites[j],length(spheres)))
    sid <- as.matrix(rep(id,length(spheres)))
    t1ROI <- as.matrix(refT1)
    NPHuman <- as.matrix(rep("NIST phantom"),length(spheres))
    
    
    data_Site <- data.frame(sid_name, sid, t1ROI, meanSite[,j], sdSite[,j], NPHuman, szSample[,j])
    
    if (j==1){
      dataTmp = rbind(data.frame(), data_Site)
      if (length(sitesNP)==1){
        dataSite2plot = data_Site
      }
    }
    else{
      dataSite2plot = rbind(dataTmp, data_Site)
      dataTmp <- dataSite2plot
    }
  }
  colnames(dataSite2plot) <- c('Site_name', 'Site', 't1ROI', 'Mean', 'Std', 'NPHuman', 'szSample')
  returnNISTData <- list("dataLong_NIST" = dataSite2plot_long,
                          "data_NIST" = dataSite2plot)
  return(returnNISTData)
}

comparison_NISThuman_human <- function(sitesH,dualSub_Sites){
  meanSite = data.frame()
  sdSite = data.frame()
  szSample = data.frame()
  rois = 1:4
  labelHumanROI <- c("Genu WM","splenium WM","Deep GM","Cortical GM")
  for (j in seq(1,length(sitesH))){
    flag=1
    
    id = data2[sitesH[j],"id"]
    
    cnt_roi = 1
    for (k in labelHumanROI){
      siteData = as.numeric(unlist(listHuman[[sitesH[j]]][rois[cnt_roi]]))
      
      #For non-voxelwise
      meanSite[cnt_roi,j] = mean(siteData)
      sdSite[cnt_roi,j] = sd(siteData)
      szSample[cnt_roi,j] = length(siteData)
      
      #Voxelwise
      sid_long <- as.matrix(rep(id,length(siteData)))
      t1ROI_long <- as.matrix(rep(k,length(siteData)))
      NPHuman_long <- as.matrix(rep("Human data"),length(siteData))
      
      data_Site_long <- data.frame(sid_long, t1ROI_long, siteData)
      
      if (j==1 && flag==1){
        dataTmp_long = rbind(data.frame(), data_Site_long)
        flag=0
        if (length(sitesH)==1){
          dataSite2plot_long = data_Site_long
        }
      }
      else{
        dataSite2plot_long = rbind(dataTmp_long,data_Site_long)
        dataTmp_long <- dataSite2plot_long
      }
      
      cnt_roi = cnt_roi + 1
    }
    
    #Non-voxelwise
    sid_name <- as.matrix(rep(dualSub_Sites[j],length(rois)))
    sid <- as.matrix(rep(id,length(rois)))
    t1ROI <- as.matrix(labelHumanROI)
    NPHuman <- as.matrix(rep("Human data"),length(rois))
    
    data_Site <- data.frame(sid_name, sid, t1ROI, meanSite[,j], sdSite[,j], NPHuman, szSample[,j])
    
    if (j==1){
      dataTmp = rbind(data.frame(), data_Site)
      if (length(sitesH)==1){
        dataSite2plot = data_Site
      }
    }
    else{
      dataSite2plot = rbind(dataTmp, data_Site)
      dataTmp <- dataSite2plot
    }
  }
  colnames(dataSite2plot) <- c('Site_name', 'Site', 't1ROI', 'Mean', 'Std', 'NPHuman', 'szSample')
  returnHumanData <- list("dataLong_human" = dataSite2plot_long,
                          "data_human" = dataSite2plot)
  return(returnHumanData)
}