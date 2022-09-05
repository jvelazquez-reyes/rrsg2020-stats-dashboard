getHumanData <- function(site){
  rois = 1:4
  labelHumanROI <- c("Genu WM","splenium WM","Deep GM","Cortical GM")
  for (j in seq(1,length(site))){
    flag=1
    id = data2[site[j],"id"]
    age = data2[site[j],"age"]
    sex = data2[site[j],"sex"]
    vendor = data2[site[j],'MRI.vendor']
    #Find SID_Site
    #dumIndSite = intersect(site[j],labelSidSite[,1])
    #indFiltSite = match(dumIndSite,labelSidSite[,1])
    cnt_roi = 1
    for (k in labelHumanROI){
      #rowIndex = match(site[j],as.numeric(data2[,"id"]))
      siteData = as.numeric(unlist(listHuman[[site[j]]][rois[cnt_roi]]))
      
      #Voxelwise
      sid_long <- as.matrix(rep(id,length(siteData)))
      age_long <- as.matrix(rep(age,length(siteData)))
      sex_long <- as.matrix(rep(sex,length(siteData)))
      roi_long <- as.matrix(rep(k,length(siteData)))
      vendor_long <-as.matrix(rep(vendor,length(siteData)))
      
      data_Site_long <- data.frame(sid_long, vendor_long, age_long, sex_long, roi_long, siteData)
      
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
      
      cnt_roi = cnt_roi + 1
    }
  }
  returnHumanData <- list("dataLong_human" = dataSite2plot_long)
  return(returnHumanData)
}