##Call Python script from R##

#Disable prompt to install miniconda
#Sys.setenv(RETICULATE_PYTHON = "../my_env/bin/python")
#reticulate::source_python(paste(getwd(),"/nist.py", sep = ""))
#Source nist.R temperature_correction tool
source("nist.R")

##PHANTOM DATASET##
data <- read.csv("3T_NIST_T1maps_database.csv")
data[] <- gsub("[][]", "", as.matrix(data))
colnames(data)[1] <- gsub('^...','',colnames(data)[1])

submission <- 1:38
listSpheres = list()
list2append = list()
for (i in submission){
  for (j in seq(1,14)){
    dataSphere = gsub("\\. ","",data[i,j+grep("^T1...NIST.sphere.1$", colnames(data))-1])
    dataSphere = as.matrix(as.numeric(unlist(strsplit(dataSphere," "))))
    dataSphere = dataSphere[!is.na(dataSphere)]
    
    list2append[[j]] = dataSphere
  }
  listSpheres[[i]] = list2append
}

##COMPARE MAGNITUDE VS COMPLEX##
source("comparison_magnitude_complex.R")

cases <- c(1,seq(11,25,2),32,34)

#p-value > 0.5, there's no statistical difference between magnitude and complex
magVScomp <- comparison_magnitude_complex(cases)

#Germany <- 13:26
#Montreal <- c(4,11,12,27:30,38)
#US <- c(3,5:10,32:37)
#London <- 1:2
#Australia <- 31
#id = data[scans[j],"id"]

##LOADING NIST_whitelists##
whitelist <- fromJSON(file = "NIST_whitelists.json")

##FILTER PER SITE
allLondon <- c(1.001,1.002)
allUS <- c(2.001,4.001,4.002,4.003,4.004,4.005,4.006,10.001,10.002,10.003,10.004,11.001,11.002)
allMontreal <- c(3.001,5.001,5.002,7.001,7.002,8.001,8.002,12.001)
allGermany <- c(6.001,6.002,6.003,6.004,6.005,6.006,6.007,6.008,6.009,6.010,6.011,6.012,6.013,6.014)
allAustralia <- c(9.001)

filteredSites <- whitelist$whitelists$`one measurement per scanner`$whitelist

London <- intersect(filteredSites,allLondon)
US <- intersect(filteredSites,allUS)
Montreal <- intersect(filteredSites,allMontreal)
Germany <- intersect(filteredSites,allGermany)
Australia <- intersect(filteredSites,allAustralia)

labelSidSite <- matrix(0L, nrow = length(filteredSites), ncol = 2)
for (ii in seq(1,length(filteredSites))){
  if(filteredSites[ii] %in% London){labelSidSite[ii,1] = filteredSites[ii]
  labelSidSite[ii,2] = paste(filteredSites[ii],"London")}
  if(filteredSites[ii] %in% US){labelSidSite[ii,1] = filteredSites[ii]
  labelSidSite[ii,2] = paste(filteredSites[ii],"US")}
  if(filteredSites[ii] %in% Montreal){labelSidSite[ii,1] = filteredSites[ii]
  labelSidSite[ii,2] = paste(filteredSites[ii],"Montreal")}
  if(filteredSites[ii] %in% Germany){labelSidSite[ii,1] = filteredSites[ii]
  labelSidSite[ii,2] = paste(filteredSites[ii],"Germany")}
  if(filteredSites[ii] %in% Australia){labelSidSite[ii,1] = filteredSites[ii]
  labelSidSite[ii,2] = paste(filteredSites[ii],"Australia")}
}

##FILTER PER MRI VENDOR##
allSiemens <- c(1.001,1.002,2.001,3.001,8.001,8.002,9.001,11.001,11.002,12.001)
allGE <- c(4.001,4.002,4.003,4.004,4.005,4.006,10.001,10.002,10.003,10.004)
allPhilips <- c(5.001,5.002,6.001,6.002,6.003,6.004,6.005,6.006,6.007,6.008,6.009,6.010,6.011,
                6.012,6.013,6.014,7.001,7.002)

Siemens <- intersect(filteredSites,allSiemens)
GE <- intersect(filteredSites,allGE)
Philips <- intersect(filteredSites,allPhilips)

labelSidVendor <- matrix(0L, nrow = length(filteredSites), ncol = 2)
for (ii in seq(1,length(filteredSites))){
  if(filteredSites[ii] %in% Siemens){labelSidVendor[ii,1] = filteredSites[ii]
  labelSidVendor[ii,2] = paste(filteredSites[ii],"Siemens")}
  if(filteredSites[ii] %in% GE){labelSidVendor[ii,1] = filteredSites[ii]
  labelSidVendor[ii,2] = paste(filteredSites[ii],"GE")}
  if(filteredSites[ii] %in% Philips){labelSidVendor[ii,1] = filteredSites[ii]
  labelSidVendor[ii,2] = paste(filteredSites[ii],"Philips")}
}

##ANALYSIS WITHIN GROUPS ACROSS SITES
source("comparison_across_sites.R")

MeasSites <- comparison_across_sites(filteredSites)
SiteGermany <- comparison_across_sites(Germany)
SiteMontreal <- comparison_across_sites(Montreal)
SiteUS <- comparison_across_sites(US)
SiteLondon <- suppressWarnings(comparison_across_sites(London))
SiteAustralia <- suppressWarnings(comparison_across_sites(Australia))

##COMPARISON BETWEEN MEASURED AND REFERENCE T1 VALUES##
source("measuredT1_against_referenceT1.R")

#scans <- 1:4
RefVSMeas <- measuredT1_against_referenceT1(filteredSites)
sdFilteredSites <- measuredT1_against_referenceT1(filteredSites)
sdMontreal <- measuredT1_against_referenceT1(Montreal)
sdGermany <- measuredT1_against_referenceT1(Germany)

##HIERARCHICAL_SHIFT_FUNCTION##
source("hierarchical_shift_function.R")

dataSites <- MeasSites$dataSite_long
if(!exists("HSFData")){
  HSFData <- hierarchical_shift_function(dataSites)
} else {
  load("./res/HSF.RData")
}

##HUMAN DATASET##
data2 <- read.csv("3T_human_T1maps_database.csv")
data2[] <- gsub("[][]", "", as.matrix(data2))

submissionHuman <- 1:56
listHuman = list()
list2appendHuman = list()
for (i in submissionHuman){
  for (j in seq(1,4)){
    dataHuman = gsub("\\. ","",data2[i,j+grep("^T1...genu..WM.$", colnames(data2))-1])
    dataHuman = as.matrix(as.numeric(unlist(strsplit(dataHuman," "))))
    dataHuman = dataHuman[!is.na(dataHuman)]
    
    list2appendHuman[[j]] = dataHuman
  }
  listHuman[[i]] = list2appendHuman
}

##UNAM##
dataHumanAll <- c(1:56)
dataHumanMexico <- c(18:55)
dataHumanCanada <- c(8,56)
dataHumanUS <- c(1:7,9)
dataHumanItaly <- c(10:14)
dataHumanGermany <- c(16,17)
dataHumanAustralia <- 15

source("getHumanData.R")
sitesHuman <- getHumanData(dataHumanAll)
sitesHuman_Mexico <- getHumanData(dataHumanMexico)
sitesHuman_Canada <- getHumanData(dataHumanCanada)
sitesHuman_US <- getHumanData(dataHumanUS)
sitesHuman_Italy <- getHumanData(dataHumanItaly)
sitesHuman_Germany <- getHumanData(dataHumanGermany)
sitesHuman_Australia <- getHumanData(dataHumanAustralia)
listHumanData <- c(sitesHuman_Canada,sitesHuman_US,sitesHuman_Italy,sitesHuman_Germany,sitesHuman_Australia)
labelHumanSite <- c("CAN-MEX","US-MEX","ITA-MEX","GER-MEX","AUS-MEX")
labelHumanROI <- c("Genu WM","splenium WM","Deep GM","Cortical GM")
for (ii in seq(1,length(listHumanData))){
  cnt_roi = 1
  rois = 1:4
  for (jj in labelHumanROI){
    refMEX = mean(subset(sitesHuman_Mexico$dataLong_human, roi_long==jj)$siteData)
    curSite = mean(subset(listHumanData[ii]$dataLong_human, roi_long==jj)$siteData)
    dfmeanHuman2 = data.frame(labelHumanSite[ii],cnt_roi,jj,100*(curSite-refMEX)/refMEX)
    if (ii==1 && cnt_roi==1){
      dfmeanHuman = dfmeanHuman2
    }
    else{
      dfmeanHuman = rbind(dfmeanHuman,dfmeanHuman2)
    }
    cnt_roi = cnt_roi + 1
  }
}
colnames(dfmeanHuman) <- c('Site','roi_num','roi_lab','dif')

##COMPARISON OF NIST PHANTOM AND HUMAN DATASETS##
indNISTphantom <- c(3,4,5:10,25,26,28,31)
a = match(indNISTphantom,data2[,"id"])
indHUMANdata <- c(9,8,2:7,16,17,56,15)

#Sites who submitted both, NIST phantom and human data
dualSub_Sites_np = c('Biomedical Engineering, Case Western Reserve University - 2.001',
                  'McGill University Health Centre - Montreal General Hospital - 3.001',
                  'Keck Medical Center of University of Southern California GE 1 - 4.001',
                  'Keck Medical Center of University of Southern California GE 2 - 4.002',
                  'Keck Medical Center of University of Southern California GE 1 - 4.003',
                  'Keck Medical Center of University of Southern California GE 1 - 4.004',
                  'Keck Medical Center of University of Southern California GE 2 - 4.005',
                  'Keck Medical Center of University of Southern California GE 2 - 4.006',
                  'Philips Research Hamburg - 6.013',
                  'Philips Research Hamburg - 6.014',
                  'McGill University Health Centre - Glen Site - Cedars - 7.002',
                  'Liverpool Hospital Australia - 9.001')

dualSub_Sites_h = c('Biomedical Engineering, Case Western Reserve University - 4.001',
                     'McGill University Health Centre - Montreal General Hospital - 3.001',
                     'Keck Medical Center of University of Southern California GE 1 - 2.001',
                     'Keck Medical Center of University of Southern California GE 2 - 2.002',
                     'Keck Medical Center of University of Southern California GE 1 - 2.003',
                     'Keck Medical Center of University of Southern California GE 1 - 2.004',
                     'Keck Medical Center of University of Southern California GE 2 - 2.005',
                     'Keck Medical Center of University of Southern California GE 2 - 2.006',
                     'Philips Research Hamburg - 8.001',
                     'Philips Research Hamburg - 8.002',
                     'McGill University Health Centre - Glen Site - Cedars - 10.001',
                     'Liverpool Hospital Australia - 7.001')

source("comparison_NISTHuman_nist.R")

compNISTHuman_nist <- comparison_NISTHuman_nist(indNISTphantom,dualSub_Sites_np)
compNISTHuman_human <- comparison_NISThuman_human(indHUMANdata,dualSub_Sites_h)
compNISTHuman <- rbind(compNISTHuman_nist$data_NIST,compNISTHuman_human$data_human)

