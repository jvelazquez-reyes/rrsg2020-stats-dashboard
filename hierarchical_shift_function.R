hierarchical_shift_function <- function(dataSites){
  #e <- environment()
  listHSFDiff <- list()
  listHSFBootstrapDiff <- list()
  tempTitle = temperature_correction(20,42)
  for (sph in seq(14,1)){
    uniqueSites = unique(dataSites$ID_Site_long)
    listdat1 = list()
    listdat2 = list()
    lengthDAT = array()
    
    # analysis parameters
    np = length(uniqueSites) # Number of submissions of interest
    qseq <- seq(0.1,0.9,0.1) # quantiles
    alpha <- 0.05
    nboot <- 1000 # bootstrap
    tr <- 0.2 # group trimmed mean for each quantile
    nq <- length(qseq) #quantile length
    
    for (x in seq(1,length(uniqueSites))) {
      subdata = subset(dataSites, ID_Site_long == uniqueSites[x] & sph_long == sph)
      listdat1[[x]] = subdata$t1_long
      listdat2[[x]] = subdata$siteData
      lengthDAT[x] = nrow(subdata)
    }
    dat1 = stri_list2matrix(listdat1, byrow=TRUE, fill=NA)
    dat2 = stri_list2matrix(listdat2, byrow=TRUE, fill=NA)
    dat1 = `dim<-`(as.numeric(dat1), dim(dat1))
    dat2 = `dim<-`(as.numeric(dat2), dim(dat2))
    nt = min(lengthDAT)

    df <- tibble(rt = c(as.vector(dat1[,1:nt]), as.vector(dat2[,1:nt])),
                 cond = factor(c(rep("Reference", nt*np),rep("Measured", nt*np))),
                 id = factor(rep(unique(dataSites$ID_Site_long),nt*2)))
    
    out_hsf <- hsf(df, rt ~ cond + id)
    
    df1_plot <- tibble(difference = as.vector(t(out_hsf[["individual_sf"]])),
                  quantile = rep(out_hsf[["quantiles"]], each=np),
                  id = factor(rep(unique(dataSites$ID_Site_long),nq)))
    
    df1_plot.md <- tibble(difference = out_hsf[["group_differences"]],
                     quantile = qseq,
                     ymin = out_hsf[["hdi"]][seq(1,18,2)],
                     ymax = out_hsf[["hdi"]][seq(2,18,2)])
    
    hsf_colors <- setNames(viridis(length(uniqueSites)), unique(df1_plot$id))
    p <- plotly_plot <- plot_ly(df1_plot) %>%
      add_trace(df1_plot, x = ~quantile, y = ~difference, color = ~id, colors = hsf_colors,
                                type = 'scatter', mode = 'markers+lines', marker = list(size = 5),
                                hoverinfo = 'text',
                                text = ~paste('<br> Decile: ', quantile,
                                              '<br> Difference (ms): ', signif(difference,4),
                                              '<br> ID: ', id)) %>%
      layout(xaxis = list(title=list(text="Deciles", font=list(size=18)), tickfont=list(size=15), zeroline=F, showline=T, 
                          linewidth=2, linecolor="black", mirror=T, showgrid=F, range=list(0,1.15), tickvals=seq(0.1,0.9,0.1)),
             yaxis = list(title=list(text="Reference - Measured T1 (ms)", font=list(size=18)), tickfont=list(size=15), zeroline=F, showline=T, 
                          linewidth=2, linecolor="black", mirror=T, showgrid=F, range=list(-300,300)),
             legend = list(title=list(text="<b>Site ID</b>")),
             annotations = list(x=0.5, y=250, text=paste("Reference T1: ", signif(tempTitle[sph],5), " ms"),
                                showarrow = FALSE, font = list(size=20, color="black"))) %>%
      add_trace(x = c(0, 1), y = c(0, 1),
                  type = "scatter", mode = "lines", line = list(color = 'black', width = 2), showlegend = FALSE) %>%
      add_trace(data=df1_plot.md, x = ~quantile, y = ~difference,
                type = "scatter", mode = "markers+lines", marker = list(color = 'black', size = 10),
                line = list(color = 'black', width = 4), showlegend = FALSE)
    
    listHSFDiff[[sph]] <- p
    
    # Bootstrapped confidence intervals
    out_hsf_pb <- hsf_pb(df, rt ~ cond + id, nboot = nboot)
    
    df2_plot <- tibble(difference = as.vector(t(out_hsf_pb[["individual_sf"]])),
                       quantile = rep(out_hsf_pb[["quantiles"]], each=np),
                       id = factor(rep(unique(dataSites$ID_Site_long),nq)))
    
    df2_plot.md <- tibble(difference = out_hsf_pb[["group_differences"]],
                          quantile = qseq,
                          ymin = out_hsf_pb[["hdi"]][seq(1,18,2)],
                          ymax = out_hsf_pb[["hdi"]][seq(2,18,2)])
    
    p <- plotly_plot <- plot_ly(df2_plot) %>%
      add_trace(df2_plot, x = ~quantile, y = ~difference, color = ~id, colors = hsf_colors,
                type = 'scatter', mode = 'markers+lines', marker = list(size = 5),
                hoverinfo = 'text',
                text = ~paste('<br> Decile: ', quantile,
                              '<br> Difference (ms): ', signif(difference,4),
                              '<br> ID: ', id)) %>%
      layout(xaxis = list(title=list(text="Deciles", font=list(size=18)), tickfont=list(size=15), zeroline=F, showline=T, 
                          linewidth=2, linecolor="black", mirror=T, showgrid=F, range=list(0,1.3), tickvals=seq(0.1,0.9,0.1)),
             yaxis = list(title=list(text="Reference - Measured T1 (ms)", font=list(size=18)), tickfont=list(size=15), zeroline=F, showline=T, 
                          linewidth=2, linecolor="black", mirror=T, showgrid=F, range=list(-300,300)),
             legend = list(title=list(text="<b>Site ID</b>", font = list(size = 10)), 
                           font = list(size = 9), x=0.75, y=0.99, bgcolor = "rgba(0,0,0,0)"),
             annotations = list(x=0.5, y=250, text=paste("Reference T1: ", signif(tempTitle[sph],5), " ms"),
                                showarrow = FALSE, font = list(size=20, color="black"))) %>%
      add_trace(x = c(0, 1), y = c(0, 1),
                type = "scatter", mode = "lines", line = list(color = 'black', width = 2), showlegend = FALSE) %>%
      add_trace(data=df2_plot.md, x = ~quantile, y = ~difference,
                error_y = ~list(symmetric = FALSE, color = 'black', array = ymax - difference,
                                arrayminus = difference - ymin),
                type = "scatter", mode = "markers+lines", marker = list(color = 'black', size = 10),
                line = list(color = 'black', width = 4), showlegend = FALSE)
    
    listHSFBootstrapDiff[[sph]] <- p
  }
  
  returnHSF <- list("diffDeciles" = listHSFDiff,
                    "diffBootstrapDiff" = listHSFBootstrapDiff,
                    "a"=df1_plot,
                    "b"=df2_plot)
  
  return(returnHSF)
}