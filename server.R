library("irr")
library("sjPlot")
library("plotly")
library("shiny")
library("shinydashboard")

source("allStats.R")

# Define server logic required to build shiny app
function(input, output) {
  ###MENU HOME###
  output$overviewTxt1 <- renderUI({
    a(HTML("<font size=6>About the Challenge</font></b>"), href=paste("https://blog.ismrm.org/2019/12/12/reproducibility-challenge-2020-join-the-reproducible-research-and-quantitative-mr-study-groups-in-their-efforts-to-standardize-t1-mapping/"), target="_blank")
  })
  
  output$overviewTxt2 <- renderUI({
    a(HTML("<font size=6>GitHub Organization</font></b>"), href=paste("https://github.com/rrsg2020/"), target="_blank")
  })
  
  output$overviewTxt3 <- renderUI({
    a(HTML("<font size=6>Dataset OSF</font></b>"), href=paste("https://osf.io/ywc9g/"), target="_blank")
  })
  
  output$overviewTxt4 <- renderUI({
    a(HTML("<font size=6>General Dashboard</font></b>"), href=paste("http://rrsg2020.herokuapp.com/"), target="_blank")
  })
  
  ###MAGNITUDE VS COMPLEX ANALYSIS###
  MagCom_colors <- setNames(viridis(length(cases)), unique(magVScomp$dataMagComp$sid))
  output$MagComp <- renderPlotly({
    if (input$typeComparison == "Difference"){
      plot_ly(magVScomp$dataMagComp, x = ~refT1, y = ~abs(diff), split = ~as.factor(sid), color = ~as.factor(sid), colors = MagCom_colors) %>%
        filter(sid %in% input$DiffSitesID) %>%
        add_trace(type = 'scatter', mode = 'lines+markers',
                  hoverinfo = 'text',
                  text = ~paste('<br> Site: ', sid,
                                '<br> Difference (ms): ', signif(abs(diff),3),
                                '<br> Reference T1 (ms): ', signif(refT1,5))) %>%
        layout(xaxis = list(title=list(text="Reference T1 value (ms)", font=list(size=18)), tickfont=list(size=15),
                            zeroline=F, showline=T, linewidth=2, linecolor="black", mirror=T, showgrid=F),
               yaxis = list(title=list(text="Difference (ms)", font=list(size=18)), tickfont=list(size=15),
                            zeroline=F, showline=T, linewidth=2, linecolor="black", mirror=T, showgrid=F,
                            range=list(0,unname(apply(abs(magVScomp$dataMagComp),2,max))[4]+5)),
               legend = list(title=list(text="<b>Site ID</b>"), x=0.8, y=0.95, bgcolor = "rgba(0,0,0,0)"))
    }
    else if (input$typeComparison == "Difference (%)"){
      plot_ly(magVScomp$dataMagComp, x = ~refT1, y = ~abs(percDiff), split = ~as.factor(sid), color = ~as.factor(sid), colors = MagCom_colors) %>%
        filter(sid %in% input$DiffSitesID) %>%
        add_trace(type = 'scatter', mode = 'lines+markers',
                  hoverinfo = 'text',
                  text = ~paste('<br> Site: ', sid,
                                '<br> Difference (%): ', signif(abs(percDiff),4),
                                '<br> Reference T1 (ms): ', signif(refT1,5))) %>%
        layout(xaxis = list(title=list(text="Reference T1 value (ms)", font=list(size=18)), tickfont=list(size=15), 
                            zeroline=F, showline=T, linewidth=2, linecolor="black", mirror=T, showgrid=F),
               yaxis = list(title=list(text="Percentage difference (%)", font=list(size=18)), tickfont=list(size=15),
                            zeroline=F, showline=T, linewidth=2, linecolor="black", mirror=T, showgrid=F,
                            range=list(0,unname(apply(abs(magVScomp$dataMagComp),2,max))[5]+5)),
               legend = list(title=list(text="<b>Site ID</b>"), x=0.8, y=0.95, bgcolor = "rgba(0,0,0,0)"))
    }
  })
  
  output$BAMagComp <- renderPlotly({
    if (input$typeComparison == "Difference (%)"){
      plot_ly(magVScomp$dataMagComp) %>%
        add_trace(magVScomp$dataMagComp, x = ~average, y = ~percDiff, color = ~as.factor(sid), 
                  colors = MagCom_colors, type = 'scatter', mode = 'markers', marker = list(size = 8),
                  hoverinfo = 'text',
                  text = ~paste('<br> Difference (%): ', signif(percDiff,4),
                                '<br> Average T1 (ms): ', signif(average,5),
                                '<BR> Reference T1 (ms): ', signif(refT1,5),
                                '<br> ID: ', sid)) %>%
        layout(xaxis = list(title=list(text="Average T1 (ms)", font=list(size=18)), tickfont=list(size=15), zeroline=F, 
                            showline=T, linewidth=2, linecolor="black", mirror=T, showgrid=F,
                            range=list(0,unname(apply(magVScomp$dataMagComp,2,max))[8]+600)),
               yaxis = list(title=list(text="Percentage difference (%)", font=list(size=18)), tickfont=list(size=15), zeroline=F,
                            showline=T, linewidth=2, linecolor="black", mirror=T, showgrid=F,
                            range=list(unname(apply(magVScomp$dataMagComp,2,min))[5]-20,unname(apply(magVScomp$dataMagComp,2,max))[5]+20)),
               legend = list(title=list(text="<b>Site ID</b>"), x=0.85, y=0.95, bgcolor = "rgba(0,0,0,0)")) %>%
        add_trace(x = c(0,unname(apply(magVScomp$dataMagComp,2,max))[8]+600), y = mean(magVScomp$dataMagComp$percDiff),
                  type='scatter', mode = "lines", line = list(dash = "solid", width = 2, color = "black"),
                  showlegend = FALSE, hoverinfo = "none") %>%
        add_trace(x = c(0,unname(apply(magVScomp$dataMagComp,2,max))[8]+600),
                  y = mean(magVScomp$dataMagComp$percDiff) + 1.96*sd(magVScomp$dataMagComp$percDiff),
                  type='scatter', mode = "lines", line = list(dash = "dash", color = "firebrick"),
                  showlegend = FALSE, hoverinfo = "none") %>%
        add_trace(x = c(0,unname(apply(magVScomp$dataMagComp,2,max))[8]+600),
                  y = mean(magVScomp$dataMagComp$percDiff) - 1.96*sd(magVScomp$dataMagComp$percDiff),
                  type='scatter', mode = "lines", line = list(dash = "dash", color = "firebrick"),
                  showlegend = FALSE, hoverinfo = "none") %>%
        layout(annotations = list(x=1400, y=mean(magVScomp$dataMagComp$percDiff)-10,
                                  text=paste("Mean = ",signif(mean(magVScomp$dataMagComp$percDiff),3)),
                                  showarrow = FALSE, font = list(size=12, color="black"))) %>%
        layout(annotations = list(x=1400, y=mean(magVScomp$dataMagComp$percDiff) + 1.96*sd(magVScomp$dataMagComp$percDiff) + 10,
                                  text=paste("Mean+1.96SD = ",signif(mean(magVScomp$dataMagComp$percDiff)+1.96*sd(magVScomp$dataMagComp$percDiff),3)),
                                  showarrow = FALSE, font = list(size=12, color="firebrick"))) %>%
        layout(annotations = list(x=1400, y=mean(magVScomp$dataMagComp$percDiff) - 1.96*sd(magVScomp$dataMagComp$percDiff) - 10,
                                  text=paste("Mean-1.96SD = ",signif(mean(magVScomp$dataMagComp$percDiff)-1.96*sd(magVScomp$dataMagComp$percDiff),3)),
                                  showarrow = FALSE, font = list(size=12, color="firebrick")))
    }
    else if (input$typeComparison == "Difference"){
      plot_ly(magVScomp$dataMagComp) %>%
        add_trace(magVScomp$dataMagComp, x = ~average, y = ~diff, color = ~as.factor(sid), 
                  colors = MagCom_colors, type = 'scatter', mode = 'markers', marker = list(size = 8),
                  hoverinfo = 'text',
                  text = ~paste('<br> Difference (ms): ', signif(diff,4),
                                '<br> Average T1 (ms): ', signif(average,5),
                                '<BR> Reference T1 (ms): ', signif(refT1,5),
                                '<br> ID: ', sid)) %>%
        layout(xaxis = list(title=list(text="Average T1 (ms)", font=list(size=18)), tickfont=list(size=15), zeroline=F, 
                            showline=T, linewidth=2, linecolor="black", mirror=T, showgrid=F,
                            range=list(0,unname(apply(magVScomp$dataMagComp,2,max))[8]+600)),
               yaxis = list(title=list(text="Difference (ms)", font=list(size=18)), tickfont=list(size=15),
                            zeroline=F, showline=T, linewidth=2, linecolor="black", mirror=T, showgrid=F,
                            range=list(unname(apply(magVScomp$dataMagComp,2,min))[4]-20,unname(apply(magVScomp$dataMagComp,2,max))[4]+20)),
               legend = list(title=list(text="<b>Site ID</b>"), x=0.85, y=0.95, bgcolor = "rgba(0,0,0,0)")) %>%
        add_trace(x = c(0,unname(apply(magVScomp$dataMagComp,2,max))[8]+600), y = mean(magVScomp$dataMagComp$diff),
                  type='scatter', mode = "lines", line = list(dash = "solid", width = 2, color = "black"),
                  showlegend = FALSE, hoverinfo = "none") %>%
        add_trace(x = c(0,unname(apply(magVScomp$dataMagComp,2,max))[8]+600),
                  y = mean(magVScomp$dataMagComp$diff) + 1.96*sd(magVScomp$dataMagComp$diff),
                  type='scatter', mode = "lines", line = list(dash = "dash", color = "firebrick"),
                  showlegend = FALSE, hoverinfo = "none") %>%
        add_trace(x = c(0,unname(apply(magVScomp$dataMagComp,2,max))[8]+600),
                  y = mean(magVScomp$dataMagComp$diff) - 1.96*sd(magVScomp$dataMagComp$diff),
                  type='scatter', mode = "lines", line = list(dash = "dash", color = "firebrick"),
                  showlegend = FALSE, hoverinfo = "none") %>%
        layout(annotations = list(x=1400, y=mean(magVScomp$dataMagComp$diff)-10,
                                  text=paste("Mean = ",signif(mean(magVScomp$dataMagComp$diff),3)),
                                  showarrow = FALSE, font = list(size=12, color="black"))) %>%
        layout(annotations = list(x=1400, y=mean(magVScomp$dataMagComp$diff) + 1.96*sd(magVScomp$dataMagComp$diff) + 10,
                                  text=paste("Mean+1.96SD = ",signif(mean(magVScomp$dataMagComp$diff)+1.96*sd(magVScomp$dataMagComp$diff),3)),
                                  showarrow = FALSE, font = list(size=12, color="firebrick"))) %>%
        layout(annotations = list(x=1400, y=mean(magVScomp$dataMagComp$diff) - 1.96*sd(magVScomp$dataMagComp$diff) - 10,
                                  text=paste("Mean-1.96SD = ",signif(mean(magVScomp$dataMagComp$diff)-1.96*sd(magVScomp$dataMagComp$diff),3)),
                                  showarrow = FALSE, font = list(size=12, color="firebrick")))
    }
  })
  
  sphere_colors <- setNames(viridis(14), unique(signif(magVScomp$dataCorr$refT1,5)))
  output$CorrMagComp <- renderPlotly({
    plotly_plot <- plot_ly(magVScomp$dataCorr) %>%
      filter(sid %in% input$CorrSitesID) %>%
      add_trace(magVScomp$dataCorr, x = ~Complex, y = ~Magnitude, color = ~as.factor(signif(refT1,5)), 
                colors = sphere_colors, type = 'scatter', mode = 'markers', marker = list(size = 8),
                hoverinfo = 'text',
                text = ~paste('<br> Complex (ms): ', signif(Complex,5),
                              '<br> Magnitude (ms): ', signif(Magnitude,5),
                              '<br> Reference T1 (ms): ', signif(refT1,5),
                              '<br> ID: ', sid)) %>%
      layout(xaxis = list(title=list(text="Complex T1 value (ms)", font=list(size=18)), tickfont=list(size=15),
                          zeroline=F, showline=T, linewidth=2, linecolor="black", mirror=T, showgrid=F,
                          range=list(0,unname(apply(magVScomp$dataCorr,2,max))[5]+100)),
             yaxis = list(title=list(text="Magnitude T1 value (ms)", font=list(size=18)), tickfont=list(size=15),
                          zeroline=F, showline=T, linewidth=2, linecolor="black", mirror=T, showgrid=F,
                          range=list(0,unname(apply(magVScomp$dataCorr,2,max))[4]+100)),
             legend = list(title=list(text="<b>T1 (ms)</b>", font=list(size = 10)),
                           font = list(size = 9), yanchor="top", xanchor="left", x=0.01, y=0.99, bgcolor = "rgba(0,0,0,0)")) %>%
      add_trace(x = c(0, unname(apply(magVScomp$dataCorr,2,max))[5]+100), y = c(0, unname(apply(magVScomp$dataCorr,2,max))[4]+100),
                type = "scatter", mode = "lines", line = list(color = 'black', width = 2), showlegend = FALSE)
  })
  
  output$PearsonCorr <- renderTable(magVScomp$PearsonCorr)
  
  output$DispAllPointsMagComp_oneSite <- renderPlotly({
    plot_ly(magVScomp$PearsonCorrSphere) %>%
      filter(sid_long %in% input$CorrSitesIDallDP) %>%
      add_trace(magVScomp$PearsonCorrSphere, x = ~compData, y = ~magData, color = ~as.factor(signif(refT1_long,5)), 
                colors = sphere_colors, type = 'scatter', mode = 'markers', marker = list(size = 8),
                hoverinfo = 'text',
                text = ~paste('<br> Complex (ms): ', signif(compData,5),
                              '<br> Magnitude (ms): ', signif(magData,5),
                              '<br> Reference T1 (ms): ', signif(refT1_long,5),
                              '<br> ID: ', sid_long)) %>%
      layout(xaxis = list(title=list(text="Complex T1 value (ms)", font=list(size=18)), tickfont=list(size=15), zeroline=F, 
                          showline=T, linewidth=2, linecolor="black", mirror=T, showgrid=F,
                          range=list(0,unname(apply(magVScomp$PearsonCorrSphere,2,max))[5]+100)),
             yaxis = list(title=list(text="Magnitude T1 value (ms)", font=list(size=18)), tickfont=list(size=15),
                          zeroline=F, showline=T, linewidth=2, linecolor="black", mirror=T, showgrid=F,
                          range=list(0,unname(apply(magVScomp$PearsonCorrSphere,2,max))[4]+100)),
             legend = list(title=list(text="<b>T1 (ms)</b>", font=list(size = 10)),
                           font = list(size = 9), yanchor="top", xanchor="left", x=0.01, y=0.99, bgcolor = "rgba(0,0,0,0)")) %>%
      add_trace(x = c(0, unname(apply(magVScomp$PearsonCorrSphere,2,max))[5]+100), y = c(0, unname(apply(magVScomp$PearsonCorrSphere,2,max))[4]+100),
                type = "scatter", mode = "lines", line = list(color = 'black', width = 2), showlegend = FALSE)
  })
  
  toListen <- reactive({input$CorrSitesIDallDP})
  observeEvent(toListen(), {
    dataMagCompSphere = comparison_magnitude_complex(cases)
    spheres = 1:14
    refT1 = temperature_correction(20,42)
    corr_per_sphere = reactiveValues(df = data.frame(Sphere=as.integer(), ReferenceT1=as.numeric(), Pearson=as.numeric(), Lin=as.numeric()))
    
    for (ii in seq(1,length(spheres))){
      data_per_sphere = subset(dataMagCompSphere$PearsonCorrSphere, sid_long == input$CorrSitesIDallDP & sph_long == spheres[ii])
      corr_per_sphere$df[ii,1] = spheres[ii]
      corr_per_sphere$df[ii,2] = signif(refT1[ii],5)
      corr_per_sphere$df[ii,3] = cor(data_per_sphere$magData,data_per_sphere$compData)
      corr_per_sphere$df[ii,4] = CCC(data_per_sphere$magData,data_per_sphere$compData)[[1]][1]
    }
    
    output$PearsonAllPointsMagComp_oneSite <- renderTable({corr_per_sphere$df})
  })
  
  output$DispAllPointsMagComp_allSites <- renderPlotly({
    plot_ly(magVScomp$PearsonCorrSphere) %>%
      add_trace(magVScomp$PearsonCorrSphere, x = ~compData, y = ~magData, color = ~as.factor(signif(refT1_long,5)), 
                colors = sphere_colors, type = 'scatter', mode = 'markers', marker = list(size = 8),
                hoverinfo = 'text',
                text = ~paste('<br> Complex (ms): ', signif(compData,5),
                              '<br> Magnitude (ms): ', signif(magData,5),
                              '<br> Reference T1 (ms): ', signif(refT1_long,5),
                              '<br> ID: ', sid_long)) %>%
      layout(xaxis = list(title=list(text="Complex T1 value (ms)", font=list(size=18)), tickfont=list(size=15), 
                          zeroline=F, showline=T, linewidth=2, linecolor="black", mirror=T, showgrid=F,
                          range=list(0,unname(apply(magVScomp$PearsonCorrSphere,2,max))[5]+100)),
             yaxis = list(title=list(text="Magnitude T1 value (ms)", font=list(size=18)), tickfont=list(size=15),
                          zeroline=F, showline=T, linewidth=2, linecolor="black", mirror=T, showgrid=F,
                          range=list(0,unname(apply(magVScomp$PearsonCorrSphere,2,max))[4]+100)),
             legend = list(title=list(text="<b>T1 (ms)</b>", font=list(size = 10)),
                           font = list(size = 9), yanchor="bottom", xanchor="right", x=0.99, y=0.01, bgcolor = "rgba(0,0,0,0)")) %>%
      add_trace(x = c(0, unname(apply(magVScomp$PearsonCorrSphere,2,max))[5]+100), y = c(0, unname(apply(magVScomp$PearsonCorrSphere,2,max))[4]+100),
                type = "scatter", mode = "lines", line = list(color = 'black', width = 2), showlegend = FALSE)
  })
  
  spheres = 1:14
  refT1 = temperature_correction(20,42)
  corr_per_sphere = data.frame(Sphere=as.integer(), ReferenceT1=as.numeric(), Pearson=as.numeric(), Lin=as.numeric())
  
  for (ii in seq(1,length(spheres))){
    data_per_sphere = subset(magVScomp$PearsonCorrSphere, sph_long == spheres[ii])
    corr_per_sphere[ii,1] = spheres[ii]
    corr_per_sphere[ii,2] = signif(refT1[ii],5)
    corr_per_sphere[ii,3] = cor(data_per_sphere$magData,data_per_sphere$compData)
    corr_per_sphere[ii,4] = CCC(data_per_sphere$magData,data_per_sphere$compData)[[1]][1]
  }
  
  output$PearsonAllPointsMagComp_allSites <- renderTable({corr_per_sphere})
  
  ###NIST PHANTOM (MEASURED VS REFRENCE T1 ANALYSIS)###
  sitesFiltered_colors <- setNames(viridis(length(filteredSites)), unique(MeasSites$dataSite$ID_Site))
  output$CompFiltSites <- renderPlotly({
    plot_ly(MeasSites$dataSite, x = ~refT1, y = ~Mean, split = ~ID_Site, color = ~ID_Site, colors = sitesFiltered_colors,
            error_y = ~list(array = Std, color = '#000000')) %>%
      filter(sid %in% input$FiltSitesID) %>%
      add_trace(type = 'scatter', mode = 'lines+markers',
                hoverinfo = 'text',
                text = ~paste('<br> Site: ', sid,
                              '<br> Measured T1: ', signif(Mean,6),
                              '<br> Reference T1: ', signif(refT1,6),
                              '<br> Sphere: ', Sphere)) %>%
      layout(xaxis = list(title=list(text="Reference T1 value (ms)",font=list(size=18)), tickfont=list(size=15),
                          zeroline=F, showline=T, linewidth=2, linecolor="black", mirror=T, showgrid=F,
                          range=list(0,as.numeric(unname(apply(MeasSites$dataSite,2,max))[4])+100)),
             yaxis = list(title=list(text="Mean T1 value (ms)", font=list(size=18)), tickfont=list(size=15),
                          zeroline=F, showline=T, linewidth=2, linecolor="black", mirror=T, showgrid=F,
                          range=list(0,as.numeric(unname(apply(MeasSites$dataSite,2,max))[5])+100)),
             legend = list(title=list(text="<b>Site ID</b>", font=list(size = 10)),
                           font = list(size = 9), xanchor="left", yanchor="top", x=0.01, y=0.8, bgcolor = "rgba(0,0,0,0)"))
  })
  
  output$sdFilteredSites <- renderPlotly({
    sdFiltered_colors <- setNames(viridis(length(filteredSites)), unique(sdFilteredSites$stdData$ID_Site))
    plot_ly(sdFilteredSites$stdData, x = ~reference, y = ~stdValues/reference, split = ~ID_Site, color = ~ID_Site, colors = sdFiltered_colors) %>%
      filter(sid %in% input$FiltSitesID) %>%
      add_trace(type = 'scatter', mode = 'lines+markers',
                hoverinfo = 'text',
                text = ~paste('<br> SD/Reference T1: ', signif(stdValues/reference,3),
                              '<br> SD: ', signif(stdValues,3),
                              '<br> Reference T1: ', signif(reference,5),
                              '<br> Site: ', ID_Site)) %>%
      layout(xaxis = list(title=list(text="Reference T1 value (ms)", font=list(size=18)), tickfont=list(size=15), 
                          zeroline=F, showline=T, linewidth=2, linecolor="black", mirror=T, showgrid=F),
             yaxis = list(title=list(text="SD/Reference T1", font=list(size=18)), tickfont=list(size=15),
                          zeroline=F, showline=T, linewidth=2, linecolor="black", mirror=T, showgrid=F, range=list(0,0.2)),
             legend = list(title=list(text="<b>Site ID</b>", font=list(size = 10)),
                           font = list(size = 9), xanchor="right", yanchor="top", x=0.99, y=0.8, bgcolor = "rgba(0,0,0,0)"))
  })
  
  output$DispAllPoints <- renderPlotly({
    if (input$DispAllSite == "Montreal"){
      DispersionAllPoints = SiteMontreal
    }
    else if (input$DispAllSite == "Germany"){
      DispersionAllPoints = SiteGermany
    }
    else if (input$DispAllSite == "US"){
      DispersionAllPoints = SiteUS
    }
    else if (input$DispAllSite == "London"){
      DispersionAllPoints = SiteLondon
    }
    else if (input$DispAllSite == "Australia"){
      DispersionAllPoints = SiteAustralia
    }
    
    specSite_colors <- setNames(viridis(length(unique(DispersionAllPoints$dataSite_long$ID_Site_long))), unique(DispersionAllPoints$dataSite_long$ID_Site_long))
    plot_ly(DispersionAllPoints$dataSite_long) %>%
      add_trace(DispersionAllPoints$dataSite_long, x = ~t1_long, y = ~siteData, color = ~ID_Site_long,
                colors = specSite_colors, type = 'scatter', mode = 'markers', marker = list(size = 8),
                hoverinfo = 'text',
                text = ~paste('<br> Measured T1 value (ms): ', signif(siteData,5),
                              '<br> Reference T1 value (ms): ', signif(t1_long,5),
                              '<br> ID: ', sid_long)) %>%
      layout(xaxis = list(title=list(text="Reference T1 value (ms)", font=list(size=18)), tickfont=list(size=15), 
                          zeroline=F, showline=T, linewidth=2, linecolor="black", mirror=T, showgrid=F,
                          range=list(0,as.numeric(unname(apply(DispersionAllPoints$dataSite_long,2,max))[4])+100)),
             yaxis = list(title=list(text="Measured T1 value (ms)", font=list(size=18)), tickfont=list(size=15),
                          zeroline=F, showline=T, linewidth=2, linecolor="black", mirror=T, showgrid=F,
                          range=list(0,as.numeric(unname(apply(DispersionAllPoints$dataSite_long,2,max))[5])+100)),
             legend = list(title=list(text="<b>Site ID</b>", font=list(size = 10)),
                           font = list(size = 9), x=0.75, y=0.01, bgcolor = "rgba(0,0,0,0)")) %>%
      add_trace(x = c(0, as.numeric(unname(apply(DispersionAllPoints$dataSite_long,2,max))[4])+100),
                y = c(0, as.numeric(unname(apply(DispersionAllPoints$dataSite_long,2,max))[5])+100),
                type = "scatter", mode = "lines", line = list(color = 'black', width = 2), showlegend = FALSE)
  })
  
  output$Disp4Site <- renderPlotly({
    if (input$selectCompSite == "Montreal"){
      RefVSMeas = measuredT1_against_referenceT1(scans = Montreal)
    }
    else if (input$selectCompSite == "Germany"){
      RefVSMeas = measuredT1_against_referenceT1(scans = Germany)
    }
    else if (input$selectCompSite == "US"){
      RefVSMeas = measuredT1_against_referenceT1(scans = US)
    }
    else if (input$selectCompSite == "London"){
      RefVSMeas = measuredT1_against_referenceT1(scans = London)
    }
    else if (input$selectCompSite == "Australia"){
      RefVSMeas = measuredT1_against_referenceT1(scans = Australia)
    }
    
    sitesFiltBA_colors <- setNames(viridis(length(unique(RefVSMeas$BAData$ID_Site))), unique(RefVSMeas$BAData$ID_Site))
    plot_ly(RefVSMeas$BAData) %>%
      add_trace(RefVSMeas$BAData, x = ~reference, y = ~measValue, color = ~ID_Site, 
                colors = sitesFiltBA_colors, type = 'scatter', mode = 'markers', marker = list(size = 8),
                hoverinfo = 'text',
                text = ~paste('<br> Measured T1 value (ms): ', signif(measValue,5),
                              '<br> Reference T1 value (ms): ', signif(reference,5),
                              '<br> Sphere: ', sph)) %>%
      layout(xaxis = list(title=list(text="Reference T1 value (ms)", font=list(size=18)), tickfont=list(size=15), 
                          zeroline=F, showline=T, linewidth=2, linecolor="black", mirror=T, showgrid=F,
                          range=list(0,as.numeric(unname(apply(RefVSMeas$BAData,2,max))[6])+100)),
             yaxis = list(title=list(text="Measured T1 value (ms)", font=list(size=18)), tickfont=list(size=15),
                          zeroline=F, showline=T, linewidth=2, linecolor="black", mirror=T, showgrid=F,
                          range=list(0,as.numeric(unname(apply(RefVSMeas$BAData,2,max))[5])+100)),
             legend = list(title=list(text="<b>Site ID</b>", font=list(size = 10)),
                           font = list(size = 9), x=0.75, y=0.01, bgcolor = "rgba(0,0,0,0)")) %>%
      add_trace(x = c(0,as.numeric(unname(apply(RefVSMeas$BAData,2,max))[6])+100), y = c(0,as.numeric(unname(apply(RefVSMeas$BAData,2,max))[5])+100),
                type = "scatter", mode = "lines", line = list(color = 'black', width = 2), showlegend = FALSE)
  })
  
  output$BA4Site <- renderPlotly({
    if (input$selectCompSite == "Montreal"){
      RefVSMeas = measuredT1_against_referenceT1(scans = Montreal)
    }
    else if (input$selectCompSite == "Germany"){
      RefVSMeas = measuredT1_against_referenceT1(scans = Germany)
    }
    else if (input$selectCompSite == "US"){
      RefVSMeas = measuredT1_against_referenceT1(scans = US)
    }
    else if (input$selectCompSite == "London"){
      RefVSMeas = measuredT1_against_referenceT1(scans = London)
    }
    else if (input$selectCompSite == "Australia"){
      RefVSMeas = measuredT1_against_referenceT1(scans = Australia)
    }
    
    sitesFiltBA_colors <- setNames(viridis(length(unique(RefVSMeas$BAData$ID_Site))), unique(RefVSMeas$BAData$ID_Site))
    plot_ly(RefVSMeas$BAData) %>%
      add_trace(RefVSMeas$BAData, x = ~average, y = ~perc_difference, color = ~ID_Site, 
                colors = sitesFiltBA_colors, type = 'scatter', mode = 'markers', marker = list(size = 8),
                hoverinfo = 'text',
                text = ~paste('<br> Difference (%): ', signif(perc_difference,4),
                              '<br> Average T1: ', signif(average,5),
                              '<BR> Reference T1: ', signif(reference,5),
                              '<br> Sphere: ', sph)) %>%
      layout(xaxis = list(title=list(text="Average T1 (ms)", font=list(size=18)), tickfont=list(size=15), 
                          zeroline=F, showline=T, linewidth=2, linecolor="black", mirror=T, showgrid=F,
                          range=list(0,as.numeric(unname(apply(RefVSMeas$BAData,2,max))[9])+800)),
             yaxis = list(title=list(text="Percentage difference (%)", font=list(size=18)), tickfont=list(size=15),
                          zeroline=F, showline=T, linewidth=2, linecolor="black", mirror=T, showgrid=F,
                          range=list(-40,40)),
             legend = list(title=list(text="<b>Site ID</b>"), x=0.75, y=0.95, bgcolor = "rgba(0,0,0,0)")) %>%
      add_trace(x = c(0,as.numeric(unname(apply(RefVSMeas$BAData,2,max))[9])+800), y = mean(RefVSMeas$BAData$perc_difference),
                type='scatter', mode = "lines", line = list(dash = "solid", width = 2, color = "black"),
                showlegend = FALSE, hoverinfo = "none") %>%
      add_trace(x = c(0,as.numeric(unname(apply(RefVSMeas$BAData,2,max))[9])+800),
                y = mean(RefVSMeas$BAData$perc_difference) + 1.96*sd(RefVSMeas$BAData$perc_difference),
                type='scatter', mode = "lines", line = list(dash = "dash", color = "firebrick"),
                showlegend = FALSE, hoverinfo = "none") %>%
      add_trace(x = c(0,as.numeric(unname(apply(RefVSMeas$BAData,2,max))[9])+800),
                y = mean(RefVSMeas$BAData$perc_difference) - 1.96*sd(RefVSMeas$BAData$perc_difference),
                type='scatter', mode = "lines", line = list(dash = "dash", color = "firebrick"),
                showlegend = FALSE, hoverinfo = "none") %>%
      layout(annotations = list(x=1500, y=mean(RefVSMeas$BAData$perc_difference)-3,
                                text=paste("Mean = ",signif(mean(RefVSMeas$BAData$perc_difference),3)),
                                showarrow = FALSE, font = list(size=12, color="black"))) %>%
      layout(annotations = list(x=1500, y=mean(RefVSMeas$BAData$perc_difference) + 1.96*sd(RefVSMeas$BAData$perc_difference) + 5,
                                text=paste("Mean+1.96SD = ",signif(mean(RefVSMeas$BAData$perc_difference)+1.96*sd(RefVSMeas$BAData$perc_difference),3)),
                                showarrow = FALSE, font = list(size=12, color="firebrick"))) %>%
      layout(annotations = list(x=1500, y=mean(RefVSMeas$BAData$perc_difference) - 1.96*sd(RefVSMeas$BAData$perc_difference) - 5,
                                text=paste("Mean-1.96SD = ",signif(mean(RefVSMeas$BAData$perc_difference)-1.96*sd(RefVSMeas$BAData$perc_difference),3)),
                                showarrow = FALSE, font = list(size=12, color="firebrick")))
  })
  
  output$Disp4Vendor <- renderPlotly({
    if (input$selectCompVendor == "Siemens"){
      RefVSMeas = measuredT1_against_referenceT1(scans = Siemens)
    }
    else if (input$selectCompVendor == "GE"){
      RefVSMeas = measuredT1_against_referenceT1(scans = GE)
    }
    else if (input$selectCompVendor == "Philips"){
      RefVSMeas = measuredT1_against_referenceT1(scans = Philips)
    }
    
    vendorFiltBA_colors <- setNames(viridis(length(unique(RefVSMeas$BAData$ID_Vendor))), unique(RefVSMeas$BAData$ID_Vendor))
    plot_ly(RefVSMeas$BAData) %>%
      add_trace(RefVSMeas$BAData, x = ~reference, y = ~measValue, color = ~ID_Vendor, 
                colors = vendorFiltBA_colors, type = 'scatter', mode = 'markers', marker = list(size = 8),
                hoverinfo = 'text',
                text = ~paste('<br> Measured T1 value (ms): ', signif(measValue,5),
                              '<br> Reference T1 value (ms): ', signif(reference,5),
                              '<br> Sphere: ', sph)) %>%
      layout(xaxis = list(title=list(text="Reference T1 value (ms)", font=list(size=18)), tickfont=list(size=15),
                          zeroline=F, showline=T, linewidth=2, linecolor="black", mirror=T, showgrid=F,
                          range=list(0,as.numeric(unname(apply(RefVSMeas$BAData,2,max))[6])+100)),
             yaxis = list(title=list(text="Measured T1 value (ms)", font=list(size=18)), tickfont=list(size=15),
                          zeroline=F, showline=T, linewidth=2, linecolor="black", mirror=T, showgrid=F,
                          range=list(0,as.numeric(unname(apply(RefVSMeas$BAData,2,max))[5])+100)),
             legend = list(title=list(text="<b>MRI vendor</b>", font=list(size = 10)),
                           font = list(size = 9), x=0.67, y=0.01, bgcolor = "rgba(0,0,0,0)")) %>%
      add_trace(x = c(0,as.numeric(unname(apply(RefVSMeas$BAData,2,max))[6])+100), y = c(0,as.numeric(unname(apply(RefVSMeas$BAData,2,max))[5])+100),
                type = "scatter", mode = "lines", line = list(color = 'black', width = 2), showlegend = FALSE)
  })
  
  output$BA4Vendor <- renderPlotly({
    if (input$selectCompVendor == "Siemens"){
      RefVSMeas = measuredT1_against_referenceT1(scans = Siemens)
    }
    else if (input$selectCompVendor == "GE"){
      RefVSMeas = measuredT1_against_referenceT1(scans = GE)
    }
    else if (input$selectCompVendor == "Philips"){
      RefVSMeas = measuredT1_against_referenceT1(scans = Philips)
    }
    
    vendorFiltBA_colors <- setNames(viridis(length(unique(RefVSMeas$BAData$ID_Vendor))), unique(RefVSMeas$BAData$ID_Vendor))
    plot_ly(RefVSMeas$BAData) %>%
      add_trace(RefVSMeas$BAData, x = ~average, y = ~perc_difference, color = ~ID_Vendor, 
                colors = vendorFiltBA_colors, type = 'scatter', mode = 'markers', marker = list(size = 8),
                hoverinfo = 'text',
                text = ~paste('<br> Difference (%): ', signif(perc_difference,4),
                              '<br> Average T1: ', signif(average,5),
                              '<BR> Reference T1: ', signif(reference,5),
                              '<br> Sphere: ', sph)) %>%
      layout(xaxis = list(title=list(text="Average T1 (ms)", font=list(size=18)), tickfont=list(size=15), 
                          zeroline=F, showline=T, linewidth=2, linecolor="black", mirror=T, showgrid=F,
                          range=list(0,as.numeric(unname(apply(RefVSMeas$BAData,2,max))[9])+800)),
             yaxis = list(title=list(text="Percentage difference (%)", font=list(size=18)), tickfont=list(size=15),
                          zeroline=F, showline=T, linewidth=2, linecolor="black", mirror=T, showgrid=F, range=list(-40,40)),
             legend = list(title=list(text="<b>MRI vendor</b>"), x=0.72, y=0.95, bgcolor = "rgba(0,0,0,0)")) %>%
      add_trace(x = c(0,as.numeric(unname(apply(RefVSMeas$BAData,2,max))[9])+800), y = mean(RefVSMeas$BAData$perc_difference),
                type='scatter', mode = "lines", line = list(dash = "solid", width = 2, color = "black"),
                showlegend = FALSE, hoverinfo = "none") %>%
      add_trace(x = c(0,as.numeric(unname(apply(RefVSMeas$BAData,2,max))[9])+800),
                y = mean(RefVSMeas$BAData$perc_difference) + 1.96*sd(RefVSMeas$BAData$perc_difference),
                type='scatter', mode = "lines", line = list(dash = "dash", color = "firebrick"),
                showlegend = FALSE, hoverinfo = "none") %>%
      add_trace(x = c(0,as.numeric(unname(apply(RefVSMeas$BAData,2,max))[9])+800),
                y = mean(RefVSMeas$BAData$perc_difference) - 1.96*sd(RefVSMeas$BAData$perc_difference),
                type='scatter', mode = "lines", line = list(dash = "dash", color = "firebrick"),
                showlegend = FALSE, hoverinfo = "none") %>%
      layout(annotations = list(x=1500, y=mean(RefVSMeas$BAData$perc_difference)-3,
                                text=paste("Mean = ",signif(mean(RefVSMeas$BAData$perc_difference),3)),
                                showarrow = FALSE, font = list(size=12, color="black"))) %>%
      layout(annotations = list(x=1500, y=mean(RefVSMeas$BAData$perc_difference) + 1.96*sd(RefVSMeas$BAData$perc_difference) + 5,
                                text=paste("Mean+1.96SD = ",signif(mean(RefVSMeas$BAData$perc_difference)+1.96*sd(RefVSMeas$BAData$perc_difference),3)),
                                showarrow = FALSE, font = list(size=12, color="firebrick"))) %>%
      layout(annotations = list(x=1500, y=mean(RefVSMeas$BAData$perc_difference) - 1.96*sd(RefVSMeas$BAData$perc_difference) - 5,
                                text=paste("Mean-1.96SD = ",signif(mean(RefVSMeas$BAData$perc_difference)-1.96*sd(RefVSMeas$BAData$perc_difference),3)),
                                showarrow = FALSE, font = list(size=12, color="firebrick")))
  })
  
  output$AcErrorAllPoints <- renderPlotly({
    if (input$AcErrorAllSite == "Montreal"){
      AcErrorAllPoints = SiteMontreal
    }
    else if (input$AcErrorAllSite == "Germany"){
      AcErrorAllPoints = SiteGermany
    }
    else if (input$AcErrorAllSite == "US"){
      AcErrorAllPoints = SiteUS
    }
    else if (input$AcErrorAllSite == "London"){
      AcErrorAllPoints = SiteLondon
    }
    else if (input$AcErrorAllSite == "Australia"){
      AcErrorAllPoints = SiteAustralia
    }
    
    AcErrorAllPoints = subset(AcErrorAllPoints$dataSite_long, ac_error <= input$errorThr[2])
    
    specSite_colors <- setNames(viridis(length(unique(AcErrorAllPoints$ID_Site_long))), unique(AcErrorAllPoints$ID_Site_long))
    plot_ly(AcErrorAllPoints) %>%
      add_trace(AcErrorAllPoints, x = ~t1_long, y = ~ac_error, color = ~ID_Site_long,
                colors = specSite_colors, type = 'scatter', mode = 'markers', marker = list(size = 8),
                hoverinfo = 'text',
                text = ~paste('<br> Accuracy Error (%): ', signif(ac_error,5),
                              '<br> Reference T1 value (ms): ', signif(t1_long,5),
                              '<br> ID: ', sid_long)) %>%
      layout(xaxis = list(title=list(text="Reference T1 value (ms)", font=list(size=18)), tickfont=list(size=15), 
                          zeroline=F, showline=T, linewidth=2, linecolor="black", mirror=T, showgrid=F,
                          range=list(0,as.numeric(unname(apply(AcErrorAllPoints,2,max))[4])+100)),
             yaxis = list(title=list(text="Accuracy Error (%)", font=list(size=18)), tickfont=list(size=15),
                          zeroline=F, showline=T, linewidth=2, linecolor="black", mirror=T, showgrid=F, range=list(0,60)),
             legend = list(title=list(text="<b>Site ID</b>", font=list(size = 10)),
                           font = list(size = 9), x=0.75, y=0.99, bgcolor = "rgba(0,0,0,0)"))
  })
  
  output$HSF1 <- renderPlotly({HSFData$diffBootstrapDiff[[14]]})
  output$HSF2 <- renderPlotly({HSFData$diffBootstrapDiff[[13]]})
  output$HSF3 <- renderPlotly({HSFData$diffBootstrapDiff[[12]]})
  output$HSF4 <- renderPlotly({HSFData$diffBootstrapDiff[[11]]})
  output$HSF5 <- renderPlotly({HSFData$diffBootstrapDiff[[10]]})
  output$HSF6 <- renderPlotly({HSFData$diffBootstrapDiff[[9]]})
  output$HSF7 <- renderPlotly({HSFData$diffBootstrapDiff[[8]]})
  output$HSF8 <- renderPlotly({HSFData$diffBootstrapDiff[[7]]})
  output$HSF9 <- renderPlotly({HSFData$diffBootstrapDiff[[6]]})
  output$HSF10 <- renderPlotly({HSFData$diffBootstrapDiff[[5]]})
  output$HSF11 <- renderPlotly({HSFData$diffBootstrapDiff[[4]]})
  output$HSF12 <- renderPlotly({HSFData$diffBootstrapDiff[[3]]})
  output$HSF13 <- renderPlotly({HSFData$diffBootstrapDiff[[2]]})
  output$HSF14 <- renderPlotly({HSFData$diffBootstrapDiff[[1]]})
  
  ###HUMAN DATASET ANALYSIS###
  output$boxPlotHuman <- renderPlotly({
    sitesHuman$dataLong_human$roi_long <- factor(sitesHuman$dataLong_human$roi_long,
                                                 c("Genu WM", "splenium WM", "Deep GM", "Cortical GM"))
    plot_ly(sitesHuman$dataLong_human, x = ~roi_long, y = ~siteData, color = ~roi_long,
            type = 'violin', box = list(visible = TRUE), meanline = list(visible = TRUE)) %>%
      add_trace(hoveron = 'points+violins', points = 'all', hoverinfo = 'text',
                text = ~paste0('<br> Measured value: ', signif(siteData,5),
                               '<br>ROI: ', roi_long,
                               '<br>SID: ', factor(sid_long)), showlegend = FALSE) %>%
      layout(xaxis = list(title=list(text="Region of Interest", font=list(size=18)), tickfont=list(size=15), 
                          zeroline=F, showline=T, linewidth=2, linecolor="black", mirror=T, showgrid=F),
             yaxis = list(title=list(text="Measured T1 value (ms)", font=list(size=18)), tickfont=list(size=15), 
                          zeroline=F, showline=T, linewidth=2, linecolor="black", mirror=T, showgrid=F,
                          range=list(as.numeric(unname(apply(sitesHuman$dataLong_human,2,min))[6])-100,
                                     as.numeric(unname(apply(sitesHuman$dataLong_human,2,max))[6])+100)),
             legend = list(title=list(text="<b>ROI</b>"), orientation="h", xanchor="center", 
                           x=0.5, y=0.95, bgcolor = "rgba(0,0,0,0)"))
  })
  
  output$humanMEX_all <- renderPlotly({
    plot_ly(sitesHuman_Mexico$dataLong_human, x = ~roi_long, y = ~siteData, color = ~roi_long,
            type = 'violin', box = list(visible = TRUE), meanline = list(visible = TRUE)) %>%
      add_trace(hoveron = 'points+violins', points = 'all', hoverinfo = 'text',
                text = ~paste0('<br> Measured value: ', signif(siteData,5),
                               '<br>ROI: ', roi_long,
                               '<br>SID: ', factor(sid_long)), showlegend = FALSE) %>%
      layout(xaxis = list(title=list(text="Region of Interest", font=list(size=18)), tickfont=list(size=15), 
                          zeroline=F, showline=T, linewidth=2, linecolor="black", mirror=T, showgrid=F),
             yaxis = list(title=list(text="Measured T1 value (ms)", font=list(size=18)), tickfont=list(size=15),
                          zeroline=F, showline=T, linewidth=2, linecolor="black", mirror=T, showgrid=F,
                          range=list(as.numeric(unname(apply(sitesHuman$dataLong_human,2,min))[6])-100,
                                     as.numeric(unname(apply(sitesHuman$dataLong_human,2,max))[6])+100)),
             legend = list(title=list(text="<b>ROI</b>"), orientation="h", xanchor="center", 
                           x=0.5, y=0.95, bgcolor = "rgba(0,0,0,0)"))
  })
  
  output$humanCAN_all <- renderPlotly({
    plot_ly(sitesHuman_Canada$dataLong_human, x = ~roi_long, y = ~siteData, color = ~roi_long,
            type = 'violin', box = list(visible = TRUE), meanline = list(visible = TRUE)) %>%
      add_trace(hoveron = 'points+violins', points = 'all', hoverinfo = 'text',
                text = ~paste0('<br> Measured value: ', signif(siteData,5),
                               '<br>ROI: ', roi_long,
                               '<br>SID: ', factor(sid_long)), showlegend = FALSE) %>%
      layout(xaxis = list(title=list(text="Region of Interest", font=list(size=18)), tickfont=list(size=15), 
                          zeroline=F, showline=T, linewidth=2, linecolor="black", mirror=T, showgrid=F),
             yaxis = list(title=list(text="Measured T1 value (ms)", font=list(size=18)), tickfont=list(size=15),
                          zeroline=F, showline=T, linewidth=2, linecolor="black", mirror=T, showgrid=F,
                          range=list(as.numeric(unname(apply(sitesHuman$dataLong_human,2,min))[6])-100,
                                     as.numeric(unname(apply(sitesHuman$dataLong_human,2,max))[6])+100)),
             legend = list(title=list(text="<b>ROI</b>"), orientation="h", xanchor="center", 
                           x=0.5, y=0.95, bgcolor = "rgba(0,0,0,0)"))
  })
  
  output$humanUS_all <- renderPlotly({
    sitesHuman_US$dataLong_human$roi_long <- factor(sitesHuman_US$dataLong_human$roi_long,
                                                    c("Genu WM", "splenium WM", "Deep GM", "Cortical GM"))
    plot_ly(sitesHuman_US$dataLong_human, x = ~roi_long, y = ~siteData, color = ~roi_long,
            type = 'violin', box = list(visible = TRUE), meanline = list(visible = TRUE)) %>%
      add_trace(hoveron = 'points+violins', points = 'all', hoverinfo = 'text',
                text = ~paste0('<br> Measured value: ', signif(siteData,5),
                               '<br>ROI: ', roi_long,
                               '<br>SID: ', factor(sid_long)), showlegend = FALSE) %>%
      layout(xaxis = list(title=list(text="Region of Interest", font=list(size=18)), tickfont=list(size=15), 
                          zeroline=F, showline=T, linewidth=2, linecolor="black", mirror=T, showgrid=F),
             yaxis = list(title=list(text="Measured T1 value (ms)", font=list(size=18)), tickfont=list(size=15),
                          zeroline=F, showline=T, linewidth=2, linecolor="black", mirror=T, showgrid=F,
                          range=list(as.numeric(unname(apply(sitesHuman$dataLong_human,2,min))[6])-100,
                                     as.numeric(unname(apply(sitesHuman$dataLong_human,2,max))[6])+100)),
             legend = list(title=list(text="<b>ROI</b>"), orientation="h", xanchor="center", 
                           x=0.5, y=0.95, bgcolor = "rgba(0,0,0,0)"))
  })
  
  output$humanITA_all <- renderPlotly({
    plot_ly(sitesHuman_Italy$dataLong_human, x = ~roi_long, y = ~siteData, color = ~roi_long,
            type = 'violin', box = list(visible = TRUE), meanline = list(visible = TRUE)) %>%
      add_trace(hoveron = 'points+violins', points = 'all', hoverinfo = 'text',
                text = ~paste0('<br> Measured value: ', signif(siteData,5),
                               '<br>ROI: ', roi_long,
                               '<br>SID: ', factor(sid_long)), showlegend = FALSE) %>%
      layout(xaxis = list(title=list(text="Region of Interest", font=list(size=18)), tickfont=list(size=15), 
                          zeroline=F, showline=T, linewidth=2, linecolor="black", mirror=T, showgrid=F),
             yaxis = list(title=list(text="Measured T1 value (ms)", font=list(size=18)), tickfont=list(size=15),
                          zeroline=F, showline=T, linewidth=2, linecolor="black", mirror=T, showgrid=F,
                          range=list(as.numeric(unname(apply(sitesHuman$dataLong_human,2,min))[6])-100,
                                     as.numeric(unname(apply(sitesHuman$dataLong_human,2,max))[6])+100)),
             legend = list(title=list(text="<b>ROI</b>"), orientation="h", xanchor="center", 
                           x=0.5, y=0.95, bgcolor = "rgba(0,0,0,0)"))
  })
  
  output$humanGER_all <- renderPlotly({
    plot_ly(sitesHuman_Germany$dataLong_human, x = ~roi_long, y = ~siteData, color = ~roi_long,
            type = 'violin', box = list(visible = TRUE), meanline = list(visible = TRUE)) %>%
      add_trace(hoveron = 'points+violins', points = 'all', hoverinfo = 'text',
                text = ~paste0('<br> Measured value: ', signif(siteData,5),
                               '<br>ROI: ', roi_long,
                               '<br>SID: ', factor(sid_long)), showlegend = FALSE) %>%
      layout(xaxis = list(title=list(text="Region of Interest", font=list(size=18)), tickfont=list(size=15), 
                          zeroline=F, showline=T, linewidth=2, linecolor="black", mirror=T, showgrid=F),
             yaxis = list(title=list(text="Measured T1 value (ms)", font=list(size=18)), tickfont=list(size=15),
                          zeroline=F, showline=T, linewidth=2, linecolor="black", mirror=T, showgrid=F,
                          range=list(as.numeric(unname(apply(sitesHuman$dataLong_human,2,min))[6])-100,
                                     as.numeric(unname(apply(sitesHuman$dataLong_human,2,max))[6])+100)),
             legend = list(title=list(text="<b>ROI</b>"), orientation="h", xanchor="center", 
                           x=0.5, y=0.95, bgcolor = "rgba(0,0,0,0)"))
  })
  
  output$humanAUS_all <- renderPlotly({
    plot_ly(sitesHuman_Australia$dataLong_human, x = ~roi_long, y = ~siteData, color = ~roi_long,
            type = 'violin', box = list(visible = TRUE), meanline = list(visible = TRUE)) %>%
      add_trace(hoveron = 'points+violins', points = 'all', hoverinfo = 'text',
                text = ~paste0('<br> Measured value: ', signif(siteData,5),
                               '<br>ROI: ', roi_long,
                               '<br>SID: ', factor(sid_long)), showlegend = FALSE) %>%
      layout(xaxis = list(title=list(text="Region of Interest", font=list(size=18)), tickfont=list(size=15), 
                          zeroline=F, showline=T, linewidth=2, linecolor="black", mirror=T, showgrid=F),
             yaxis = list(title=list(text="Measured T1 value (ms)", font=list(size=18)), tickfont=list(size=15),
                          zeroline=F, showline=T, linewidth=2, linecolor="black", mirror=T, showgrid=F,
                          range=list(as.numeric(unname(apply(sitesHuman$dataLong_human,2,min))[6])-100,
                                     as.numeric(unname(apply(sitesHuman$dataLong_human,2,max))[6])+100)),
             legend = list(title=list(text="<b>ROI</b>"), orientation="h", xanchor="center", 
                           x=0.5, y=0.95, bgcolor = "rgba(0,0,0,0)"))
  })
  
  output$humanMEX_vendor <- renderPlotly({
    if (input$selectMEXVendor == "Philips"){
      dataMEX_vendor = subset(sitesHuman_Mexico$dataLong_human, as.character(vendor_long)=="Philips")
    }
    else if (input$selectMEXVendor == "GE"){
      dataMEX_vendor = subset(sitesHuman_Mexico$dataLong_human, as.character(vendor_long)=="GE")
    }
    
    plot_ly(dataMEX_vendor, x = ~roi_long, y = ~siteData, color = ~roi_long,
            type = 'violin', box = list(visible = TRUE), meanline = list(visible = TRUE)) %>%
      add_trace(hoveron = 'points+violins', points = 'all', hoverinfo = 'text',
                text = ~paste0('<br> Measured value: ', signif(siteData,5),
                               '<br>ROI: ', roi_long,
                               '<br>SID: ', factor(sid_long)), showlegend = FALSE) %>%
      layout(xaxis = list(title=list(text="Region of Interest", font=list(size=18)), tickfont=list(size=15), 
                          zeroline=F, showline=T, linewidth=2, linecolor="black", mirror=T, showgrid=F),
             yaxis = list(title=list(text="Measured T1 value (ms)", font=list(size=18)), tickfont=list(size=15),
                          zeroline=F, showline=T, linewidth=2, linecolor="black", mirror=T, showgrid=F,
                          range=list(as.numeric(unname(apply(sitesHuman_Mexico$dataLong_human,2,min))[6])-100,
                                     as.numeric(unname(apply(sitesHuman_Mexico$dataLong_human,2,max))[6])+100)),
             legend = list(title=list(text="<b>ROI</b>"), x=0.75, y=0.95, bgcolor = "rgba(0,0,0,0)"))
  })
  
  sitesHuman_colors <- setNames(rainbow(length(listHumanData)), unique(dfmeanHuman$Site))
  output$CompHumanSites <- renderPlotly({
    plot_ly(dfmeanHuman, x = ~roi_lab, y = ~dif, split = ~Site, color = ~Site, colors = sitesHuman_colors) %>%
      filter(Site %in% input$FiltHumanSitesID) %>%
      add_trace(type = 'scatter', mode = 'lines+markers',
                hoverinfo = 'text',
                text = ~paste('<br> Site: ', Site,
                              '<br> Difference (%): ', signif(dif,4),
                              '<br> ROI: ', roi_lab)) %>%
      layout(xaxis = list(title=list(text="Region of Interest", font=list(size=18)), tickfont=list(size=15), 
                          zeroline=F, showline=T, linewidth=2, linecolor="black", mirror=T, showgrid=F),
             yaxis = list(title=list(text="Percentage difference (%)", font=list(size=18)), tickfont=list(size=15),
                          zeroline=F, showline=T, linewidth=2, linecolor="black", mirror=T, showgrid=F, range=list(-30,70)),
             legend = list(title=list(text="<b>Site ID</b>"), x=0.8, y=0.95, bgcolor = "rgba(0,0,0,0)"))
  })
  
  sitesHuman$dataLong_human$age_long=as.numeric(as.character(sitesHuman$dataLong_human$age_long))
  output$humanAge_all <- renderPlotly({
    #sdFiltered_colors <- setNames(rainbow(nrow(sdFilteredSites$stdData)), sdFilteredSites$stdData$ID_Site)
    plot_ly(sitesHuman$dataLong_human[order(sitesHuman$dataLong_human$age_long),], x = ~age_long, y = ~siteData, split = ~roi_long) %>%
      add_trace(type = 'scatter', mode = 'markers',
                hoverinfo = 'text',
                text = ~paste('<br> Site: ', sid_long,
                              '<br> ROI: ', roi_long,
                              '<br> T1 value: ', signif(siteData,5))) %>%
      layout(xaxis = list(title=list(text="Age (years)", font=list(size=18)), categoryarray = ~names, categoryorder = "array", tickfont=list(size=15), 
                          zeroline=F, showline=T, linewidth=2, linecolor="black", mirror=T, showgrid=F),
             yaxis = list(title=list(text="Measured T1 value (ms)", font=list(size=18)), tickfont=list(size=15),
                          zeroline=F, showline=T, linewidth=2, linecolor="black", mirror=T, showgrid=F,
                          range=list(0,as.numeric(unname(apply(sitesHuman$dataLong_human,2,max))[6])+100)),
             legend = list(title=list(text="<b>ROI</b>"), x=0.75, y=0.95, bgcolor = "rgba(0,0,0,0)"))
    
  })
  
  #covs_colors <- setNames(rainbow(12), unique(compNISTHuman$Site))
  maxX = as.numeric(unname(apply(compNISTHuman,2,max, na.rm=T))[4])
  maxY = as.numeric(unname(apply(compNISTHuman,2,max, na.rm=T))[5])
  maxSampleSize = as.numeric(unname(apply(compNISTHuman,2,max, na.rm=T))[7])
  
  output$NISTHumanCOV_STD_np <- renderPlotly({
    if (input$cov_std == "Coefficient of variation"){
      plot_ly(compNISTHuman_nist$data_NIST) %>%
        add_trace(compNISTHuman_nist$data_NIST, x = ~Mean, y = ~100*Std/Mean, color = ~Site_name,
                  colors = c('blue','red'), type = 'scatter', mode = 'markers', marker = list(size = ~szSample*30/maxSampleSize),
                  hoverinfo = 'text',
                  text = ~paste('<br> CoV (%): ', signif(100*Std/Mean,4),
                                '<br> STD (ms): ', signif(Std,4),
                                '<br> Mean (ms): ', signif(Mean,5),
                                '<br> Reference T1 (ms): ', signif(t1ROI,5),
                                '<br> Sample size: ', szSample,
                                '<br> Site ID: ', Site)) %>%
        layout(xaxis = list(title=list(text="Mean value (ms)", font=list(size=18)), tickfont=list(size=15), 
                            zeroline=F, showline=T, linewidth=2, linecolor="black", mirror=T, showgrid=F, range=list(300,maxX+100)),
               yaxis = list(title=list(text="Coefficient of variation (%)", font=list(size=18)), tickfont=list(size=15), 
                            zeroline=F, showline=T, linewidth=2, linecolor="black", mirror=T, showgrid=F, range=list(0,25))
               )
    }
    else if (input$cov_std == "Standard deviation"){
      plot_ly(compNISTHuman_nist$data_NIST) %>%
        add_trace(compNISTHuman_nist$data_NIST, x = ~Mean, y = ~Std, color = ~Site_name,
                  colors = c('blue','red'), type = 'scatter', mode = 'markers', marker = list(size = ~szSample*30/maxSampleSize),
                  hoverinfo = 'text',
                  text = ~paste('<br> STD (ms): ', signif(Std,4),
                                '<br> Mean (ms): ', signif(Mean,5),
                                '<br> CoV (%): ', signif(100*Std/Mean,4),
                                '<br> Reference T1 (ms): ', signif(t1ROI,5),
                                '<br> Sample size: ', szSample,
                                '<br> Site ID: ', Site)) %>%
        layout(xaxis = list(title=list(text="Mean value (ms)", font=list(size=18)), tickfont=list(size=15), 
                            zeroline=F, showline=T, linewidth=2, linecolor="black", mirror=T, showgrid=F, range=list(0,maxX+100)),
               yaxis = list(title=list(text="Standard deviation (ms)", font=list(size=18)), tickfont=list(size=15), 
                            zeroline=F, showline=T, linewidth=2, linecolor="black", mirror=T, showgrid=F, range=list(0,maxY+100)),
               legend = list(title=list(text="<b>Site ID</b>"), x=0.6, y=0.95, bgcolor = "rgba(0,0,0,0)"))
    }
  })
  
  output$NISTHumanCOV_STD_h <- renderPlotly({
    if (input$cov_std == "Coefficient of variation"){
      plot_ly(compNISTHuman_human$data_human) %>%
        add_trace(compNISTHuman_human$data_human, x = ~Mean, y = ~100*Std/Mean, color = ~Site_name,
                  colors = c('blue','red'), type = 'scatter', mode = 'markers', marker = list(size = ~szSample*30/maxSampleSize),
                  hoverinfo = 'text',
                  text = ~paste('<br> CoV (%): ', signif(100*Std/Mean,4),
                                '<br> STD (ms): ', signif(Std,4),
                                '<br> Mean (ms): ', signif(Mean,5),
                                '<br> ROI: ', t1ROI,
                                '<br> Sample size: ', szSample,
                                '<br> Site ID: ', Site)) %>%
        layout(xaxis = list(title=list(text="Mean value (ms)", font=list(size=18)), tickfont=list(size=15), 
                            zeroline=F, showline=T, linewidth=2, linecolor="black", mirror=T, showgrid=F, range=list(300,maxX+100)),
               yaxis = list(title=list(text="Coefficient of variation (%)", font=list(size=18)), tickfont=list(size=15), 
                            zeroline=F, showline=T, linewidth=2, linecolor="black", mirror=T, showgrid=F, range=list(0,25))
               )
    }
    else if (input$cov_std == "Standard deviation"){
      plot_ly(compNISTHuman_human$data_human) %>%
        add_trace(compNISTHuman_human$data_human, x = ~Mean, y = ~Std, color = ~Site_name,
                  colors = c('blue','red'), type = 'scatter', mode = 'markers', marker = list(size = ~szSample*30/maxSampleSize),
                  hoverinfo = 'text',
                  text = ~paste('<br> STD (ms): ', signif(Std,4),
                                '<br> Mean (ms): ', signif(Mean,5),
                                '<br> CoV (%): ', signif(100*Std/Mean,4),
                                '<br> ROI: ', t1ROI,
                                '<br> Sample size: ', szSample,
                                '<br> Site ID: ', Site)) %>%
        layout(xaxis = list(title=list(text="Mean value (ms)", font=list(size=18)), tickfont=list(size=15), 
                            zeroline=F, showline=T, linewidth=2, linecolor="black", mirror=T, showgrid=F, range=list(0,maxX+100)),
               yaxis = list(title=list(text="Standard deviation (ms)", font=list(size=18)), tickfont=list(size=15),
                            zeroline=F, showline=T, linewidth=2, linecolor="black", mirror=T, showgrid=F, range=list(0,maxY+100)),
               legend = list(title=list(text="<b>Site ID</b>"), x=0.6, y=0.95, bgcolor = "rgba(0,0,0,0)"))
    }
  })
}