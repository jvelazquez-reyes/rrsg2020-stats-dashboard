library(shiny)
library(shinythemes)
library(shinydashboard)
library(gridExtra)

path_to_src = "D:\\acer\\Documents\\PhD application\\Polymtl\\Internship2020\\rrsg2020-stats-dashboard\\plot\\"
source(paste(path_to_src, "allStats.R", sep = ""))

# Define UI for application that draws a histogram
ui <- navbarPage("T1 mapping challenge statistics", theme = shinytheme("flatly"),
                 
                 #TAB 1
                 tabPanel("Magnitud VS Complex",
                          sidebarLayout(
                              sidebarPanel(
                                  selectizeInput(
                                      inputId = "DiffSitesID", 
                                      label = "Select a site", 
                                      choices = unique(magVScomp$dataMagComp$sid),
                                      #selected = "1.001",
                                      multiple = TRUE
                                  ),
                                  
                                  radioButtons(inputId = "typeComparison",
                                               label = "Choose the type of plot to display",
                                               choices = c("Difference", "Difference (%)"),
                                               #selected = "Difference")
                                  ),
                                  
                                  selectizeInput(
                                      inputId = "CorrSitesID", 
                                      label = "Select a site to show a dispersion plot", 
                                      choices = unique(magVScomp$dataCorr$sid),
                                      #selected = "1.001",
                                      multiple = FALSE
                                  ),
                                  
                                  h2("Correlation coefficients"),
                                  tableOutput(outputId = "PearsonCorr"),
                                  
                                  helpText("Mathieu, B., et al. MathieuPaperName")
                                  
                              ),
                              
                              mainPanel(
                                  h3("Difference between Magnitude and Complex"),
                                  plotlyOutput(outputId = "MagComp"),
                                  h3("Correlation between Magnitude and Complex"),
                                  plotlyOutput(outputId = "CorrMagComp")
                              )
                          )
                 ),
                 
                 #TAB 2
                 tabPanel("Comparison across sites",
                          tabsetPanel(
                          
                                      tabPanel(sidebarLayout(
                                          sidebarPanel(
                                              selectizeInput(
                                                  inputId = "SiteUSID", 
                                                  label = "Select a site", 
                                                  choices = unique(SiteUS$dataSite$Site),
                                                  multiple = TRUE
                                              ),
                                              
                                              helpText("Mathieu, B., et al. MathieuPaperName")
                                              
                                          ),
                                          
                                          mainPanel(
                                              h3("US Data"),
                                              plotlyOutput(outputId = "CompUS")
                                          )
                                      )
                                      ),
                                      tabPanel(sidebarLayout(
                                          sidebarPanel(
                                              selectizeInput(
                                                  inputId = "SiteGermanyID", 
                                                  label = "Select a site", 
                                                  choices = unique(SiteGermany$dataSite$Site),
                                                  multiple = TRUE
                                              ),
                                              
                                              helpText("Mathieu, B., et al. MathieuPaperName")
                                              
                                          ),
                                          
                                          mainPanel(
                                              h3("Germany Data"),
                                              plotlyOutput(outputId = "CompGermany")
                                          )
                                      )
                                      )
                          )

                 ),

                 #TAB 3
                 tabPanel("Reliability",
                         sidebarLayout(
                         sidebarPanel(
                             radioButtons(inputId = "typeplot",
                                          label = "Choose the type of plot to display",
                                          choices = c("Bland-Altman", "Dispersion"),
                                          selected = "Bland-Altman")
                         ),
                         
                         mainPanel(
                             h3("Plots"),
                             plotlyOutput(outputId = "distPlot"),
                             h3("Correlation coefficients"),
                             tableOutput(outputId = "corrTable")
                         )
                     )
                 ),
                 
                 #TAB 4
                 tabPanel("Plotly",
                          sidebarLayout(
                              sidebarPanel(
                                  selectizeInput(
                                      inputId = "SitesID", 
                                      label = "Select a site", 
                                      choices = unique(stats$test$sid),
                                      selected = "1.001",
                                      multiple = TRUE
                                  )
                              ),
                              
                              mainPanel(
                                  h3("Plot different sites"),
                                  plotlyOutput(outputId = "multPlot")
                              )
                          )
                 ),
                 
                 #TAB 5
                 tabPanel("Bias",
                          sidebarLayout(
                              sidebarPanel(
                                  radioButtons(inputId = "typeplot2",
                                               label = "Choose the type of plot to display",
                                               choices = c("Standard Deviation", "Root Mean Square Error"),
                                               selected = "Standard Deviation")
                              ),
                              
                              mainPanel(
                                  h3("Plots"),
                                  plotlyOutput(outputId = "distPlot2")
                              )
                          ))
    
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    #TAB 1
    MagCom_colors <- setNames(rainbow(nrow(magVScomp$dataMagComp)), magVScomp$dataMagComp$sid)
    output$MagComp <- renderPlotly({
        if (input$typeComparison == "Difference"){
            plot_ly(magVScomp$dataMagComp, x = ~sph, y = ~diff, split = ~sid, color = ~sid, colors = MagCom_colors) %>%
                filter(sid %in% input$DiffSitesID) %>%
                group_by(sid) %>%
                add_trace(type = 'scatter', mode = 'lines+markers',
                          hoverinfo = 'text',
                          text = ~paste('<br> Site: ', sid,
                                        '<br> Difference: ', diff,
                                        '<br> Sphere: ', sph))
        }
        else if (input$typeComparison == "Difference (%)"){
            plot_ly(magVScomp$dataMagComp, x = ~sph, y = ~percDiff, split = ~sid, color = ~sid, colors = MagCom_colors) %>%
                filter(sid %in% input$DiffSitesID) %>%
                #group_by(sid) %>%
                add_trace(type = 'scatter', mode = 'lines+markers',
                          hoverinfo = 'text',
                          text = ~paste('<br> Site: ', sid,
                                        '<br> Difference (%): ', percDiff,
                                        '<br> Sphere: ', sph))
        }
    })
        
    output$CorrMagComp <- renderPlotly({
        p = ggplot(data = filter(magVScomp$dataCorr, sid %in% input$CorrSitesID)) +
            geom_point(aes(x = Complex, y = Magnitude), color = "black", size = 1.5) +
            labs(x = "Complex T1 value (ms)", y = "Magnitude T1 value (ms)") +
            geom_smooth(aes(x = Complex, y = Magnitude), method = "lm", se = TRUE, color = "red", lwd = 0.5) +
            geom_abline(intercept = 0, slope = 1, lwd = 0.7, col = "blue") +
            theme(axis.line = element_line(colour = "black"), 
                  panel.grid.major = element_blank(), 
                  panel.grid.minor = element_blank(), 
                  panel.border = element_blank(), 
                  panel.background = element_blank()) +
            theme_bw() + theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
                               axis.title = element_text(size = 12),
                               axis.text = element_text(size = 12))
        
        ggplotly(p)
    })
    
    output$PearsonCorr <- renderTable(magVScomp$PearsonCorr)
    
    #TAB 2
    US_colors <- setNames(rainbow(nrow(SiteUS$dataSite)), SiteUS$dataSite$Site)
    output$CompUS <- renderPlotly({
        plot_ly(SiteUS$dataSite, x = ~Sphere, y = ~Mean, split = ~Site, color = ~Site, colors = US_colors) %>%
            filter(Site %in% input$SiteUSID) %>%
            #group_by(sid) %>%
            add_trace(type = 'scatter', mode = 'lines+markers',
                      hoverinfo = 'text',
                      text = ~paste('<br> Site: ', Site,
                                    '<br> Mean: ', Mean,
                                    '<br> Sphere: ', Sphere))
    })
    
    Germany_colors <- setNames(rainbow(nrow(SiteGermany$dataSite)), SiteGermany$dataSite$Site)
    output$CompGermany <- renderPlotly({
        plot_ly(SiteGermany$dataSite, x = ~Sphere, y = ~Mean, split = ~Site, color = ~Site, colors = Germany_colors) %>%
            filter(Site %in% input$SiteGermanyID) %>%
            #group_by(sid) %>%
            add_trace(type = 'scatter', mode = 'lines+markers',
                      hoverinfo = 'text',
                      text = ~paste('<br> Site: ', Site,
                                    '<br> Mean: ', Mean,
                                    '<br> Sphere: ', Sphere))
    })
    
    
    #TAB 3
    output$distPlot <- renderPlotly({
        if (input$typeplot == "Bland-Altman"){
            plotType <- stats$Bland_Altman
        }
        if (input$typeplot == "Dispersion"){
            plotType <- stats$Dispersion
        }
        plot1 = plotType[[1]]
        plot2 = plotType[[2]]
        plot3 = plotType[[3]]
        plot4 = plotType[[4]]
        subplot(list(plot1,plot2,plot3,plot4), nrows = 2, margin = 0.06, shareX = T, shareY = T)
    })
    
    output$corrTable <- renderTable(stats$Correlation_coefficients)
    
    #TAB 4
    output$multPlot <- renderPlotly({
        #req(input$SitesID)
        #if (identical(input$SitesID, "")) return(NULL)
        plot_ly(stats$test, x = ~sph, y = ~stdValues, split = ~sid) %>%
            filter(sid %in% input$SitesID) %>%
            #group_by(sid) %>%
            add_lines()
    })
    #output$multPlot <- renderPlotly({
    #    req(input$SitesID)
    #    if (identical(input$SitesID, "")) return(NULL)
    #    multPlot <- ggplot(stats$test, aes(x = stats$test$sph, y = stats$test$stdValues)) +
    #        geom_line(size = 1.5) +
    #        scale_colour_manual(values = c("darkred", "blue", "dark green", "red"))
    #    ggplotly(multPlot)
    #})
    
    #TAB 5
    output$distPlot2 <- renderPlotly({
        if (input$typeplot2 == "Standard Deviation"){
            plotType2 <- stats$STD
        }
        if (input$typeplot2 == "Root Mean Square Error"){
            plotType2 <- stats$RMSE
        }
        plot12 = plotType2[[1]]
        plot22 = plotType2[[2]]
        plot32 = plotType2[[3]]
        plot42 = plotType2[[4]]
        subplot(list(plot12,plot22,plot32,plot42), nrows = 2, margin = 0.06, shareX = T, shareY = T)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
