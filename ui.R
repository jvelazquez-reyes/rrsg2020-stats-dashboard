library("Metrics")
library("ggplot2")
library("rogme")
library("tibble")
library("stringi")
library("rjson")
library("viridis")
library("irr")
library("DescTools")
library("sjPlot")
library("plotly")
library("shiny")
library("shinydashboard")

source("allStats.R")

### ui header
header <- dashboardHeader(
  title = "T1 mapping challenge statistics",
  titleWidth = 450
)

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Home", tabName = "menuHome", icon = icon("home")),
    menuItem("NIST phantom", tabName = "menuNISTphantom", icon = icon("dashboard"),
             menuSubItem(text = "Magnitude/Complex signal", tabName = "subMagComp", icon = icon("chart-bar")),
             menuSubItem(text = "Measurements/Reference T1", tabName = "subMeasRef", icon = icon("chart-bar"))),
    menuItem("Human brain", tabName = "menuHuman", icon = icon("brain"))
    
  )
)

### ui body
body <- dashboardBody(
  
  tabItems(
    # First tab content
    tabItem(tabName = "menuHome",
            h2("Welcome to the ISMRM2020 Reproducible Challenge!"),
            
            br(),
            
            fluidRow(
              box(width=6, status="primary",
                  htmlOutput("overviewTxt1"),
                  mainPanel(img(src='challengePitch.png', height="400%", width="150%", align = "center"))),
              box(width=6, status="warning",
                  htmlOutput("overviewTxt2"),
                  mainPanel(img(src='rrsg2020Repo.png', height="300%", width="150%", align = "center")))
            ),
            
            fluidRow(
              box(width=6, status="primary",
                  htmlOutput("overviewTxt3"),
                  mainPanel(img(src='dataChallenge.png', height="100%", width="150%", align = "center"))),
              box(width=6, status="warning",
                  htmlOutput("overviewTxt4"),
                  mainPanel(img(src='generalDashboard.png', height="300%", width="150%", align = "center")))
            )
    ),
    
    # Second tab content
    tabItem(tabName = "subMagComp",
            fluidRow(
              box(width=2, solidHeader=TRUE, status="primary",
                  selectizeInput(
                    inputId = "DiffSitesID",
                    label = "Select site",
                    choices = unique(magVScomp$dataMagComp$sid),
                    selected = unique(magVScomp$dataMagComp$sid),
                    multiple = TRUE),
                  
                  radioButtons(inputId = "typeComparison",
                               label = "Choose the type of plot to display",
                               choices = c("Difference", "Difference (%)"),
                               selected = "Difference (%)"
                  )),
              
              box(title="Comparison Magnitude/Complex", width=5, status="warning", solidHeader=TRUE,
                  plotlyOutput(outputId = "MagComp")),
              
              box(title="Bland-Altman plot", width=5, status="warning", solidHeader=TRUE,
                  plotlyOutput(outputId = "BAMagComp"))
            ),
            
            fluidRow(
              box(width=2, solidHeader=TRUE, status="primary",
                  selectizeInput(
                    inputId = "CorrSitesID",
                    label = "Select site", 
                    choices = unique(magVScomp$dataCorr$sid),
                    multiple = FALSE
                  )),
              
              box(title="Correlation Magnitude/Complex (mean values)", width=5, solidHeader=TRUE,
                  collapsible=TRUE, collapsed=TRUE, status="warning", plotlyOutput(outputId = "CorrMagComp")),
              
              box(title="Correlation coefficients Magnitude/Complex (mean values)", width=2, solidHeader=TRUE,
                  status="warning", collapsible=TRUE, collapsed=TRUE, tableOutput(outputId = "PearsonCorr"))
            ),
            
            fluidRow(
              box(width=2, solidHeader=TRUE, status="primary",
                  selectizeInput(
                    inputId = "CorrSitesIDallDP",
                    label = "Select site", 
                    choices = unique(magVScomp$PearsonCorrSphere$sid_long),
                    multiple = FALSE
                  )),
              
              box(title="Correlation Magnitude/Complex (one site - all datapoints)", width=5, solidHeader=TRUE, status="warning",
                  plotlyOutput(outputId = "DispAllPointsMagComp_oneSite")),
              
              box(title="Correlation coefficients Magnitude/Complex (one site - all datapoints)", width=3, solidHeader=TRUE, status="warning",
                  tableOutput(outputId = "PearsonAllPointsMagComp_oneSite"))
            ),
            
            fluidRow(
              column(2),
              box(title="Correlation Magnitude/Complex (all sites - all datapoints)", width=5, 
                  solidHeader=TRUE, status="warning", plotlyOutput(outputId = "DispAllPointsMagComp_allSites")),
              
              box(title="Correlation coefficients Magnitude/Complex (all sites - all datapoints)", width=3, 
                  solidHeader=TRUE, status="warning", tableOutput(outputId = "PearsonAllPointsMagComp_allSites"))
            )
    ),
    
    tabItem(tabName = "subMeasRef",
            tabBox(width = 12,
                   tabPanel(title="General",
                            fluidRow(
                              box(width=2, solidHeader=TRUE, status="primary",
                                  selectizeInput(
                                    inputId = "FiltSitesID", 
                                    label = "Select group", 
                                    choices = unique(MeasSites$dataSite$sid),
                                    selected = unique(MeasSites$dataSite$sid),
                                    multiple = TRUE)),
                              
                              box(title="Mean Measured/Reference T1 values (all sites)", width=5, solidHeader=TRUE, status="warning",
                                  plotlyOutput(outputId = "CompFiltSites")),
                              
                              box(title="Standard deviation Measured/Reference T1 (all sites)", width=5, solidHeader=TRUE, status="warning",
                                  plotlyOutput(outputId = "sdFilteredSites"))
                            ),
                            
                            fluidRow(
                              box(width=2, solidHeader=TRUE, status="primary",
                                  selectInput(inputId = "DispAllSite",
                                              label = "Choose site:",
                                              choices = c("Montreal","Germany","US","London","Australia"),
                                              selected = "Montreal")),
                              box(title="Measured/Reference T1 values (all sites - all datapoints)", width=5, solidHeader=TRUE, status="warning",
                                  plotlyOutput(outputId = "DispAllPoints"))
                            ),
                            
                            fluidRow(
                              box(width=2, solidHeader=TRUE, status="primary",
                                  selectInput(inputId = "selectCompSite",
                                              label = "Choose site:",
                                              choices = c("Montreal","Germany","US","London","Australia"),
                                              selected = "Montreal")),
                              
                              box(title="Correlation Measured/Reference T1 values (by Site)", width=5, solidHeader=TRUE, status="warning",
                                  plotlyOutput(outputId = "Disp4Site")),
                              
                              box(title="Bland-Altman plot Measured/Reference T1 (by Site)", width=5, solidHeader=TRUE, status="warning",
                                  plotlyOutput(outputId = "BA4Site"))
                            ),
                            
                            fluidRow(
                              box(width=2, solidHeader=TRUE, status="primary",
                                  selectInput(inputId = "selectCompVendor",
                                              label = "Choose MRI Vendor:",
                                              choices = c("Siemens","GE","Philips"),
                                              selected = "Siemens")),
                              
                              box(title="Correlation Measured/Reference T1 values (by MRI Vendor)", width=5, solidHeader=TRUE, status="warning",
                                  plotlyOutput(outputId = "Disp4Vendor")),
                              
                              box(title="Bland-Altman plot Measured/Reference T1 (by MRI Vendor)", width=5, solidHeader=TRUE, status="warning",
                                  plotlyOutput(outputId = "BA4Vendor"))
                            ),
                            
                            fluidRow(
                              box(width=2, solidHeader=TRUE, status="primary",
                                  selectInput(inputId = "AcErrorAllSite",
                                              label = "Choose site:",
                                              choices = c("Montreal","Germany","US","London","Australia"),
                                              selected = "Montreal"),
                                  sliderInput(inputId = "errorThr",
                                              label = "Accuracy error range",
                                              min = 0, max = 50,
                                              value = c(0,50), step = 1)),
                              
                              box(title="Accuracy Error", width=5, solidHeader=TRUE, status="warning",
                                  plotlyOutput(outputId = "AcErrorAllPoints"))
                            )
                            
                   ),
                   tabPanel(title="Hierarchical Shift Function (HSF)",
                            h2("HSF with bootstrapped confidence intervals"),
                            fluidRow(
                              box(width=6, plotlyOutput(outputId = "HSF1")),
                              box(width=6, plotlyOutput(outputId = "HSF2"))
                            ),
                            
                            fluidRow(
                              box(width=6, plotlyOutput(outputId = "HSF3")),
                              box(width=6, plotlyOutput(outputId = "HSF4"))
                            ),
                            
                            fluidRow(
                              box(width=6, plotlyOutput(outputId = "HSF5")),
                              box(width=6, plotlyOutput(outputId = "HSF6"))
                            ),
                            
                            fluidRow(
                              box(width=6, plotlyOutput(outputId = "HSF7")),
                              box(width=6, plotlyOutput(outputId = "HSF8"))
                            ),
                            
                            fluidRow(
                              box(width=6, plotlyOutput(outputId = "HSF9")),
                              box(width=6, plotlyOutput(outputId = "HSF10"))
                            ),
                            
                            fluidRow(
                              box(width=6, plotlyOutput(outputId = "HSF11")),
                              box(width=6, plotlyOutput(outputId = "HSF12"))
                            ),
                            
                            fluidRow(
                              box(width=6, plotlyOutput(outputId = "HSF13")),
                              box(width=6, plotlyOutput(outputId = "HSF14"))
                            )
                   )
            )
            
    ),
    
    # Third tab content
    tabItem(tabName = "menuHuman",
            h2("T1 value measured in different regions of the human brain"),
            fluidRow(
              box(title="All Sites", width=4, solidHeader=TRUE, status="warning",
                  plotlyOutput(outputId = "boxPlotHuman")),
              box(title="Mexico", width=4, solidHeader=TRUE, status="warning",
                  plotlyOutput(outputId = "humanMEX_all")),
              box(title="Canada", width=4, solidHeader=TRUE, status="warning",
                  plotlyOutput(outputId = "humanCAN_all"))
            ),
            
            fluidRow(
              box(title="United States", width=4, solidHeader=TRUE, status="warning",
                  plotlyOutput(outputId = "humanUS_all")),
              box(title="Italy", width=4, solidHeader=TRUE, status="warning",
                  plotlyOutput(outputId = "humanITA_all")),
              box(title="Germany", width=4, solidHeader=TRUE, status="warning",
                  plotlyOutput(outputId = "humanGER_all"))
            ),
            
            fluidRow(
              box(title="Australia", width=4, solidHeader=TRUE, status="warning",
                  plotlyOutput(outputId = "humanAUS_all"))
            ),
            
            h2("T1 values by two vendors (MEXICO)"),
            fluidRow(
              box(width=2, solidHeader=TRUE, status="primary",
                  selectInput(inputId = "selectMEXVendor",
                              label = "Choose MRI Vendor:",
                              choices = c("Philips","GE"),
                              selected = "Philips")),
              
              box(title="Measured T1 values (by MRI Vendor)", width=5, solidHeader=TRUE, status="warning",
                  plotlyOutput(outputId = "humanMEX_vendor"))
            ),
            
            fluidRow(
              box(width=2, solidHeader=TRUE, status="primary",
                  selectizeInput(
                    inputId = "FiltHumanSitesID", 
                    label = "Select site", 
                    choices = unique(dfmeanHuman$Site),
                    selected = unique(dfmeanHuman$Site),
                    multiple = TRUE)),
              
              box(title="Percentage difference (%) with MEX as reference", width=5, solidHeader=TRUE, status="warning",
                  plotlyOutput(outputId = "CompHumanSites"))
            ),
            
            fluidRow(
              column(2),
              box(title="Measured T1 value VS Age (all sites)", width=5, solidHeader=TRUE, status="warning",
                  plotlyOutput(outputId = "humanAge_all"))
            ),
            
            h2("Comparison of NIST phantom and Human data"),
            fluidRow(
              box(width=2, solidHeader=TRUE, status="primary",
                  radioButtons(inputId = "cov_std",
                               label = "Select:",
                               choices = c("Coefficient of variation", "Standard deviation"),
                               selected = "Coefficient of variation")),
              
              box(title="NIST Phantom dataset", width=10, solidHeader=TRUE, status="warning",
                  plotlyOutput(outputId = "NISTHumanCOV_STD_np"))
            ),
            
            fluidRow(
              column(2),
              box(title="Human dataset", width=10, solidHeader=TRUE, status="warning",
                  plotlyOutput(outputId = "NISTHumanCOV_STD_h"))
            )
    )
  )
)

dashboardPage(skin = "blue",
              header,
              sidebar,
              body
)