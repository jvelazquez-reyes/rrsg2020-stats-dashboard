library(shiny)
library(shinythemes)
library(shinydashboard)
library(gridExtra)

path_to_src = "D:\\acer\\Documents\\PhD application\\Polymtl\\Internship2020\\rrsg2020-stats-dashboard\\plot\\"
source(paste(path_to_src, "allStats.R", sep = ""))

# Define UI for application that draws a histogram
ui <- navbarPage("T1 mapping challenge statistics", theme = shinytheme("flatly"),
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
                 tabPanel("Plotly",
                          sidebarLayout(
                              sidebarPanel(
                                  selectizeInput(
                                      inputId = "SitesID", 
                                      label = "Select a site", 
                                      choices = unique(stats$test$sid),
                                      selected = "1.001",
                                      multiple = TRUE
                                  ),
                              ),
                              
                              mainPanel(
                                  h3("Plot different sites"),
                                  plotlyOutput(outputId = "multPlot")
                              )
                          )
                 ),
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
