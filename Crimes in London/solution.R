library(shiny)
library(shinythemes)
library(data.table)
library(plyr)
library(ggplot2)
library(DT)

crimeAll2019 <- read.csv("C:/Users/syuria.amirrudin/OneDrive - Shell/Documents/Work/Oxford Brookes Uni/Summer - Data Visualisation/Day 1 Session 2/Crime data/crimes-2019-all.csv")
crime2019 <- read.csv("C:/Users/syuria.amirrudin/OneDrive - Shell/Documents/Work/Oxford Brookes Uni/Summer - Data Visualisation/Day 1 Session 2/Crime data/crimes-2019-types.csv")
crimeType <- read.csv("C:/Users/syuria.amirrudin/OneDrive - Shell/Documents/Work/Oxford Brookes Uni/Summer - Data Visualisation/Day 1 Session 2/Crime data/crimes-types.csv")
crimeDensity <-  read.csv("C:/Users/syuria.amirrudin/OneDrive - Shell/Documents/Work/Oxford Brookes Uni/Summer - Data Visualisation/Day 3 Session 5/Tutorial 5 Solution/crimes-density.csv")

crimeMonth <- unique(crimeAll2019$Month)

myui <- fluidPage(theme = shinytheme("cerulean"),
                navbarPage(
                    "Crime in London",
                    tabPanel(
                        "Crime data",
                        sidebarPanel(
                            tags$h3("Crime data"),
                            textInput(inputId = "txtLocation", 
                                      label = "Location", 
                                      value = ""),
                            textInput(inputId = "txtBrief", 
                                      label = "Brief Description", 
                                      value = ""),
                            selectInput(inputId = "siType",
                                        label = "Crime Type",
                                        choices = crimeType),
                            radioButtons(inputId = "rbMonth",
                                         label = "Month",
                                         choices = crimeMonth),
                            actionButton(inputId = "btnSubmit",
                                         label = "Submit")
                        ),
                        mainPanel(
                            tags$h1("About the crime"),
                            tags$h4("Description"),
                            verbatimTextOutput("txtOutput"),
                            tags$h4("Crime data - The first 10 rows"),
                            tableOutput("tabledataHead"),
                            tags$h4("Crime data - The last 10 rows"),
                            tableOutput("tabledataTail"),
                        )
                    ),
                    tabPanel(
                      "Bar Graph",
                      sidebarPanel(
                        selectInput(inputId = "siTypeBG",
                                    label = "Crime Type",
                                    choices = crimeType),
                      ),
                      mainPanel(
                        plotOutput(outputId = "bar"),
                        verbatimTextOutput("txtOTitle"),
                        tableOutput("tableTotalCrimes")
                      )
                    ),
                    tabPanel(
                        "Scatter Plot",
                        mainPanel(
                          tags$h4("Crime vs Population Density in London"),
                          plotOutput(outputId = "scatter", width = "100%", inline=TRUE),
                          tableOutput("tableCrimeDensity")
                        )
                    ),
                    tabPanel(
                      "Pie Chart",
                      mainPanel(
                        tags$h4("Crime by Type"),
                        plotOutput(outputId = "piechart", width = "100%", inline=TRUE),
                        DT::dataTableOutput("tableCrimeByType")
                      )
                    ),
                    tabPanel(
                      "Doughnut Chart",
                      sidebarPanel(
                        radioButtons(inputId = "rbMonthDoughnut",
                                     label = "Month",
                                     choices = crimeMonth),
                      ),
                      mainPanel(
                        tags$h4("Crime by Type"),
                        plotOutput(outputId = "DoughnutChart", width = "100%", inline=TRUE),
                      )
                    )
                )
)

myserver <- function(input, output, session){
    
    datasetInputHead <- reactive({
        crimeData <- crimeAll2019[which(crimeAll2019$Crime.type == input$siType),]
        crimeData <- crimeData[which(crimeData$Month == input$rbMonth),]
        
        print(head(crimeData,10))
    })
      
    datasetInputTail <- reactive({
      crimeData <- crimeAll2019[which(crimeAll2019$Crime.type == input$siType),]
      crimeData <- crimeData[which(crimeData$Month == input$rbMonth),]
      
      print(tail(crimeData,10))
    })
    
    output$txtOutput <- renderText({
        paste(input$txtLocation, input$txtBrief, sep = "\n\n")
    })
    
    output$tabledataHead <- renderTable({
        if (input$btnSubmit>0){
          isolate(datasetInputHead())
        }
    })
    
    output$tabledataTail <- renderTable({
        if (input$btnSubmit>0){
          isolate(datasetInputTail())
        }
    })
    
    # Week 4 - Bar chart
    
    output$bar <- renderPlot({
      crimeData <- crimeAll2019[which(crimeAll2019$Crime.type == input$siTypeBG),]
      
      totalCrime <- count(crimeData, "Month")
      
      specific_title <- c("Total number of ", input$siTypeBG, "crimes in London for each month")
      
      barplot(totalCrime$freq,
              main = specific_title,
              ylab = "Total",
              xlab = "Month",
              names.arg = totalCrime$Month,
              col = rainbow(length(totalCrime$Month))
      )
    })
    
    dataTotalCrime <- reactive({
      crimeData <- crimeAll2019[which(crimeAll2019$Crime.type == input$siTypeBG),]
      
      totalCrime <- count(crimeData, "Month")
      print(totalCrime)
      
    })
    
    output$txtOTitle <- renderText({
      title_table <- c("Total crime of ", input$siTypeBG)
      paste(title_table, sep = "\n\n")
    })
    
    output$tableTotalCrimes <- renderTable({
      isolate(dataTotalCrime())
    })
    
    # Week 6 - RMarkdown - Data Pre-Processing
    
    # Week 7 - Scatter Plot
    
    output$scatter <- renderPlot({
      ggplot(crimeDensity, aes(x=Population, y=freq)) + 
        geom_point() +
        geom_text(aes(label = LSOA.name))
    }, height = 800, width = 1200)
    
    output$tableCrimeDensity <- renderTable({
        print(crimeDensity)
    })
    
    
    # Week 8 - Pie Chart
    
    output$piechart <- renderPlot({
      crimeByType <- crimeAll2019[,c("Month","Crime.type")]
      crimeByType <- count(crimeByType,Crime.type)
        
      pie(crimeByType$freq, 
          labels = crimeByType$Crime.type, 
          main = "Pie chart of total crimes in London for 2019")
    }, height = 800, width = 1200)
    
    output$tableCrimeByType <- DT::renderDataTable({
      crimeByType <- crimeAll2019[,c("Month","Crime.type")]
      crimeByType <- count(crimeByType,"Crime.type")
      
      print(crimeByType)
    })
    
    output$DoughnutChart <- renderPlot({
      crimeByType <- crimeAll2019[which(crimeAll2019$Month == input$rbMonthDoughnut),]
      crimeByType <- crimeByType[,c("Month","Crime.type")]
      crimeByType <- count(crimeByType, Crime.type)
      
      crimeByType <- arrange(crimeByType,desc(Crime.type))
      crimeByType <- mutate(crimeByType, ypos = cumsum(n) - 0.5*n)
      
      ggplot(crimeByType, aes(x = 2, y = n, fill = Crime.type)) +
        geom_bar(width = 1, stat = "identity", color = "white") +
        coord_polar("y", start = 0)+
        geom_text(aes(y = ypos, label = n), color = "white")+
        theme_void()+
        xlim(0.5, 2.5)
    }, height = 800, width = 1200)
}

# Run the application 
shinyApp(ui = myui, server = myserver)
