library(shiny)
library(shinythemes)
library(data.table)
library(plyr)
library(ggplot2)

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
    
    # Week 4
    
    output$bar <- renderPlot({
        crimeData <- crimeAll2019[which(crimeAll2019$Crime.type == input$siTypeBG),]
        
        totalCrimes <- count(crimeData, "Month")
        
        barplot(totalCrimes$freq,
                main = "Total number of crimes in London for each month",
                ylab = "Total",
                xlab = "Month",
                names.arg = totalCrimes$Month,
                col = rainbow(length(totalCrimes$Month))
                )
    })
    
    dataTotalCrime <- reactive({
      crimeData <- crimes2019[which(crimes2019$Crime.type == input$siTypeBG),]
      
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
    
    # Week 7
    
    output$scatter <- renderPlot({
      ggplot(crimeDensity, aes(x=Population, y=freq)) + 
        geom_point() +
        geom_text(aes(label = LSOA.name))
    }, height = 800, width = 1200)
    
    output$tableCrimeDensity <- renderTable({
        print(crimeDensity)
    })
}

# Run the application 
shinyApp(ui = myui, server = myserver)
