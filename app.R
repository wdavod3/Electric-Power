#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(ggplot2)
library(usmap)
library(DT)

#Set up powerData and clean up csv file
powerData <- read.table(file = "annual_generation_state.csv", sep = ",", header = TRUE)
genData <- as.numeric(gsub(",","",powerData$GENERATION..Megawatthours.))
powerData$GENERATION..Megawatthours. <- genData
newState <- subset(powerData, as.factor(powerData$STATE) != "  ")
powerData <- newState
powerData$STATE = toupper(powerData$STATE)
powerData$STATE <- as.factor(powerData$STATE)
powerData$TYPE.OF.PRODUCER <- as.factor(powerData$TYPE.OF.PRODUCER)
powerData$ENERGY.SOURCE <- as.factor(powerData$ENERGY.SOURCE)
powerData <- subset(powerData, powerData$GENERATION..Megawatthours. >= 0)
powerData <- subset(powerData, (as.factor(powerData$ENERGY.SOURCE) != "Other") &
                      (as.factor(powerData$ENERGY.SOURCE) != "Other Gases") &
                      (as.factor(powerData$ENERGY.SOURCE) != "Other Biomass") &
                      (as.factor(powerData$ENERGY.SOURCE) != "Pumped Storage"))
powerData <- subset(powerData, powerData$TYPE.OF.PRODUCER == "Total Electric Power Industry")
levels(powerData$ENERGY.SOURCE)[levels(powerData$ENERGY.SOURCE)=="Wood and Wood Derived Fuels"] <- "Wood"
levels(powerData$ENERGY.SOURCE)[levels(powerData$ENERGY.SOURCE)=="Natural Gas"] <- "Nat Gas"
levels(powerData$ENERGY.SOURCE)[levels(powerData$ENERGY.SOURCE)=="Solar Thermal and Photovoltaic"] <- "Solar"
levels(powerData$ENERGY.SOURCE)[levels(powerData$ENERGY.SOURCE)=="Hydroelectric Conventional"] <- "Hydro"
levels(powerData$ENERGY.SOURCE)[levels(powerData$ENERGY.SOURCE)=="Geothermal"] <- "Geo"
levels(powerData$ENERGY.SOURCE)[levels(powerData$ENERGY.SOURCE)=="Petroleum"] <- "Petro"
total <- subset(powerData, as.factor(powerData$ENERGY.SOURCE) == "Total")
powerData <- subset(powerData, as.factor(powerData$ENERGY.SOURCE) != "Total")

#Create a dataset to hold the percent for the data
colnames(total)[5] <- "TOTAL"
total$ENERGY.SOURCE <- NULL
percent <- merge(total, powerData, by = c("STATE", "suYEAR", "TYPE.OF.PRODUCER"))
percent <- transform(percent, PERCENT = GENERATION..Megawatthours. / TOTAL)
percent$TOTAL <- NULL
percent$GENERATION..Megawatthours. <- NULL

#Create a dataset to use for comparing the states
stateComp <- powerData
stateComp$TYPE.OF.PRODUCER <- NULL

stateCompP <- percent
stateCompP$TYPE.OF.PRODUCER <- NULL

#Make a list for the names of the states
stateNames <- c(state.name, "US-Total", "Washington D.C.")

#Create datasets to use for displaying the maps
mapNames <- powerData
colnames(mapNames)[2] <- "state"

mapPercent <- percent
colnames(mapPercent)[1] <- "state"

#Create lists to hold the names for the dropdown variables
choicesList <- c("Coal" = "Coal", "Geo" = "Geo", "Hydro" = "Hydro", "Nat Gas" = "Nat Gas", "Nuclear" = "Nuclear", "Petro" = "Petro", "Solar" = "Solar", "Wind" = "Wind", "Wood" = "Wood")
yearList <- c(1990:2019) 

# Create the shiny navbarPage
ui <- navbarPage(
  title = "Will Davidson CS 424 Project 1", 
  
    #Bar Graphs ------------------------------------------------------------------------------------------------------
    navbarMenu(title = "Stacked Bar Chart",
               tabPanel(title = "Energy Numbers",
                        plotOutput("barEnergy")),
               
               tabPanel(title =  "Energy Percent",
                        plotOutput("barPercent"))
               ),
  
    #Line Graphs ------------------------------------------------------------------------------------------------------
    navbarMenu(title = "Line Chart",
               tabPanel(title = "Energy Numbers", 
                        fluidRow(plotOutput("lineEnergy")),
                        
                        fluidRow(checkboxGroupInput("energy", label = "Energy Sources", choices = choicesList, 
                                                    selected = choicesList),
                                 actionLink("energyAll", label = "Select All")
                                 )
                        ),
               
               tabPanel(title =  "Energy Percent",
                        fluidRow(plotOutput("linePercent")),
                        
                        fluidRow(checkboxGroupInput("percent", label = "Energy Sources", choices = choicesList, 
                                                    selected = choicesList),
                                 actionButton("percentAll", label = "Select All")
                        )
                        
                        )
               ),
  
    #Table Data ------------------------------------------------------------------------------------------------------
    navbarMenu(title = "Table Data", 
               tabPanel(title = "Energy Numbers",
                        dataTableOutput("tableEnergy")),
               
               tabPanel(title =  "Energy Percent",
                        dataTableOutput("tablePercent"))
               ),
  
    #State comparison ------------------------------------------------------------------------------------------------------
    navbarMenu(title = "State Comparison",
               tabPanel(title = "Energy Numbers", 
                        fluidRow(column(2, selectInput("stateEnergy", label = "Select Energy", choices = choicesList, selected = "Coal")), 
                                  column(2, selectInput("yaerEnergy", label = "Select Year", choices = yearList, selected = "1990"))),
                        
                        fluidRow(selectInput("state1", label = "State 1", choices = stateNames, selected = "US-Total")),
                        fluidRow(column(4, plotOutput("stateBar1", height = 300)), column(4, plotOutput("stateLine1", height = 300)), column(2, dataTableOutput("stateTable1", height = 50))
                        ),
                        
                        fluidRow(selectInput("state2", label = "State 2", choices = stateNames, selected = "Illinois")),
                        fluidRow(column(4, plotOutput("stateBar2", height = 300)), column(4, plotOutput("stateLine2", height = 300)), column(2, dataTableOutput("stateTable2", height = 50))
                        )
                        
                ),
               
               tabPanel(title = "Energy Percent", 
                        fluidRow(column(2, selectInput("statePercent", label = "Select Energy", choices = choicesList, selected = "Coal")), 
                                 column(2, selectInput("yaerPercent", label = "Select Year", choices = yearList, selected = "1990"))),
                        
                        fluidRow(selectInput("state1p", label = "State 1", choices = percent$STATE, selected = "US-TOTAL")),
                        fluidRow(column(4, plotOutput("stateBar1p", height = 300)), column(4, plotOutput("stateLine1p", height = 300)), column(2, dataTableOutput("stateTable1p", height = 50))
                        ),
                        
                        
                        fluidRow(selectInput("state2p", label = "State 2", choices = percent$STATE, selected = "IL")),
                        fluidRow(column(4, plotOutput("stateBar2p", height = 300)), column(4, plotOutput("stateLine2p", height = 300)), column(2, dataTableOutput("stateTable2p", height = 50))
                        )
                        
                        )
               ),
  
    #Map comparison ------------------------------------------------------------------------------------------------------
    navbarMenu(title = "Map Comparison", 
               tabPanel(title = "Energy Numbers", 
                        fluidRow(column(2, selectInput("mapEnergyT", label = "Select Energy", choices = choicesList, selected = "Coal")), 
                                 column(2, selectInput("mapYearT", label = "Select Year", choices = yearList, selected = 1990))),
                        
                        fluidRow(plotOutput("map1", height = 350)),
                        
                        fluidRow(column(2, selectInput("mapEnergyB", label = "Select Energy", choices = choicesList, selected = "Coal")), 
                                 column(2, selectInput("mapYearB", label = "Select Year", choices = yearList, selected = 1990))),
                        
                        fluidRow(plotOutput("map2", height = 350))
                        ),
               
               
               tabPanel(title = "Energy Percent", 
                        fluidRow(column(2, selectInput("mapPercentT", label = "Select Energy", choices = choicesList, selected = "Coal")), 
                                 column(2, selectInput("mapPercentYT", label = "Select Year", choices = yearList, selected = 1990))),
                        
                        fluidRow(plotOutput("map3", height = 350)), 
                        fluidRow(column(2, selectInput("mapPercentB", label = "Select Energy", choices = choicesList, selected = "Coal")), 
                                 column(2, selectInput("mapPercentYB", label = "Select Year", choices = yearList, selected = 1990))),
                        
                        fluidRow(plotOutput("map4", height = 350))
                        
                        )
             ),
  
    #About ------------------------------------------------------------------------------------------------------
    tabPanel(title = "About", 
             p("This data presented in the app comes from https://www.eia.gov/electricity/data/state/. "),
             p("This data covers the different energy sources used for each state, Washington DC, and the toal for the US from the years 1990 to 2019. "),
             p("This app was written by Will Davidson on 2/13/2021.")
            )
)

server <- function(input, output) {
  
  #--------------------------------------- BAr Graphs
  output$barEnergy <- renderPlot({ggplot(powerData, aes(fill = ENERGY.SOURCE, y=GENERATION..Megawatthours., x=suYEAR)) + geom_bar(position="stack", stat="identity") + labs(title = "Energy Source Per Year", subtitle = "From 1990-2019", fill = "Energy Sources", x = "Years (yr)", y = "Energy (MWh)")})
  
  output$barPercent <- renderPlot({ggplot(powerData, aes(fill = ENERGY.SOURCE, y=GENERATION..Megawatthours., x=suYEAR)) +  geom_bar(position="fill", stat="identity") + labs(title = "Energy Source Percent Per Year", subtitle = "From 1990-2019", fill = "Energy Sources", x = "Years (yr)", y = "Energy Percent (%)")})


  #--------------------------------------- Line Graphs  
  energyData = reactive({
    return(powerData[powerData$ENERGY.SOURCE%in%input$energy,])
  })
  
  output$lineEnergy <- renderPlot({ggplot(data = energyData()) + stat_summary(aes(x=suYEAR, y=GENERATION..Megawatthours., color=ENERGY.SOURCE), geom = "line") + labs(title = "Energy Source Per Year", subtitle = "From 1990-2019", color = "Energy Sources", x = "Years (yr)", y = "Energy (MWh)")+ coord_cartesian(ylim = c(0,1e+08))})
  
  percentData = reactive({
    return(percent[percent$ENERGY.SOURCE%in%input$energy,])
  })
  
  output$linePercent <- renderPlot({ggplot(data = percentData()) + stat_summary(aes(x=suYEAR, y=PERCENT, color=ENERGY.SOURCE), geom = "line") + labs(title = "Energy Source Percent Per Year", subtitle = "From 1990-2019", color = "Energy Sources", x = "Years (yr)", y = "Energy Percent (%)") + coord_cartesian(ylim = c(0,1.0))})  

  #--------------------------------------- Tables
  output$tableEnergy <- DT::renderDataTable(
    DT::datatable({powerData}, options = list(pageLength = 15, lengthChange = FALSE), rownames = FALSE))
  
  output$tablePercent <- DT::renderDataTable(
    DT::datatable({percent}, options = list(pageLength = 15, lengthChange = FALSE), rownames = FALSE))
  
  #--------------------------------------- State Comparison for numbers
  #State 1
  stateData1e = reactive({
    return(powerData[powerData$ENERGY.SOURCE%in%input$stateEnergy,])
  })
  
  
  tableData1 = reactive({
    return(stateComp[stateComp$ENERGY.SOURCE%in%input$stateEnergy, ])
  })
  
  output$stateBar1 <- renderPlot({ggplot(data = stateData1e(), aes(fill = ENERGY.SOURCE, y=GENERATION..Megawatthours., x=suYEAR)) + geom_bar(position="stack", stat="identity") + labs(fill = "Energy Sources", x = "Years (yr)", y = "Energy (MWh)")})
  
  output$stateLine1 <- renderPlot({ggplot(data = stateData1e()) + stat_summary(aes(x=suYEAR, y=GENERATION..Megawatthours., color=ENERGY.SOURCE), geom = "line") + labs(color = "Energy Sources", x = "Years (yr)", y = "Energy (MWh)")+ coord_cartesian(ylim = c(0,1e+08))})

  output$stateTable1 <- DT::renderDataTable(
    DT::datatable({tableData1()}, options = list(pageLength = 5, lengthChange = FALSE, searching = FALSE), rownames = FALSE))  
 
  #State 2
  stateData2e = reactive({
    return(powerData[powerData$ENERGY.SOURCE%in%input$stateEnergy,])
  })
  
  
  tableData2 = reactive({
    return(stateComp[stateComp$ENERGY.SOURCE%in%input$stateEnergy, ])
  })
  
  output$stateBar2 <- renderPlot({ggplot(data = stateData2e(), aes(fill = ENERGY.SOURCE, y=GENERATION..Megawatthours., x=suYEAR)) + geom_bar(position="stack", stat="identity") + labs(fill = "Energy Sources", x = "Years (yr)", y = "Energy (MWh)")})
  
  output$stateLine2 <- renderPlot({ggplot(data = stateData2e()) + stat_summary(aes(x=suYEAR, y=GENERATION..Megawatthours., color=ENERGY.SOURCE), geom = "line") + labs(color = "Energy Sources", x = "Years (yr)", y = "Energy (MWh)")+ coord_cartesian(ylim = c(0,1e+08))})
  
  output$stateTable2 <- DT::renderDataTable(
    DT::datatable({tableData2()}, options = list(pageLength = 5, lengthChange = FALSE, searching = FALSE), rownames = FALSE))
  
  #--------------------------------------- State Comparison for percent
  #State 1
  output$stateBar1p <- renderPlot({ggplot(powerData, aes(fill = ENERGY.SOURCE, y=GENERATION..Megawatthours., x=suYEAR)) +  geom_bar(position="fill", stat="identity") + labs(fill = "Energy Sources", x = "Years (yr)", y = "Energy Percent (%)")})

  output$stateLine1p <- renderPlot({ggplot(data = percentData()) + stat_summary(aes(x=suYEAR, y=PERCENT, color=ENERGY.SOURCE), geom = "line") + labs(color = "Energy Sources", x = "Years (yr)", y = "Energy Percent (%)") + coord_cartesian(ylim = c(0,1.0))})  
  
  output$stateTable1p <- DT::renderDataTable(
    DT::datatable({stateCompP}, options = list(pageLength = 5, lengthChange = FALSE, searching = FALSE), rownames = FALSE))  
  
  #State 2
  output$stateBar2p <- renderPlot({ggplot(powerData, aes(fill = ENERGY.SOURCE, y=GENERATION..Megawatthours., x=suYEAR)) +  geom_bar(position="fill", stat="identity") + labs(fill = "Energy Sources", x = "Years (yr)", y = "Energy Percent (%)")})

  output$stateLine2p <- renderPlot({ggplot(data = percentData()) + stat_summary(aes(x=suYEAR, y=PERCENT, color=ENERGY.SOURCE), geom = "line") + labs(color = "Energy Sources", x = "Years (yr)", y = "Energy Percent (%)") + coord_cartesian(ylim = c(0,1.0))})  
  
  output$stateTable2p <- DT::renderDataTable(
    DT::datatable({stateCompP}, options = list(pageLength = 5, lengthChange = FALSE, searching = FALSE), rownames = FALSE))
  
  #------------------------------------- Map Comparison Energy
  #Map 1
  mapEnergy1 = reactive({
    return(mapNames[mapNames$ENERGY.SOURCE%in%input$mapEnergyT, ])
  })
  mapYear1 = reactive({
    return(mapNames[mapNames$suYEAR%in%input$mapYearT, ])
  })
  
  output$map1 <- renderPlot({plot_usmap(data = mapYear1(), values = "GENERATION..Megawatthours.", labels=FALSE) + scale_fill_continuous( low = "white", high = "orange", name = "Energy", label = scales::comma)  + theme(legend.position = "right")})
  
  #Map 2
  mapEnergy2 = reactive({
    return(mapNames[mapNames$ENERGY.SOURCE%in%input$mapEnergyB, ])
  })
  mapYear2 = reactive({
    return(mapNames[mapNames$suYEAR%in%input$mapYearB, ])
  })
  
  output$map2 <- renderPlot({plot_usmap(data = mapEnergy2(), values = "GENERATION..Megawatthours.", labels=FALSE) + scale_fill_continuous( low = "white", high = "orange", name = "Energy", label = scales::comma)  + theme(legend.position = "right")})

  #------------------------------------- Map Comparison Percent
  #Map 3
  mapEnergy3 = reactive({
    return(mapPercent[mapPercent$ENERGY.SOURCE%in%input$mapPercentT, ])
  })
  mapYear3 = reactive({
    return(mapPercent[mapPercent$suYEAR%in%input$mapPercentYT, ])
  })
  
  output$map3 <- renderPlot({plot_usmap(data = mapYear3(), values = "PERCENT", labels=FALSE) + scale_fill_continuous( low = "white", high = "orange", name = "Energy", label = scales::comma)  + theme(legend.position = "right")})
  
  #Map 4
  mapEnergy4 = reactive({
    return(mapPercent[mapPercent$ENERGY.SOURCE%in%input$mapPercentB, ])
  })
  mapYear4 = reactive({
    return(mapPercent[mapPercent$suYEAR%in%input$mapPercentYB, ])
  })
  
  output$map4 <- renderPlot({plot_usmap(data = mapEnergy4(), values = "PERCENT", labels=FALSE) + scale_fill_continuous( low = "white", high = "orange", name = "Energy", label = scales::comma)  + theme(legend.position = "right")})
  
  
}

shinyApp(ui = ui, server = server)
