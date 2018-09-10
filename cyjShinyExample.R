### cyjshiny example

#libraries and dependencies
library(shiny)
library(rcytoscapejs)

#import example dataframe 

## ui ##
ui <- fluidPage(
  ## app title 
  titlePanel("CyJShiny Example"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput('file1', 'Import Dataset',
                accept=c('text/csv', 
                         'text/comma-separated-values,text/plain', 
                         '.csv')),
      checkboxInput('header', 'Header', TRUE),
      radioButtons('sep', 'Separator',
                   c(Comma=',',
                     Semicolon=';',
                     Tab='\t'),
                   ','),
      radioButtons('quote', 'Quote',
                   c(None='',
                     'Double Quote'='"',
                     'Single Quote'="'"),
                   '"')
    ),
  mainPanel(
    tabsetPanel(
      tabPanel("Data", tableOutput("data")),
      tabPanel("Cytoscape", rcytoscapejsOutput("regcytoplot", height="600px"))
    )
  )
  
))



## server ##

server <- function (input, output){
  
  #import example data in reactive environment 
  CytoscapeTable.reg <- reactive({
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    
    data <- read.csv(inFile$datapath, header = input$header, sep=input$sep, 
                     quote = input$quote)
    
    #Format for cytoscape 
    ppifinal <- data.frame(Protein= data$Dataset, Interactor = data$Gene.Name)
    
    
    ppifinal
  })
  
  #render table 
  output$data <- renderTable(
    CytoscapeTable.reg()
  )
  
  #create reactive nodes
  nodeData.reg <- reactive({
    
    FilteredDataset <- CytoscapeTable.reg()
    id <- c(as.character(FilteredDataset$Interactor),levels(FilteredDataset$Protein))
    name <- id
    
    nodeData <- data.frame(id, name, stringsAsFactors=FALSE)
    
    return(nodeData)
    
  })
  
  #create reactive edges
  edge.Data.reg <- reactive({
    
    FilteredDataset <- CytoscapeTable.reg()
    
    source <- FilteredDataset$Protein
    target <- FilteredDataset$Interactor
    
    edge <- data.frame(source, target, stringsAsFactors=FALSE)
    
    
  })
  
  #Create a network
  network.reg <- reactive({
    
    nodeData <- nodeData.reg()
    edge.Data <- edge.Data.reg()
    createCytoscapeJsNetwork(nodeData, edge.Data)
    
  })
  
  #Reactive plot that renders regular cytoscape plot
  output$regcytoplot <- renderRcytoscapejs({
    
    network <- network.reg()
    rcytoscapejs(network$nodes, network$edges, showPanzoom=TRUE)
    
  })
  
  
}


## execute app 
shinyApp(ui = ui, server = server)
