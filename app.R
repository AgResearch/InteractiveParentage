#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Interactive Parentage Threshold"),
   
   # Sidebar with a slider input for threshold
   sidebarLayout(
      sidebarPanel(
        fileInput("file1", "Choose CSV parentage File", accept= c(
          "text/csv",
          "text/comma-separated-values,text/plain",
          ".csv")
        ),
        sliderInput("rel_threshold",
                     "Rel Threshold:",
                     min = 0,
                     max = 1,
                     value = 0.3),
        sliderInput("emm_threshold",
                    "Mismatch Rate Threshold:",
                    min = 0,
                    max = 1,
                    step=0.001,
                    value = 0.015),
        downloadButton("downloadData", "Download")
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        tabsetPanel(
          tabPanel( title="DataTable",
            tableOutput("contents")
          ),
          tabPanel(title="Father Plot",
            plotOutput("distPlotFather", width="900px", height="640px",hover = "plot_hover")
          ),
          tabPanel(title="Mother Plot",
                   plotOutput("distPlotMother", width="900px", height="640px",hover = "plot_hover")
          )
        )
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
   observe({
     
     dataIn <- reactive({
       file = input$file1
       if (is.null(file)){
         return (NULL)
       }
       df <- read.csv(file$datapath, header = TRUE)
       return(df)
     })
     
     output$distPlotFather <- renderPlot({
       
       if(is.null(dataIn())){
         return(NULL)
       }
       
       displayData <- dataIn()
       
       if ("Fatherrel" %in% names(displayData)){
       
         # draw the histogram with the specified number of bins
         partype <- "Father"
         ggplot(displayData, aes(x=Fatherrel, y=mmrateFather-exp.mmrateFather, col=as.factor(BestFatherMatch)), group=as.factor(BestFatherMatch)) + 
           geom_point() +
           geom_vline(xintercept=input$rel_threshold) +
           geom_hline(yintercept=input$emm_threshold)
       }else{
         return(NULL)
       }
     })
     
     output$distPlotMother <- renderPlot({
       
       if(is.null(dataIn())){
         return(NULL)
       }
       
       displayData <- dataIn()
       
       if ("Motherrel" %in% names(displayData)){
         
         # draw the histogram with the specified number of bins
         partype <- "Mother"
         ggplot(displayData, aes(x=Motherrel, y=mmrateMother-exp.mmrateMother, col=as.factor(BestMotherMatch)), group=as.factor(BestMotherMatch)) + 
           geom_point() +
           geom_vline(xintercept=input$rel_threshold) +
           geom_hline(yintercept=input$emm_threshold)
       }else{
         return(NULL)
       }
     })
     
     output$contents <- renderTable({
       if(is.null(dataIn())){
         return(NULL)
       }
       dataIn()
     })
     
     output$downloadData <- downloadHandler(
       filename = "matches.csv",
       content = function(file_name){
         write.csv(dataIn(),file=file_name,row.names=FALSE, quote=FALSE)
       }
     )
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

