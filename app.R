#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

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
        radioButtons("partype","ParentageType:",c("Sire Only" = "Father", "Dam Only"="Dam", "Both"="both")),
        sliderInput("threshold",
                     "Threshold:",
                     min = 0,
                     max = 1,
                     value = 0.3)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        tabsetPanel(
          tabPanel( title="DataTable",
            tableOutput("contents")
          ),
          tabPanel(title="Plot",
            plotOutput("distPlot", width="640px", height="640px",hover = "plot_hover")
          )
        )
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
   observe({
     
     file = input$file1
     if (is.null(file)){
       return (NULL)
     }
     contents <- read.csv(file$datapath, header = TRUE)
     
     output$distPlot <- renderPlot({
       # draw the histogram with the specified number of bins
       seqID <- contents$seqID
       nind <- length(seqID)
       fcolo <- rep("black", nind) 
       EMMrate <- contents[, paste0("mmrate", input$partype)] - contents[, paste0("exp.mmrate", input$partype)]
       plot(EMMrate ~ contents[, paste0(input$partype, "rel")], main = paste("Best", input$partype, "Matches"), xlab = "Estimated Relatedness", 
            ylab = "Excess mismatch rate",col=fcolo[match(contents$seqID,seqID)], cex=0.8)
       abline(v=input$threshold, col="grey")
     })
     
     output$contents <- renderTable({
       contents
     })
   })
}

# Run the application 
shinyApp(ui = ui, server = server)
