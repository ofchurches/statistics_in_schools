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

chocolate_raw_data <- readRDS(file = "chocolate_raw_data.Rda")

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("The relationship between price and cocoa content of chocolate bars - Halloween statistics with Parkside Primary School"),
  
  tabsetPanel(type = "tabs",
              tabPanel("Data",
                       
                       # Sidebar with a slider input for number of bins 
                       sidebarLayout(
                         sidebarPanel(
                           tags$h4("Data collected from supermarkets in Adelaide on 30/10/2018")
                         ),
                         
                         # Show a plot of the generated distribution
                         mainPanel(
                           tableOutput('tbl')
                         )
                       )
              ), 
              tabPanel("Histograms",
                       
                       # Sidebar with a slider input for number of bins 
                       sidebarLayout(
                         sidebarPanel(
                           tags$h4("Histograms of the two variables - price per 100G and percent cocoa"), 
                           radioButtons("show_hist", "Show the maker",
                                        c("No" = "No", 
                                          "Yes" = "Yes"))
                         ),
                         
                         # Show a plot of the generated distribution
                         mainPanel(
                           plotOutput("cocoaHist"), 
                           plotOutput("priceHist")
                         )
                       )
              ), 
              tabPanel("Scatter",
                       
                       # Sidebar with a slider input for number of bins 
                       sidebarLayout(
                         sidebarPanel(
                           tags$h4("Scatter plot of the two variables - price per 100G and percent cocoa"), 
                           radioButtons("show_scat", "Show the maker",
                                        c("No" = "No", 
                                          "Yes" = "Yes"))
                         ),
                         
                         # Show a plot of the generated distribution
                         mainPanel(
                           plotOutput("scatter")
                         )
                       )
              )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$tbl <- renderTable({chocolate_raw_data})  
  
  output$cocoaHist <- renderPlot({
    
    if(input$show_hist == "Yes"){
      
      ggplot(data = chocolate_raw_data, aes(x = `Percent cocoa solids (%)`, fill = Maker)) + 
        geom_histogram(binwidth = 10) + 
        labs(title = "Percent cocoa solids (%)")
      
    } else {
      
      ggplot(data = chocolate_raw_data, aes(x = `Percent cocoa solids (%)`)) + 
        geom_histogram(binwidth = 10) + 
        labs(title = "Percent cocoa solids (%)")
      
    }
    
  })
  
  output$priceHist <- renderPlot({
    
    if(input$show_hist == "Yes"){
      
      ggplot(data = chocolate_raw_data, aes(x = `Price per 100 G`, fill = Maker)) + 
        geom_histogram(binwidth = 1) + 
        labs(title = "Price per 100 G")
      
    } else {
      
      ggplot(data = chocolate_raw_data, aes(x = `Price per 100 G`)) + 
        geom_histogram(binwidth = 1) + 
        labs(title = "Price per 100 G")
      
    }
    
  })
  
  output$scatter <- renderPlot({
    
    if(input$show_scat == "Yes"){
      
      ggplot(data = chocolate_raw_data, aes(x = `Price per 100 G`, y = `Percent cocoa solids (%)`, colour = Maker)) + 
        geom_point(alpha = .5, size = 10)
      
    } else {
      
      ggplot(data = chocolate_raw_data, aes(x = `Price per 100 G`, y = `Percent cocoa solids (%)`)) + 
        geom_point(alpha = .5, size = 10)
      
    }
    
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)