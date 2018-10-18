library(ggplot2)
library(shiny)
library(scales)

ui <- fluidPage(
  
  titlePanel("How to win on 'The Price is Right'"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Slider for the answer ----
      sliderInput("answer", "Move the slider to select an answer:",
                  min = 1, max = 1000,
                  value = 500), 
      textOutput("selected_var")
      
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Plot ----
      plotOutput(outputId = "sysPlot"), 
      plotOutput(outputId = "guessPlot")
      
    )
  )
)

server <- function(input, output) {
  
  sliderValues <- reactive({
    
    input$answer
    
  })
  
  framed_sys <- reactive({
    
    lower_limit <- 1
    upper_limit <- 1000
    
    answer <- sliderValues()
    
    guesses <- floor(mean(c(lower_limit, upper_limit)))
    count_sys <- 1
    
    this_guess <- guesses[count_sys]
    
    while (this_guess != answer){
      
      if (guesses[count_sys] < answer){
        
        lower_limit <- c(lower_limit, guesses[count_sys])
        upper_limit <- c(upper_limit, upper_limit[count_sys])
        
        count_sys <- count_sys + 1
        
        guesses <- c(guesses, ceiling(mean(c(lower_limit[count_sys], upper_limit[count_sys]))))
        
      } else {
        
        lower_limit <- c(lower_limit, lower_limit[count_sys])
        upper_limit <- c(upper_limit, guesses[count_sys])
        
        count_sys <- count_sys + 1
        
        guesses <- c(guesses, floor(mean(c(lower_limit[count_sys], upper_limit[count_sys]))))
        
      }
      
      this_guess <- guesses[count_sys]
      
    }
    
    counter_sys <- 1:count_sys
    framed <- data.frame(guesses, lower_limit, upper_limit, counter_sys)
    
    return(framed)
    
  })
  
  
  output$sysPlot <- renderPlot({
    
    if (input$answer == 500){
      
      framed <- data.frame(x = c(0, 1), y = c(0, 1))
      
      ggplot(data = framed, aes(x = x, y = y)) + 
        geom_text(x = .5, y = .5, label = "If the answer is 500 you get it on the first attempt") + 
        labs(x = "", y = "")
      
    } else {
      
      ggplot(data = framed_sys(), aes(x = counter_sys)) + 
        geom_ribbon(aes(ymin = lower_limit, ymax = upper_limit), fill = "red", alpha = .5) + 
        geom_line(aes(y = guesses)) + 
        scale_x_continuous(breaks = c(0, 1,4,7,10), limits = c(0, 10)) +
        labs(title = "Systematically guessing the mean of the range", x = "Attempt", y = "Guess on each attempt\n(range within this guess is in red)")
      
    }
  })
  
  
  framed_guess <- reactive({
    
    lower_limit <- 1
    upper_limit <- 1000
    
    answer <- sliderValues()
    
    guesses <- floor(sample(lower_limit:upper_limit, 1))
    count_guess <- 1
    
    this_guess <- guesses[count_guess]
    
    while (this_guess != answer){
      
      if (guesses[count_guess] < answer){
        
        lower_limit <- c(lower_limit, guesses[count_guess])
        upper_limit <- c(upper_limit, upper_limit[count_guess])
        
        count_guess <- count_guess + 1
        
        guesses <- c(guesses, ceiling(sample(lower_limit[count_guess]:upper_limit[count_guess], 1)))
        
      } else {
        
        lower_limit <- c(lower_limit, lower_limit[count_guess])
        upper_limit <- c(upper_limit, guesses[count_guess])
        
        count_guess <- count_guess + 1
        
        guesses <- c(guesses, floor(sample(lower_limit[count_guess]:upper_limit[count_guess], 1)))
        
      }
      
      this_guess <- guesses[count_guess]
      
    }
    
    counter_guess <- 1:count_guess
    framed <- data.frame(guesses, lower_limit, upper_limit, counter_guess)
    
    return(framed)
    
  })
  
output$guessPlot <- renderPlot({
  
    ggplot(data = framed_guess(), aes(x = counter_guess)) + 
      geom_ribbon(aes(ymin = lower_limit, ymax = upper_limit), fill = "red", alpha = .5) + 
      geom_line(aes(y = guesses)) + 
      scale_x_continuous(breaks = pretty_breaks(), limits = c(0, max(framed_guess()$counter_guess))) + 
      labs(title = "Random guessing within range", x = "Attempt", y = "Guess on each attempt\n(range within this guess is in red)", 
           title = "The black line shows the best guess on each turn, the red region is the range the answer could be in on each turn")
    
    
  })
  
  output$selected_var <- renderText({ 
    
    if (max(framed_guess()$counter_guess) < max(framed_sys()$counter_sys)){
      
      "Guessing got there got there faster on this trial"
      
    } else {
      
      "Finding the mean of the range systematically got there faster on this trial"
      
    }
  })
  
}

shinyApp(ui, server)