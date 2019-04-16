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
   titlePanel("Drawing Balls Experiment"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         sliderInput("reps",
                     "Number of Repetitions",
                     min = 1,
                     max = 1000,
                     value = 100),
         sliderInput("threshold",
                     "Threshold for Choosing Boxes:",
                     min = 0,
                     max = 1,
                     value = 0.5),
         numericInput("seed", label = h3("Choose Random Seed"), value = 777)

      )
      ,
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("distPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  data <- reactive({threshold = input$threshold
           n = input$reps
           set.seed(input$seed)
           base <- data.frame(reps = 1:n)
           num0.vec <- c()
           num1.vec <- c()
           num2.vec <- c()
           num3.vec <- c()
           num4.vec <- c()
           
           sample.mat <- matrix(nrow = n, ncol =4)
           
           
           box1 <- c("blue", "blue","red")
           box2 <- c("blue", "blue", "red", "red", "red", "white")
           
           for (i in 1:n){
             if(runif(1) > threshold){
               sample.mat[i,] = sample(box1, 4, replace = TRUE)
             } else{
               sample.mat[i,] = sample(box2, 4, replace = FALSE)
             }
             blue = (sample.mat == "blue")
             row.sum = rowSums(blue)
             
             num0.vec[i] = sum((row.sum == 0), na.rm = TRUE)/i
             num1.vec[i] = sum((row.sum == 1), na.rm = TRUE)/i
             num2.vec[i] = sum((row.sum == 2), na.rm = TRUE)/i
             num3.vec[i] = sum((row.sum == 3), na.rm = TRUE)/i
             num4.vec[i] = sum((row.sum == 4), na.rm = TRUE)/i
             
           }
           
           base <- 
             base %>%
             mutate(
               zero = num0.vec,
               one = num1.vec,
               two = num2.vec,
               three = num3.vec,
               four = num4.vec)
          
            base
             })
  
   output$distPlot <- renderPlot({

     ggplot(data = data(), aes(x = reps)) + 
       geom_line(aes(y = zero, color = "0")) +
       geom_line(aes(y = one, color = "1")) +
       geom_line(aes(y = two, color = "2")) +
       geom_line(aes(y = three, color = "3")) +
       geom_line(aes(y = four, color = "4")) +
       scale_color_manual(name = "Colors", values = c("green", "blue", "red", "orange", "grey")) + 
       ggtitle("Relative Frequency of number of blue balls") +
       ylab("Frequency") +
       xlab("Repititions")
   })
   
}

# Run the application 
shinyApp(ui = ui, server = server)

