# Title: Savings Growth Shiny App 
# Inputs: N/A
# Outputs : Shiny App (Check Shiny App Upload)
# Description: the script for a Shiny App that displays 3 different 

library(shiny)
library(ggplot2)
library(tidyverse)

# Loading Applications for R Shiny Application 

#' @title annuity
#' @description calculates the future value of an annuity compounded yearly
#' @param contrib contribution (i.e. how much you deposit at the end of each year)
#' @param rate annual rate of return
#' @param years number of years
#' @return calculated value of future annuity

annuity <- function(contrib, rate, years){
  if (rate == 0) {
    return(years * contrib)
  }
  return(contrib * ((((1 + rate)^years) - 1)/rate))
}



#' @title future_value
#' @description calculates the future value of a compounded interest function
#' @param amount initial invested amount
#' @param rate annual rate of return
#' @param years number of years
#' @return calculated future value

future_value <- function(amount, rate, years){
  amount * (1 + rate)^years
}

#' @title growing_annuity
#' @description calculates the future value of a growing annuity compounded yearly
#' @param contrib contribution (i.e. how much you deposit at the end of each year)
#' @param growth growth rate
#' @param rate annual rate of return
#' @param years number of years
#' @return calculated value of a growing annuity

growing_annuity <- function(contrib, growth, rate, years){
  if (years == 0) {
    return(0)
  }
  if (rate == 0) {
    return(sum(((1 + growth)^(1:years) * contrib)))
    } else if (growth == 0){
      return(annuity(contrib, rate, years))
    }else if  (rate == growth){
      return(contrib*years * (1 + rate)^(years - 1)*(1 + rate))
    } else {
  return(contrib * ((1 + rate)^years - (1 + growth)^years)/(rate - growth))}
}

# Define UI for application that draws a graph and spits out a text output
ui <- fluidPage(
  
  # Application title
  titlePanel("Savings Returns Application"),
  h5("Calculate your Returns on Savings Utilizing 3 Different Contribution Strategies"),
  
  # Sliders and Selectors for a number of variables
  fluidRow(
    column(4,
      sliderInput("initial_amount",
                  "Initial Amount",
                  min = 0,
                  max = 100000,
                  value = 1000,
                  step = 200,
                  pre = "$"),
      sliderInput("annual_contribution",
                  "Annual Contribution",
                  min = 0,
                  max = 50000,
                  value = 2000,
                  step = 500,
                  pre = "$")),
      column(4,
      sliderInput("return_rate",
                  "Return Rate (In Percentage)",
                  min = 0,
                  max = 20,
                  value = 5,
                  step = 0.1,
                  post = "%"),
      sliderInput("growth_rate",
                  "Growth Rate (In Percentage)",
                  min = 0,
                  max = 20,
                  value = 2,
                  step = 0.1,
                  post = "%")),
      column(4,
      sliderInput("timer",
                  "Years",
                  min = 0,
                  max = 50,
                  value = 20,
                  step = 1),
      selectInput("facet",
                  "Facet?",
                  c("No", "Yes"))
      )
    ,
    
    # Show a plot of the generated distribution
    mainPanel(
      h4("Timelines"),
      plotOutput("distPlot", width = "150%"),
      
      # Show a chart regarding the balances of the different contribution stratgies 
      h4("Balances"),
      verbatimTextOutput("view")
    )
  )
)

# Define server logic required produce out plot and graph
server <- function(input, output) {
  
  # Defining a Reactive Framework/Dataframe to work with
  data <- reactive({
    #Predefining variables
    times <- 0:input$timer
    initial_amount = input$initial_amount
    annual_contribution = input$annual_contribution
    return_rate = input$return_rate/100
    growth_rate = input$growth_rate/100
    
    #defining each of the column vectors
    mode_1 <- c()
    mode_2 <- c()
    mode_3 <- c()
    
    #for loop to fill out data table
    for (i in 1:length(times)){
      # mode 1 calc
      mode_1[i] <- future_value(initial_amount, rate = return_rate, years = times[i])
      
      # mode 2 calc
      mode_2[i] <- future_value(initial_amount, rate = return_rate, years = times[i]) + annuity(contrib = annual_contribution, rate = return_rate, years = times[i])
      
      # mode 3 calc
      mode_3[i] <- future_value(initial_amount, rate = return_rate, years = times[i]) + growing_annuity(contrib = annual_contribution, rate = return_rate, growth = growth_rate, years = times[i])
    }
    
    #creating modalities dataframe
    modalities <- 
      data.frame(year = times, no_contrib = mode_1, fixed_contrib = mode_2, growing_contrib = mode_3)
      
    modalities
  })
  
  #Defining the output plot 
  output$distPlot <- renderPlot({
    # Plot for the non-faceted data
    non_facet <- 
      ggplot(data = gather(data(), key = "Invest_type", value = "amount", 2:4) %>%
               mutate(Invest_type = factor(Invest_type, levels = c("no_contrib", "fixed_contrib", "growing_contrib"))), 
             aes(x = year, y = amount, color = Invest_type)) +
      geom_line() + 
      geom_point(size = .8) +
      xlab("Number of Years") +
      ylab("Total Value of Savings") +
      ggtitle("Value Timelines of 3 Different Investment Portfolios") +
      scale_color_manual("Ivestment Type", 
                         values = c("red2", "green2", "blue2"),
                         labels = c("No Contribution", "Fixed Contribution", "Growing Contribution"))
    
    # A plot for facted data
    facet_plot <- 
      ggplot(data = gather(data(), key = "Invest_type", value = "amount", 2:4) %>%
               mutate(Invest_type = factor(Invest_type, levels = c("no_contrib", "fixed_contrib", "growing_contrib"))), 
             aes(x = year, y = amount, group = Invest_type, fill = Invest_type, color = Invest_type)) +
      geom_area(aes(color = Invest_type), alpha=0.6) + 
      geom_point(size = .8) +
      xlab("Number of Years") +
      ylab("Total Calue of Savings") +
      ggtitle("Value Timelines of 3 Different Investment Portfolios") +
      facet_wrap(.~Invest_type, labeller = as_labeller(c('no_contrib' = "No Contribution", 
                                                         'fixed_contrib' = "Fixed Contribution", 
                                                         'growing_contrib' = "Growing Contribution"))) + 
      theme(legend.position = "none")
        
    # Defining a system for which plot that we decide to use
    if (input$facet == "Yes"){
      return(facet_plot)
    } 
    
    non_facet
  
    })
  
  # Developing a view option by which we can export a table 
  output$view <- renderPrint({
    (data())
  })

    

}

# Run the application 
shinyApp(ui = ui, server = server)

