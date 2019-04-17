library(shiny)
library(ggplot2)

# Define UI for workout2
ui <- fluidPage(
   
   # Application title
   titlePanel("Tracking Different Investment/Savings Methods"),
   
   # Input Widgets
   fluidRow(
      column(4,
        sliderInput("initial_amount",
                    "Initial Amount:",
                    min = 0,
                    max = 100000,
                    step = 500,
                    value = 1000,
                    pre = "$"),
        sliderInput("annual_contribution",
                    "Annual Contribution:",
                    min = 0,
                    max = 50000,
                    step = 500,
                    value = 2000,
                    pre = "$")),
      column(4, 
        sliderInput("return_rate",
                    "Return Rate:",
                    min = 0,
                    max = 20,
                    step = 0.1,
                    value = 5,
                    post = "%"),
        sliderInput("growth_rate",
                    "Growth Rate:",
                    min = 0,
                    max = 20,
                    step = 0.1,
                    value = 2,
                    post = "%")
        ),
      column(4, 
        sliderInput("years",
                    "Years:",
                    min = 0,
                    max = 50,
                    step = 1,
                    value = 20),
        selectInput("facet_type",
                    "Facet?",
                    c("No","Yes"))
        )
      ),
   #Output the Plot
   fluidRow(
     h4("Timelines"),
     plotOutput("plot")
   ),
   #Output the data table
   fluidRow(
     h4("Balances"),
     tableOutput("balances")
   )
)


# Define server logic required to draw the plot and produce the data table
server <- function(input, output) {
  
  #Creating functions
  #' @title future_value
  #' @description Calculate the amount of money after a period of time with a given rate of return 
  #' @param amount a numeric value of the initial invested amount
  #' @param rate a double of the annual rate of return
  #' @param years an integer for amount of years
  #' @return the computes the the future value of an investment 
  
  future_value <- function(amount, rate, years) {
    value <- amount*((1+rate)^(years))
    return(value)
  }
  
  #' @title annuity
  #' @description Calculate the amount of money after a period of time with a given rate of return with a yearly investment
  #' @param contrib a numeric value of the contribution amount
  #' @param rate a double of the annual rate of return
  #' @param years an integer for amount of years
  #' @return the computes the the future value of an investment 
  
  annuity <- function(contrib, rate, years) {
    value <- contrib*((((1+rate)^years)-1)/rate)
    return(value)
  }
  
  #' @title annuity
  #' @description Calculate the amount of money after a period of time with a given rate of return with a growingyearly investment
  #' @param contrib a numeric value of the first contribution
  #' @param rate a double of the annual rate of return
  #' @param growth a double of the annual growth of investment
  #' @param years an integer for amount of years
  #' @return the computes the the future value of an investment 
  
  growing_annuity <- function(contrib, rate, growth, years) {
    if (rate == growth){
      return (contrib)
    }
    else {
      value <- contrib*((((1+rate)^years)-((1+growth)^years))/(rate-growth))
      return(value)
    }
  }

  
  #Output Graph
    output$plot <- renderPlot({
       #creating vectors to store intermediate values
        years <- input$years
        initial_amount <- input$initial_amount
        if (years == 0){
          modalities <-  data.frame(Year = 0L,no_contrib = as.double(initial_amount), fixed_contrib = as.double(initial_amount), growing_contrib = as.double(initial_amount))
        }
        else {
          annual_contribution <- input$annual_contribution
          return_rate <- (input$return_rate/100)
          growth_rate <- (input$growth_rate/100)
          no_contrib <- c(initial_amount,1:years)
          fixed_contrib <- c(initial_amount,1:years)
          growing_contrib <- c(initial_amount,1:years)
            
           for(i in 1:years){
            fv <- future_value(amount=initial_amount,rate=return_rate,years=i)
            fva <- annuity(annual_contribution,return_rate,i)
            fvga <- growing_annuity(annual_contribution,return_rate, growth_rate,i)
            no_contrib[i+1] <- fv
            fixed_contrib[i+1] <- fva + fv
            growing_contrib[i+1] <- fvga + fv
           }
            modalities <-  data.frame(Year = 0L:years,no_contrib = no_contrib,fixed_contrib = fixed_contrib, growing_contrib = growing_contrib)
          
        }
     if (input$facet_type == "No"){
        #display data graph
          ggplot(data= modalities,aes(Year))+ 
            geom_line(aes(y=no_contrib,color="#red"))+
            geom_line(aes(y=fixed_contrib,color="blue")) +
            geom_line(aes(y=growing_contrib, color = "green"))+
            geom_point(aes(y=no_contrib,color="#red"))+
            geom_point(aes(y=fixed_contrib,color="blue")) +
            geom_point(aes(y=growing_contrib, color = "green"))+
            ggtitle("Comparison of 3 Investment Methods") +
            theme( axis.line = element_line(colour = "black", size = 1, linetype = "solid")) + 
            scale_x_continuous(name = "Time (years)", breaks= 0:years, limits = c(0,years)) + 
            scale_y_continuous(name = "Balance ($)", limits = c(0,NA)) +
            scale_color_discrete(name = "Investment Method", labels = c("Only Initial Investment", "Initial Investment + Annuity", "Initial Investment + Growing Annuity")) +
            theme(legend.background = element_rect(size=0.5, linetype="solid", colour ="black"))
      }
      else{
        #create data frame 
          facet_modalities <- data.frame(Year = 0L:years,value = c(no_contrib,fixed_contrib,growing_contrib),Type = c(rep(c("No Contribution","Annual Contribution","Growing Annual Contribution"),each = years+1)))
          facet_modalities$Type <- factor(facet_modalities$Type, levels = c("No Contribution","Annual Contribution","Growing Annual Contribution"))
        #display faceted graph
          ggplot(data= facet_modalities,aes(x=Year,color = Type, fill = Type))+ 
            geom_line(aes(y=value)) + 
            geom_area(aes(y=value), position = "identity", alpha = 0.4) + 
            geom_point(aes(y=value),size = 0.5) + 
            facet_wrap(~Type) +
            ggtitle("Comparison of 3 Investment Methods") +
            theme( axis.line = element_line(colour = "black", size = 1, linetype = "solid")) + 
            scale_x_continuous(name = "Time (years)", breaks= 0:years, limits = c(0,years)) + 
            scale_y_continuous(name = "Balance ($)",limits = c(0,NA)) +
            theme(legend.background = element_rect(size=0.5, linetype="solid", colour ="black"))
      }
    })
  #Output Data Table
    output$balances <- renderTable({
      years <- input$years
      initial_amount <- input$initial_amount
      if (years == 0){
        expr = data.frame(Year = 0L,no_contrib = as.double(initial_amount), fixed_contrib = as.double(initial_amount), growing_contrib = as.double(initial_amount))
      }
      else {
       annual_contribution <- input$annual_contribution
       return_rate <- (input$return_rate/100)
       growth_rate <- (input$growth_rate/100)
       no_contrib <- c(initial_amount,1:years)
       fixed_contrib <- c(initial_amount,1:years)
       growing_contrib <- c(initial_amount,1:years)
      
       for(i in 1:years){
          fv <- future_value(amount=initial_amount,rate=return_rate,years=i)
          fva <- annuity(annual_contribution,return_rate,i)
          fvga <- growing_annuity(annual_contribution,return_rate, growth_rate,i)
          no_contrib[i+1] <- fv
          fixed_contrib[i+1] <- fva + fv
          growing_contrib[i+1] <- fvga + fv
       }
         expr = data.frame(Year = 0:years,no_contrib = no_contrib,fixed_contrib = fixed_contrib, growing_contrib = growing_contrib)
      }
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

