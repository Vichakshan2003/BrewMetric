library(shiny)
library(janitor)
library(tidyverse)
library(ggplot2)
data = read.csv("Starbucks Datathon File.csv")
mean(data$Calories)
mean(data$Sodium..mg.)
mean(data$Sugars..g.)
mean(data$Protein..g.)
# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Starbucks Nutritional Analysis"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      sliderInput("bins",
                  "Number of bins:",
                  min = 1,
                  max = 50,
                  value = 30),
      radioButtons("variablechoice","Choice of Nutrient",
                   choices = c("Calories", "Sugars..g.", "Protein..g.", "Sodium..mg."), 
                   selected = "Calories"), 
      
      selectInput(inputId = "XVar",
                  label = "Select your 1st Nutrient",
                  choices = c("Calories", "Sugars..g.", "Protein..g.", "Sodium..mg."), 
                  selected = "Calories"),
      
      selectInput(inputId = "YVar",
                  label = "Select your 2nd Nutrient",
                  choices = c("Calories", "Sugars..g.", "Protein..g.", "Sodium..mg."), 
                  selected = "Calories"),
      sliderInput("inputCalories", "Desired Calories", min = 0, max = max(data$Calories, na.rm = TRUE), value = 100),
      sliderInput("inputSugars", "Desired Sugars (g)", min = 0, max = max(data$Sugars..g., na.rm = TRUE), value = 10),
      sliderInput("inputProtein", "Desired Protein (g)", min = 0, max = max(data$Protein..g., na.rm = TRUE), value = 5),
      sliderInput("inputSodium", "Desired Sodium (mg)", min = 0, max = max(data$Sodium..mg., na.rm = TRUE), value = 100),
      
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot"),
      textOutput("label"),
      verbatimTextOutput("summaryVar"),
      plotOutput("scatterplot"),
      textOutput("recommendations")
      
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output){
  output$distPlot <- renderPlot({
    if(input$variablechoice == "Calories") { x = data[, 4] }
    if(input$variablechoice == "Protein..g."){ x = data[, 13]}
    if(input$variablechoice == "Sugars..g.") { x = data[, 12] }
    if(input$variablechoice == "Sodium..mg.") { x = data[, 8] }
    
    bins = seq(min(x), max(x), length.out = input$bins +1)
    
    #make the histogram
    hist(x, breaks = bins, col = "lightblue", border = "black",
         xlab = input$variablechoice,
         main=paste("Histogram of", input$variablechoice))
  })
  output$recommendations <- renderText({
    # Calculate distances and select top 5
    distances <- sqrt((data$Calories - input$inputCalories)^2 +
                        (data$Sugars..g. - input$inputSugars)^2 +
                        (data$Protein..g. - input$inputProtein)^2 +
                        (data$Sodium..mg. - input$inputSodium)^2)
    
    data$Distance <- distances
    # Sorting and selecting the top 5 closest matches
    recommendations <- data %>%
      arrange(Distance) %>%
      select(Beverage_category, Beverage, Distance) %>%
      head(5)
    
    # Formatting the recommendations for display
    recommendationText <- paste("Top 5 Recommendations:\n", 
                                apply(recommendations, 1, function(x) paste(x[1], "-", x[2], "\n")), 
                                collapse = "")
    return(recommendationText)
  })
  
  output$summaryVar = renderPrint({
    if(input$variablechoice == "Calories") { x = data[, 4] }
    if(input$variablechoice == "Protein..g."){ x = data[, 13]}
    if(input$variablechoice == "Sugars..g.") { x = data[, 12] }
    if(input$variablechoice == "Sodium..mg.") { x = data[, 8] }
    summary(x)
    
  })
  
  output$label = renderText({
    paste("Summary of", input$variablechoice,":")
  })
  
  output$scatterplot = renderPlot({
    Starby = data[ ,c(input$XVar, input$YVar, input$Color)]
    
    plot(Starby[,1],Starby[,2],
         xlab = colnames(Starby)[1],
         ylab = colnames(Starby)[2],
         main = paste("Scatterplot of", input$XVar, "Vs", input$YVar),
         col = "violet", pch =15
    )
    
    model = lm(Starby[,2] ~Starby[,1])
    abline(model, col= "blue", lwd = 2)
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)