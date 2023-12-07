library(shiny)
library(caret)

 data <- read.csv("D:\\Customer\\Customers_seg_dataset1.csv")

data$gender <- as.factor(data$gender)
data$age <- as.numeric(data$age)
data$spending.score <- as.numeric(data$spending.score)

# Encode gender
data$gender <- as.numeric(factor(data$gender, levels = c("Male", "Female")))

# Split the data into training and testing sets
set.seed(123)
trainIndex <- createDataPartition(data$salary_in_k., p = 0.8, 
                                  list = FALSE, 
                                  times = 1)
train_data <- data[trainIndex, ]
test_data <- data[-trainIndex, ]

model <- lm(salary_in_k. ~ gender + age + spending.score, data = train_data)

ui <- fluidPage(
  titlePanel("Salary Prediction App"),
  sidebarLayout(
    sidebarPanel(
      selectInput("gender", "Select Gender:", choices = c("Male", "Female")),
      numericInput("age", "Enter Age:", value = 25, min = 18, max = 100),
      numericInput("spending_score", "Enter Spending Score:", value = 50, min = 0, max = 100),
      actionButton("predict_button", "Predict Salary")
    ),
    mainPanel(
      textOutput("predicted_salary")
    )
  )
)

server <- function(input, output) {
  predictions <- eventReactive(input$predict_button, {
    if (!is.null(input$gender) && !is.null(input$age) && !is.null(input$spending_score)) {
      new_data <- data.frame(
        gender = as.numeric(factor(input$gender, levels = c("Male", "Female"))),
        age = input$age,
        spending.score = input$spending_score
      )
      prediction <- predict(model, newdata = new_data)
      formatted_prediction <- scales::dollar(round(prediction * 1000, 2))
      return(formatted_prediction)
    } else {
      return("Please provide all input values.")
    }
  })
  
  output$predicted_salary <- renderText({
    predictions()
  })
}

shinyApp(ui,server)
