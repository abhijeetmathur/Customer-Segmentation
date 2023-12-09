library(caret)
library(tidyverse)

data <- read.csv("D:\\Customer\\Customers_seg_dataset1.csv")

data$gender <- as.factor(data$gender)
data$age <- as.numeric(data$age)
data$spending.score <- as.numeric(data$spending.score)

data$gender <- as.numeric(factor(data$gender, levels = c("Male", "Female")))

set.seed(123)
trainIndex <- createDataPartition(data$salary_in_k., p = 0.8, list = FALSE, times = 1)
train_data <- data[trainIndex, ]
testing_data <- data[-trainIndex, ]

model <- lm(salary_in_k. ~ gender + age + spending.score, data = train_data)
predictions = predict(model,newdata = testing_data)

rmse <- sqrt(mean((testing_data$salary_in_k. - predictions)^2))

range_of_target_variable <- max(testing_data$salary_in_k.) - min(testing_data$salary_in_k.)

percentage_accuracy <- (range_of_target_variable - rmse) / range_of_target_variable * 100

cat("Percentage Accuracy:", percentage_accuracy, "%\n")
