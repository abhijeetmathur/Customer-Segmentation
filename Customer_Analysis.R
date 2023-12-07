customer_data = Customers_seg_dataset
customer_data


#Changing the name of the col name
colnames(customer_data) <- c('Customer ID','Gender','Age', 'Annual Income(k$)', 'Spending Score (1-100)')
customer_data


#Name of the col
names(customer_data)


#structure of a data set
str(customer_data)


##visualize data
data.table::data.table(customer_data)


#summary of a data
summary(customer_data)


#some statistical value of feature
sd(customer_data$Age)
sd(customer_data$`Annual Income(k$)`)
sd(customer_data$Gender)
IQR(customer_data$Age)
IQR(customer_data$`Annual Income(k$)`)


#Bar Plot
#For Gender
gender = table(customer_data$Gender)
barplot(gender, main="Using barplot to display Gender comparision",
        ylab ="Number",
        xlab ="Gender",
        col = rainbow(3),
        legend=rownames(gender),
        ylim=c(0, 200))


#Pie Chart
pct = round((gender/sum(gender))*100)
lbs = paste(c("Female", " Male")," ", pct, "%", sep=" ")
library(plotrix)
pie3D(gender, labels = lbs, main = " Pie Chart depicting ratio of Female and Male")


#Histogram

#For Age
hist(customer_data$Age,
     col ="red",
     xlab ='Age class',
     ylab = "Frequency",
     labels = TRUE,
     xlim = c(10,80),
     ylim = c(0,50))

#For Annual Income
hist(customer_data$`Annual Income(k$)`,
     col ="green",
     xlab ='Income class',
     ylab = "Frequency",
     labels = TRUE,
     xlim = c(0,140),
     ylim = c(0,50))

#For Spending Score
hist(customer_data$`Spending Score (1-100)`,
     col ="blue",
     xlab ='Spending',
     ylab = "Frequency",
     labels = TRUE,
     xlim = c(0,105),
     ylim = c(0,50))


#Boxplot

#For Age
boxplot(customer_data$Age,
        col = "blue",
        main = " Boxplot for Descriptive Analysis of age", labels = TRUE)

#For Spending Score
boxplot(customer_data$`Spending Score (1-100)`,
        col = "red",
        main = " Boxplot for Descriptive Analysis of Spending Score", labels = TRUE)


#Density Plot

#Density plot for Annual Income
plot(density(customer_data$`Annual Income(k$)`),
     col = "yellow",
     main = " Density plot for Annual Income",
     xlab = "Income",
     ylab = "Density")
polygon(density(customer_data$`Annual Income(k$)`), col = "yellow")

#spending
plot(density(customer_data$`Spending Score (1-100)`),
     col = "yellow",
     main = " Density plot for Annual Spending",
     xlab = "Income",
     ylab = "Density")
polygon(density(customer_data$`Spending Score (1-100)`), col = "green")