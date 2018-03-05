#Load the dataset
usedcars <- read.csv(file.choose())
View(usedcars)
#To explore the dataset
str(used_car)
#summarize numeric variables
summary(usedcars$year)
summary(usedcars[c("price", "mileage")])
#Examples for calculating the median and the mean
#Calculate the mean income
(36000 + 44000 + 56000) / 3
mean(c(36000, 44000, 56000))
#The median income
median(c(36000, 44000, 56000))
#Range of used car prices
range(usedcars$price)
#The difference o range
diff(range(usedcars$price))
#IQR for used car prices
IQR(usedcars$price)
#Quantile to calculate five-number summary
quantile(usedcars$price)
#To see the percentage of the data exisiting in the certain intervals 
quantile(usedcars$price, probs = c(0.01, 0.99))
#TO decide the quantile distribution with the help of seq
quantile(usedcars$price, seq(from = 0, to = 1, by = 0.20))
#Boxplot of used car prices and mileage
boxplot(usedcars$price, main="Boxplot of Used Car Prices",
        ylab="Price ($)")
boxplot(usedcars$mileage, main="Boxplot of Used Car Mileage",
        ylab="Odometer (mi.)")
#Histograms of used car prices and mileage
hist(usedcars$price, main = "Histogram of Used Car Prices",
     xlab = "Price ($)")
hist(usedcars$mileage, main = "Histogram of Used Car Mileage",
     xlab = "Odometer (mi.)")
#Variance and standard deviation of the used car data
var(usedcars$price)
sd(usedcars$price)
var(usedcars$mileage)
sd(usedcars$mileage)
## Exploring numeric variables
#One-way tables for the used car data
table(usedcars$year)
table(usedcars$model)
table(usedcars$color)
#Compute table proportions
model_table <- table(usedcars$model)
prop.table(model_table)
#Round the data
color_table <- table(usedcars$color)
color_pct <- prop.table(color_table) * 100
round(color_pct, digits = 1)
##Exploring relationships between variables 
#Scatterplot of price vs. mileage
plot(x = usedcars$mileage, y = usedcars$price,
     main = "Scatterplot of Price vs. Mileage",
     xlab = "Used Car Odometer (mi.)",
     ylab = "Used Car Price ($)")
#New variable indicating conservative colors
usedcars$conservative <-
  usedcars$color %in% c("Black", "Gray", "Silver", "White")
#Checking our variable
table(usedcars$conservative)
#Crosstab of conservative by model
library(gmodels)
CrossTable(x = usedcars$model, y = usedcars$conservative)
