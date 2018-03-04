#Importing the data set into R
#Converting certain variables into certain factors
insur_data <- read.csv(file.choose(), stringsAsFactors = TRUE)
View(insur_data)
#Getting a brief summary of the data
str(insur_data)
#Summary statistics
sum <- summary(insur_data$expenses)
print(sum)
#Visualizing the distribution and loading the package
library(ggplot2)
hist_p <- hist(insur_data$expenses)
#Seeing the distribution with table for factor variables
table(insur_data$region)
#Exploring the relationship between the variables
#before fitting the linear regression
cor_matrix <- cor(insur_data[c("age", "bmi", "children", "expenses")])
print(cor_matrix)
#Visualizing the features in a scatter-plot matrix
visual_dat <- pairs(insur_data[c("age", "bmi", "children", "expenses")])
print(visual_dat)
#Intalling and loading the library 
#Adding more information to the plot
install.packages("psych")
library(psych)
library(stats)
sca_plot <- pairs.panels(insur_data[c("age", "bmi", "children", "expenses")]) 
#Training a model on the plot
#changing the reference level
insur_data$sex <- relevel(insur_data$sex, ref = "male")
ins_model <- lm(expenses ~ age + children + bmi + sex + smoker + region, data = insur_data)
print(ins_model)
#Summary of the model performance
summary(ins_model) 
#To improve the model perfromance
#Added a non-linear term for age
#Created an indicator for obesity
#Specified an interaction between obesity and smoking
insur_data$age2 <- insur_data$age ^ 2
insur_data$bmi30 <- ifelse(insur_data$bmi >= 30, 1, 0)
ins_model_two <- lm(expenses ~ age + age2 + children + bmi + sex + bmi30*smoker +
                  region, data = insur_data)
#Summary of the model
summary(ins_model_two)


