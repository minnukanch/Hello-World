#Importing and viewing the data in R
b_data <- read.csv(file.choose())
View(b_data)
#Getting a brief view of the data in R
str(b_data)
#Eliminating the identity variable, so that erroneous findings could be avoided
b_data <- b_data[,-1]
View(b_data)
#Inpecting the diagnosis variable
table(b_data$diagnosis)
#The target feature should be coded as a factor in machine learning
b_data_diagnosis <- factor(b_data$diagnosis, levels = c("B", "M"),
                           labels = c("Benign", "Malignant"))
#Calculating the propotion and rounding it
r_data <- round(prop.table(table(b_data_diagnosis)) * 100, digits = 1)
print(r_data)
#Getting the summary of the above data
sum_data <- summary(b_data[c("radius_mean", "area_mean", "smoothness_mean")])
print(sum_data)
#Normalizing the features to a standard range of values to avoid dominations
normalize_data <- function(x){
  return((x - min(x)) / (max(x) - min(x)))
}
testing_function <- normalize_data(x = c(1,2,3,4,5))
#Applying  the above function to the data frame
b_data_n <- as.data.frame(lapply(b_data[,2:31], normalize_data))
View(b_data_n)
#To see the summary of the above converted data
sum_dat1 <- summary(b_data_n$area_mean)
print(sum_dat1)
#Data Preparation - creating training and test data
b_data_train <- b_data_n[1:469,]
View(b_data_train)
b_data_test <- b_data_n[470:569,] 
View(b_data_test)
#Storing the class variables which is necesaary to use the knn model
b_data_train_labels <- b_data[1:469,1]
b_data_test_labels <- b_data[470:569,1]
#Loading the library class package
library(class)
b_data_pred <- knn(train = b_data_train, test = b_data_test, 
                   cl = b_data_train_labels, k = 21)
print(b_data_pred)
# Using the cross-validation model in order to evaluate the performance of the model
library(gmodels)
cross_data <- CrossTable(x = b_data_test_labels, y = b_data_pred,
                         prop.chisq = FALSE)
#Using the z-score tranformation for improving the predictive accuracy
b_data_z <- as.data.frame(scale(b_data[,-1]))
View(b_data_z)
#Getting the summary of the above data
summary(b_data_z$area_mean)
#Data Preparation - dividing into training and test data set
z_train <- b_data_z[1:469,]
z_test <- b_data_z[470:569,]
z_train_labels <- b_data[1:469, 1]
z_test_labels <-  b_data[470:569, 1]
z_pred <- knn(train = z_train, test = z_test, cl = z_train_labels, k = 21) 
#validating with the helpof cross validation table
Z_validate <- CrossTable(x = z_test_labels, y = z_pred,
                         prop.chisq = FALSE)

#################################The End###########################################






