#Witnessing the data in R 
teens_data <- read.csv(file.choose())
View(teens_data)
#Glimpse of the data using the str function
str(teens_data)
#To find out the number of NA values in the categorical data
table_teen <- table(teens_data$gender, useNA = "ifany")
table_teen
#To find out the number of NA values in the numeric data
summary(teens_data$age)
#Fixing the age of the teenagers since it is not accurate
#Viewing the summary and checking
teens_data$age <- ifelse(teens_data$age >= 13 & teens_data$age < 20,
                         teens_data$age, NA)
summary(teens_data$age)
View(teens_data$age)
#Creating the dummy variables to use the NA values 
#such that larger part of the data is not missed
#Checking the data with help of table function
teens_data$female <- ifelse(teens_data$gender == "F" & 
                              !is.na(teens_data$gender),1,0)
teens_data$no_gender <- ifelse(is.na(teens_data$gender), 1,0)
table(teens_data$gender, useNA = "ifany")
table(teens_data$female, useNA = "ifany")
table(teens_data$no_gender, useNA = "ifany")
#Calculating the mean of the age variable
#Aggregating the age by the grad year
mean(teens_data$age, na.rm = TRUE)
aggregate(data = teens_data, age ~ gradyear, mean, na.rm = TRUE)
ave_age <- ave(teens_data$age, teens_data$gradyear, 
               FUN = function(x){
                 mean(x, na.rm = TRUE)
               })
#Replacing the NA values in the dataframe
teens_data$age <- ifelse(is.na(teens_data$age), ave_age, teens_data$age)
summary(teens_data$age)
#Training a model on the data
#Loading the package
library(stats)
interests <- teens_data[5:40]
interests_z <- as.data.frame(lapply(interests, scale))
summary(interests_z)
View(interests_z)
#Setting the seed such that results match the output that follow
#Formulating the k-means
set.seed(2345)
teen_clusters <- kmeans(interests_z, 5)
teen_clusters
#seeing the size of the cluster
size <- teen_clusters$size
size
#Looking at the co-ordinates of the clusters
co <- teen_clusters$centers
#Adding the clusters to the data frame 
teens_data$clusters <- teen_clusters$cluster
View(teens_data)
teens_data[1:5, c("cluster", "gender", "age", "friends")]
aggregate(data = teens_data, age ~ clusters, mean)
aggregate(data = teens_data,   no_gender ~ clusters, mean)

fviz_cluster(teen_clusters, data = teens_data[5:40], 
             pallette = c("2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"),
             ellipse.type = "euclid",
             star.plot = TRUE,
             repel = TRUE,
             ggtheme = theme_minimal()
)


?kmeans()

