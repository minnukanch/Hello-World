#Classification using the Naive Bayes algorithm
#Loading the data into R
spam_data <- read.csv(file.choose())
View(spam_data)
#Exploring and preparing the data for analysis
#Having a brief look at the dataset
str(spam_data)
#Converting the type variable into factor
type_factor <- factor(spam_data$type, levels = c("ham","spam"))
str(type_factor)
#Using the table function to see the quantity in each category
table(spam_data$type)
#Cleaning and standardizing the text 
#Loading the text mining package 
#Re-creating the object using vector source
library(tm)
sms_corpus <- VCorpus(VectorSource(spam_data$text))
print(sms_corpus)
#Inspecting the corpus using the list functions
inpect_corpus <- inspect(sms_corpus[1:2])
#To see the first message of the corpus document
Char_corpus <- as.character(sms_corpus[[1]])
print(Char_corpus)
#To see many messages at the same time
many_corpus <- lapply(sms_corpus[1:4], as.character)
print((many_corpus))
#Cleaning the data
#Converting to lower-case letters
sms_corpus_clean <- tm_map(sms_corpus, content_transformer(tolower))
check_corpus <- as.character(sms_corpus_clean[[1]])
print(check_corpus)
#Removing the numbers
sms_corpus_clean <- tm_map(sms_corpus_clean, removeNumbers) 
as.character(sms_corpus_clean[[23]])
#Removing the stopwords
sms_corpus_clean <- tm_map(sms_corpus_clean, removeWords, stopwords())
#Removing the punctuation
replacePunctuation <- function(x){
  gsub("[[:punct:]]+", " ", x)
}
sms_corpus_clean <- tm_map(sms_corpus_clean, FUN = removePunctuation)
check_corpus <- as.character(sms_corpus_clean[[23]])
print(check_corpus)
#Stemming the words from the corpus
#loading the required package
install.packages("SnowballC")
library(SnowballC)
sms_corpus_clean <- tm_map(sms_corpus_clean, stemDocument)
#Stripping the white space accordingly
sms_corpus_clean <- tm_map(sms_corpus_clean, stripWhitespace)
as.character(sms_corpus_clean[[3]])
#Tokenization of the words
sms_dtm <- DocumentTermMatrix(sms_corpus_clean)
View(sms_dtm)
#Forming the DTM matrix from the raw data directly
sms_dtm2 <- DocumentTermMatrix(sms_corpus, control = list(tolower = TRUE,
                                                          removeNumbers = TRUE,
                        stopwords = function(x){
                          removeWords(x, stopwords())
                        },
                                                          removePunctuation = TRUE,
                                                          stemming = TRUE
))
sms_dtm
sms_dtm2
#Data Preparation - splitting into training and test set
sms_dtm_train <- sms_dtm[1:4169,]
sms_dtm_test <- sms_dtm[4170:5559,]
#Storing the labels or the target variables
sms_train_labels <- spam_data[1:4169,]$type
sms_test_labels <- spam_data[4170:5559,]$type
#To compare the propotion of the spam and ham data
test_prop1 <- prop.table(table(sms_train_labels))
print(test_prop1)
test_prop2 <- prop.table(table(sms_test_labels))
print(test_prop2)
#Installing the wordcloud package 
#To visualize through word cloud, the most appearing word
install.packages("wordcloud")
library(wordcloud)
#Creating the word cloud to find out the most common words
wordcloud(sms_corpus_clean, min.freq = 10, random.order = FALSE)
#Creating a word cloud for the non-copora spam and ham
spam <- subset(spam_data, type == "spam")
ham <- subset(spam_data, type == "ham")
wordcloud_spam <- wordcloud(spam$text, max.words = 40, scale = c(3,0.4), random.order = FALSE)
print(wordcloud_spam)
wordcloud_ham <- wordcloud(ham$text, max.words = 40, scale = c(3,0.4), random.order = FALSE)
print(wordcloud_ham)
#Finding out the most frequency words
#This is the last step in the data preparation process
find_freq <- findFreqTerms(sms_dtm_train, 5)
print(find_freq)
str(find_freq)
#Splitting into train and test sets using the most frequency words
sms_dtm_freq_train <- sms_dtm_train[, find_freq]
sms_dtm_freq_test <- sms_dtm_test[, find_freq]
#Since naive bayes obeys only to the categorical feature
#Creating a function to change Yes or No
convert_counts <- function(x){
  x <- ifelse(x > 0, "Yes", "No")
}
#To fix the apply function for converting into the desired matrix
sms_train <- apply(sms_dtm_freq_train, MARGIN = 2, convert_counts)
sms_test <-  apply(sms_dtm_freq_train, MARGIN = 2, convert_counts)
View(sms_train)
#Loading the package for applying the naive bayes algorithm
library("e1071")
sms_classifier <- naiveBayes(sms_train, sms_train_labels)
sms_test_pred <- predict(sms_classifier, sms_test)
#Loading the gmodels package to evaluate the performance
library(gmodels)
CrossTable(sms_test_pred, sms_test_labels, prop.chisq = FALSE, prop.t = FALSE,
           dnn = c('predicted', 'actual'))
#Evaluating the performance of the model
sms_classifier2 <- naiveBayes(sms_train, sms_train_labels, laplace = 1)
sms_test_pred2 <- predict(sms_classifier, sms_test)
#Using the cross table
CrossTable(sms_test_pred2, sms_test_labels, prop.chisq = FALSE, prop.t = FALSE,
           dnn = c('predicted', 'actual'))

##################################THE END############################################





