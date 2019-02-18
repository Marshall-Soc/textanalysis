
##############################
##  pres51.R: Code for Week 6, Day #1
##  Note: Corresponds to pres61.pdf.
##        See pres61.pdf for references.
##  Author: Marshall A. Taylor
##############################

### BEGIN ###

#Set working directory
setwd("set_working_directory")


#Necessary packages
install.packages(c("tm", "e1071", "gmodels", 
                   "textstem", "textreg",
                   "caret"))
library(tm)
library(e1071)
library(gmodels)
library(textstem)
library(textreg)
library(caret)


#Load in data
reviews <- read.csv(file = "food_reviews.csv", header = T, row.names = 1)


#Take a look at the categories we have
table(reviews$Score)


#Preprocess the texts
  reviews.tm <- VCorpus(VectorSource(reviews$Text))
  
  #Clean it up
  removeAllPunct <- function(x) gsub("[[:punct:]]", " ", x)
  removeSpecialChars <- function(x) gsub("[^a-zA-Z0-9 ]", " ", x)
  
  reviews.tm <- tm_map(reviews.tm, content_transformer(removeAllPunct))
  reviews.tm <- tm_map(reviews.tm, content_transformer(removeSpecialChars))
  reviews.tm <- tm_map(reviews.tm, content_transformer(tolower))
  reviews.tm <- tm_map(reviews.tm, removeNumbers)
  reviews.tm <- tm_map(reviews.tm, removeWords, stopwords("english"))
  reviews.tm <- tm_map(reviews.tm, stripWhitespace)
  
  reviews.tm <- convert.tm.to.character(reviews.tm)
  reviews.tm <- lemmatize_strings(reviews.tm, 
                                       dictionary = lexicon::hash_lemmas)
  
  reviews.tm <- VCorpus(VectorSource(reviews.tm)) #Converts character vector back to
                                              #tm corpus
  inspect(reviews.tm[[1]]) #Make sure everything looks ok (just taking a look at doc #1)


#Convert corpus to a DTM and remove sparse terms
reviews.dtm <- DocumentTermMatrix(reviews.tm)
reviews.dtm <- removeSparseTerms(reviews.dtm, .999)


#Break up data into training and testing data (most of the code from here on out is 
      #modified from here: https://www.rdocumentation.org/packages/e1071/versions/1.7-0.1/topics/naiveBayes)
reviews.raw.train <- reviews[1:1000,]
reviews.raw.test <- reviews[1001:nrow(reviews),]

reviews.dtm.train <- reviews.dtm[1:1000,]
reviews.dtm.test <- reviews.dtm[1001:nrow(reviews),]

reviews.tm.train <- reviews.tm[1:1000]
reviews.tm.test <- reviews.tm[1001:nrow(reviews)]


#Hopefully the proportions across the two categories are similar between
  #the training and test sets (they should be, since the reviews should be in the
  #data frame at random)
prop.table(table(reviews.raw.train$Score))
prop.table(table(reviews.raw.test$Score))


#The words across the testing and training sets should be the same. Following El-Beltagy
  #(2017, see slides for full reference), let's subset the columns to be the most 
  #frequent terms in the training set
freq_terms <- findFreqTerms(reviews.dtm.train, 10)
reviews.train <- as.matrix(DocumentTermMatrix(reviews.tm.train,
                                              list(dictionary = freq_terms)))
reviews.test <- as.matrix(DocumentTermMatrix(reviews.tm.test,
                                              list(dictionary = freq_terms)))


#Make our DTMs binary matrices
binarize <- function(x) {
  x <- ifelse(x > 0, 1, 0)
  x <- factor(x, levels = c(0, 1), labels = c("No", "Yes"))
}

reviews.train <- apply(reviews.train, MARGIN = 2, binarize)
reviews.test <- apply(reviews.test, MARGIN = 2, binarize)


#Document classification

  #Train the classifier on the training data of n = 10,000 docs
  reviews.nb.class <- naiveBayes(x = reviews.train, 
                                 y = reviews.raw.train$Score, laplace = 1)

  #Take a look at some of the conditional probabilities
  reviews.nb.class$table$awesome
  
  #Classify the remaining 4,000 reviews as finding the review to be either "bad" 
    #or "good" 
  reviews.test.pred <- predict(reviews.nb.class, reviews.test)
  

#How well did the classifier perform? We can look at what is called the 
  #"confusion matrix"
confusionMatrix(reviews.test.pred, reviews.raw.test$Score)


#Save your R objects
save.image("classification.RData")

### END ###
