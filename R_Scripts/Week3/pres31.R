
##############################
##  pres31.R: Code for Week 3, Day #1
##  Note: Corresponds to pres31.pdf.
##        See pres31.pdf for references.
##  Author: Marshall A. Taylor
##############################

### BEGIN ###

#Set working directory
setwd("set_working_directory")


#Necessary packages
install.packages(c("NLP", "tm"))
library(NLP)
library(tm)


#Load in data
nyarticle.data <- read.csv(file = "nytimes_data.csv", 
                           header = T, row.names = 1)
nyarticle.data$snippet[1:5]


#Make the "snippet" variable into a corpus
ny.corpus <- VCorpus(VectorSource(nyarticle.data$snippet))

#Inspect the corpus
inspect(ny.corpus[1:5])

meta(ny.corpus[[1]])

for (i in 1:5) {
  inspect(ny.corpus[[i]])
} #To look at just, say, doc #1: inspect(ny.corpus[[1]])


#Preprocess the texts
  #Remove punctuation
  ny.corpus <- tm_map(ny.corpus, removePunctuation)
  inspect(ny.corpus[[1]])

  #Remove capitalization
  ny.corpus <- tm_map(ny.corpus, content_transformer(tolower))
  inspect(ny.corpus[[1]])
  
  #Remove numbers
  ny.corpus <- tm_map(ny.corpus, removeNumbers)
  inspect(ny.corpus[[1]])
  
  #Remove "stop words"
  stopwords("en") #View the stop words
  ny.corpus <- tm_map(ny.corpus, removeWords, stopwords("english"))
  inspect(ny.corpus[[1]])
  
  #"Stem" words
  ny.corpus <- tm_map(ny.corpus, stemDocument)
  inspect(ny.corpus[[1]])
  
  #Remove excess whitespace
  ny.corpus <- tm_map(ny.corpus, stripWhitespace)
  inspect(ny.corpus[[1]])
  
  #Take a look at a handful
  for (i in 1:5) {
    inspect(ny.corpus[[i]])
  }
  
  
#Turn preprocessed corpus into a document-term matrix
ny.dtm <- DocumentTermMatrix(ny.corpus)


#Inspect the DTM
inspect(ny.dtm)


#Turn the DTM into a data frame
ny.dtm.df <- as.data.frame(as.matrix(ny.dtm))


#Removing sparse terms
inspect(removeSparseTerms(ny.dtm, .95))
inspect(removeSparseTerms(ny.dtm, .9))
inspect(removeSparseTerms(ny.dtm, .5))
inspect(removeSparseTerms(ny.dtm, .1))


#Save your data
save.image("text_preprocessing.RData")

