
##############################
##  pres41.R: Code for Week 4, Day #1
##  Note: Corresponds to pres41.pdf.
##        See pres41.pdf for references.
##  Author: Marshall A. Taylor
##############################

### BEGIN ###

#Set working directory
setwd("set_working_directory")


#Necessary packages
install.packages(c("quanteda", "devtools", "stringr", "NLP", "tm", 
                   "textstem", "textreg", "ggplot2"))
devtools::install_github("quanteda/quanteda.corpora")
library(quanteda)
library(quanteda.corpora)
library(stringr)
library(NLP)
library(tm)
library(textstem)
library(textreg)
library(ggplot2)


#Load the pre-built SOTU corpus
sotu.corpus <- quanteda.corpora::data_corpus_sotu

#Inspect the corpus
sotu.corpus$documents$texts[1] #First entry
sotu.corpus$documents$President[1]
sotu.corpus$documents$texts[239] #Last entry
sotu.corpus$documents$President[239]


#Which SOTU's specifically mention "immigration" words?
      #Code adapted from here: https://cbail.github.io/SICSS_Dictionary-Based_Text_Analysis.html
  #Make the dictionary
  immigr_dict <- c("immigration", "immigrant", "immigrate")
  
  #Take care of a little bit of preprocessing
    #Turn it into a tm corpus for cleaning
    sotu.corpus.tm <- VCorpus(VectorSource(sotu.corpus$documents$texts))
  
    #Clean it up
    removeAllPunct <- function(x) gsub("[[:punct:]]", " ", x)
    sotu.corpus.tm <- tm_map(sotu.corpus.tm, removeAllPunct) #I used this instead
                                                #of removePunctuation so that I
                                                #could replace the punctuation with a
                                                #a space. this prevents words with,
                                                #say, dashes from running together
    sotu.corpus.tm <- tm_map(sotu.corpus.tm, PlainTextDocument)
    removeSpecialChars <- function(x) gsub("[^a-zA-Z0-9 ]", " ", x)
    #removeSpecialChars function from here: https://stackoverflow.com/questions/30994194/quotes-and-hyphens-not-removed-by-tm-package-functions-while-cleaning-corpus
    sotu.corpus.tm <- tm_map(sotu.corpus.tm, removeSpecialChars)
    sotu.corpus.tm <- tm_map(sotu.corpus.tm, PlainTextDocument)
    sotu.corpus.tm <- tm_map(sotu.corpus.tm, content_transformer(tolower))
    sotu.corpus.tm <- tm_map(sotu.corpus.tm, removeNumbers)
    sotu.corpus.tm <- tm_map(sotu.corpus.tm, removeWords, stopwords("english"))
    sotu.corpus.tm <- tm_map(sotu.corpus.tm, stripWhitespace)
    
    #Let's not stem, but instead lemmatize (i.e, convert every word to its base
        #dictionary word
    sotu.corpus.tm2 <- convert.tm.to.character(sotu.corpus.tm)
    sotu.corpus.tm2 <- lemmatize_strings(sotu.corpus.tm2, 
                                      dictionary = lexicon::hash_lemmas)
    
    #Take a look at the cleaned, lemmatized texts (first speech)
    sotu.corpus.tm2[1]
    
    #Attach the cleaned docs back to our dataset, but separate from the "texts"
      #object, which contrains the uncleaned texts.
    sotu.corpus$documents$docs_clean <- sotu.corpus.tm2
  

  #Extract the relevant SOTUs
  sotu.data <- as.data.frame(sotu.corpus$documents)
  sotu_immigr <- sotu.data[str_detect(sotu.data$docs_clean, 
                                      "immigration|immigrant|immigrate"),]
  
  #Some text examples (using the raw texts)
  head(sotu_immigr$texts, 1)
  
  #Who were the presidents who talked about immigration?
  head(sotu_immigr$President, 77) #Alternatively, since I want to see all 77 who 
                      #mention immigration, I could just use sotu_immigr$President
                      #without the head() function, which is only necessary if you
                      #want to see the top entries
  
  #When?
  head(sotu_immigr$Date, 39)


#Time trend
  #First, convert to a DTM so we can count the number of times the dictionary words
    #are used
  sotu.dtm <- DocumentTermMatrix(sotu.corpus.tm)
  
  #Count the number of times each dictionary item shows up
  immigr.count <- as.matrix(DocumentTermMatrix(sotu.corpus.tm, 
                                               list(dictionary = immigr_dict)))
  rownames(immigr.count) <- rownames(sotu.data)
  
  #Get the row sums so you get the total number of times the dictionary words 
    #are used per SOTU
  immigr.sum <- rowSums(immigr.count)
  
  #"Normalize" by total SOTU word count so you can control for differences in
    #speech length, then attach it back to our dataset
  total.words <- rowSums(as.matrix(sotu.dtm))
  immigr.normal <- immigr.sum/total.words
  sotu.data <- cbind(sotu.data, immigr.normal)
  
  #Visualize the time trend
  sotu.data <- sotu.data[order(sotu.data$Date),]
  sotu.data$Date <- as.Date(sotu.data$Date)

  ggplot(sotu.data, aes(x = Date, y = immigr.normal, group = as.factor(party))) +
    geom_line(aes(color = as.factor(party))) +
    ylab("Proportion of Immigration Words in SOTU") +
    xlab("Address Date") +
    theme_bw() +
    theme(axis.title = element_text(face = "bold"),
          legend.title = element_text(face = "bold")) +
    scale_color_manual(name = "Party\nAffiliation",
                       breaks = c("Democratic", "Democratic-Republican",
                                  "Federalist", "Independent",
                                  "Republican", "Whig"),
                       values = c("#2332A9", "#fdb507", "#ec008c",
                                  "#417c7c", "#990000", "#00b159"))
  
  #Box plot
  ggplot(sotu.data, aes(x = as.factor(party), y = immigr.normal, 
                        fill = as.factor(party))) +
    geom_col() +
    ylab("Proportion of Immigration Words in SOTU") +
    xlab("") +
    theme_bw() +
    theme(axis.title = element_text(face = "bold"),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          legend.title = element_text(face = "bold"),
          legend.position = "bottom") +
    scale_fill_manual(name = "Party\nAffiliation",
                       breaks = c("Democratic", "Democratic-Republican",
                                  "Federalist", "Independent",
                                  "Republican", "Whig"),
                       values = c("#2332A9", "#fdb507", "#ec008c",
                                  "#417c7c", "#990000", "#00b159")) 
  
#Save your data
save.image("dictionaries.RData")

