
##############################
##  pres42.R: Code for Week 34, Day #2
##  Note: Corresponds to pres42.pdf.
##        See pres42.pdf for references.
##  Author: Marshall A. Taylor
##############################

### BEGIN ###

#Set working directory
setwd("set_working_directory")


#Necessary packages
install.packages(c("devtools", "ggplot2", "quanteda", 
                   "quanteda.corpora", "tm", "textreg",
                   "texstem", "plyr", "tidyr",
                   "dplyr", "syuzhet", "reshape2"))
library(devtools)
install_github("trinker/lexicon")
install_github("trinker/sentimentr")
install_github("quanteda/quanteda.corpora")
library(quanteda)
library(quanteda.corpora)
library(lexicon)
library(sentimentr)
library(tm)
library(textreg)
library(textstem)
library(ggplot2)
library(plyr)
library(tidyr)
library(dplyr)
library(syuzhet)
library(reshape2)

#Load in and preprocess data
sotu.corpus <- quanteda.corpora::data_corpus_sotu


#A little cleaning
  #Turn into a tm corpus
  sotu.corpus.tm <- VCorpus(VectorSource(sotu.corpus$documents$texts))

  #Some preprocessors (we need punctuation, so don't remove it)
  sotu.corpus.tm <- tm_map(sotu.corpus.tm, content_transformer(tolower))
  sotu.corpus.tm <- tm_map(sotu.corpus.tm, removeNumbers)
  sotu.corpus.tm <- tm_map(sotu.corpus.tm, stripWhitespace)

  inspect(sotu.corpus.tm[[179]])

  #Convert the corpus to a character vector to use for sentiment analysis
  sotu.corpus.tm <- convert.tm.to.character(sotu.corpus.tm)

#Sentiment analysis using sentimentr
  #Take a look at some the available sentiment dictionaries
  sample_n(lexicon::hash_sentiment_jockers_rinker, 20)
  sample_n(lexicon::hash_sentiment_nrc, 20)

  #Some valence shifters:
      #1 = Neutral context word
      #2 = Negator
      #3 = Amplifier
      #4 = De-Amplifier
  lexicon::hash_valence_shifters[which(lexicon::hash_valence_shifters$y==3)]

  #Sentence-level sentiment
  sentences <- get_sentences(sotu.corpus.tm)
  sent.sentiment <- sentiment(sentences)

  #Visualize the sentence-level sentiment in the LBJ's 1964 SOTU
  ggplot(sent.sentiment[which(sent.sentiment$element_id==179),], 
         aes(x = sentence_id, y = sentiment,
             fill = ifelse(sent.sentiment$sentiment[which(
               sent.sentiment$element_id==179)] < 0, "0", "1"))) +
    geom_histogram(stat = "identity") +
    xlab("Sequence (in Sentences)") + ylab("Sentiment Polarity") +
    ylim(-1,1) +
    theme_bw() +
    theme(axis.title = element_text(face = "bold"),
          legend.position = "none")

  #What are the positive and negative words in this SOTU?
  extract_sentiment_terms(sotu.corpus$documents$texts[179])

  #Take a look at the sentiment-coded SOTU transcript
  sentiment.bySOTU <- sentiment_by(sentences, by = NULL) 
  highlight(sentiment.bySOTU)

  #Sentiment by different SOTUs
  presidents <- as.data.frame(sotu.corpus$documents$President)
  colnames(presidents) <- "President"
  presidents$element_id <- 1:nrow(presidents)
  sent.sentiment <- join(sent.sentiment, presidents, by = "element_id")
  
  ggplot(sent.sentiment[which(sent.sentiment$element_id>=214),], 
         aes(x = as.factor(element_id), y = sentiment,
         fill = as.factor(President))) +
    geom_boxplot(aes(fill = as.factor(President)), 
                 outlier.shape = NA) +
    geom_jitter(width = 0.2, alpha = 0.2) +
    labs(y = "Sentiment Polarity", x = "") +
    theme_bw() +
    theme(axis.title = element_text(face = "bold"),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          legend.title = element_text(face = "bold"),
          legend.position = "bottom") +
    scale_fill_discrete(name = "President", breaks = c("Clinton", "Bush", "Obama",
                                                     "Trump"))

  #Average sentiment by party
  party <- as.data.frame(sotu.corpus$documents$party)
  colnames(party) <- "party"
  party$element_id <- 1:nrow(party)
  sentiment.bySOTU <- join(sentiment.bySOTU, party, by = "element_id")
  
  ggplot(sentiment.bySOTU[which(sentiment.bySOTU$party=="Democratic" |
                                  sentiment.bySOTU$party=="Republican"),], 
         aes(x = as.factor(party), y = ave_sentiment,
             fill = as.factor(party))) +
    geom_boxplot(aes(fill = as.factor(party)), 
                 outlier.shape = NA) +
    geom_jitter() +
    labs(y = "Sentiment Polarity", x = "") +
    theme_bw() +
    theme(axis.title = element_text(face = "bold"),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          legend.title = element_text(face = "bold"),
          legend.position = "right") +
    scale_fill_manual(name = "Party",
                      breaks = c("Democratic", "Republican"),
                      values = c("#2332A9", "#990000"))

  #By party over time
  date <- as.data.frame(sotu.corpus$documents$Date)
  colnames(date) <- "date"
  date$element_id <- 1:nrow(date)
  sentiment.bySOTU <- join(sentiment.bySOTU, date, by = "element_id")
  
  sentiment.bySOTU <- sentiment.bySOTU[order(sentiment.bySOTU$date),]
  sentiment.bySOTU$date <- as.Date(sentiment.bySOTU$date)
  
  ggplot(sentiment.bySOTU[which(sentiment.bySOTU$party=="Democratic" |
                                  sentiment.bySOTU$party=="Republican"),], 
         aes(x = date, y = ave_sentiment, group = as.factor(party))) +
    geom_density(aes(fill = as.factor(party)), stat = "identity", alpha = .3) +
    ylab("Sentiment Polarity") +
    xlab("Address Date") +
    theme_bw() +
    theme(axis.title = element_text(face = "bold"),
          legend.title = element_text(face = "bold")) +
    scale_fill_manual(name = "Party\nAffiliation",
                       breaks = c("Democratic", "Republican"),
                       values = c("#2332A9", "#990000"))
  
  #Classify emotions
  emotions <- get_nrc_sentiment(sotu.corpus.tm)
  
  #Normalize emotion count by total word count
  emotions <- emotions/sentiment.bySOTU$word_count
  
  #Visualize emotion trends
  emotions <- melt(emotions, measure.vars = colnames(emotions))
  
  ggplot(emotions[which(emotions$variable!="positive" & emotions$variable!="negative"),], 
         aes(x = as.factor(variable), y = value*100,
             fill = as.factor(variable))) +
    geom_boxplot(aes(fill = as.factor(variable)),
                 outlier.shape = NA) +
    geom_jitter(width = 0.2, alpha = 0.2) +
    labs(y = "% of SOTU Words that are Emotion Words", x = "") +
    theme_bw() +
    theme(axis.title = element_text(face = "bold"),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          legend.title = element_text(face = "bold"),
          legend.position = "right") +
    guides(fill = guide_legend(title = "Emotion"))


#Save your R objects
save.image("sentiment.RData")

### END ###
