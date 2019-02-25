
##############################
##  pres71.R: Code for Week 7, Day #1
##  Note: Corresponds to pres71.pdf.
##        See pres71.pdf for references.
##  Author: Marshall A. Taylor
##############################

### BEGIN ###

#Set working directory
setwd("set_working_directory")


#Necessary packages
install.packages(c("tm", "topicmodels", "ggplot2",
                   "quanteda.corpora", "reshape2",
                   "tidytext", "dplyr", "textstem",
                   "textreg", "LDAvis", "servr",
                   "pheatmap", "grid"))
library(tm)
library(topicmodels)
library(ggplot2)
library(quanteda.corpora)
library(reshape2)
library(tidytext)
library(dplyr)
library(textstem)
library(textreg)
library(LDAvis)
library(servr)
library(pheatmap)
library(grid)


#Load in data
load("data_corpus_sotu.rda")
sotus <- data_corpus_sotu$documents
sotus <- sotus[which(sotus$Date>="1989-02-09"),]


#Preprocess the texts
sotus.tm <- VCorpus(VectorSource(sotus$texts))

#Clean it up
removeAllPunct <- function(x) gsub("[[:punct:]]", " ", x)
removeSpecialChars <- function(x) gsub("[^a-zA-Z0-9 ]", " ", x)

sotus.tm <- tm_map(sotus.tm, content_transformer(removeAllPunct))
sotus.tm <- tm_map(sotus.tm, content_transformer(removeSpecialChars))
sotus.tm <- tm_map(sotus.tm, content_transformer(tolower))
sotus.tm <- tm_map(sotus.tm, removeNumbers)
sotus.tm <- tm_map(sotus.tm, removeWords, stopwords("english"))
sotus.tm <- tm_map(sotus.tm, stripWhitespace)

sotus.tm <- convert.tm.to.character(sotus.tm)
sotus.tm <- lemmatize_strings(sotus.tm, 
                              dictionary = lexicon::hash_lemmas)

sotus.tm <- VCorpus(VectorSource(sotus.tm)) #Converts character vector back to
#tm corpus
inspect(sotus.tm[[1]]) #Make sure everything looks ok (just taking a look at doc #1)


#Convert corpus to a DTM and remove sparse terms
sotus.dtm <- DocumentTermMatrix(sotus.tm)
sotus.dtm <- removeSparseTerms(sotus.dtm, .9)


#Estimate some topics
sotus.topics.10 <- LDA(sotus.dtm, k = 10, control = list(seed = 567))

  #What are the top terms per topic?
  terms(sotus.topics.10, 10)
  
  #Visualize the word distributions for top words per topic
  top.terms <- sotus.topics.10@beta
  colnames(top.terms) <- sotus.topics.10@terms 
  top.terms <- exp(top.terms)
  top.terms <- t(top.terms)
  colnames(top.terms) <- paste("topic_", 1:10, sep = "")
  
  top.terms <- melt(top.terms)
  top.terms <- top.terms %>% 
    group_by(Var2) %>%
    top_n(10, value) %>%
    ungroup() %>%
    arrange(Var2, -value) %>%
    mutate(order = row_number())
  
  ggplot(top.terms, aes(x = rev(order), y = value, fill = Var2)) +
    geom_bar(stat = "identity", color = "black") +
    ylim(0, .03) +
    xlab("") + ylab("") +
    facet_wrap(~Var2, scales = "free") +
    coord_flip() +
    guides(fill = F) +
    theme_bw() +
    scale_x_continuous(breaks = rev(top.terms$order),
                       labels = top.terms$Var1,
                       expand = c(0,0))
  

#Estimate a 5-topic solution
sotus.topics.5 <- LDA(sotus.dtm, k = 5, control = list(seed = 567))

  #Visualize the word distributions for top words per topic
  top.terms <- sotus.topics.5@beta
  colnames(top.terms) <- sotus.topics.5@terms 
  top.terms <- exp(top.terms)
  top.terms <- t(top.terms)
  colnames(top.terms) <- paste("topic_", 1:5, sep = "")
  
  top.terms <- melt(top.terms)
  top.terms <- top.terms %>% 
    group_by(Var2) %>%
    top_n(10, value) %>%
    ungroup() %>%
    arrange(Var2, -value) %>%
    mutate(order = row_number())
  
  ggplot(top.terms, aes(x = rev(order), y = value, fill = Var2)) +
    geom_bar(stat = "identity", color = "black") +
    ylim(0, .03) +
    xlab("") + ylab("") +
    facet_wrap(~Var2, scales = "free") +
    coord_flip() +
    guides(fill = F) +
    theme_bw() +
    scale_x_continuous(breaks = rev(top.terms$order),
                       labels = top.terms$Var1,
                       expand = c(0,0))
  

#Estimate a 20-topic solution
sotus.topics.20 <- LDA(sotus.dtm, k = 20, control = list(seed = 567))
  
  #Visualize the word distributions for top words per topic
  top.terms <- sotus.topics.20@beta
  colnames(top.terms) <- sotus.topics.20@terms 
  top.terms <- exp(top.terms)
  top.terms <- t(top.terms)
  colnames(top.terms) <- paste("topic_", 1:20, sep = "")
  
  top.terms <- melt(top.terms)
  top.terms <- top.terms %>% 
    group_by(Var2) %>%
    top_n(10, value) %>%
    ungroup() %>%
    arrange(Var2, -value) %>%
    mutate(order = row_number())
  
  ggplot(top.terms, aes(x = rev(order), y = value, fill = Var2)) +
    geom_bar(stat = "identity", color = "black") +
    ylim(0, .04) +
    xlab("") + ylab("") +
    facet_wrap(~Var2, scales = "free") +
    coord_flip() +
    guides(fill = F) +
    theme_bw() +
    scale_x_continuous(breaks = rev(top.terms$order),
                       labels = top.terms$Var1,
                       expand = c(0,0))
  

#Remove some extra terms
sotus.tm <- VCorpus(VectorSource(sotus$texts))

sotus.tm <- tm_map(sotus.tm, content_transformer(removeAllPunct))
sotus.tm <- tm_map(sotus.tm, content_transformer(removeSpecialChars))
sotus.tm <- tm_map(sotus.tm, content_transformer(tolower))
sotus.tm <- tm_map(sotus.tm, removeNumbers)
sotus.tm <- tm_map(sotus.tm, removeWords, stopwords("english"))
sotus.tm <- tm_map(sotus.tm, stripWhitespace)

sotus.tm <- convert.tm.to.character(sotus.tm)
sotus.tm <- lemmatize_strings(sotus.tm, 
                              dictionary = lexicon::hash_lemmas)

sotus.tm <- VCorpus(VectorSource(sotus.tm))
sotus.tm <- tm_map(sotus.tm, removeWords, c("will", "must", "can",
                                            "american", "america",
                                            "year", "people", "country"))
sotus.tm <- tm_map(sotus.tm, stripWhitespace)

sotus.dtm <- DocumentTermMatrix(sotus.tm)
sotus.dtm <- removeSparseTerms(sotus.dtm, .9)

sotus.topics.10.2 <- LDA(sotus.dtm, k = 10, control = list(seed = 567))

top.terms <- sotus.topics.10.2@beta
colnames(top.terms) <- sotus.topics.10.2@terms 
top.terms <- exp(top.terms)
top.terms <- t(top.terms)
colnames(top.terms) <- paste("topic_", 1:10, sep = "")

top.terms <- melt(top.terms)
top.terms <- top.terms %>% 
  group_by(Var2) %>%
  top_n(10, value) %>%
  ungroup() %>%
  arrange(Var2, -value) %>%
  mutate(order = row_number())

ggplot(top.terms, aes(x = rev(order), y = value, fill = Var2)) +
  geom_bar(stat = "identity", color = "black") +
  ylim(0, .02) +
  xlab("") + ylab("") +
  facet_wrap(~Var2, scales = "free") +
  coord_flip() +
  guides(fill = F) +
  theme_bw() +
  scale_x_continuous(breaks = rev(top.terms$order),
                     labels = top.terms$Var1,
                     expand = c(0,0))


#After playing around with different k, I think a 6-topic solution looks pretty good
sotus.topics.6 <- LDA(sotus.dtm, k = 6, control = list(seed = 567))

top.terms <- sotus.topics.6@beta
colnames(top.terms) <- sotus.topics.6@terms 
top.terms <- exp(top.terms)
top.terms <- t(top.terms)
colnames(top.terms) <- paste("topic_", 1:6, sep = "")

top.terms <- melt(top.terms)
top.terms <- top.terms %>% 
  group_by(Var2) %>%
  top_n(10, value) %>%
  ungroup() %>%
  arrange(Var2, -value) %>%
  mutate(order = row_number())

ggplot(top.terms, aes(x = rev(order), y = value, fill = Var2)) +
  geom_bar(stat = "identity", color = "black") +
  ylim(0, .02) +
  xlab("") + ylab("") +
  facet_wrap(~Var2, scales = "free") +
  coord_flip() +
  guides(fill = F) +
  theme_bw() +
  scale_x_continuous(breaks = rev(top.terms$order),
                     labels = top.terms$Var1,
                     expand = c(0,0))


#Interactive visualization with high prob and lift
json <- createJSON(phi = exp(sotus.topics.6@beta), #need to exponential to turn
                                            #the log probabilities back into
                                            #probabilities so rows sum to 1
                   theta = sotus.topics.6@gamma,
                   doc.length = rowSums(as.matrix(sotus.dtm)),
                   term.frequency = colSums(as.matrix(sotus.dtm)),
                   vocab = colnames(as.matrix(sotus.dtm)))
serVis(json, out.dir = "vis", open.browser = interactive())


#Engagement in the topics across SOTUs
  #Top topics per SOTU
  topics(sotus.topics.6, 1)
  
  #Heat map
  doc.topic <- sotus.topics.6@gamma
  colnames(doc.topic) <- c("One Great Nation", "New Jobs", "State of the Economy",
                           "Welfare State", "Children and Schooling", "War on Terror")
  rownames(doc.topic) <- sotus$`_document`
  
  draw_colnames_45 <- function (coln, ...) { #Code from here: https://stackoverflow.com/questions/15505607/diagonal-labels-orientation-on-x-axis-in-heatmaps
    m = length(coln)
    x = (1:m)/m - 1/2/m
    grid.text(coln, x = x, y = unit(0.96, "npc"), vjust = .5, 
              hjust = 1.2, rot = 45, gp = gpar(...)) ## Was 'hjust=0' and 'rot=270'
  }
  assignInNamespace(x="draw_colnames",value="draw_colnames_45",
                    ns=asNamespace("pheatmap"))
  
  pheatmap(doc.topic, show_rownames = T, show_colnames = T, digits = 3, 
           display_numbers = T, treeheight_row = 0, treeheight_col = 0, 
           cluster_rows = F, cluster_cols = F, number_format = "%.3f")
  
  #Engagement over time
  sotus <- cbind(sotus, doc.topic)

  sotus.time <- melt(sotus, measure.vars = c("One Great Nation", "New Jobs", 
                                             "State of the Economy",
                                             "Welfare State", "Children and Schooling", 
                                             "War on Terror"), 
                     id.vars = "Date")
  sotus.time$year <- substring(sotus.time$Date, 1, 4)
  
  ggplot(sotus.time, aes(x = year, y = value, fill = variable)) +
    geom_bar(stat = "identity", color = "black") +
    ylab("Proportion of Speech Engaged with the Topic") +
    xlab("Date") +
    scale_x_discrete(breaks = seq(1989, 2019, 4)) +
    theme_bw() +
    theme(legend.position = "bottom",
          axis.title = element_text(face = "bold"),
          legend.title = element_text(face = "bold")) +
    guides(fill = guide_legend(title = "Topic"))
  
#Save your R objects
save.image("lda.RData")

### END ###
