
##############################
##  pres32.R: Code for Week 3, Day #2
##  Note: Corresponds to pres32.pdf.
##        See pres32.pdf for references.
##  Author: Marshall A. Taylor
##############################

### BEGIN ###

#Set working directory
setwd("set_working_directory")


#Necessary packages
install.packages(c("NLP", "tm", "SnowballC", "ggplot2", 
                   "reshape2", "ggpubr"))
library(NLP)
library(tm)
library(SnowballC)
library(ggplot2)
library(reshape2)
library(ggpubr)


#Load in the data and make it a DTM
nyarticle.data <- read.csv(file = "nytimes_data.csv", 
                           header = T, row.names = 1)

ny.corpus <- VCorpus(VectorSource(nyarticle.data$snippet))

ny.corpus <- tm_map(ny.corpus, removePunctuation)
removeSpecialChars <- function(x) gsub("[^a-zA-Z0-9 ]","",x)
  #removeSpecialChars function from here: https://stackoverflow.com/questions/30994194/quotes-and-hyphens-not-removed-by-tm-package-functions-while-cleaning-corpus
ny.corpus <- tm_map(ny.corpus, removeSpecialChars)
ny.corpus <- tm_map(ny.corpus, PlainTextDocument)
ny.corpus <- tm_map(ny.corpus, content_transformer(tolower))
ny.corpus <- tm_map(ny.corpus, removeNumbers)
ny.corpus <- tm_map(ny.corpus, removeWords, stopwords("english"))
ny.corpus <- tm_map(ny.corpus, stemDocument)
ny.corpus <- tm_map(ny.corpus, stripWhitespace)

ny.dtm <- DocumentTermMatrix(ny.corpus)


#Find the most frequent terms
freq.terms <- colSums(as.matrix(ny.dtm))
freq.terms <- sort(freq.terms, decreasing = T)

  #Top 10 terms
  head(freq.terms, 10)
  
  #Top 20 terms
  head(freq.terms, 20)

  #Bottom 10 terms
  tail(freq.terms, 10)
  
  #Find terms that occur at least 3 times
  findFreqTerms(ny.dtm, lowfreq = 3)


#Visualize top 20 terms
top.terms <- as.data.frame(freq.terms[1:20])
colnames(top.terms) <- "counts"
top.terms$terms <- factor(rownames(top.terms), levels = rownames(top.terms))

ggplot(top.terms, aes(x = terms, y = counts)) +
  geom_bar(stat = "identity", color = "black", fill = "#a96a96") +
  ylab("Frequency") + xlab("") +
  scale_y_continuous(breaks = seq(0, 10, by = 1)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1,
                                   face = "bold"))


#Word correlations

  #Find words that tend to co-occur with "white"
  findAssocs(ny.dtm, "white", .5)
  
  #Find words that tend to co-occur with "ralli"
  findAssocs(ny.dtm, "ralli", .5)
  
  
#tf-idf scores
ny.dtm.tfidf <- weightTfIdf(ny.dtm)


#Visualize some tf-idf scores
rownames(ny.dtm.tfidf) <- 1:32
ny.dtm.tfidf2 <- melt(as.matrix(ny.dtm.tfidf[c("4", "20", "24", "31"),]), 
                      id = rownames(ny.dtm.tfidf))
ny.dtm.tfidf2 <- as.data.frame(ny.dtm.tfidf2)
ny.dtm.tfidf2 <- ny.dtm.tfidf2[order(ny.dtm.tfidf2$Docs, -ny.dtm.tfidf2$value),]
tfidf.doc4 <- ny.dtm.tfidf2[which(ny.dtm.tfidf2$Docs=="4"),]
tfidf.doc20 <- ny.dtm.tfidf2[which(ny.dtm.tfidf2$Docs=="20"),]
tfidf.doc24 <- ny.dtm.tfidf2[which(ny.dtm.tfidf2$Docs=="24"),]
tfidf.doc31 <- ny.dtm.tfidf2[which(ny.dtm.tfidf2$Docs=="31"),]

tfidf.doc4$Terms <- factor(tfidf.doc4$Terms, levels = tfidf.doc4$Terms)
tfidf.doc20$Terms <- factor(tfidf.doc20$Terms, levels = tfidf.doc20$Terms)
tfidf.doc24$Terms <- factor(tfidf.doc24$Terms, levels = tfidf.doc24$Terms)
tfidf.doc31$Terms <- factor(tfidf.doc31$Terms, levels = tfidf.doc31$Terms)


doc4.plot <- ggplot(tfidf.doc4[1:10,], aes(x = Terms, y = value)) +
  geom_bar(stat = "identity", color = "black", fill = "#d11141") +
  ylab("tf-idf Score") + xlab("") +
  ylim(0, 0.8) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1,
                                   face = "bold"),
        plot.title = element_text(face = "bold")) +
  ggtitle("Article #1, published 8/12/2017")

doc20.plot <- ggplot(tfidf.doc20[1:10,], aes(x = Terms, y = value)) +
  geom_bar(stat = "identity", color = "black", fill = "#00aedb") +
  ylab("tf-idf Score") + xlab("") +
  ylim(0, 0.8) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1,
                                   face = "bold"),
        plot.title = element_text(face = "bold")) +
  ggtitle("Article #2, published 8/25/2017")

doc24.plot <- ggplot(tfidf.doc24[1:10,], aes(x = Terms, y = value)) +
  geom_bar(stat = "identity", color = "black", fill = "#00b159") +
  ylab("tf-idf Score") + xlab("") +
  ylim(0, 0.8) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1,
                                   face = "bold"),
        plot.title = element_text(face = "bold")) +
  ggtitle("Article #3, published 9/12/2017")

doc31.plot <- ggplot(tfidf.doc31[1:10,], aes(x = Terms, y = value)) +
  geom_bar(stat = "identity", color = "black", fill = "#f37735") +
  ylab("tf-idf Score") + xlab("") +
  ylim(0, 0.8) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1,
                                   face = "bold"),
        plot.title = element_text(face = "bold")) +
  ggtitle("Article #4, published 12/15/2017")

ggarrange(doc4.plot, doc20.plot, doc24.plot, doc31.plot,
          align = "h")


#Save your R objects
save.image("freq_objects.RData")

### END ###



