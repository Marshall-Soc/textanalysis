
##############################
##  pres101.R: Code for Week 10, Day #1
##  Note: Corresponds to pres101.pdf.
##        See pres101.pdf for references.
##  Author: Marshall A. Taylor
##############################

### BEGIN ###

#Set working directory
setwd("set_working_directory")


#Necessary packages
install.packages(c("tm", "ggplot2",
                   "textreg", "text2vec", 
                   "dplyr", "tidytext",
                   "wordcloud"))
library(tm)
library(ggplot2)
library(textreg)
library(text2vec)
library(dplyr)
library(tidytext)
library(wordcloud)


#Load in data
imdb.data <- readRDS(file = "imdb_data.rds")


#Prepare data
corpus <- VCorpus(VectorSource(imdb.data$review))

removeAllPunct <- function(x) gsub("[[:punct:]]", " ", x)
removeSpecialChars <- function(x) gsub("[^a-zA-Z0-9 ]", " ", x)

corpus <- tm_map(corpus, content_transformer(removeAllPunct))
corpus <- tm_map(corpus, content_transformer(removeSpecialChars))
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus <- tm_map(corpus, stripWhitespace)

corpus <- convert.tm.to.character(corpus)


#Prepare term co-occurrence matrix
we.tokens <- space_tokenizer(corpus)
we.it <- itoken(we.tokens, progressbar = F)
we.vocab <- create_vocabulary(we.it)
we.vocab <- prune_vocabulary(we.vocab, 
                             doc_proportion_min=.001, doc_proportion_max=.999)
we.vect <- vocab_vectorizer(we.vocab)
we.tcm <- create_tcm(we.it, we.vect, skip_grams_window = 5L,
                     skip_grams_window_context = "symmetric")

  #Take a look at a few rows and columns
  as.matrix(we.tcm)[1:15, 100:200] 


#Get word vectors
set.seed(3212)
we.glove = GlobalVectors$new(word_vectors_size = 50, 
                             vocabulary = we.vocab, x_max = 100)
we.main = we.glove$fit_transform(we.tcm, n_iter = 10)
we.words <- we.glove$components
we.vectors = we.main + t(we.words)

  #Take a look at the word vector matrix
  we.vectors[1:5, 1:50]


#What words are closest to "horror"?
horror <- we.vectors["horror", , drop = F]
cos_sim = sim2(x = we.vectors, y = horror, 
               method = "cosine", norm = "l2") #Using cosine similarities to find
                                        #how close each word in the corpus is to
                                        #the vector for "horror"
head(sort(cos_sim[,1], decreasing = TRUE), 10)

  #In wordcloud form
  png("horror.png", width=12,height=8, units='in', res=300)
  wordcloud(words = rownames(cos_sim), freq = cos_sim[1:nrow(cos_sim)], 
            min.freq = .2, max.words=300, random.order = F, rot.per = 0.35, 
            colors=brewer.pal(8, "Blues"), scale=c(2, .1))
  dev.off()
  

#What words are closest to "horror" when you remove "slasher" 
  #from the context window?
horror <- we.vectors["horror", , drop = F] -
            we.vectors["slasher", , drop = F]
cos_sim = sim2(x = we.vectors, y = horror, 
               method = "cosine", norm = "l2")
head(sort(cos_sim[,1], decreasing = TRUE), 10)

  #In wordcloud form
  png("horror_slasher.png", width = 8, height = 6, units='in', res=300)
  wordcloud(words = rownames(cos_sim), freq = cos_sim[1:nrow(cos_sim)], 
            min.freq = .2, max.words=300, random.order = F, rot.per = 0.35, 
            colors=brewer.pal(8, "Blues"), scale=c(1, .1))
  dev.off()


#What if we *include* "slasher" in the context window?
horror <- we.vectors["horror", , drop = F] +
  we.vectors["slasher", , drop = F]
cos_sim = sim2(x = we.vectors, y = horror, 
               method = "cosine", norm = "l2")
head(sort(cos_sim[,1], decreasing = TRUE), 10)


#What are the context words for "action" given that "fight" is in the 
  #context window?
action <- we.vectors["action", , drop = F] +
  we.vectors["fight", , drop = F]
cos_sim = sim2(x = we.vectors, y = action, 
               method = "cosine", norm = "l2")
head(sort(cos_sim[,1], decreasing = TRUE), 10)


#What are the context words for "action" given that "fight" is *not* 
  #in the context window?
action <- we.vectors["action", , drop = F] -
  we.vectors["fight", , drop = F]
cos_sim = sim2(x = we.vectors, y = action, 
               method = "cosine", norm = "l2")
head(sort(cos_sim[,1], decreasing = TRUE), 10)


#"halloween" + "myers"?
halloween <- we.vectors["halloween", , drop = F] +
            we.vectors["myers", , drop = F]
cos_sim = sim2(x = we.vectors, y = halloween, 
               method = "cosine", norm = "l2")
head(sort(cos_sim[,1], decreasing = TRUE), 20)

  #In wordcloud form
  png("halloween_myers.png", width = 8, height = 6, units='in', res=300)
  wordcloud(words = rownames(cos_sim), freq = cos_sim[1:nrow(cos_sim)], 
            min.freq = .2, max.words=300, random.order = F, rot.per = 0.35, 
            colors=brewer.pal(8, "Oranges"), scale=c(2, .1))
  dev.off()


#"halloween" + "zombies"?
halloween <- we.vectors["halloween", , drop = F] +
            we.vectors["zombies", , drop = F]
cos_sim = sim2(x = we.vectors, y = halloween, 
               method = "cosine", norm = "l2")
head(sort(cos_sim[,1], decreasing = TRUE), 10)

  #In wordcloud form
  png("halloween_zombies.png", width = 7, height = 5, units='in', res=300)
  wordcloud(words = rownames(cos_sim), freq = cos_sim[1:nrow(cos_sim)], 
            min.freq = .2, max.words=300, random.order = F, rot.per = 0.35, 
            colors=brewer.pal(8, "Oranges"), scale=c(1, .1))
  dev.off()


#Visualize some of the contexts
  #Let's first reduce the size of the vector matrix
we.tokens <- space_tokenizer(corpus)
we.it <- itoken(we.tokens, progressbar = F)
we.vocab <- create_vocabulary(we.it)
we.vocab <- prune_vocabulary(we.vocab, 
                             doc_proportion_min=.05, doc_proportion_max=.9999)
we.vect <- vocab_vectorizer(we.vocab)

we.tcm <- create_tcm(we.it, we.vect, skip_grams_window = 5L,
                     skip_grams_window_context = "symmetric")

set.seed(3212)
we.glove = GlobalVectors$new(word_vectors_size = 50, 
                             vocabulary = we.vocab, x_max = 100)
we.main = we.glove$fit_transform(we.tcm, n_iter = 10)
we.words <- we.glove$components
we.vectors = we.main + t(we.words)

  #Now we'll plot
we.output.pca <- prcomp(t(we.vectors), center=T, scale.=T)
we.plot.pca <- as.data.frame(we.output.pca$rotation[,1:2])

ggplot(we.plot.pca, aes(x=PC1, y=PC2)) +
  geom_text(label=rownames(we.vectors)) +
  theme_bw() +
  xlab("Dimension #1 (46.03%)") + ylab("Dimension #2 (5.02%)") +
  theme(axis.title=element_text(face="bold")) 


#Using unigrams and bigrams
we.tokens <- space_tokenizer(corpus)
we.it <- itoken(we.tokens, progressbar = F)
we.vocab <- create_vocabulary(we.it, ngram = c(1L, 2L))
we.vocab <- prune_vocabulary(we.vocab, 
                             doc_proportion_min=.001, doc_proportion_max=.999)
we.vect <- vocab_vectorizer(we.vocab)
we.tcm <- create_tcm(we.it, we.vect, skip_grams_window = 5L,
                     skip_grams_window_context = "symmetric")

set.seed(3212)
we.glove = GlobalVectors$new(word_vectors_size = 50, 
                             vocabulary = we.vocab, x_max = 100)
we.main = we.glove$fit_transform(we.tcm, n_iter = 10)
we.words <- we.glove$components
we.vectors = we.main + t(we.words)

ac <- we.vectors["annoying_characters", , drop = F]
cos_sim = sim2(x = we.vectors, y = ac, 
               method = "cosine", norm = "l2")
head(sort(cos_sim[,1], decreasing = TRUE), 20)


#Save your R objects
save.image("we.RData")

### END ###
