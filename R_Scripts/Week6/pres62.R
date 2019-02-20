
##############################
##  pres62.R: Code for Week 6, Day #2
##  Note: Corresponds to pres62.pdf.
##        See pres62.pdf for references.
##  Author: Marshall A. Taylor
##############################

### BEGIN ###

#Set working directory
setwd("set_working_directory")


#Necessary packages
install.packages(c("tm", "ggplot2", "textstem", 
                   "textreg", "text2vec"))
install.packages("factoextra") #If asked if you want to "install
    #from sources," enter "no" in the console and press enter.

library(tm)
library(ggplot2)
library(factoextra)
library(textstem)
library(textreg)
library(quanteda.corpora)
library(text2vec)


#Load in data
load("data_corpus_sotu.rda") #from quanteda.corpora (Benoit and Watanabe 2019)
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
sotus.dtm <- removeSparseTerms(sotus.dtm, .95)


#Clustering
  #Get cosine similarities
  sotus.dtm <- as.matrix(sotus.dtm)
  sotus.cosine <- sim2(sotus.dtm, method = "cosine", norm = "l2")
  
  #Convert similarity to a dissimilarity
  sotus.cosine.d <- 1 - sotus.cosine
  cor(c(sotus.cosine), c(sotus.cosine.d)) #Method check: should be a perfect
                                      #negative correlation: -1
  
  #How many clusters?
  complete.link = function(diss,k) {hcut(diss, k, hc_method = "complete")}
  fviz_nbclust(sotus.dtm, FUNcluster = complete.link, 
               method = "silhouette", diss = sotus.cosine.d, k.max = 20)
  
  #Let's go with the 6-cluster solution
  sotus.clusters <- hcut(as.dist(sotus.cosine.d), k = 4,
                            hc_method = "complete", isdiss = T)

  #Take a look at the 6-cluster solution in a dendrogram
  sotus.clusters$labels <- rownames(sotus)
  fviz_dend(sotus.clusters, cex = .5,
            k_colors = c("#E69F00", "#56B4E9", "#009E73", 
                         "#0072B2", "#D55E00", "#999999"),
            type = "phylogenic", repel = T,
            phylo_layout = "layout_as_tree")
  
#What words are highly associated with each cluster?
  #Code from here: https://cran.r-project.org/web/packages/textmineR/vignettes/b_document_clustering.html
p_words <- colSums(sotus.dtm) / sum(sotus.dtm)

cluster_words <- lapply(unique(sotus.clusters$cluster), function(x){
  rows <- sotus.dtm[sotus.clusters$cluster == x,]
  rows <- rows[, colSums(rows) > 0]
  colSums(rows)/sum(rows) - p_words[colnames(rows)]
})

cluster_summary <- data.frame(cluster = unique(sotus.clusters$cluster),
                              size = as.numeric(table(sotus.clusters$cluster)),
                              top_words = sapply(cluster_words, function(d){
                                paste(
                                  names(d) [order(d, decreasing = TRUE)][1:5], 
                                  collapse = ", ")
                              }),
                              stringsAsFactors = FALSE)
cluster_summary

#Save your R objects
save.image("clustering.RData")

### END ###
