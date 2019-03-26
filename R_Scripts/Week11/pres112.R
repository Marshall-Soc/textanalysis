
##############################
##  pres112.R: Code for Week 11, Day #2
##  Note: Corresponds to pres112.pdf.
##        See pres112.pdf for references.
##  Author: Marshall A. Taylor
##############################

### BEGIN ###

#Set working directory
setwd("set_working_directory")


#Necessary packages
install.packages(c("devtools", "tm", "textstem",
                   "textreg", "igraph", "htmlwidgets"))
devtools::install_github("cbail/textnets")
library(textnets)
library(tm)
library(textstem)
library(textreg)
library(igraph)
library(htmlwidgets)


#Load in SOTU data (1790-2019)
load("networks.RData")
corpus <- data_corpus_sotu$documents


#Clean data
sotus.tm <- VCorpus(VectorSource(corpus$texts))

removeSpecialChars <- function(x) gsub("[^a-zA-Z0-9 ]", " ", x)

sotus.tm <- tm_map(sotus.tm, removePunctuation)
sotus.tm <- tm_map(sotus.tm, content_transformer(removeSpecialChars))
sotus.tm <- tm_map(sotus.tm, content_transformer(tolower))
sotus.tm <- tm_map(sotus.tm, removeNumbers)
sotus.tm <- tm_map(sotus.tm, removeWords, stopwords("english"))
sotus.tm <- tm_map(sotus.tm, stripWhitespace)

sotus.tm <- convert.tm.to.character(sotus.tm)
sotus.tm <- lemmatize_strings(sotus.tm, 
                              dictionary = lexicon::hash_lemmas)

sotus.tm <- VCorpus(VectorSource(sotus.tm))

sotus.dtm <- DocumentTermMatrix(sotus.tm)
sotus.dtm <- weightTfIdf(sotus.dtm)
sotus.dtm <- removeSparseTerms(sotus.dtm, .05)


#Take a look at the matrix
inspect(sotus.dtm)


#Bipartite graph (subset of the matrix)
corpus.net <- as.matrix(sotus.dtm[230:240,]) %>% 
  graph.incidence(directed = F, weighted = T)

corpus.el <- as_edgelist(corpus.net)

colnames(corpus.el) <- c("doc", "word")

V(corpus.net)$mode <- ifelse(V(corpus.net)$name %in% corpus.el[, "doc"], T, F)
V(corpus.net)$color <- ifelse(V(corpus.net)$mode, "#0072B2", "#D55E00")
V(corpus.net)$shape <- ifelse(V(corpus.net)$mode, "circle", "square")
V(corpus.net)$name[1:11] <- rownames(corpus[230:240,])
E(corpus.net)$color <- "lightgray"

corpus.net.sub <- delete_edges(corpus.net, E(corpus.net)[weight <= 1.163e-04])
V(corpus.net.sub)$degree <- strength(corpus.net.sub)
corpus.net.sub <- delete_vertices(corpus.net.sub, V(corpus.net.sub)[degree==0])

plot(corpus.net.sub, vertex.label.cex = 0.8, 
     vertex.label.color = "black", layout = layout_with_graphopt)

  #Sample plot, but with a different layout
plot(corpus.net.sub, vertex.label.cex = 0.5, 
     vertex.label.color = "black", layout = layout_as_bipartite,
     vertex.size = 7)


#One-mode projections (binarized)
  #Binarize the DTM
  sotus.dtm2 <- removeSparseTerms(DocumentTermMatrix(sotus.tm), .05)
  dtm.mat <- as.matrix(sotus.dtm2)
  dtm.mat <- ifelse(dtm.mat >= 10, 1, 0)
  dtm.mat <- dtm.mat[230:240,]

  #Get the one-mode projections
  doc.net <- dtm.mat %*% t(dtm.mat)
  word.net <- t(dtm.mat) %*% dtm.mat

  #Doc one-mode projection
  doc.net[1:5, 1:5]
  rowSums(dtm.mat)[1:5]
  
  #Word one-mode projection
  word.net[1:5, 1:5]
  colSums(dtm.mat)[1:5]
  
  #Plot them
  doc.net <- graph_from_adjacency_matrix(doc.net, 
                                         mode = "undirected", weighted = T,
                                         diag = F)
  word.net <- graph_from_adjacency_matrix(word.net, 
                                          mode = "undirected", weighted = T,
                                          diag = F)

  V(doc.net)$name <- rownames(corpus[230:240,])
  doc.net <- delete_edges(doc.net, E(doc.net)[weight <= 15])
  V(doc.net)$degree <- strength(doc.net)
  doc.net <- delete_vertices(doc.net, V(doc.net)[degree==0])
  plot(doc.net, vertex.label.cex = 0.8, 
       vertex.color = "#0072B2", vertex.shape = "circle",
       vertex.label.color = "black", layout = layout_with_fr)
  
  word.net <- delete_edges(word.net, E(word.net)[weight <= 2])
  V(word.net)$degree <- strength(word.net)
  word.net <- delete_vertices(word.net, V(word.net)[degree==0])
  plot(word.net, vertex.label.cex = 0.8, 
       vertex.color = "#D55E00", vertex.shape = "square",
       vertex.label.color = "black", layout = layout_with_fr)
  

#textnets (don't run this in class; it will take a while)
#sotu.docs.net <- PrepText(corpus, groupvar = "President", 
                       #textvar = "texts", node_type = "groups", 
                       #tokenizer = "words", remove_stop_words = TRUE, 
                       #remove_numbers = T)
#sotus.words.net <- PrepText(corpus, groupvar = "President", 
                             #textvar = "texts", node_type = "words", 
                             #tokenizer = "words", remove_stop_words = TRUE, 
                             #remove_numbers = T)

#docs.network <- CreateTextnet(sotu.docs.net)
#words.network <- CreateTextnet(sotus.words.net)

  #Visualize document-by-document network
  VisTextNet(docs.network)
  
  #Same, but with force-directed spacing (see VisTextNet2.R)
  VisTextNet2(docs.network)
  
  #3D plot
  VisTextNetD3(docs.network)

  
#Save your R objects
save.image("networks.RData")

### END ###