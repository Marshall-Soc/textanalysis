
##############################
##  pres121.R: Code for Week 12, Day #1
##  Note: Corresponds to pres121.pdf.
##        See pres121.pdf for references.
##  Author: Marshall A. Taylor
##############################

### BEGIN ###

#Set working directory
setwd("set_working_directory")

data_corpus_sotu$documents$President
#Necessary packages
install.packages(c("devtools", 
                   "igraph", 
                   "ggplot2",
                   "ggrepel"))
devtools::install_github("cbail/textnets")
library(textnets)
library(igraph)
library(ggplot2)
library(ggrepel)


#Read in textnets objects
load("networks2.RData") #For class

#Don't run these; they take too long to do in class. You've already read in these 
  #objects above.
  #Like last week, we are grouping the speeches by president (thereby treating all
  #speeches by a particular president as a "document" rather than an individual speech.
  #However, now we are applying a part-of-speech classifier in order to create a 
  #two-mode text network where only nouns and noun phrases (compound nouns) 
  #are retained.

#sotu.docs.nouns <- PrepText(corpus, groupvar = "President", textvar = "texts", 
  #node_type = "groups", tokenizer = "words", pos = "nouns", 
  #remove_stop_words = T, compound_nouns = T)
  #sotu.words.nouns <- PrepText(corpus, groupvar = "President", textvar = "texts", 
  #node_type = "words", tokenizer = "words", pos = "nouns", 
  #remove_stop_words = T, compound_nouns = T)

  #docs.network <- CreateTextnet(sotu.docs.nouns)
  #words.network <- CreateTextnet(sotu.words.nouns)


#Visualize
set.seed(310)
VisTextNet(docs.network)

#set.seed(310)
#VisTextNet(words.network, alpha = .05, 
  #label_degree_cut = 500) Takes too long to generate.


#Weighted degree centralities
docs.degree <- degree(docs.network)
docs.wdegree <- strength(docs.network)
docs.wdegree <- docs.degree * 
  ((docs.wdegree/docs.degree)^.25)

#words.degree <- degree(words.network) Takes too long to do in class.
#words.wdegree <- strength(words.network)
#words.wdegree <- words.degree * 
  #((words.wdegree/words.degree)^.25)

  #Which presidents have the highest weighted degree centrality?
  head(sort(docs.wdegree, decreasing = T), 10)
  
  #The lowest?
  head(sort(docs.wdegree, decreasing = F), 10)
  
  #What words have the highest weighted degree centrality?
  head(sort(words.wdegree, decreasing = T), 10)
  
  #Plotting it
  top.pres <- as.data.frame(sort(docs.wdegree, decreasing = T))
  colnames(top.pres) <- "wdegree"
  top.pres$pres <- factor(rownames(top.pres),
                          levels = rownames(top.pres))
  
  ggplot(top.pres, aes(x = '1', y = wdegree)) + 
    geom_boxplot(fill = "gray75") +
    geom_text(aes(label = pres), 
                position = position_jitter(0.2)) +
    xlab("") + ylab("Weighted Degree Centrality") +
    theme_classic() +
    theme(axis.title = element_text(face = "bold"),
          axis.ticks.x = element_blank(),
          axis.text.x = element_blank())
  
  top.words <- as.data.frame(sort(words.wdegree, decreasing = T))
  colnames(top.words) <- "wdegree"
  top.words$words <- rownames(top.words)
  
  ggplot(top.words, aes(x = '1', y = wdegree)) + 
    geom_boxplot(fill = "gray75", outlier.shape = NA) +
    geom_text_repel(aes(label = ifelse(top.words$wdegree >= 
                                         quantile(top.words$wdegree, .999), 
                                 top.words$words, ""))) +
    xlab("") + ylab("Weighted Degree Centrality") +
    theme_classic() +
    theme(axis.title = element_text(face = "bold"),
          axis.ticks.x = element_blank(),
          axis.text.x = element_blank())
  

    #Connections among high degree presidents 
    V(docs.network)$wdegree <- docs.wdegree
    top.network <- induced_subgraph(docs.network, vids = c("Obama", "Reagan", "Clinton",
                                                             "Trump", "Bush", "Ford",
                                                             "Nixon", "Carter", "Kennedy"
                                                             ))
    top.degree <- degree(top.network)
    top.wdegree <- strength(top.network)
    top.wdegree2 <- top.degree * 
      ((top.wdegree/top.degree)^.25) 
    V(top.network)$wdegree2 <- top.wdegree2
    
    fine = 100
    pal = colorRampPalette(c("orange", "purple"))
    degree.col = pal(fine)[as.numeric(cut(top.wdegree2, breaks = fine))]
    
      #From her
    minC <- rep(-Inf, vcount(top.network))
    maxC <- rep(Inf, vcount(top.network))
    minC[1] <- maxC[1] <- 0
    fr <- layout_with_fr(top.network, minx=minC, maxx=maxC,
                         miny=minC, maxy=maxC)
    par(mar=c(0, 0, 0, 0))
    par(mfrow=c(1,2))
    plot(top.network, layout = fr,
         edge.color = "black", edge.width = E(top.network)$weight*1500,
         vertex.color = degree.col, vertex.label.color = "black",
         edge.label = round(E(top.network)$weight, 5)*1000,
         edge.label.color = "black", edge.label.cex = .5)
    plot(top.network, layout = fr,
         edge.color = "black", edge.width = E(top.network)$weight*1500,
         vertex.color = degree.col, vertex.label.color = "black",
         vertex.label = round(V(top.network)$wdegree2, 2),
         vertex.label.cex = .75)
    par(mfrow=c(1,1))
    par(mar=c(5.1, 4.1, 4.1, 2.1))


#Weighted betweenness centralities
docs.wbetween <- TextCentrality(docs.network)
#words.wbetween <- TextCentrality(words.network) Takes too long to do in class.

  #Which presidents have the highest weighted betweenness centrality?
  docs.wbetween <- docs.wbetween[order(-docs.wbetween$betweenness_centrality),]
  head(docs.wbetween, 10)
  
  #The lowest?
  tail(docs.wbetween, 10)
  
  #Plotting it
  top.pres.bet <- as.data.frame(docs.wbetween[1])
  colnames(top.pres.bet) <- "wbetween"
  top.pres.bet$pres <- factor(rownames(top.pres.bet),
                          levels = rownames(top.pres.bet))
  
  ggplot(top.pres.bet, aes(x = '1', y = wbetween)) + 
    geom_boxplot(fill = "gray75", outlier.shape = NA) +
    geom_text(aes(label = pres), 
              position = position_jitter(0.2)) +
    xlab("") + ylab("Weighted Betweenness Centrality") +
    theme_classic() +
    theme(axis.title = element_text(face = "bold"),
          axis.ticks.x = element_blank(),
          axis.text.x = element_blank())
  
  #Scale the vertices in the document-level projection by their weighted betweenness
  set.seed(310)
  VisTextNet(docs.network, betweenness = T)


#Save your R objects
save.image("networks2.RData")

### END ###