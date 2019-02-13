
##############################
##  pres51.R: Code for Week 5, Day #2
##  Note: Corresponds to pres52.pdf.
##        See pres52.pdf for references.
##  Author: Marshall A. Taylor
##############################

### BEGIN ###

#Set working directory
setwd("set_working_directory")


#Allocate additional memory for CleanNLP package
options(java.parameters = "-Xmx4096m")


#Necessary packages
install.packages(c("pacman", "ggplot2", "ggpubr",
                   "tidyverse", "tidytext", "cleanNLP",
                   "RCurl"))
library(pacman)
pacman::p_load_gh(c(
  "trinker/termco", 
  "trinker/coreNLPsetup",        
  "trinker/tagger"
)) #Run the pacman() lines before the library() lines.
    #If asked if you want to "install from sources the
    #package which needs compilation," type "no" in the
    #console and press enter.

library(tagger)
library(ggplot2)
library(ggpubr)
library(tidyverse)
library(tidytext)
library(cleanNLP)
library(RCurl)

#Load in data
blogs <- read.csv(file = "blogs100.csv", header = T, row.names = 1)

blogs$documents[[1]]


#Apply the POS tagger to the first blog post
tag_pos(as.character(blogs$documents[1]))

tag_pos(as.character(blogs$documents[1])) %>% c()


#What do these tags mean?
penn_tags()


#Apply the tagger across all the blog posts
pos.d <- tag_pos(as.character(blogs[which(blogs$rating=="Liberal"),]
                                       $documents))
pos.r <- tag_pos(as.character(blogs[which(blogs$rating=="Conservative"),]
                                       $documents))
liberal <- plot(pos.d)
conservative <- plot(pos.r)

liberal.plot <- ggplot(liberal$data, aes(x = Terms, y = Prop)) +
  geom_bar(aes(x = Terms, y = Prop), stat = "identity", color = "black",
           fill = "#2332A9") +
  xlab("Liberal Blog Parts of Speech") + ylab("Proportion of Total Blog Words") +
  ylim(0, .2) +
  theme_bw() +
  theme(axis.title = element_text(face = "bold")) +
  coord_flip()

conservative.plot <- ggplot(conservative$data, aes(x = Terms, y = Prop)) +
  geom_bar(aes(x = Terms, y = Prop), stat = "identity", color = "black",
           fill = "#990000") +
  xlab("Conservative Blog Parts of Speech") + ylab("Proportion of Total Blog Words") +
  ylim(0, .2) +
  theme_bw() +
  theme(axis.title = element_text(face = "bold")) +
  coord_flip()

ggarrange(liberal.plot, conservative.plot, align = "v")


#Let's simplify the categories (code adapted from here: https://github.com/trinker/tagger)
pos.d <- tag_pos(as.character(blogs[which(blogs$rating=="Liberal"),]
                              $documents)) %>% as_basic()
pos.r <- tag_pos(as.character(blogs[which(blogs$rating=="Conservative"),]
                              $documents)) %>% as_basic()
liberal <- plot(pos.d)
conservative <- plot(pos.r)

liberal.plot <- ggplot(liberal$data, aes(x = Terms, y = Prop)) +
  geom_bar(aes(x = Terms, y = Prop), stat = "identity", color = "black",
           fill = "#2332A9") +
  xlab("Liberal Blog Parts of Speech") + ylab("Proportion of Total Blog Words") +
  ylim(0, .3) +
  theme_bw() +
  theme(axis.title = element_text(face = "bold")) +
  coord_flip()

conservative.plot <- ggplot(conservative$data, aes(x = Terms, y = Prop)) +
  geom_bar(aes(x = Terms, y = Prop), stat = "identity", color = "black",
           fill = "#990000") +
  xlab("Conservative Blog Parts of Speech") + ylab("Proportion of Total Blog Words") +
  ylim(0, .3) +
  theme_bw() +
  theme(axis.title = element_text(face = "bold")) +
  coord_flip()

ggarrange(liberal.plot, conservative.plot, align = "v")


#Count the tags
counts <- tag_pos(as.character(blogs$documents))
counts <- count_tags(counts, blogs$rating)

counts <- tag_pos(as.character(blogs$documents)) %>% as_basic()
counts <- count_tags(counts, blogs$rating)


#Most common nouns/verbs (code adapted from here: https://www.andrewheiss.com/blog/2018/12/28/tidytext-pos-arabic/)

###FOR CLASS: everything in this section through the cnlp_annotate() functions take
  #forever; so, for to follow along in class, skip everything through cnlp_annotate()
  #and just load up these R objects directly with the following two lines (you can 
  #find these RDS files in Sakai)
tagged.lib <- readRDS("tagged_lib.rds")
tagged.con <- readRDS("tagged_con.rds")

cnlp_download_corenlp() #Expect this download to take a while
cnlp_init_corenlp(language = "en")

liberal.blogs <- blogs[which(blogs$rating == "Liberal"),]
conservative.blogs <- blogs[which(blogs$rating == "Conservative"),]

tagged.lib <- cnlp_annotate(as.character(liberal.blogs$documents), as_strings = T,
                              doc_var = "docname") #This takes a good while!
tagged.con <- cnlp_annotate(as.character(conservative.blogs$documents), as_strings = T,
                              doc_var = "docname") #This takes a good while!

liberal.terms <- tagged.lib %>% cnlp_get_token()
conservative.terms <- tagged.con %>% cnlp_get_token()

  #Most common nouns
  lib.nouns <- liberal.terms %>% 
    filter(str_detect(upos, "NOUN")) %>% 
    count(word, sort = TRUE) %>% 
    filter(nchar(word) > 1) %>%
    anti_join(stop_words, by = "word") %>% 
    top_n(10, n) %>% 
    mutate(word = fct_inorder(word))
  
  con.nouns <- conservative.terms %>% 
    filter(str_detect(upos, "NOUN")) %>% 
    count(word, sort = TRUE) %>% 
    filter(nchar(word) > 1) %>%
    anti_join(stop_words, by = "word") %>% 
    top_n(10, n) %>% 
    mutate(word = fct_inorder(word))

  #Most common verbs
  lib.verbs <- liberal.terms %>% 
    filter(str_detect(upos, "VERB")) %>% 
    count(word, sort = TRUE) %>% 
    filter(nchar(word) > 2) %>%
    anti_join(stop_words, by = "word") %>% 
    top_n(10, n) %>% 
    mutate(word = fct_inorder(word))
  
  con.verbs <- conservative.terms %>% 
    filter(str_detect(upos, "VERB")) %>% 
    count(word, sort = TRUE) %>% 
    filter(nchar(word) > 2) %>%
    anti_join(stop_words, by = "word") %>% 
    top_n(10, n) %>% 
    mutate(word = fct_inorder(word))

  #Plot the nouns
  liberal.plot <- ggplot(lib.nouns, aes(x = fct_rev(word), y = n)) +
    geom_bar(aes(x = fct_rev(word), y = n), stat = "identity", color = "black",
             fill = "#2332A9") +
    xlab("Top Nouns in Liberal Blog Posts") + ylab("Frequency") +
    ylim(0, 200) +
    theme_bw() +
    theme(axis.title = element_text(face = "bold")) +
    coord_flip()
  
  conservative.plot <- ggplot(con.nouns, aes(x = fct_rev(word), y = n)) +
    geom_bar(aes(x = fct_rev(word), y = n), stat = "identity", color = "black",
             fill = "#990000") +
    xlab("Top Nouns in Conservative Blog Posts") + ylab("Frequency") +
    ylim(0, 200) +
    theme_bw() +
    theme(axis.title = element_text(face = "bold")) +
    coord_flip()
  
  ggarrange(liberal.plot, conservative.plot, align = "v")

  #Plot the verbs
  liberal.plot <- ggplot(lib.verbs, aes(x = fct_rev(word), y = n)) +
    geom_bar(aes(x = fct_rev(word), y = n), stat = "identity", color = "black",
             fill = "#2332A9") +
    xlab("Top Verbs in Liberal Blog Posts") + ylab("Frequency") +
    ylim(0, 50) +
    theme_bw() +
    theme(axis.title = element_text(face = "bold")) +
    coord_flip()
  
  conservative.plot <- ggplot(con.verbs, aes(x = fct_rev(word), y = n)) +
    geom_bar(aes(x = fct_rev(word), y = n), stat = "identity", color = "black",
             fill = "#990000") +
    xlab("Top Verbs in Conservative Blog Posts") + ylab("Frequency") +
    ylim(0, 50) +
    theme_bw() +
    theme(axis.title = element_text(face = "bold")) +
    coord_flip()
  
  ggarrange(liberal.plot, conservative.plot, align = "v")

  #Plot the lemmas instead of raw words to get transform entries like "'re"
    #and "'ve" to "are" and "have
  lib.verbs <- liberal.terms %>% 
    filter(str_detect(upos, "VERB")) %>%
    count(lemma, sort = TRUE) %>%
    filter(nchar(lemma) > 2) %>%
    set_colnames(c("word", "n")) %>%
    anti_join(stop_words, by = "word") %>%
    top_n(10, n) %>% 
    mutate(word = fct_inorder(word))

  con.verbs <- conservative.terms %>% 
    filter(str_detect(upos, "VERB")) %>%
    count(lemma, sort = TRUE) %>%
    filter(nchar(lemma) > 2) %>%
    set_colnames(c("word", "n")) %>%
    anti_join(stop_words, by = "word") %>%
    top_n(10, n) %>% 
    mutate(word = fct_inorder(word))

  #Plot the new verb counts
  liberal.plot <- ggplot(lib.verbs, aes(x = fct_rev(word), y = n)) +
    geom_bar(aes(x = fct_rev(word), y = n), stat = "identity", color = "black",
             fill = "#2332A9") +
    xlab("Top Verb Lemmas in Liberal Blog Posts") + ylab("Frequency") +
    ylim(0, 35) +
    theme_bw() +
    theme(axis.title = element_text(face = "bold")) +
    coord_flip()
  
  conservative.plot <- ggplot(con.verbs, aes(x = fct_rev(word), y = n)) +
    geom_bar(aes(x = fct_rev(word), y = n), stat = "identity", color = "black",
             fill = "#990000") +
    xlab("Top Verb Lemmas in Conservative Blog Posts") + ylab("Frequency") +
    ylim(0, 35) +
    theme_bw() +
    theme(axis.title = element_text(face = "bold")) +
    coord_flip()
  
  ggarrange(liberal.plot, conservative.plot, align = "v")


#Save your R objects
save.image("pos.RData")

### END ###
