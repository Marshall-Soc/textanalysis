
##############################
##  pres81.R: Code for Week 8, Day #1
##  Note: Corresponds to pres81.pdf.
##        See pres81.pdf for references.
##  Author: Marshall A. Taylor
##############################

### BEGIN ###

#Set working directory
setwd("set_working_directory")


#Necessary packages
install.packages(c("tm", "stm", "ggplot2",
                   "reshape2", "tidytext", "dplyr", 
                   "textstem", "textreg", "grid",
                   "devtools", "LDAvis", "servr"))
library(tm)
library(stm)
library(ggplot2)
library(reshape2)
library(tidytext)
library(dplyr)
library(textstem)
library(textreg)
library(grid)
library(LDAvis)
library(servr)
devtools::install_github("mroberts/stmBrowser", dependencies = T)
library(stmBrowser)


#Load in data
blogs <- read.csv(file = "poliblogs2008.csv", header = T, row.names = 1)


#Preprocess the texts
blog.texts <- VCorpus(VectorSource(blogs$documents))

#Clean it up
removeAllPunct <- function(x) gsub("[[:punct:]]", " ", x)
removeSpecialChars <- function(x) gsub("[^a-zA-Z0-9 ]", " ", x)

blog.texts <- tm_map(blog.texts, content_transformer(removeAllPunct))
blog.texts <- tm_map(blog.texts, content_transformer(removeSpecialChars))
blog.texts <- tm_map(blog.texts, content_transformer(tolower))
blog.texts <- tm_map(blog.texts, removeNumbers)
blog.texts <- tm_map(blog.texts, removeWords, stopwords("english"))
blog.texts <- tm_map(blog.texts, stripWhitespace)

blog.texts <- convert.tm.to.character(blog.texts)
blog.texts <- lemmatize_strings(blog.texts, 
                              dictionary = lexicon::hash_lemmas)

blog.texts <- VCorpus(VectorSource(blog.texts)) #Converts character vector back to
                                      #tm corpus
inspect(blog.texts[[1]]) #Make sure everything looks ok (just taking a look at doc #1)


#Convert to a DTM and remove sparse terms
blog.texts <- DocumentTermMatrix(blog.texts)
blog.texts <- removeSparseTerms(blog.texts, .99)


#Remove sparse terms and format text data for use with the stm package
prepped.blogs <- readCorpus(blog.texts, type = "slam")
prepped.blogs <- prepDocuments(prepped.blogs$documents, prepped.blogs$vocab,
                               blogs[c("rating", "day")])
docs <- prepped.blogs$documents
vocab <- prepped.blogs$vocab
meta <- prepped.blogs$meta


#How many topics?
k.number <- searchK(documents = docs, vocab = vocab, K = seq(10, 50, by = 10),
                    data = meta, prevalence =~ rating + s(day),
                    proportion = 0.5, heldout.seed = 123, seed = 123,
                    init.type = "LDA", verbose = T)
plot(k.number)

ggplot(data = k.number$results, aes(x = semcoh, y = exclus)) +
  geom_text(aes(label = K)) +
  xlab("Semantic Coherence") +
  ylab("Exclusivity") +
  theme_bw() +
  theme(axis.title = element_text(face = "bold"),
        panel.grid = element_blank())
  

#Estimate some topics -- Don't run this in class; it'll take too long
#blog.topics <- stm(documents = docs, vocab = vocab, K = 20,
                   #data = meta, prevalence =~ rating + s(day),
                   #seed = 123, init.type = "LDA", max.em.its = 75,
                   #verbose = T)
blog.topics <- readRDS(file = "blog_topics1.rds")


#What are the top terms per topic?
plot(blog.topics, type = "summary", n = 5, main = "") #Highest prob
plot(blog.topics, type = "summary", n = 5, main = "", #Highest lift
     labeltype = "lift")

labelTopics(blog.topics)


#Top documents associated with topic #18
findThoughts(blog.topics, texts = as.character(blogs$documents),
             n = 3, topics = 18)


#Interactive visualization with high prob and lift
  ##Need to marginalize the logbeta distribution across the ideo covarates.
  #Begin marginalization
      #Marginalization code from here: https://github.com/bstewart/stm/blob/master/R/sageLabels.R
logbeta <- blog.topics$beta$logbeta
margbeta <- exp(logbeta[[1]])
if(length(logbeta) > 1) {
  weights <- blog.topics$settings$covariates$betaindex
  tab <- table(weights)
  weights <- tab/sum(tab)
  margbeta <- margbeta*weights[1]
  for(i in 2:length(blog.topics$beta$logbeta)) {
    margbeta <- margbeta + exp(blog.topics$beta$logbeta[[i]])*weights[i]
  }
}
  #End marginalization
json <- createJSON(phi = margbeta,
                   theta = blog.topics$theta,
                   doc.length = rowSums(as.matrix(blog.texts)),
                   term.frequency = colSums(as.matrix(blog.texts)),
                   vocab = vocab)
serVis(json, out.dir = "vis", open.browser = interactive())


#Another interactive viz
stmBrowser(blog.topics, data = blogs, c("rating", "day"),
           text = "documents") #Note that this is sampling only 1,000
                                #blogs, so results may differ

#Topic prevalence
blog.effects <- estimateEffect(formula = c(5, 15, 18) ~ rating + s(day),
                               stmobj = blog.topics, metadata = meta)
summary(blog.effects)


#Plot the prevalence differences
plot(blog.effects, covariate = "rating", topics = c(5, 15, 18),
     model = blog.topics, method = "difference",
     cov.value1 = "Liberal", cov.value2 = "Conservative",
     xlab = "More Conservative vs. More Liberal",
     xlim = c(-.1, .1), labeltype = "custom",
     custom.labels = c("Fossil Fuels", "Iraq War", "Clinton Campaign"))

plot(blog.effects, covariate = "day", topics = c(5, 15, 18),
     model = blog.topics, method = "continuous",
     xaxt = "n", xlab = "Year: 2008", printlegend = F,
     font.lab = 2)
monthseq <- seq(from = as.Date("2008-01-01"),
                to = as.Date("2008-12-01"), by = "month")
monthnames <- months(monthseq)
axis(1, at = as.numeric(monthseq) - min(as.numeric(monthseq)),
     labels = monthnames)
legend(250, .095, c("Fossil Fuels", "Iraq War", "Clinton Campaign"),
       pch = 15, col = c("red", "green", "blue"))


#Interactions
blog.interact <- estimateEffect(formula = c(5) ~ rating * poly(day, 2),
                               stmobj = blog.topics, metadata = meta)

plot(blog.interact, covariate = "day", topics = 5,
     model = blog.topics, method = "continuous",
     xaxt = "n", xlab = "Year: 2008", printlegend = F,
     font.lab = 2, moderator = "rating", moderator.value = "Liberal",
     linecol = "blue", ylim = c(0,0.06),
     main = '"Fossil Fuels" Topic')
plot(blog.interact, covariate = "day", topics = 5,
     model = blog.topics, method = "continuous",
     xaxt = "n", xlab = "Year: 2008", printlegend = F,
     font.lab = 2, moderator = "rating", moderator.value = "Conservative",
     linecol = "red", add = T)
monthseq <- seq(from = as.Date("2008-01-01"),
                to = as.Date("2008-12-01"), by = "month")
monthnames <- months(monthseq)
axis(1, at = as.numeric(monthseq) - min(as.numeric(monthseq)),
     labels = monthnames)
legend(250, .015, c("Liberal", "Conservative"),
       pch = 15, col = c("blue", "red"))


#Topic content
#Estimate some topics -- Don't run this in class; it'll take too long
#blog.topics2 <- stm(documents = docs, vocab = vocab, K = 20,
  #data = meta, prevalence =~ rating + s(day), content =~ rating,
  #seed = 123, init.type = "LDA", max.em.its = 75,
  #verbose = T)
blog.topics2 <- readRDS(file = "blog_topics2.rds")
labelTopics(blog.topics2) #Make sure the topic labels still line up


#Plot content differences
plot(blog.topics2, type = "perspectives", topics = 5, n = 50,
     plabels = c("Fossil Fuels,\nConservative", "Fossil Fuels,\nLiberal"))

plot(blog.topics2, type = "perspectives", topics = 15, n = 50,
     plabels = c("Iraq War,\nConservative", "Iraq War,\nLiberal"))


#Topic correlations
topic.corrs <- topicCorr(blog.topics)
plot(topic.corrs, vertex.color = "gray75")


#Save your R objects
save.image("stm.RData")

### END ###
