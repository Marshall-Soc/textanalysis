
##############################
##  pres51.R: Code for Week 5, Day #1
##  Note: Corresponds to pres51.pdf.
##        See pres51.pdf for references.
##  Author: Marshall A. Taylor
##############################

### BEGIN ###

#Set working directory
setwd("set_working_directory")


#Necessary packages
install.packages(c("pacman", "ggplot2", "ggpubr"))
library(pacman)
pacman::p_load_gh("trinker/entity")
library(entity)
library(ggplot2)
library(ggpubr)


#Load in data
blogs <- read.csv(file = "blogs100.csv", header = T, row.names = 1)

blogs$documents[[1]]


#Apply the "Person" tagger
people.d <- person_entity(as.character(blogs[which(blogs$rating=="Liberal"),]
                                       $documents))
people.r <- person_entity(as.character(blogs[which(blogs$rating=="Conservative"),]
                                       $documents))
liberal <- plot(people.d, min = 2)
conservative <- plot(people.r, min = 2)

liberal.plot <- ggplot(liberal$data, aes(x = entity, y = frequency)) +
  geom_bar(aes(x = entity, y = frequency), stat = "identity", color = "black",
           fill = "#2332A9") +
  xlab("People Mentioned in Liberal Blogs") + ylab("Count") +
  ylim(0, 60) +
  theme_bw() +
  theme(axis.title = element_text(face = "bold")) +
  coord_flip()

conservative.plot <- ggplot(conservative$data, aes(x = entity, y = frequency)) +
  geom_bar(aes(x = entity, y = frequency), stat = "identity", color = "black",
           fill = "#990000") +
  xlab("People Mentioned in Conservative Blogs") + ylab("Count") +
  ylim(0, 60) +
  theme_bw() +
  theme(axis.title = element_text(face = "bold")) +
  coord_flip()

ggarrange(liberal.plot, conservative.plot, align = "v")


#Apply the "Organization" tagger
org.d <- organization_entity(as.character(blogs[which(blogs$rating=="Liberal"),]
                                       $documents))
org.r <- organization_entity(as.character(blogs[which(blogs$rating=="Conservative"),]
                                       $documents))
liberal <- plot(org.d, min = 2)
conservative <- plot(org.r, min = 2)

liberal.plot <- ggplot(liberal$data, aes(x = entity, y = frequency)) +
  geom_bar(aes(x = entity, y = frequency), stat = "identity", color = "black",
           fill = "#2332A9") +
  xlab("Organizations Mentioned in Liberal Blogs") + ylab("Count") +
  ylim(0, 20) +
  theme_bw() +
  theme(axis.title = element_text(face = "bold")) +
  coord_flip()

conservative.plot <- ggplot(conservative$data, aes(x = entity, y = frequency)) +
  geom_bar(aes(x = entity, y = frequency), stat = "identity", color = "black",
           fill = "#990000") +
  xlab("Organizations Mentioned in Conservative Blogs") + ylab("Count") +
  ylim(0, 20) +
  theme_bw() +
  theme(axis.title = element_text(face = "bold")) +
  coord_flip()

ggarrange(liberal.plot, conservative.plot, align = "v")


#Apply the "Locations" tagger
loc.d <- location_entity(as.character(blogs[which(blogs$rating=="Liberal"),]
                                          $documents))
loc.r <- location_entity(as.character(blogs[which(blogs$rating=="Conservative"),]
                                          $documents))
liberal <- plot(loc.d, min = 2)
conservative <- plot(loc.r, min = 2)

liberal.plot <- ggplot(liberal$data, aes(x = entity, y = frequency)) +
  geom_bar(aes(x = entity, y = frequency), stat = "identity", color = "black",
           fill = "#2332A9") +
  xlab("Locations Mentioned in Liberal Blogs") + ylab("Count") +
  ylim(0, 45) +
  theme_bw() +
  theme(axis.title = element_text(face = "bold")) +
  coord_flip()

conservative.plot <- ggplot(conservative$data, aes(x = entity, y = frequency)) +
  geom_bar(aes(x = entity, y = frequency), stat = "identity", color = "black",
           fill = "#990000") +
  xlab("Organizations Mentioned in Conservative Blogs") + ylab("Count") +
  ylim(0, 45) +
  theme_bw() +
  theme(axis.title = element_text(face = "bold")) +
  coord_flip()

ggarrange(liberal.plot, conservative.plot, align = "v")


#Difference in proportions
people.d <- person_entity(as.character(blogs[which(blogs$rating=="Liberal"),]
                                       $documents))
people.r <- person_entity(as.character(blogs[which(blogs$rating=="Conservative"),]
                                       $documents))
liberal <- plot(people.d, min = 2)
conservative <- plot(people.r, min = 2)

liberal <- liberal$data
conservative <- conservative$data

merged <- merge(liberal, conservative, by = "entity", all = T)
merged[is.na(merged)] <- 0

merged$p.x <- merged$frequency.x/colSums(merged[, 2, drop = F])
merged$p.y <- merged$frequency.y/colSums(merged[, 3, drop = F])

merged$diff <- merged$p.x - merged$p.y

merged <- merged[order(merged$diff),]
merged$entity <- factor(merged$entity, levels = merged$entity)

ggplot(merged, aes(x = entity, y = diff)) +
  geom_bar(aes(x = entity, y = diff), fill = ifelse(merged$diff>=0, 
                                                   "#2332A9", "#990000"), 
           stat = "identity") +
  theme_bw() +
  ylab("People Mentioned on Conservative Blogs vs. People Mentioned on Liberal Blogs") +
  xlab("") +
  theme(axis.title = element_text(face = "bold")) +
  coord_flip()

  
#Save your R objects
save.image("ner.RData")

### END ###
