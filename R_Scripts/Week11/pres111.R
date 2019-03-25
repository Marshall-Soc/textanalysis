
##############################
##  pres111.R: Code for Week 11, Day #1
##  Note: Corresponds to pres111.pdf.
##        See pres111.pdf for references.
##  Author: Marshall A. Taylor
##############################

### BEGIN ###

#Set working directory
setwd("set_working_directory")


#Necessary packages
install.packages(c("igraph", "network",
                   "NetData"))
library(igraph)
library(network)
library(NetData)


#Load in data
data("flo")


#Take a look at the matrix
flo


#As a graph
flo <- graph_from_adjacency_matrix(flo, mode = "undirected")
plot(flo, vertex.color = "#00aedb", vertex.label.color = "black",
     edge.color="black")


#Another example, as a matrix
data("kracknets")
krack <- krack_full_data_frame[c("ego", "alter", "reports_to_tie")] %>%
  subset(reports_to_tie > 0) %>%
  graph.data.frame() %>%
  get.adjacency() %>%
  as.matrix()


#As a graph
krack <- graph_from_adjacency_matrix(krack, mode = "directed")
plot(krack, vertex.color = "#00aedb", vertex.label.color = "black",
     edge.color="black", edge.arrow.size=0.3)


#Betweenness Centrality
krack.bet <- betweenness(krack, directed = T)

  #Color by betweenness
fine = 20
pal = colorRampPalette(c("orange", "purple"))
betweeb = pal(fine)[as.numeric(cut(krack.bet, breaks = fine))]

plot(krack, vertex.color = between, vertex.label.color = "black",
     edge.color="black", edge.arrow.size=0.3)


#Save your R objects
save.image("networks.RData")

### END ###