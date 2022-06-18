library(igraph)
library(dplyr)
library(ggplot2)

############## Q1 ##################
data<-read.csv("D:/MSc Business Analytics/Social Network Analysis/Assignment 1/asoiaf-all-edges.csv")
data<-data[,c("Source", "Target", "weight")]

#create an igraph graph
set.seed(123)
g <- graph_from_data_frame(data, directed=FALSE)
plot(g, main="Initial graph")


############## Q2 ##################
### Number of vertices
Number_of_vertices<-V(g)
Number_of_vertices #796
gorder(g)

###Number of edges
Number_of_edges<-gsize(g)
Number_of_edges #2823

###Diameter of the graph
is.weighted(g)
E(g)$weight 

Diameter_of_the_graph<-diameter(g, directed = FALSE, unconnected = TRUE, weights= E(g)$weight)
Diameter_of_the_graph #53


###Number of triangles
Number_of_triangles<-count_triangles(g, vids = V(g))
sum(Number_of_triangles) #16965

## Adjacenct triangles
plot(g, vertex.label=Number_of_triangles)


###The top-10 characters of the network as far as their degree is concerned
s<-sort(degree(g), decreasing=TRUE)
s[c(1:10)]

###The top-10 characters of the network as far as their weighted degree is concerned
s1<-sort(strength(g), decreasing=TRUE)
s1[c(1:10)]


############## Q3 ##################

set.seed(123)
plot(g, edge.arrow.width = 0.8, edge.arrow.size = 0.3, vertex.size = 3,edge.color="yellow1", vertex.color="violetred2", vertex.label = NA, main="Graph of the entire network")

# we want a sub-network containing the all vertices that have more than 10 connections in the network:
more_than_10 <- degree(g)>10
g2 <- induced.subgraph(graph=g,vids=more_than_10)
vertex_colors <- rep("pink", gorder(g2))
plot(g2, vertex.color = vertex_colors,
     vertex.label = NA, edge.arrow.width = 0.8,vertex.shape="csquare",
     edge.arrow.size = 0.2, vertex.size = 5, main="Network with Nodes with more than 10 connections")

###The density of a graph is the ratio of the number of edges and the number of possible edges.

#edge density of the entire graph
sum(which_loop(g))
round(edge_density(g, loops=F), 4) #0.0089

#edge density of the subgraph
which_loop(g2)
round(edge_density(g2, loops=F), 4) #0.1259

############## Q4 ##################

#Closeness measures how many steps are required to access every other node from a given node. 
#It's a measure of how long information takes to arrive (who hears news first?). Higher values mean less centrality.

closeness<-sort(closeness(g,  weights= E(g)$weight, normalized=TRUE))
closeness[c(1:15)]

closenessf<-sort(closeness(g,  weights= E(g)$weight, normalized=FALSE))
closenessf[c(1:15)]

#Betweenness measures brokerage or gatekeeping potential. 
#It is (approximately) the number of shortest paths between nodes that pass through a particular node.

betweennes<-sort(betweenness(g,  weights= E(g)$weight), decreasing=TRUE)
betweennes[c(1:15)]

#Jon Snow 
betweenness(g, v="Jon-Snow", weights= E(g)$weight ) #41698.94 
estimate_closeness(g, v="Jon-Snow", cutoff=0,  weights= E(g)$weight) #0.0001106072

############## Q5 ##################

#Page rank approximates probability that any message will arrive to a particular node. 

pr<-sort(page_rank(g)$vector)

set.seed(123)
npageRank <- page_rank(g)
npageRank

# Represent PageRank as size and color of vertices
V(g)$size <- 3 + 100 * round(npageRank$vector[V(g)], 2)
V(g)$color <- 100 * round(npageRank$vector[V(g)], 2) + 98

plot(g, layout = layout.fruchterman.reingold,
     vertex.size = V(g)$size,
     vertex.color= V(g)$color,
     vertex.label = NA,
     vertex.frame.color= "white",
     vertex.label.color = "pink",
     vertex.label.family = "sans",
     edge.width=1,  
     edge.arrow.width=0.5,
     edge.color="plum",
     main="Top Nodes by page rank")

