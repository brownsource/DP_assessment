rm(list = ls()) # Remove all the objects we created so far.
install.packages("igraph") # Install the iGraph package
library(igraph) # Load the igraph package

graph_edges <- data.frame(read.csv("../not_shared/data/demo_edges.csv", header = TRUE, stringsAsFactors = FALSE))
graph_nodes <- data.frame(read.csv("../not_shared/data/demo_nodes.csv", header = TRUE, stringsAsFactors = FALSE))

g1 <- graph.data.frame(
  graph_edges[,1:2],
  directed = FALSE
)

plot(g1, 
     edge.arrow.size=.5,
     vertex.color="gold",
     vertex.size=5,
     vertex.frame.color="gray",
     vertex.label.color="black",
     vertex.label.cex=0.8,
     vertex.label.dist=20000,
     edge.curved=0.0)

E(g1)
V(g1)

g1 <- set_graph_attr(g1, "name", "NAME")
g1 <- set_graph_attr(g1, "label", "LABEL")
graph_attr_names(g1)


V(g1)$name <- graph_nodes$Label[match(V(g1)$id, nodes$ID)]




# <- partner_list$Name[match(nodes$Id,partner_list$ID)]

g1[]