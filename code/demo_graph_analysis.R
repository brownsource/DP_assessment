rm(list = ls()) # Remove all the objects we created so far.
install.packages("igraph") # Install the iGraph package
library(igraph) # Load the igraph package
help("plot.igraph") # See this for ideas on how to change the formatting

# Load in the data table
graph_edges <- data.frame(read.csv("../not_shared/data/demo_edges.csv", header = TRUE, stringsAsFactors = FALSE))
graph_nodes <- data.frame(read.csv("../not_shared/data/demo_nodes.csv", header = TRUE, stringsAsFactors = FALSE))

# Create a graph data frame from the edges table
g1 <- graph.data.frame(
  graph_edges[,1:2],
    directed = FALSE
)

# Create a demo plot
plot(g1, 
     vertex.size=5, 
     vertex.label=V(g1)$name,
     vertex.color="blue", 
     vertex.frame.color="blue",
     vertex.label.color="black",
     vertex.label.cex=0.8, 
     vertex.label.dist=1,
     vertex.label=TRUE,
     vertex.label.font=.001,
     
     edge.curved=0.0,
     edge.arrow.size=0.01,
     edge.color="grey",  
     
     rescale=TRUE,
     add=FALSE)

# Create a graph data from both tables
g2 <- graph_from_data_frame(graph_edges, directed = FALSE, vertices = graph_nodes)
print(g2, e=TRUE, v=TRUE)
plot(g2, 
     vertex.label=V(g2)$label,
     vertex.size=5,
     vertex.color="blue", 
     vertex.frame.color="blue",
     vertex.label.color="black",
     vertex.label.cex=0.8, 
     vertex.label.dist=1,
     vertex.label=TRUE,
     vertex.label.font=.001,
     
     edge.curved=0.0,
     edge.arrow.size=0.01,
     edge.color="grey",  
     
     rescale=TRUE,
     add=FALSE)





#Lists the edges and vertices
E(g2)
V(g2)

# Assign Labels and names to the graph


# Create a colour palatte

# Filter certain nodes / edges


# Calculate:
# Density
# Vertex degree
# View as histogram
# Degree distribution
# View as histogram
# Power law
# Graph connectivity
# Shortest patth
# Diameter
# Average path length
# Print an adjacency matrix
# Degree centrality, Closeness, Betweennes, Eigenvector
# Prestige


from <- c(graph_edges[,1])
to <- c(graph_edges[,2])
structure <- data.frame("from" = from, "to" = to)
g <- graph.data.frame(structure)
#V(g)$label <- names

names <- data.frame(graph_nodes[,1:2])
V(g)$label <- names[V(g)$name]
plot(g)

