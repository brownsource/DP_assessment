#### Calculate network statistics

#CLEAR WORKSPACE
rm(list = ls()) # Remove all the objects we created so far.

#INITIALISING R
#Check to see if required packages are present
if(!require(igraph)){
  install.packages("igraph")
}
library(igraph)

if(!require(devtools)){
  install.packages("devtools")
}

if (!require("ForceAtlas2")) devtools::install_github("analyxcompany/ForceAtlas2")
library("ForceAtlas2")


#LOAD THE DATA
#Load in external graph files
survey_edges <- data.frame(read.csv("../not_shared/output/edges.csv", header = TRUE, stringsAsFactors = FALSE))
survey_nodes <- data.frame(read.csv("../not_shared/output/nodes.csv", header = TRUE, stringsAsFactors = FALSE))

#Create an igraph objects
network_all <- graph_from_data_frame(d=survey_edges, vertices = survey_nodes, directed = T)
network_basic_needs <- induced.subgraph(network_all, which(V(network_all)$Filter.Sector.Basic_needs==TRUE))
network_education <- induced.subgraph(network_all, which(V(network_all)$Filter.Sector.Education==TRUE))
network_food_security <- induced.subgraph(network_all, which(V(network_all)$Filter.Sector.Food_security==TRUE))
network_health <- induced.subgraph(network_all, which(V(network_all)$Filter.Sector.Health==TRUE))
network_livelihoods <- induced.subgraph(network_all, which(V(network_all)$Filter.Sector.Livelihoods==TRUE))
network_protection <- induced.subgraph(network_all, which(V(network_all)$Filter.Sector.Protection==TRUE))
network_shelter <- induced.subgraph(network_all, which(V(network_all)$Filter.Sector.Shelter==TRUE))
network_wash <- induced.subgraph(network_all, which(V(network_all)$Filter.Sector.WASH==TRUE))

#Create dataframe for network stats
network_statistics_sector <- data.frame(Graph=NA, Count_nodes=NA, Count_edges=NA, Density=NA, Centrality_out=NA, Centrality_in=NA, Centrality_all=NA, Shortest_path=NA, Avg_path_length=NA, Diameter=NA)
network_statistics_sector <- data.frame(Graph=NA)
#Create graph labels
graphs <- data.frame(names c("All", "Basic_needs", "Education", "Food_security", "Health", "Livelihoods", "Protection", "Shelter", "WASH"))
#Create list of graph names
graphs_names <- data.frame(c("network_all", "network_basic_needs", "network_education", "network_food_security", "network_health", "network_livelihoods", "network_protection", "network_shelter", "network_wash"))


graph_stats <- function(x){
  c(
    #set Graph name
    network_statistics_sector <- rbind(network_statistics_sector$Graph,graphs[2])
  )
}
    #Count graph nodes
    vcount(graphs_names[i])
    network_statistics$Count_nodes <- vcount(graphs_names[i])
    #Count graph edges
    network_statistics$Count_edges <- ecount(graphs_names[i])
    # Density
    network_statistics$Density <- graph.density(graphs_names[i], loops = FALSE)
    # Average outbound degree centrality
    network_statistics$Centrality_out <- mean(degree(graphs_names[i], mode="out"))
    # Average inbound degree centrality
    network_statistics$Centrality_in <- mean(degree(graphs_names[i], mode="in"))
    # Average undirected degree centrality
    network_statistics$Centrality_all <- mean(degree(graphs_names[i], mode="all"))
    # Shortest path
    network_statistics$Shortest_path <- NA
    # Average path length
    network_statistics$Avg_path_length <- NA
    # Diameter
    network_statistics$Diameter <- diameter(graphs_names[i], directed=TRUE)
  )
}




lapply(seq_along(graphs_names),
       function(i) plot(get(graphs_names[i]), 
                        layout=layout_all,
                        main=graphs[i]))


for(i in graphs_names){

}
rm(i)

plot(network_all)

layout_all <- layout_with_fr(network_all)

#Plot network charts
V(network_all)$color <- "Grey80"
V(network_all)$size <- 15
plot(network_all, layout=layout_all, edge.arrow.size=.4,vertex.label=NA,main="all")
#plot(network_basic_needs, edge.arrow.size=.4,vertex.label=NA,main="basic needs")
#plot(network_education, edge.arrow.size=.4,vertex.label=NA,main="education")
#plot(network_food_security, edge.arrow.size=.4,vertex.label=NA,main="food security")
#plot(network_health, edge.arrow.size=.4,vertex.label=NA,main="health")
#plot(network_livelihoods, edge.arrow.size=.4,vertex.label=NA,main="livelihoods")
#plot(network_protection, edge.arrow.size=.4,vertex.label=NA,main="protection")
#plot(network_shelter, edge.arrow.size=.4,vertex.label=NA,main="shelter")
#plot(network_wash, edge.arrow.size=.4,vertex.label=NA,main="wash")

#Plot replied and nominated
V(network_all)[V(network_all)$Replied_or_nominated == "Replied"]$color <- "#1A9641"
V(network_all)[V(network_all)$Replied_or_nominated == "Nominated"]$color <- "#D7191C"
V(network_all)$size <- 15
plot(network_all, layout=layout_all, edge.arrow.size=.4,vertex.label=NA,main="all - replied and nominated")

#Plot centrality_out
V(network_all)$color <- "Grey80"
V(network_all)$centrality_out <- degree(network_all, mode="out")
V(network_all)$size <- (V(network_all)$centrality_out-min(V(network_all)$centrality_out))/(max(V(network_all)$centrality_out)-min(V(network_all)$centrality_out)) * (25-5)+5
plot(network_all, layout=layout_all, edge.arrow.size=.4,vertex.label=NA,main="all - centrality_out")

#Plot centrality_in
V(network_all)$color <- "Grey80"
V(network_all)$centrality_in <- degree(network_all, mode="in")
V(network_all)$size <- (V(network_all)$centrality_in-min(V(network_all)$centrality_in))/(max(V(network_all)$centrality_in)-min(V(network_all)$centrality_in)) * (25-5)+5
plot(network_all, layout=layout_all, edge.arrow.size=.4,vertex.label=NA,main="all - centrality_in")

#Plot centrality_all
V(network_all)$color <- "Grey80"
V(network_all)$centrality_all <- degree(network_all, mode="all")
V(network_all)$size <- (V(network_all)$centrality_all-min(V(network_all)$centrality_all))/(max(V(network_all)$centrality_all)-min(V(network_all)$centrality_all)) * (25-5)+5
plot(network_all, layout=layout_all, edge.arrow.size=.4,vertex.label=NA,main="all - centrality_all")

#Plot betweeness
V(network_all)$color <- "Grey80"
V(network_all)$betweenness <- betweenness(network_all)
V(network_all)$size <- (V(network_all)$betweenness-min(V(network_all)$betweenness))/(max(V(network_all)$betweenness)-min(V(network_all)$betweenness)) * (25-5)+5
plot(network_all, layout=layout_all, edge.arrow.size=.4,vertex.label=NA,main="all - betweenness")

plot(degree.distribution(network_all))

