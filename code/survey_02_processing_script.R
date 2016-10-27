#### Calculate network statistics

#CLEAR WORKSPACE
rm(list = ls()) # Remove all the objects we created so far.

#INITIALISING R
#Check to see if required packages are present
if(!require(igraph)){
  install.packages("igraph")
  library(igraph)
}
if(!require(devtools)){
  install.packages("devtools")
}

if (!require("ForceAtlas2")) devtools::install_github("analyxcompany/ForceAtlas2")
library("ForceAtlas2")


#LOAD THE DATA
#Load in external graph files
survey_edges <- data.frame(read.csv("../not_shared/output/edges.csv", header = TRUE, stringsAsFactors = FALSE))
survey_nodes <- data.frame(read.csv("../not_shared/output/nodes.csv", header = TRUE, stringsAsFactors = FALSE))

#Create an igraph object
survey_network <- graph_from_data_frame(d=survey_edges, vertices = survey_nodes, directed = T)

#CALCLUATE GENERAL NETWORK STATS
network_statistics <- data.frame(Graph=NA,
                                 Density=NA,
                                 Centrality_out=NA,
                                 Centrality_in=NA,
                                 Centrality_all=NA,
                                 Shortest_path=NA,
                                 Avg_path_length=NA,
                                 Diameter=NA)
#set Graph name
network_statistics$Graph <- "ALL"
# Density
network_statistics$Density <- graph.density(survey_network, loops = FALSE)
# Average outbound degree centrality
network_statistics$Centrality_out <- mean(degree(survey_network, mode="out"))
# Average inbound degree centrality
network_statistics$Centrality_in <- mean(degree(survey_network, mode="in"))
# Average undirected degree centrality
network_statistics$Centrality_all <- mean(degree(survey_network, mode="all"))
# Shortest path
network_statistics$Shortest_path <- NA
# Average path length
network_statistics$Avg_path_length <- NA
# Diameter
network_statistics$Diameter <- diameter(survey_network, directed=TRUE)


#CALCLUATE NODE STATS
node_statistics <- data.frame(Organisation_ID=NA,
                              Organisation_name=NA,
                                 Density=NA,
                                 Centrality_out=NA,
                                 Centrality_in=NA,
                                 Centrality_all=NA,
                                 Shortest_path=NA,
                                 Avg_path_length=NA,
                                 Diameter=NA)
#Outbound degree
degree(survey_network, mode="out")
#Inbund degree (Prestige)
degree(survey_network, mode="in")
#Undirected degree
degree(survey_network, mode="all")
#Betweenness centrality
betweenness(survey_network, 
            v = V(survey_network), 
            directed = TRUE, 
            weights = NULL,
            nobigint = TRUE, 
            normalized = FALSE)


#CALCLUATE EDGE STATS
#Edge betweenness centrality
edge_betweenness(survey_network, 
                 e = E(survey_network), 
                 directed = TRUE, 
                 weights = NULL)

#VISUALISATIONS
#Degree distribution

#COMMUNITY DETECTION
