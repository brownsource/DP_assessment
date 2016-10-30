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

#Create an igraph object
survey_network <- graph_from_data_frame(d=survey_edges, vertices = survey_nodes, directed = T)

## CREATE SECTOR LEVEL NETWORK STATISTICS TABLE
network_statistics_sector <- data.frame(Graph=NA,
                                 Count_nodes=NA,
                                 Count_edges=NA,
                                 Density=NA,
                                 Centrality_out=NA,
                                 Centrality_in=NA,
                                 Centrality_all=NA,
                                 Shortest_path=NA,
                                 Avg_path_length=NA,
                                 Diameter=NA)


## CALCLUATE GENERAL NETWORK STATS

#set Graph name
network_statistics$Graph <- "ALL"
#Count graph nodes
vcount(survey_network)
network_statistics$Count_nodes <- vcount(survey_network)
#Count graph edges
network_statistics$Count_edges <-ecount(survey_network)
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

#CREATE A VERTEX OF FILTERNAMES
filter_names <- c("Filter.Sector.Basic_needs",
                  "Filter.Sector.Education",
                  "Filter.Sector.Food_security",
                  "Filter.Sector.Health",
                  "Filter.Sector.Livelihoods",
                  "Filter.Sector.Protection",
                  "Filter.Sector.Shelter",
                  "Filter.Sector.WASH")
filter_names[3]


network_statistics <- cbind(network_statistics, filter_names[4])

for (i in 1:8) {
  print(filter_names[i])
  network_statistics <- cbind(network_statistics, filter_names[i])
}
 

substr(filter_names, 15, nchar(filter_names[1]))
 
#THIS FILTERS... NEED TO INCLUDE IN THE LOOP
temp <- induced.subgraph(survey_network, which(V(survey_network)$Filter.Sector.Basic_needs==TRUE))


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
