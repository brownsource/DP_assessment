#### CREATES THE REQUIRED NODE AND EDGE TABLES FOR NETWORK ANALYSIS
#### YOU NEED TO SPECIFY THE COUNTRY AT THE START OF THE SCRIPT

### INPUT: 
## not_shared/output/[COUNTRY]/[COUNTRY]_nodes.csv
## not_shared/output/[COUNTRY]/[COUNTRY]_edges.csv

### OUTPUT:
## not_shared/output/[COUNTRY]/[COUNTRY]_nodes.csv
## not_shared/output/[COUNTRY]/[COUNTRY]_edges.csv
## not_shared/output/[COUNTRY]/[COUNTRY]_comments.csv

### CLEAR WORKSPACE
rm(list = ls()) # Remove all the objects we created so far.

### INSTALL PACKAGES IF REQUIRED
if(!require(extrafont)){
  install.packages("extrafont")
}
if(!require(igraph)){
  install.packages("igraph")
}
if(!require(devtools)){
  install.packages("devtools")
}

### INSTALL LIBRARIES IF REQUIRED
### NEED TO CHANGE THIS BLOCK OF CODE
library(extrafont)
font_import()
loadfonts()
library(igraph)
if (!require("ForceAtlas2")) devtools::install_github("analyxcompany/ForceAtlas2")
library("ForceAtlas2")
library(plyr)

### SELECT WHICH COUNTRY
country <- "jordan"
# country <- "lebanon"

### LOAD THE DATA
survey_edges <- data.frame(read.csv(paste("../not_shared/output/", country, "/", country, "_edges.csv", sep=""),
                                    header = TRUE, 
                                    stringsAsFactors = FALSE))
survey_nodes <- data.frame(read.csv(paste("../not_shared/output/", country, "/", country, "_nodes.csv", sep=""),
                                    header = TRUE,
                                    stringsAsFactors = FALSE))

## CREATE THE IGRAPH OBJECTS
#Full country network
survey_network <- graph_from_data_frame(d=survey_edges, vertices = survey_nodes, directed = F)
survey_network <- simplify(survey_network, remove.multiple = F, remove.loops = T)

#Sector level networks
survey_network_basic_needs <- induced.subgraph(survey_network, which(V(survey_network)$Filter.Sector.Basic_needs=="True"))
survey_network_education <- induced.subgraph(survey_network, which(V(survey_network)$Filter.Sector.Education=="True"))
survey_network_food_security <- induced.subgraph(survey_network, which(V(survey_network)$Filter.Sector.Food_security=="True"))
survey_network_health <- induced.subgraph(survey_network, which(V(survey_network)$Filter.Sector.Health=="True"))
survey_network_livelihoods <- induced.subgraph(survey_network, which(V(survey_network)$Filter.Sector.Livelihoods=="True"))
survey_network_protection <- induced.subgraph(survey_network, which(V(survey_network)$Filter.Sector.Protection=="True"))
survey_network_shelter <- induced.subgraph(survey_network, which(V(survey_network)$Filter.Sector.Shelter=="True"))
survey_network_wash <- induced.subgraph(survey_network, which(V(survey_network)$Filter.Sector.WASH=="True"))


################################################################################
############################## SECTOR LEVEL ####################################
################################################################################

## Sector level table
network_statistics_sectors <- data.frame(Graph=NA,
                                         Count_nodes=NA,
                                         Count_edges=NA,
                                         Density=NA,
                                         Centrality_out=NA,
                                         Centrality_in=NA,
                                         Centrality_all=NA,
                                         Shortest_path=NA,
                                         Avg_path_length=NA,
                                         Diameter=NA)

## calculate stats
network_statistics_sectors$Graph <- "ALL" #set Graph name
network_statistics_sectors$Count_nodes <- vcount(survey_network) #Count graph nodes
network_statistics_sectors$Count_edges <-ecount(survey_network) #Count graph edges
network_statistics_sectors$Density <- graph.density(survey_network, loops = FALSE) # Density
network_statistics_sectors$Centrality_out <- mean(degree(survey_network, mode="out")) # Average outbound degree centrality
network_statistics_sectors$Centrality_in <- mean(degree(survey_network, mode="in")) # Average inbound degree centrality
network_statistics_sectors$Centrality_all <- mean(degree(survey_network, mode="all")) # Average undirected degree centrality
network_statistics_sectors$Shortest_path <- NA # Shortest path
network_statistics_sectors$Avg_path_length <- NA # Average path length
network_statistics_sectors$Diameter <- diameter(survey_network, directed=TRUE) # Diameter

################################################################################
############################ DP MEASURE LEVEL ##################################
################################################################################





################################################################################
########################### ORGANISATION LEVEL #################################
################################################################################

