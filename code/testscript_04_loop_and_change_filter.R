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
#network_basic_needs <- induced.subgraph(network_all, which(V(network_all)$Filter.Sector.Basic_needs==TRUE))
#network_education <- induced.subgraph(network_all, which(V(network_all)$Filter.Sector.Education==TRUE))
#network_food_security <- induced.subgraph(network_all, which(V(network_all)$Filter.Sector.Food_security==TRUE))
#network_health <- induced.subgraph(network_all, which(V(network_all)$Filter.Sector.Health==TRUE))
#network_livelihoods <- induced.subgraph(network_all, which(V(network_all)$Filter.Sector.Livelihoods==TRUE))
#network_protection <- induced.subgraph(network_all, which(V(network_all)$Filter.Sector.Protection==TRUE))
#network_shelter <- induced.subgraph(network_all, which(V(network_all)$Filter.Sector.Shelter==TRUE))
#network_wash <- induced.subgraph(network_all, which(V(network_all)$Filter.Sector.WASH==TRUE))

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
centrality_out <- degree(network_all, mode="out")
V(network_all)$size <- (centrality_out-min(centrality_out))/(max(centrality_out)-min(centrality_out)) * (25-5)+5
plot(network_all, layout=layout_all, edge.arrow.size=.4,vertex.label=NA,main="all - centrality_out")

#Plot centrality_in
V(network_all)$color <- "Grey80"
centrality_in <- degree(network_all, mode="in")
V(network_all)$size <- (centrality_in-min(centrality_in))/(max(centrality_in)-min(centrality_in)) * (25-5)+5
plot(network_all, layout=layout_all, edge.arrow.size=.4,vertex.label=NA,main="all - centrality_in")

#Plot centrality_all
V(network_all)$color <- "Grey80"
centrality_all <- degree(network_all, mode="all")
V(network_all)$size <- (centrality_all-min(centrality_all))/(max(centrality_all)-min(centrality_all)) * (25-5)+5
plot(network_all, layout=layout_all, edge.arrow.size=.4,vertex.label=NA,main="all - centrality_all")

#Colour ramp by data protection rating for technical measures
fine = 500 # this will adjust the resolving power.
palette = colorRampPalette(c('red','green'))
V(network_all)$color = palette(fine)[as.numeric(cut(V(network_all)$Format.Data_protection.Technical_measures,breaks = fine))]
V(network_all)$color
V(network_all)[V(network_all)$Replied_or_nominated == "Nominated"]$color <- "Grey80"
V(network_all)$color
plot(network_all, layout=layout_all, edge.arrow.size=.4,vertex.label=NA,main="all - technical measures")
