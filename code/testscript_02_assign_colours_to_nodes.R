#### testscript_02_assign_colours_to_nodes
rm(list = ls())

#load edges and nodes
graph_edges <- data.frame(read.csv("../not_shared/data/regional_partners_edges.csv", header = TRUE, stringsAsFactors = FALSE))
graph_nodes <- data.frame(read.csv("../not_shared/data/regional_partners_nodes.csv", header = TRUE, stringsAsFactors = FALSE))

#Acivate igraph and assign edges and nodes
library("igraph")
network_all <- graph_from_data_frame(d=graph_edges, vertices = graph_nodes, directed = F)


#Assign colours to vertexes based on type
V(network_all)[V(network_all)$type == "Country"]$color <- "#009460"
V(network_all)[V(network_all)$type == "UN Agency"]$color <- "#0072B6"
V(network_all)[V(network_all)$type == "Government"]$color <- "#FF5300"
V(network_all)[V(network_all)$type == "INGO"]$color <- "#FF8C00"
V(network_all)[V(network_all)$type == "Not yet classified"]$color <- "grey80"



#Check assigned
V(network_all)$type
V(network_all)$color


graph_layout_01 <- layout_with_fr(network_all, 
                                  coords = NULL, 
                                  dim = 2, 
                                  niter = 1000,
                                  start.temp = sqrt(vcount(network_all)), 
                                  grid = c("auto", "grid", "nogrid"),
                                  weights = NULL, 
                                  minx = NULL, 
                                  maxx = NULL, 
                                  miny = NULL, 
                                  maxy = NULL,
                                  minz = NULL, 
                                  maxz = NULL)


plot(network_all,
     layout = layout_with_fr,
     vertex.color = V(network_all)$color,
     vertex.shape = "circle",
     vertex.size = 5,
     vertex.label = NA,
     edge.color="grey80",
     edge.width=1,
     margin = 0, #Empty space margins around the plot, vector with length 4
     frame = FALSE # Option to add a frame around the chart
)

legend(x=-1.5, y=-1.1, 
       c("Country","UN Agency", "Government", "INGO", "Not yet classified"), 
       pch=21,
       col="#777777", 
       pt.bg=c("#009460","#0072B6","#FF5300","#FF8C00","grey80"), 
       pt.cex=2, 
       cex=.8, 
       bty="n", 
       ncol=1)