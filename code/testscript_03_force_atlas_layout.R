#### testscript_03_force_atlas_layout
rm(list = ls())

#load edges and nodes
graph_edges <- data.frame(read.csv("../not_shared/data/regional_partners_edges.csv", header = TRUE, stringsAsFactors = FALSE))
graph_nodes <- data.frame(read.csv("../not_shared/data/regional_partners_nodes.csv", header = TRUE, stringsAsFactors = FALSE))

#Acivate igraph and assign edges and nodes
library("igraph")
network_all <- graph_from_data_frame(d=graph_edges, vertices = graph_nodes, directed = F)

#Install the ForceAtlas package
install.packages("devtools")
if (!require("ForceAtlas2")) devtools::install_github("analyxcompany/ForceAtlas2")
library("ForceAtlas2")

##How Force Atlas is configured
layout.forceatlas2(graph, #An igraph network or a data frame of three columns: source, target, and weights
                   directed = TRUE, #Logical. TRUE if the network is directed. Ignored if graph is an igraph object
                   iterations = 100, #Number of iterations to be performed
                   linlog = FALSE, #Logical. If TRUE the algorithm uses logarithmic attraction force 'F <- log (1+F)'
                   pos = NULL, #A data frame or matrix (NumberOfNodes x dimension) of the initial locations of points. If NULL the initial positions are random
                   nohubs = FALSE, #Logical. If TRUE nodes with high indegree have more central position than nodes with outdegree (for directed graphs)
                   k = 400, #Is the repel constant: the greater the constant k the stronger the repulsion force between points
                   gravity = 1, #Gravity constant: indicates how strongly the nodes should be attracted to the center of gravity
                   ks = 0.1, #Speed constant: the greater the value of 'ks' the more movement the nodes make under the acting forces
                   ksmax = 10, #Limits the speed from above
                   delta = 1, #Modify attraction force, the weights are raised to the power of 'delta'
                   center = NULL, #Center of gravity
                   tolerate = 0.1, #Tolerance to swinging constant
                   dim = 2, #Dimension of the positions
                   plotstep = 10, #is the frequency of plotting intermediate iterations
                   plotlabels = TRUE) #Logical. If TRUE the labels should be included in the intermediate interations plot


layout1 <- layout.forceatlas2(network_all, 
                              directed = FALSE,
                              iterations = 500,
                              linlog = FALSE,
                              pos = NULL, 
                              nohubs = FALSE, 
                              k = 200, 
                              gravity = 0.5, 
                              ks = 0.1, 
                              ksmax = 10, 
                              delta = 1, 
                              center = NULL, 
                              #tolerate = 0.1, 
                              dim = 2, 
                              plotstep = 500, 
                              plotlabels = FALSE) 


plot(network_all, 
     layout=layout1,
     vertex.label = NA,
     vertex.shape = "circle",
     vertex.size = 5)

