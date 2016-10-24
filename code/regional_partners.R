# CLEAR WORKSPACE
#
# Remove all the objects we created so far.
rm(list = ls()) # Remove all the objects we created so far.

# LIBRARIES
# install.packages("network") 
# install.packages("sna")
# install.packages("visNetwork")
# install.packages("ndtv", dependencies=T)
# install.packages("png")
# install.packages("animation")
# install.packages("maps")
# install.packages("geosphere")

# ESTABLISH COLOUR SCHEME
# install.packages("RColorBrewer")
library('RColorBrewer')
# display.brewer.pal(5, "Dark2") # Example contrasting dark colours
# UNHCR colour scheme for three blues
# MAIN  HEX = #005EB8 = rgb(  0, 94,184) = WEB SAFE = 0066cc
# LIGHT HEX = #1188FB = rgb( 17,136,251) = WEB SAFE = 0099ff
# DARK  HEX = #00498F = rgb(  0, 73,143) = WEB SAFE = 003399
# Additional colour alternatives (http://www.colorhexa.com/) or (http://colorbrewer2.org/)

# INSTALL FONTS
# install.packages("sysfonts")
# install.packages("showtext")
library(sysfonts)
library(showtext)
font.add("arial", "arial.ttf") # Ideally this would be Helvetica Neue
# Some code to test that the font is working
## pdf("output/test.pdf") 
## showtext.begin() 
## par(family = "arial") 
## plot(1, main = "This font is arial", type = "n") 
## text(1, 1, "Some Text", cex = 5) 
## showtext.end() 
## dev.off() 

# LOAD IN THE DATA
graph_edges <- data.frame(read.csv("../not_shared/data/regional_partners_edges.csv", header = TRUE, stringsAsFactors = FALSE))
graph_nodes <- data.frame(read.csv("../not_shared/data/regional_partners_nodes.csv", header = TRUE, stringsAsFactors = FALSE))

# CREATE AN IGRAPH OBJECT
# Install.packages("igraph")
library("igraph")
# Load the graph object
network_all <- graph_from_data_frame(d=graph_edges, vertices = graph_nodes, directed = F)
# Remove loops
network_all <- simplify(network_all, remove.multiple = F, remove.loops = T)
# Calculate the degree for each node
deg <- degree(network_all, mode="all")
# Create a node size within a range
V(network_all)$size <- (deg-min(deg))/(max(deg)-min(deg)) * (25-5)+5
V(network_all)$label.cex  <- (deg-min(deg))/(max(deg)-min(deg)) * (1-0.5)+0.5

#colrs <- c("gray50", "tomato", "gold")
#V(net)$color <- colrs[V(net)$media.type]

graph_layout <- layout_with_fr(network_all, 
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



# DISPLAY A VISUALISATION
par(bg="white") # Set the background
plot(network_all,
     layout = graph_layout,
     vertex.color = rgb(17,136,251, maxColorValue = 255),
     vertex.frame.color = rgb(255,255,255, maxColorValue = 255),
     vertex.shape = "circle",
     vertex.size = network_all$size,
     vertex.label = V(network_all)$acronym, # Uses the label field
     vertex.label.family = "Arial",
     vertex.label.font = 1, # 1 plain, 2 bold, 3, italic, 4 bold italic, 5 symbol
     vertex.label.cex = V(network_all)$label.cex, #0.5, # Font size in proportion to node
     vertex.label.dist = 0,
     vertex.label.degree = 0,
     edge.color="grey80",
     edge.width=1,
     #edge.arrow.size=NA)
     #edge.arrow.width=NA
     #edge.lty=1 # Line type, could be 0 or “blank”, 1 or “solid”, 2 or “dashed”, 3 or “dotted”, 4 or “dotdash”, 5 or “longdash”, 6 or “twodash”
     #edge.label=NA,
     #edge.label.family=NA,
     #edge.label.font=NA,
     #edge.label.cex=NA,
     #edge.curved=0 # Edge curvature, range 0-1 (FALSE sets it to 0, TRUE to 0.5)
     margin = 0, #Empty space margins around the plot, vector with length 4
     frame = FALSE # Option to add a frame around the chart
     #main = "Chart title", # Adds a chart title
     #sub = "Chart subtitle", # Adds a subtitle
     #asp = # Numeric, the aspect ratio of a plot (y/x).
     #palette = # A color palette to use for vertex color
     #rescale = # Whether to rescale coordinates to [-1,1]. Default is TRUE
     )



library('visNetwork') 
visNetwork(graph_nodes, graph_edges, width="100%", height="400px", main="MENA regional partners")

nodes$shape <- "dot"  
nodes$shadow <- TRUE # Nodes will drop shadow
nodes$title <- nodes$media # Text on click
nodes$label <- nodes$type.label # Node label
#nodes$size <- nodes$audience.size # Node size
nodes$borderWidth <- 2 # Node border width

#nodes$color.background <- c("slategrey", "tomato", "gold")[graph_nodes$type]
nodes$color.border <- "black"
nodes$color.highlight.background <- "orange"
nodes$color.highlight.border <- "darkred"

visNetwork(graph_nodes, graph_edges)



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
# as_adjacency_matrix(network_all)
# Degree centrality, Closeness, Betweennes, Eigenvector
# Prestige




