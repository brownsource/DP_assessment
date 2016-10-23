# CLEAR WORKSPACE
#
# Remove all the objects we created so far.

rm(list = ls()) # Remove all the objects we created so far.

# LIBRARIES
#
# Key packages 
# Install those now if you do not have the latest versions. 
# (please do NOT load them yet!)

install.packages("igraph") 
install.packages("network") 
install.packages("sna")
install.packages("visNetwork")
install.packages("ndtv", dependencies=T)
install.packages("RColorBrewer")
install.packages("extrafont")
install.packages("png")
install.packages("animation")
install.packages("maps")
install.packages("geosphere")

# UNHCR MAIN COLOUR THEME
# Additional colour alternatives (http://www.colorhexa.com/) or (http://colorbrewer2.org/)
# 
# MAIN  HEX = #005EB8 = rgb(  0, 94,184) = WEB SAFE = 0066cc
# LIGHT HEX = #1188FB = rgb( 17,136,251) = WEB SAFE = 0099ff
# DARK  HEX = #00498F = rgb(  0, 73,143) = WEB SAFE = 003399

# INSTALL FONTS
#
# library('extrafont')
# font_import("Arial") # FIND AND INSTALL HELVITICA NUE
# fonts() # See what font families are available to you now.
# loadfonts(device = "win") # use device = "pdf" for pdf plot output. 

# LOAD IN THE DATA
#
graph_edges <- data.frame(read.csv("../not_shared/data/regional_partners_edges.csv", header = TRUE, stringsAsFactors = FALSE))
graph_nodes <- data.frame(read.csv("../not_shared/data/regional_partners_nodes.csv", header = TRUE, stringsAsFactors = FALSE))

# CREATE AN IGRAPH OBJECT
#
library("igraph")
network_all <- graph_from_data_frame(d=graph_edges, vertices = graph_nodes, directed = F)

# DISPLAY A VISUALISATION
#
# Remove loops
network_all <- simplify(network_all, remove.multiple = F, remove.loops = T)
par(bg="white") # Set the background
plot(network_all,
     layout=layout_with_fr(network_all)*20000,
     vertex.color=rgb(17,136,251, maxColorValue = 255),
     vertex.frame.color=NA,
     vertex.shape="circle",
     vertex.size=5,
     vertex.label=V(network_all)$label, # Uses the label field
     #vertex.label.family="Arial",
     vertex.label.font=1, # 1 plain, 2 bold, 3, italic, 4 bold italic, 5 symbol
     vertex.label.cex=0.5, # Font size in proportion to node
     vertex.label.dist=0,
     vertex.label.degree=0,
     edge.color="grey40",
     edge.width=1,
     #edge.arrow.size=NA)
     #edge.arrow.width=NA
     #edge.lty=1 # Line type, could be 0 or “blank”, 1 or “solid”, 2 or “dashed”, 3 or “dotted”, 4 or “dotdash”, 5 or “longdash”, 6 or “twodash”
     #edge.label=NA,
     #edge.label.family=NA,
     #edge.label.font=NA,
     #edge.label.cex=NA,
     #edge.curved=0 # Edge curvature, range 0-1 (FALSE sets it to 0, TRUE to 0.5)
     margin=0.2, #Empty space margins around the plot, vector with length 4
     frame=FALSE,
     main="Regional map of organisations in MENA",
     sub="Source: 3RP applications and other"
     #asp # Numeric, the aspect ratio of a plot (y/x).
     #palette # A color palette to use for vertex color
     #rescale # Whether to rescale coordinates to [-1,1]. Default is TRUE
     )

library('animation')
ani.options("convert") # Check that the package knows where to find ImageMagick
# If it doesn't know where to find it, give it the correct path for your system.
ani.options(convert="C:/Program Files/ImageMagick-6.8.8-Q16/convert.exe") 



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




