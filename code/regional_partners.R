#### CREATES A TABLE AND SELECTION OF GRAPHICS SHOWING PARTNERS IN THE REGION
#### AND IN EACH COUNTRY. RUN EITHER ALL OF THE CODE OR JUST THE RELEVANT SECTIONS

### INPUT: 
## not_shared/data/regional/regional_partners_nodes.csv
## not_shared/data/regional/regional_partners_edges.csv

### OUTPUT:
## not_shared/output/jordan/jordan_DSA.csv
## not_shared/output/jordan/jordan_dsa.png
## not_shared/output/jordan/jordan_dsa_anon.png
## not_shared/output/lebanon/lebanon_DSA.csv
## not_shared/output/lebanon/lebanon_dsa.png
## not_shared/output/lebanon/lebanon_dsa_anon.png
## not_shared/output/regional/regional_partners.pdf
## not_shared/output/regional/regional_partners.png
## not_shared/output/regional/regional_partners.tiff
## not_shared/output/regional/regional_partners_anon.pdf
## not_shared/output/regional/regional_partners_anon.png
## not_shared/output/regional/regional_partners_anon.tiff
## not_shared/output/regional/regional_partners_DSA.pdf
## not_shared/output/regional/regional_partners_DSA.png
## not_shared/output/regional/regional_partners_DSA.tiff
## not_shared/output/regional/regional_partners_DSA_anon.pdf
## not_shared/output/regional/regional_partners_DSA_anon.png
## not_shared/output/regional/regional_partners_DSA_anon.tiff

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

### LOAD THE DATA
graph_edges <- data.frame(read.csv("../not_shared/data/regional/regional_partners_edges.csv", header = TRUE, stringsAsFactors = FALSE))
graph_nodes <- data.frame(read.csv("../not_shared/data/regional/regional_partners_nodes.csv", header = TRUE, stringsAsFactors = FALSE))

## CREATE THE IGRAPH OBJECTS
#Full regional network
regional_partners <- graph_from_data_frame(d=graph_edges, vertices = graph_nodes, directed = F, remove.loops = T)
regional_partners <- simplify(regional_partners, remove.multiple = F, remove.loops = T)
#Country level networks
jordan_partners <- induced.subgraph(regional_partners, which(V(regional_partners)$jordan==1))
lebanon_partners <- induced.subgraph(regional_partners, which(V(regional_partners)$lebanon==1))
#Vector of graph names
graph_objects <- c("regional_partners","jordan_partners","lebanon_partners")

## CREATE LAYOUTS
regional_partners_layout <- layout.forceatlas2(regional_partners)
jordan_partners_layout <- layout.forceatlas2(jordan_partners)
lebanon_partners_layout <- layout.forceatlas2(jordan_partners)
#Vector of layouts
graph_layouts <- c("regional_partners_layout","jordan_partners_layout","lebanon_partners_layout")


## SET THE LAYOUTS GRAPH
## BUGGER THIS BIT ISN"T WORKING... SHOULD THE VECTORS LAYOUTS AND OBJECTS BE OF THE NAMES OR 
## OBEJCTS THEMSELVES?
for (i in 1:1){
graph_layouts[i] <- layout.forceatlas2(graph_objects[i], 
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
}

for (i in 1:3){
  print(paste(c("layout",graph_objects[i]),collapse="_"))
}
rm(i)


