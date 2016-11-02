####Script to display all of the partners listed in the region

#CLEAR WORKSPACE
rm(list = ls()) # Remove all the objects we created so far.

#INITIALISING R
#Check to see if required packages are present
if(!require(extrafont)){
  install.packages("extrafont")
  library(extrafont)
  font_import()
  loadfonts()
}

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
graph_edges <- data.frame(read.csv("../not_shared/data/regional_partners_edges.csv", header = TRUE, stringsAsFactors = FALSE))
graph_nodes <- data.frame(read.csv("../not_shared/data/regional_partners_nodes.csv", header = TRUE, stringsAsFactors = FALSE))


#Create an igraph object
network_all <- graph_from_data_frame(d=graph_edges, vertices = graph_nodes, directed = F)

#SET THE LAYOUT PARAMETERS
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

#If you need to re-run the force atlas to create a more pleaseing rotation
#For some reason either these two lines will stop the code above working OR
#It won't work if you create a plot. 
#The temporary solution is to remove all items from the environment and re-run the script
E(network_all)$from <- graph_edges$from
E(network_all)$to <- graph_edges$to

#SET THE PLOT PARAMETERS
#Assign colours to vertexes based on type
V(network_all)[V(network_all)$type == "Country"]$color <- "#009460"
V(network_all)[V(network_all)$type == "UN Agency"]$color <- "#0072B6"
V(network_all)[V(network_all)$type == "Government"]$color <- "#FF5300"
V(network_all)[V(network_all)$type == "INGO"]$color <- "#FF8C00"
V(network_all)[V(network_all)$type == "Local"]$color <- "#FFCC00"
V(network_all)[V(network_all)$type == "Other"]$color <- "grey80"
V(network_all)[V(network_all)$type == "Not yet classified"]$color <- "grey80"
#TEST V(network_all)$color


#Calculate the degree for each node
deg <- degree(network_all, mode="all")

#Fix plot parameters
V(network_all)$size <- (deg-min(deg))/(max(deg)-min(deg)) * (25-5)+5
V(network_all)$label.cex  <- (deg-min(deg))/(max(deg)-min(deg)) * (0.75-0.25)+0.25
#V(network_all)$color <- V(network_all)$color,
V(network_all)$frame.color <- rgb(255,255,255, maxColorValue = 255)
V(network_all)$shape <- "circle"
V(network_all)$label <- V(network_all)$acronym
V(network_all)$label.family = "Arial"
V(network_all)$label.font = 1
V(network_all)$cex <-V(network_all)$label.cex
E(network_all)$color <- "grey80"
E(network_all)$width <- 0.5
E(network_all)$margin <- 1

#OUTPUTTING THE CHARTS
#Initialising GhostScript... you will need to download this
#and djust the path to match your installation of Ghostscript
Sys.setenv(R_GSCMD = "C:/Program Files/gs/gs9.20/bin/gswin64c.exe")
png(file="../not_shared/output/regional_partners_hires.png",width = 10, height = 8, units = 'in', res = 300,bg = "transparent")
par(mar=c(5,3,2,2)+0.1)
plot(network_all, layout = layout1)
legend(x="bottom", 
       title.adj = 0,
       c("Country","UN Agency", "Government", "INGO", "Local", "Other","Not yet classified"), 
       pt.bg=c("#009460","#0072B6","#FF5300","#FF8C00","#FFCC00","grey80", "grey80"),
       pch=21,
       col=NA, 
       pt.cex=1.5, 
       cex=0.75, 
       bty="n", 
       horiz=TRUE)
dev.off()


pdf(file="../not_shared/output/regional_partners_hires.pdf",width = 10, height = 8)
par(mar=c(5,3,2,2)+0.1)
plot(network_all, layout = layout1)
legend(x="bottom", 
       title.adj = 0,
       c("Country","UN Agency", "Government", "INGO", "Local", "Other","Not yet classified"), 
       pt.bg=c("#009460","#0072B6","#FF5300","#FF8C00","#FFCC00","grey80", "grey80"),
       pch=21,
       col=NA, 
       pt.cex=1.5, 
       cex=0.75, 
       bty="n", 
       horiz=TRUE)
embed_fonts("../not_shared/output/regional_partners.pdf", outfile="../not_shared/output/regional_partners.pdf")
dev.off()

tiff(file="../not_shared/output/regional_partners_hires.tiff",width = 10, height = 8, pointsize = 1/300, units = 'in', res = 300)
par(mar=c(5,3,2,2)+0.1)
plot(network_all, layout = layout1)
legend(x="bottom", 
       title.adj = 0,
       c("Country","UN Agency", "Government", "INGO", "Local", "Other","Not yet classified"), 
       pt.bg=c("#009460","#0072B6","#FF5300","#FF8C00","#FFCC00","grey80", "grey80"),
       pch=21,
       col=NA, 
       pt.cex=1.5, 
       cex=0.75, 
       bty="n", 
       horiz=TRUE)
dev.off()



tiff("Plot4b.tiff", width = 4, height = 4, pointsize = 1/300, units = 'in', res = 300)
plot(x, y) # Make plot
dev.off()
