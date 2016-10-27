# Remove all the objects created so far
rm(list = ls()) # Remove all the objects we created so far.

# ESTABLISH COLOUR SCHEME
# install.packages("RColorBrewer")
library('RColorBrewer')
node_colours <- c(brewer.pal(5, "Dark2")) # Example contrasting dark colours
# UNHCR colour scheme for three blues
# MAIN  HEX = #005EB8 = rgb(  0, 94,184) = WEB SAFE = 0066cc
# LIGHT HEX = #1188FB = rgb( 17,136,251) = WEB SAFE = 0099ff
# DARK  HEX = #00498F = rgb(  0, 73,143) = WEB SAFE = 003399
# Additional colour alternatives (http://www.colorhexa.com/) or (http://colorbrewer2.org/)

### INSTALL FONTS
#install.packages('extrafont')
#library(extrafont)
#font_import()
#fonts() # Vector of font family names
#fonttable() # Show entire table

## Only necessary in session where you ran font_import()
## For PostScript output, use loadfonts(device="postscript")
## Suppress output with loadfonts(quiet=TRUE)
#loadfonts()

# Needed only on Windows - run once per R session
# Adjust the path to match your installation of Ghostscript
Sys.setenv(R_GSCMD = "C:/Program Files/gs/gs9.20/bin/gswin64c.exe")

# LOAD IN THE DATA
graph_edges <- data.frame(read.csv("../not_shared/data/regional_partners_edges.csv", header = TRUE, stringsAsFactors = FALSE))
graph_nodes <- data.frame(read.csv("../not_shared/data/regional_partners_nodes.csv", header = TRUE, stringsAsFactors = FALSE))

# CREATE AN IGRAPH OBJECT
# Install.packages("igraph")
library("igraph")
# Load the graph object
network_all <- graph_from_data_frame(d=graph_edges, vertices = graph_nodes, directed = F)

# SET VISUALISATION PARAMETERS

# 01. Calculate the degree for each node
deg <- degree(network_all, mode="all")

# 02. Create a node and label size based on degree within a fixed range
V(network_all)$size <- (deg-min(deg))/(max(deg)-min(deg)) * (25-5)+5
V(network_all)$label.cex  <- (deg-min(deg))/(max(deg)-min(deg)) * (1-0.5)+0.5
#V(network_all)$color <- node_colours[V(network_all)$type]
V(network_all)$color <- rgb(17,136,251, maxColorValue = 255)
V(network_all)$frame.color <- rgb(255,255,255, maxColorValue = 255)
V(network_all)$shape <- "circle"
V(network_all)$label <- V(network_all)$acronym
V(network_all)$label.family = "Arial"
V(network_all)$label.font = 1
V(network_all)$cex <-V(network_all)$label.cex
E(network_all)$color <- "grey80"
E(network_all)$width <- 1
E(network_all)$margin <- 1

# 04. Set chart type parameters
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

graph_layout_02 <- layout_with_kk(network_all,
                               coords = NULL, 
                               dim = 2, 
                               maxiter = 50 * vcount(network_all), 
                               epsilon = 0, 
                               kkconst = vcount(network_all), 
                               weights = NULL)



# SAVE VISUALISATIONS
pdf("../not_shared/output/regional_partners.pdf", family="Arial")
plot(network_all,
     layout = graph_layout_01,
     vertex.color = rgb(17,136,251, maxColorValue = 255),
     vertex.frame.color = rgb(255,255,255, maxColorValue = 255),
     vertex.shape = "circle",
     vertex.size = network_all$size,
     vertex.label = V(network_all)$acronym, # Uses the label field
     vertex.label.family = "Arial",
     vertex.label.font = 1, # 1 plain, 2 bold, 3, italic, 4 bold italic, 5 symbol
     vertex.label.cex = V(network_all)$label.cex, #0.5, # Font size in proportion to node
     edge.color="grey80",
     edge.width=1,
     margin = 0, #Empty space margins around the plot, vector with length 4
     frame = FALSE # Option to add a frame around the chart
)
dev.off()
embed_fonts("../not_shared/output/regional_partners.pdf", outfile="../not_shared/output/regional_partners.pdf")

legend(x=-1.5, y=-1.1, 
       c("Country","UN Agency", "Government", "INGO", "Not yet classified"), 
       pch=21,
       col="#777777", 
       pt.bg=node_colours, 
       pt.cex=2, 
       cex=.8, 
       bty="n", 
       ncol=1)

plot(network_all,
     layout = graph_layout_02,
     vertex.color = rgb(17,136,251, maxColorValue = 255),
     vertex.frame.color = rgb(255,255,255, maxColorValue = 255),
     vertex.shape = "circle",
     vertex.size = network_all$size,
     vertex.label = V(network_all)$acronym, # Uses the label field
     vertex.label.family = "Arial",
     vertex.label.font = 1, # 1 plain, 2 bold, 3, italic, 4 bold italic, 5 symbol
     vertex.label.cex = V(network_all)$label.cex, #0.5, # Font size in proportion to node
     edge.color="grey80",
     edge.width=1,
     margin = 0, #Empty space margins around the plot, vector with length 4
     frame = FALSE # Option to add a frame around the chart
)
dev.off()
embed_fonts("../not_shared/output/regional_partners.pdf", outfile="../not_shared/output/regional_partners.pdf")


