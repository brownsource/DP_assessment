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
library(plyr)

### LOAD THE DATA
graph_edges <- data.frame(read.csv("../not_shared/data/regional/regional_partners_edges.csv", header = TRUE, stringsAsFactors = FALSE))
graph_nodes <- data.frame(read.csv("../not_shared/data/regional/regional_partners_nodes.csv", header = TRUE, stringsAsFactors = FALSE))

## CREATE THE IGRAPH OBJECTS
#Full regional network
regional_partners <- graph_from_data_frame(d=graph_edges, vertices = graph_nodes, directed = F)
regional_partners <- simplify(regional_partners, remove.multiple = F, remove.loops = T)

#Country level networks
jordan_partners <- induced.subgraph(regional_partners, which(V(regional_partners)$jordan==1))
jordan_dsa_partners <- induced.subgraph(jordan_partners, which(V(jordan_partners)$DSA_jordan==1))
lebanon_partners <- induced.subgraph(regional_partners, which(V(regional_partners)$lebanon==1))
lebanon_dsa_partners <- induced.subgraph(lebanon_partners, which(V(lebanon_partners)$DSA_lebanon==1))

#Vector of graph names
graph_objects <- c("regional_partners","jordan_partners","jordan_dsa_partners","lebanon_partners","lebanon_dsa_partners")
graph_layout_names <- c("regional_partners_layout","jordan_partners_layout","jordan_dsa_partners_layout","lebanon_partners_layout","lebanon_dsa_partners_layout")

##FIX THE FORCED LAYOUTS FOR THE CHARTS
##I REALLY WANT TO PUT THIS IN A FOR LOOP!
regional_partners_layout <- layout.forceatlas2(regional_partners, 
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
jordan_partners_layout <- layout.forceatlas2(jordan_partners, 
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
jordan_dsa_partners_layout <- layout.forceatlas2(jordan_dsa_partners, 
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
lebanon_partners_layout <- layout.forceatlas2(lebanon_partners, 
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
lebanon_dsa_partners_layout <- layout.forceatlas2(lebanon_dsa_partners, 
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

#Vector of layouts
graph_layouts <- c(regional_partners_layout,jordan_partners_layout,jordan_dsa_partners_layout,lebanon_partners_layout,lebanon_dsa_partners_layout)



## SET THE CUSTOM PLOT PARAMETERS
## I HAD WANTED TO HAVE THESE BEFORE THE FORCE ATLAS SO THAT THEY COULD BE INHERITED BY THE SUBGRAPHS
## THIS CAUSES AN ERROR SO I HAVE TO APPLY AFTER FORCE ATLAS
## AGAIN, I WOULD LIKE TO PUT THIS IN A LOOP

###############################################################################################
############################################ REGION ###########################################
###############################################################################################

#### ALL
#Set colours
V(regional_partners)[V(regional_partners)$type == "Country"]$color <- "#009460"
V(regional_partners)[V(regional_partners)$type == "UN Agency"]$color <- "#0072B6"
V(regional_partners)[V(regional_partners)$type == "Government"]$color <- "#FF5300"
V(regional_partners)[V(regional_partners)$type == "INGO"]$color <- "#FF8C00"
V(regional_partners)[V(regional_partners)$type == "Local"]$color <- "#FFCC00"
V(regional_partners)[V(regional_partners)$type == "Other"]$color <- "grey80"
V(regional_partners)[V(regional_partners)$type == "Not yet classified"]$color <- "grey80"
#Other attributes
V(regional_partners)$frame.color <- rgb(255,255,255, maxColorValue = 255)
V(regional_partners)$shape <- "circle"
V(regional_partners)$label <- V(regional_partners)$acronym
V(regional_partners)$label.family = "Arial"
V(regional_partners)$label.font = 1
E(regional_partners)$color <- "grey80"
E(regional_partners)$width <- 0.5
E(regional_partners)$margin <- 1
#E(regional_partners)$from <- graph_edges$from
#E(regional_partners)$to <- graph_edges$to
#Calculate the degree for each node
regional_partners_degree <- degree(regional_partners, mode="all")
#Fix dynamic size-based parameters
V(regional_partners)$size <- (regional_partners_degree - min(regional_partners_degree)) / (max(regional_partners_degree) - min(regional_partners_degree)) * (25-5)+5
V(regional_partners)$label.cex  <- (regional_partners_degree - min(regional_partners_degree)) / (max(regional_partners_degree) - min(regional_partners_degree)) * (0.75-0.25)+0.25
V(regional_partners)$cex <- V(regional_partners)$label.cex

#SAVE: ../not_shared/output/regional/regional_partners.png

Sys.setenv(R_GSCMD = "C:/Program Files/gs/gs9.20/bin/gswin64c.exe")
png(file="../not_shared/output/regional/regional_partners.png",width = 10, height = 8, units = 'in', res = 300,bg = "transparent")
par(mar=c(5,3,2,2)+0.1)
plot(regional_partners, layout = regional_partners_layout)
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

#SAVE: ../not_shared/output/regional/regional_partners_anon.png

#remove labels
V(regional_partners)$label[V(regional_partners)$type!="Country"] <- NA
Sys.setenv(R_GSCMD = "C:/Program Files/gs/gs9.20/bin/gswin64c.exe")
png(file="../not_shared/output/regional/regional_partners_anon.png",width = 10, height = 8, units = 'in', res = 300,bg = "transparent")
par(mar=c(5,3,2,2)+0.1)
plot(regional_partners, layout = regional_partners_layout)
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

#SAVE: ../not_shared/output/regional/regional_partners_lebanon_dsa.png

#highlight partners with DSA with label
V(regional_partners)$label <- V(regional_partners)$acronym
V(regional_partners)$frame.color[V(regional_partners)$DSA_lebanon==1] <- rgb(0,0,0, maxColorValue = 255)
V(regional_partners)$color[V(regional_partners)$DSA_lebanon!=1] <- "grey80"

Sys.setenv(R_GSCMD = "C:/Program Files/gs/gs9.20/bin/gswin64c.exe")
png(file="../not_shared/output/regional/regional_partners_lebanon_dsa.png",width = 10, height = 8, units = 'in', res = 300,bg = "transparent")
par(mar=c(5,3,2,2)+0.1)
plot(regional_partners, layout = regional_partners_layout)
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

#SAVE: ../not_shared/output/regional/regional_partners_lebanon_dsa_anon.png

#remove labels
V(regional_partners)$label[V(regional_partners)$type!="Country"] <- NA

Sys.setenv(R_GSCMD = "C:/Program Files/gs/gs9.20/bin/gswin64c.exe")
png(file="../not_shared/output/regional/regional_partners_lebanon_dsa_anon.png",width = 10, height = 8, units = 'in', res = 300,bg = "transparent")
par(mar=c(5,3,2,2)+0.1)
plot(regional_partners, layout = regional_partners_layout)
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

#SAVE: ../not_shared/output/regional/regional_partners_jordan_dsa.png

V(regional_partners)[V(regional_partners)$type == "Country"]$color <- "#009460"
V(regional_partners)[V(regional_partners)$type == "UN Agency"]$color <- "#0072B6"
V(regional_partners)[V(regional_partners)$type == "Government"]$color <- "#FF5300"
V(regional_partners)[V(regional_partners)$type == "INGO"]$color <- "#FF8C00"
V(regional_partners)[V(regional_partners)$type == "Local"]$color <- "#FFCC00"
V(regional_partners)[V(regional_partners)$type == "Other"]$color <- "grey80"
V(regional_partners)[V(regional_partners)$type == "Not yet classified"]$color <- "grey80"
V(regional_partners)$frame.color <- rgb(255,255,255, maxColorValue = 255)
V(regional_partners)$label <- V(regional_partners)$acronym
V(regional_partners)$frame.color[V(regional_partners)$DSA_jordan==1] <- rgb(0,0,0, maxColorValue = 255)
V(regional_partners)$color[V(regional_partners)$DSA_jordan!=1] <- "grey80"

Sys.setenv(R_GSCMD = "C:/Program Files/gs/gs9.20/bin/gswin64c.exe")
png(file="../not_shared/output/regional/regional_partners_jordan_dsa.png",width = 10, height = 8, units = 'in', res = 300,bg = "transparent")
par(mar=c(5,3,2,2)+0.1)
plot(regional_partners, layout = regional_partners_layout)
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

#SAVE: ../not_shared/output/regional/regional_partners_jordan_dsa_anon.png

#remove labels
V(regional_partners)$label[V(regional_partners)$type!="Country"] <- NA

Sys.setenv(R_GSCMD = "C:/Program Files/gs/gs9.20/bin/gswin64c.exe")
png(file="../not_shared/output/regional/regional_partners_jordan_dsa_anon.png",width = 10, height = 8, units = 'in', res = 300,bg = "transparent")
par(mar=c(5,3,2,2)+0.1)
plot(regional_partners, layout = regional_partners_layout)
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

#SAVE: ../not_shared/output/regional/regional_partners_dsa.png

V(regional_partners)[V(regional_partners)$type == "Country"]$color <- "#009460"
V(regional_partners)[V(regional_partners)$type == "UN Agency"]$color <- "#0072B6"
V(regional_partners)[V(regional_partners)$type == "Government"]$color <- "#FF5300"
V(regional_partners)[V(regional_partners)$type == "INGO"]$color <- "#FF8C00"
V(regional_partners)[V(regional_partners)$type == "Local"]$color <- "#FFCC00"
V(regional_partners)[V(regional_partners)$type == "Other"]$color <- "grey80"
V(regional_partners)[V(regional_partners)$type == "Not yet classified"]$color <- "grey80"
V(regional_partners)$frame.color <- rgb(255,255,255, maxColorValue = 255)
V(regional_partners)$label <- V(regional_partners)$acronym
V(regional_partners)$frame.color[which(V(regional_partners)$DSA_jordan==1 | V(regional_partners)$DSA_lebanon==1)] <- rgb(0,0,0, maxColorValue = 255)
V(regional_partners)$color[which(V(regional_partners)$DSA_jordan!=1 & V(regional_partners)$DSA_lebanon!=1)] <- "grey80"

Sys.setenv(R_GSCMD = "C:/Program Files/gs/gs9.20/bin/gswin64c.exe")
png(file="../not_shared/output/regional/regional_partners_dsa.png",width = 10, height = 8, units = 'in', res = 300,bg = "transparent")
par(mar=c(5,3,2,2)+0.1)
plot(regional_partners, layout = regional_partners_layout)
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

#SAVE: ../not_shared/output/regional/regional_partners_dsa_anon.png

#remove labels
V(regional_partners)$label[V(regional_partners)$type!="Country"] <- NA

Sys.setenv(R_GSCMD = "C:/Program Files/gs/gs9.20/bin/gswin64c.exe")
png(file="../not_shared/output/regional/regional_partners_dsa_anon.png",width = 10, height = 8, units = 'in', res = 300,bg = "transparent")
par(mar=c(5,3,2,2)+0.1)
plot(regional_partners, layout = regional_partners_layout)
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


###############################################################################################
########################################### Lebanon ###########################################
###############################################################################################


#colours
V(lebanon_partners)[V(lebanon_partners)$type == "Country"]$color <- "#009460"
V(lebanon_partners)[V(lebanon_partners)$type == "UN Agency"]$color <- "#0072B6"
V(lebanon_partners)[V(lebanon_partners)$type == "Government"]$color <- "#FF5300"
V(lebanon_partners)[V(lebanon_partners)$type == "INGO"]$color <- "#FF8C00"
V(lebanon_partners)[V(lebanon_partners)$type == "Local"]$color <- "#FFCC00"
V(lebanon_partners)[V(lebanon_partners)$type == "Other"]$color <- "grey80"
V(lebanon_partners)[V(lebanon_partners)$type == "Not yet classified"]$color <- "grey80"
#Other attributes
V(lebanon_partners)$frame.color <- rgb(255,255,255, maxColorValue = 255)
V(lebanon_partners)$shape <- "circle"
V(lebanon_partners)$label <- V(lebanon_partners)$acronym
V(lebanon_partners)$label.family = "Arial"
V(lebanon_partners)$label.font = 1
E(lebanon_partners)$color <- "grey80"
E(lebanon_partners)$width <- 0.5
E(lebanon_partners)$margin <- 1
#Calculate the degree for each node
lebanon_partners_degree <- degree(lebanon_partners, mode="all")
#Fix dynamic size-based parameters
V(lebanon_partners)$size <- (lebanon_partners_degree - min(lebanon_partners_degree)) / (max(lebanon_partners_degree) - min(lebanon_partners_degree)) * (20-10)+10
V(lebanon_partners)$label.cex  <- (lebanon_partners_degree - min(lebanon_partners_degree)) / (max(lebanon_partners_degree) - min(lebanon_partners_degree)) * (0.75-0.5)+0.5
V(lebanon_partners)$cex <- V(lebanon_partners)$label.cex

#SAVE: ../not_shared/output/lebanon/lebanon_all_partners.png

Sys.setenv(R_GSCMD = "C:/Program Files/gs/gs9.20/bin/gswin64c.exe")
png(file="../not_shared/output/lebanon/lebanon_all_partners.png",width = 10, height = 8, units = 'in', res = 300,bg = "transparent")
par(mar=c(5,3,2,2)+0.1)
plot(lebanon_partners, layout = lebanon_partners_layout)
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

#SAVE: ../not_shared/output/lebanon/lebanon_all_partners_anon.png

V(lebanon_partners)$label[V(lebanon_partners)$type!="Country"] <- NA

Sys.setenv(R_GSCMD = "C:/Program Files/gs/gs9.20/bin/gswin64c.exe")
png(file="../not_shared/output/lebanon/lebanon_all_partners_anon.png",width = 10, height = 8, units = 'in', res = 300,bg = "transparent")
par(mar=c(5,3,2,2)+0.1)
plot(lebanon_partners, layout = lebanon_partners_layout)
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

#SAVE: ../not_shared/output/lebanon/lebanon_all_partners_dsa.png

#highlight partners with DSA with label
V(lebanon_partners)$label <- V(lebanon_partners)$acronym
V(lebanon_partners)$frame.color[V(lebanon_partners)$DSA_lebanon==1] <- rgb(0,0,0, maxColorValue = 255)
V(lebanon_partners)$color[V(lebanon_partners)$DSA_lebanon!=1] <- "grey80"

Sys.setenv(R_GSCMD = "C:/Program Files/gs/gs9.20/bin/gswin64c.exe")
png(file="../not_shared/output/lebanon/lebanon_all_partners_dsa.png",width = 10, height = 8, units = 'in', res = 300,bg = "transparent")
par(mar=c(5,3,2,2)+0.1)
plot(lebanon_partners, layout = lebanon_partners_layout)
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

#SAVE: ../not_shared/output/lebanon/lebanon_all_partners_dsa_anon.png

V(lebanon_partners)$label[V(lebanon_partners)$type!="Country"] <- NA

Sys.setenv(R_GSCMD = "C:/Program Files/gs/gs9.20/bin/gswin64c.exe")
png(file="../not_shared/output/lebanon/lebanon_all_partners_dsa_anon.png",width = 10, height = 8, units = 'in', res = 300,bg = "transparent")
par(mar=c(5,3,2,2)+0.1)
plot(lebanon_partners, layout = lebanon_partners_layout)
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

#colours
V(lebanon_dsa_partners)[V(lebanon_dsa_partners)$type == "Country"]$color <- "#009460"
V(lebanon_dsa_partners)[V(lebanon_dsa_partners)$type == "UN Agency"]$color <- "#0072B6"
V(lebanon_dsa_partners)[V(lebanon_dsa_partners)$type == "Government"]$color <- "#FF5300"
V(lebanon_dsa_partners)[V(lebanon_dsa_partners)$type == "INGO"]$color <- "#FF8C00"
V(lebanon_dsa_partners)[V(lebanon_dsa_partners)$type == "Local"]$color <- "#FFCC00"
V(lebanon_dsa_partners)[V(lebanon_dsa_partners)$type == "Other"]$color <- "grey80"
V(lebanon_dsa_partners)[V(lebanon_dsa_partners)$type == "Not yet classified"]$color <- "grey80"
#Other attributes
V(lebanon_dsa_partners)$frame.color <- rgb(255,255,255, maxColorValue = 255)
V(lebanon_dsa_partners)$shape <- "circle"
V(lebanon_dsa_partners)$label <- V(lebanon_dsa_partners)$acronym
V(lebanon_dsa_partners)$label.family = "Arial"
V(lebanon_dsa_partners)$label.font = 1
E(lebanon_dsa_partners)$color <- "grey80"
E(lebanon_dsa_partners)$width <- 0.5
E(lebanon_dsa_partners)$margin <- 1
#Calculate the degree for each node
lebanon_dsa_partners_degree <- degree(lebanon_dsa_partners, mode="all")
#Fix dynamic size-based parameters
V(lebanon_dsa_partners)$size <- (lebanon_dsa_partners_degree - min(lebanon_dsa_partners_degree)) / (max(lebanon_dsa_partners_degree) - min(lebanon_dsa_partners_degree)) * (20-15)+15
V(lebanon_dsa_partners)$label.cex  <- (lebanon_dsa_partners_degree - min(lebanon_dsa_partners_degree)) / (max(lebanon_dsa_partners_degree) - min(lebanon_dsa_partners_degree)) * (0.75-0.5)+0.5
V(lebanon_dsa_partners)$cex <- V(lebanon_dsa_partners)$label.cex

#SAVE: ../not_shared/output/lebanon/lebanon_dsa_partners.png

Sys.setenv(R_GSCMD = "C:/Program Files/gs/gs9.20/bin/gswin64c.exe")
png(file="../not_shared/output/lebanon/lebanon_dsa_partners.png",width = 10, height = 8, units = 'in', res = 300,bg = "transparent")
par(mar=c(5,3,2,2)+0.1)
plot(lebanon_dsa_partners, layout = lebanon_dsa_partners_layout)
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

#SAVE: ../not_shared/output/lebanon/lebanon_dsa_partners_anon.png

V(lebanon_dsa_partners)$label[V(lebanon_dsa_partners)$type!="Country"] <- NA

Sys.setenv(R_GSCMD = "C:/Program Files/gs/gs9.20/bin/gswin64c.exe")
png(file="../not_shared/output/lebanon/lebanon_dsa_partners_anon.png",width = 10, height = 8, units = 'in', res = 300,bg = "transparent")
par(mar=c(5,3,2,2)+0.1)
plot(lebanon_dsa_partners, layout = lebanon_dsa_partners_layout)
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

##SAVE TO CSV
lebanon_partner_export <- data.frame(
  name=V(lebanon_partners)$name,
  acronym=V(lebanon_partners)$acronym,
  type=V(lebanon_partners)$type,
  egypt=V(lebanon_partners)$egypt,
  iraq=V(lebanon_partners)$iraq,
  jordan=V(lebanon_partners)$jordan,
  lebanon=V(lebanon_partners)$lebanon,
  turkey=V(lebanon_partners)$turkey,
  DSA_jordan=V(lebanon_partners)$DSA_jordan,
  DSA_lebanon=V(lebanon_partners)$DSA_lebanon
)
write.csv(lebanon_partner_export, file = "../not_shared/output/lebanon/lebanon_partners.csv")

###############################################################################################
############################################ Jordan ###########################################
###############################################################################################

#colours
V(jordan_partners)[V(jordan_partners)$type == "Country"]$color <- "#009460"
V(jordan_partners)[V(jordan_partners)$type == "UN Agency"]$color <- "#0072B6"
V(jordan_partners)[V(jordan_partners)$type == "Government"]$color <- "#FF5300"
V(jordan_partners)[V(jordan_partners)$type == "INGO"]$color <- "#FF8C00"
V(jordan_partners)[V(jordan_partners)$type == "Local"]$color <- "#FFCC00"
V(jordan_partners)[V(jordan_partners)$type == "Other"]$color <- "grey80"
V(jordan_partners)[V(jordan_partners)$type == "Not yet classified"]$color <- "grey80"
#Other attributes
V(jordan_partners)$frame.color <- rgb(255,255,255, maxColorValue = 255)
V(jordan_partners)$shape <- "circle"
V(jordan_partners)$label <- V(jordan_partners)$acronym
V(jordan_partners)$label.family = "Arial"
V(jordan_partners)$label.font = 1
E(jordan_partners)$color <- "grey80"
E(jordan_partners)$width <- 0.5
E(jordan_partners)$margin <- 1
#Calculate the degree for each node
jordan_partners_degree <- degree(jordan_partners, mode="all")
#Fix dynamic size-based parameters
V(jordan_partners)$size <- (jordan_partners_degree - min(jordan_partners_degree)) / (max(jordan_partners_degree) - min(jordan_partners_degree)) * (20-10)+10
V(jordan_partners)$label.cex  <- (jordan_partners_degree - min(jordan_partners_degree)) / (max(jordan_partners_degree) - min(jordan_partners_degree)) * (0.75-0.5)+0.5
V(jordan_partners)$cex <- V(jordan_partners)$label.cex

#SAVE: ../not_shared/output/jordan/jordan_all_partners.png

Sys.setenv(R_GSCMD = "C:/Program Files/gs/gs9.20/bin/gswin64c.exe")
png(file="../not_shared/output/jordan/jordan_all_partners.png",width = 10, height = 8, units = 'in', res = 300,bg = "transparent")
par(mar=c(5,3,2,2)+0.1)
plot(jordan_partners, layout = jordan_partners_layout)
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

#SAVE: ../not_shared/output/jordan/jordan_all_partners_anon.png

V(jordan_partners)$label[V(jordan_partners)$type!="Country"] <- NA

Sys.setenv(R_GSCMD = "C:/Program Files/gs/gs9.20/bin/gswin64c.exe")
png(file="../not_shared/output/jordan/jordan_all_partners_anon.png",width = 10, height = 8, units = 'in', res = 300,bg = "transparent")
par(mar=c(5,3,2,2)+0.1)
plot(jordan_partners, layout = jordan_partners_layout)
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

#SAVE: ../not_shared/output/jordan/jordan_all_partners_dsa.png

#highlight partners with DSA with label
V(jordan_partners)$label <- V(jordan_partners)$acronym
V(jordan_partners)$frame.color[V(jordan_partners)$DSA_jordan==1] <- rgb(0,0,0, maxColorValue = 255)
V(jordan_partners)$color[V(jordan_partners)$DSA_jordan!=1] <- "grey80"

Sys.setenv(R_GSCMD = "C:/Program Files/gs/gs9.20/bin/gswin64c.exe")
png(file="../not_shared/output/jordan/jordan_all_partners_dsa.png",width = 10, height = 8, units = 'in', res = 300,bg = "transparent")
par(mar=c(5,3,2,2)+0.1)
plot(jordan_partners, layout = jordan_partners_layout)
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

#SAVE: ../not_shared/output/jordan/jordan_all_partners_dsa_anon.png

V(jordan_partners)$label[V(jordan_partners)$type!="Country"] <- NA

Sys.setenv(R_GSCMD = "C:/Program Files/gs/gs9.20/bin/gswin64c.exe")
png(file="../not_shared/output/jordan/jordan_all_partners_dsa_anon.png",width = 10, height = 8, units = 'in', res = 300,bg = "transparent")
par(mar=c(5,3,2,2)+0.1)
plot(jordan_partners, layout = jordan_partners_layout)
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

#colours
V(jordan_dsa_partners)[V(jordan_dsa_partners)$type == "Country"]$color <- "#009460"
V(jordan_dsa_partners)[V(jordan_dsa_partners)$type == "UN Agency"]$color <- "#0072B6"
V(jordan_dsa_partners)[V(jordan_dsa_partners)$type == "Government"]$color <- "#FF5300"
V(jordan_dsa_partners)[V(jordan_dsa_partners)$type == "INGO"]$color <- "#FF8C00"
V(jordan_dsa_partners)[V(jordan_dsa_partners)$type == "Local"]$color <- "#FFCC00"
V(jordan_dsa_partners)[V(jordan_dsa_partners)$type == "Other"]$color <- "grey80"
V(jordan_dsa_partners)[V(jordan_dsa_partners)$type == "Not yet classified"]$color <- "grey80"
#Other attributes
V(jordan_dsa_partners)$frame.color <- rgb(255,255,255, maxColorValue = 255)
V(jordan_dsa_partners)$shape <- "circle"
V(jordan_dsa_partners)$label <- V(jordan_dsa_partners)$acronym
V(jordan_dsa_partners)$label.family = "Arial"
V(jordan_dsa_partners)$label.font = 1
E(jordan_dsa_partners)$color <- "grey80"
E(jordan_dsa_partners)$width <- 0.5
E(jordan_dsa_partners)$margin <- 1
#Calculate the degree for each node
jordan_dsa_partners_degree <- degree(jordan_dsa_partners, mode="all")
#Fix dynamic size-based parameters
V(jordan_dsa_partners)$size <- (jordan_dsa_partners_degree - min(jordan_dsa_partners_degree)) / (max(jordan_dsa_partners_degree) - min(jordan_dsa_partners_degree)) * (20-15)+15
V(jordan_dsa_partners)$label.cex  <- (jordan_dsa_partners_degree - min(jordan_dsa_partners_degree)) / (max(jordan_dsa_partners_degree) - min(jordan_dsa_partners_degree)) * (0.75-0.5)+0.5
V(jordan_dsa_partners)$cex <- V(jordan_dsa_partners)$label.cex

#SAVE: ../not_shared/output/jordan/jordan_dsa_partners.png

Sys.setenv(R_GSCMD = "C:/Program Files/gs/gs9.20/bin/gswin64c.exe")
png(file="../not_shared/output/jordan/jordan_dsa_partners.png",width = 10, height = 8, units = 'in', res = 300,bg = "transparent")
par(mar=c(5,3,2,2)+0.1)
plot(jordan_dsa_partners, layout = jordan_dsa_partners_layout)
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

#SAVE: ../not_shared/output/jordan/jordan_dsa_partners_anon.png

V(jordan_dsa_partners)$label[V(jordan_dsa_partners)$type!="Country"] <- NA

Sys.setenv(R_GSCMD = "C:/Program Files/gs/gs9.20/bin/gswin64c.exe")
png(file="../not_shared/output/jordan/jordan_dsa_partners_anon.png",width = 10, height = 8, units = 'in', res = 300,bg = "transparent")
par(mar=c(5,3,2,2)+0.1)
plot(jordan_dsa_partners, layout = jordan_dsa_partners_layout)
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

##SAVE TO CSV
jordan_partner_export <- data.frame(
  name=V(jordan_partners)$name,
  acronym=V(jordan_partners)$acronym,
  type=V(jordan_partners)$type,
  egypt=V(jordan_partners)$egypt,
  iraq=V(jordan_partners)$iraq,
  jordan=V(jordan_partners)$jordan,
  lebanon=V(jordan_partners)$lebanon,
  turkey=V(jordan_partners)$turkey,
  DSA_jordan=V(jordan_partners)$DSA_jordan,
  DSA_lebanon=V(jordan_partners)$DSA_lebanon
)
write.csv(jordan_partner_export, file = "../not_shared/output/jordan/jordan_partners.csv")



## CODE THAT CAN BE MODIFIED FOR PDF AND TIFF OUTPUTS

# pdf(file="../not_shared/output/regional_partners_hires.pdf",width = 10, height = 8)
# par(mar=c(5,3,2,2)+0.1)
# plot(network_all, layout = layout1)
# legend(x="bottom", 
#        title.adj = 0,
#        c("Country","UN Agency", "Government", "INGO", "Local", "Other","Not yet classified"), 
#        pt.bg=c("#009460","#0072B6","#FF5300","#FF8C00","#FFCC00","grey80", "grey80"),
#        pch=21,
#        col=NA, 
#        pt.cex=1.5, 
#        cex=0.75, 
#        bty="n", 
#        horiz=TRUE)
# embed_fonts("../not_shared/output/regional_partners.pdf", outfile="../not_shared/output/regional_partners.pdf")
# dev.off()

# tiff(file="../not_shared/output/regional_partners_hires.tiff",width = 10, height = 8, pointsize = 1/300, units = 'in', res = 300)
# par(mar=c(5,3,2,2)+0.1)
# plot(network_all, layout = layout1)
# legend(x="bottom", 
#        title.adj = 0,
#        c("Country","UN Agency", "Government", "INGO", "Local", "Other","Not yet classified"), 
#        pt.bg=c("#009460","#0072B6","#FF5300","#FF8C00","#FFCC00","grey80", "grey80"),
#        pch=21,
#        col=NA, 
#        pt.cex=1.5, 
#        cex=0.75, 
#        bty="n", 
#        horiz=TRUE)
# dev.off()