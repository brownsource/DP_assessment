#### CREATES THE REQUIRED NODE AND EDGE TABLES FOR NETWORK ANALYSIS
#### YOU NEED TO SPECIFY THE COUNTRY AT THE START OF THE SCRIPT

### INPUT: 
## not_shared/output/[COUNTRY]/[COUNTRY]_nodes.csv
## not_shared/output/[COUNTRY]/[COUNTRY]_edges.csv

### OUTPUT:
## not_shared/output/[COUNTRY]/[COUNTRY]_nodes.csv
## not_shared/output/[COUNTRY]/[COUNTRY]_edges.csv
## not_shared/output/[COUNTRY]/[COUNTRY]_comments.csv

####################################################################################################################
### INITIALISE THE WORKSPACE #######################################################################################
####################################################################################################################

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

## INSTALL LIBRARIES IF REQUIRED // NEED TO CHANGE THIS BLOCK OF CODE
library(extrafont)
font_import()
loadfonts()
library(igraph)
if (!require("ForceAtlas2")) devtools::install_github("analyxcompany/ForceAtlas2")
library("ForceAtlas2")
library(plyr)
library(ggplot2)
library(png)

###Load external functions
source("code/function_multiplot.R")
source("code/function_ggplot_bar_chart.R")

####################################################################################################################
### LOAD THE EXTERNAL DATA #########################################################################################
####################################################################################################################

## Select which country // **I SHOULD TURN THIS INTO A FUNCTION**
country <- "jordan"
# country <- "lebanon"

## Load the data
survey_edges <- data.frame(read.csv(paste("../not_shared/output/", country, "/", country, "_edges.csv", sep=""), header = TRUE, stringsAsFactors = FALSE))
survey_nodes <- data.frame(read.csv(paste("../not_shared/output/", country, "/", country, "_nodes.csv", sep=""), header = TRUE, stringsAsFactors = FALSE))

####################################################################################################################
### CREATE GRAPH OBJECTS ###########################################################################################
####################################################################################################################

## Create Full country network
survey_network <- graph_from_data_frame(d=survey_edges, vertices = survey_nodes, directed = F)
survey_network <- simplify(survey_network, remove.multiple = F, remove.loops = T)

# Create sector level networks
survey_network_basic_needs <- induced.subgraph(survey_network, which(V(survey_network)$Filter.Sector.Basic_needs=="True"))
survey_network_education <- induced.subgraph(survey_network, which(V(survey_network)$Filter.Sector.Education=="True"))
survey_network_food_security <- induced.subgraph(survey_network, which(V(survey_network)$Filter.Sector.Food_security=="True"))
survey_network_health <- induced.subgraph(survey_network, which(V(survey_network)$Filter.Sector.Health=="True"))
survey_network_livelihoods <- induced.subgraph(survey_network, which(V(survey_network)$Filter.Sector.Livelihoods=="True"))
survey_network_protection <- induced.subgraph(survey_network, which(V(survey_network)$Filter.Sector.Protection=="True"))
survey_network_shelter <- induced.subgraph(survey_network, which(V(survey_network)$Filter.Sector.Shelter=="True"))
survey_network_wash <- induced.subgraph(survey_network, which(V(survey_network)$Filter.Sector.WASH=="True"))

## Create vector of network names
network_names <- c("survey_network",
                   "survey_network_basic_needs",
                   "survey_network_education",
                   "survey_network_food_security",
                   "survey_network_health",
                   "survey_network_livelihoods",
                   "survey_network_protection",
                   "survey_network_shelter",
                   "survey_network_wash")

####################################################################################################################
### CREATE DATAFRAMES AND SAVE AS CSV TABLES #######################################################################
####################################################################################################################

### create sector level table ######################################################################################

network_statistics_sectors <- data.frame(Graph=NA,
                                         Count_nodes=NA,
                                         Count_edges=NA,
                                         Density=NA,
                                         Average_degree=NA, 
                                         Power_law=NA, 
                                         Avg_Centrality_out=NA, 
                                         Avg_Centrality_in=NA, 
                                         Avg_Centrality_all=NA, 
                                         Shortest_path=NA,
                                         Avg_path_length=NA, 
                                         Diameter=NA,
                                         No_of_Clusters=NA, 
                                         ClusteringCoefficient=NA, 
                                         Edge_connectivity=NA,
                                         Rreciprocity=NA,
                                         Avg_DP_OM=NA, 
                                         Avg_DP_PM=NA, 
                                         Avg_DP_TM=NA, 
                                         Avg_DP_RDS=NA, 
                                         Avg_DP_ODS=NA, 
                                         Avg_DP_Total=NA) 
network_statistics_sectors <- network_statistics_sectors[-c(1), ]

## function to populate table x= graph y=sectorname
populate_sector_table <- function(x,y) {
  table_row <- data.frame(
              Graph = y, #set Graph name
              Count_nodes = vcount(x), #Count graph nodes
              Count_edges = ecount(x), #Count graph edges
              Density = graph.density(x, loops = FALSE), # Density
#              Average_degree=NA, #new
#              Power_law=NA, #new
              Avg_Centrality_out = mean(degree(x, mode="out")), # Average outbound degree centrality
              Avg_Centrality_in = mean(degree(x, mode="in")), # Average inbound degree centrality
              Avg_Centrality_all = mean(degree(x, mode="all")), # Average undirected degree centrality
#              Shortest_path = NA, # Shortest path
#              Avg_path_length = NA, # Average path length
              Diameter = diameter(x, directed=TRUE), # Diameter
              No_of_Clusters = clusters(x)$no,              
              ClusteringCoefficient = transitivity(x, type="global"),
              Edge_connectivity = edge_connectivity(x),
              Rreciprocity = reciprocity(x),
              Avg_DP_OM = mean(V(x)$Format.Data_protection.Organisational_measures, na.rm=TRUE),
              Avg_DP_PM = mean(V(x)$Format.Data_protection.Physical_measures, na.rm=TRUE),
              Avg_DP_TM = mean(V(x)$Format.Data_protection.Technical_measures, na.rm=TRUE),
              Avg_DP_RDS = mean(V(x)$Format.Data_protection.Rights_of_data_subject_measures, na.rm=TRUE),
              Avg_DP_ODS = mean(V(x)$Format.Data_protection.Onward_sharing_measures, na.rm=TRUE),
              Avg_DP_Total = mean(V(x)$Format.Data_protection.Data_protection_rating, na.rm=TRUE)
  )
}

## call the function to populate for all sectors
network_statistics_sectors <- rbind(network_statistics_sectors, populate_sector_table(survey_network,"all"))
network_statistics_sectors <- rbind(network_statistics_sectors, populate_sector_table(survey_network_basic_needs,"basic_needs"))
network_statistics_sectors <- rbind(network_statistics_sectors, populate_sector_table(survey_network_education,"education"))
network_statistics_sectors <- rbind(network_statistics_sectors, populate_sector_table(survey_network_food_security,"food_security"))
network_statistics_sectors <- rbind(network_statistics_sectors, populate_sector_table(survey_network_health,"health"))
network_statistics_sectors <- rbind(network_statistics_sectors, populate_sector_table(survey_network_livelihoods,"livelihoods"))
network_statistics_sectors <- rbind(network_statistics_sectors, populate_sector_table(survey_network_protection,"protection"))
network_statistics_sectors <- rbind(network_statistics_sectors, populate_sector_table(survey_network_shelter,"shelter"))
network_statistics_sectors <- rbind(network_statistics_sectors, populate_sector_table(survey_network_wash,"wash"))

## export the table as csv
folder_name <- paste("../not_shared/output/", country, "/stats/", country, "_network_statistics_sectors.csv", sep="")
dir.create(dirname(folder_name), showWarnings = FALSE)
write.csv(network_statistics_sectors, file = paste("../not_shared/output/", country, "/stats/", country, "_network_statistics_sectors.csv", sep=""), row.names=FALSE)

### create data protection level table #############################################################################

## Transpose the table and keep just values needed
names <- network_statistics_sectors$Graph
network_statistics_measures <- as.data.frame(t(network_statistics_sectors), stringsAsFactors = FALSE)
colnames(network_statistics_measures) <- names
network_statistics_measures <- network_statistics_measures[-1:-12,]

## export the table as csv
folder_name <- paste("../not_shared/output/", country, "/stats/", country, "_network_statistics_dpmeasures.csv", sep="")
dir.create(dirname(folder_name), showWarnings = FALSE)
write.csv(network_statistics_measures, file = paste("../not_shared/output/", country, "/stats/", country, "_network_statistics_dpmeasures.csv", sep=""), row.names=TRUE)

### create organisation level table ################################################################################

## function to populate table x= sector graph y= filename
create_statistics_for_sectors <- function(x, y) {
    temp <- as_data_frame(x, what = "vertices")
    network_statistics_organisations <- temp
    network_statistics_organisations <- cbind(network_statistics_organisations, Degree <- degree(x))
    network_statistics_organisations <- cbind(network_statistics_organisations, Centrality_out = degree(x, mode="out"))
    network_statistics_organisations <- cbind(network_statistics_organisations, Centrality_in = degree(x, mode="in"))
    network_statistics_organisations <- cbind(network_statistics_organisations, Centrality_all = degree(x, mode="all"))
    network_statistics_organisations <- cbind(network_statistics_organisations, Degree_centrality = NA)
    network_statistics_organisations <- cbind(network_statistics_organisations, Closeness_centrality = closeness(x))
    network_statistics_organisations <- cbind(network_statistics_organisations, Betweenness_centrality = betweenness(x))
    network_statistics_organisations <- cbind(network_statistics_organisations, Eigenvector_centrality = evcent(x)$vector)
    network_statistics_organisations <- cbind(network_statistics_organisations, Cluster_coefficent = transitivity(x, type="local"))

    folder_name <- paste("../not_shared/output/", country, "/stats/", country, y, sep="")
    dir.create(dirname(folder_name), showWarnings = FALSE)
    write.csv(network_statistics_organisations, file = paste("../not_shared/output/", country, "/stats/", country, y, sep=""), row.names=FALSE)    
  
}

## call the function to populate for all sectors
create_statistics_for_sectors(survey_network,"_network_statistics_organisations_all.csv")
create_statistics_for_sectors(survey_network_basic_needs,"_network_statistics_organisations_basic_needs.csv")
create_statistics_for_sectors(survey_network_education,"_network_statistics_organisations_education.csv")
create_statistics_for_sectors(survey_network_food_security,"_network_statistics_organisations_food_security.csv")
create_statistics_for_sectors(survey_network_health,"_network_statistics_organisations_health.csv")
create_statistics_for_sectors(survey_network_livelihoods,"_network_statistics_organisations_livelihoods.csv")
create_statistics_for_sectors(survey_network_protection,"_network_statistics_organisations_protection.csv")
create_statistics_for_sectors(survey_network_shelter,"_network_statistics_organisations_shelter.csv")
create_statistics_for_sectors(survey_network_wash,"_network_statistics_organisations_wash.csv")

####################################################################################################################
### CREATE SUMMARY CHARTS ##########################################################################################
####################################################################################################################

folder_name <- paste("../not_shared/output/",country,"/data_protection/measures/", sep="")
dir.create(dirname(folder_name), showWarnings = FALSE)

chart_export <- draw_bar_chart("Organisational data protection measures by sector", "data_protection_organisational_measures_by_sector",
                               "Data protection rating", "Sector", network_statistics_sectors, network_statistics_sectors$Avg_DP_OM,
                               network_statistics_sectors$Graph, network_statistics_sectors$Avg_DP_OM)

chart_export <- draw_bar_chart("Physical data protection measures by sector", "data_protection_physical_measures_by_sector", 
                               "Data protection rating", "Sector", network_statistics_sectors, network_statistics_sectors$Avg_DP_PM, 
                               network_statistics_sectors$Graph, network_statistics_sectors$Avg_DP_PM)

chart_export <- draw_bar_chart("Technical data protection measures by sector", "data_protection_technical_measures_by_sector",
                               "Data protection rating", "Sector", network_statistics_sectors, network_statistics_sectors$Avg_DP_TM, 
                               network_statistics_sectors$Graph, network_statistics_sectors$Avg_DP_TM)

chart_export <- draw_bar_chart("Rights of data subject data protection measures by sector", "data_protection_rights_of_data_subject_measures_by_sector",
                               "Data protection rating", "Sector", network_statistics_sectors, network_statistics_sectors$Avg_DP_RDS, 
                               network_statistics_sectors$Graph, network_statistics_sectors$Avg_DP_RDS)

chart_export <- draw_bar_chart("Onward data sharing data protection measures by sector", "data_protection_onward_data_sharing_measures_by_sector",
                               "Data protection rating", "Sector", network_statistics_sectors, network_statistics_sectors$Avg_DP_ODS, 
                               network_statistics_sectors$Graph, network_statistics_sectors$Avg_DP_ODS)

chart_export <- draw_bar_chart("Average data protection measures by sector", "data_protection_average_total_measures_by sector",
                               "Data protection rating", "Sector", network_statistics_sectors, network_statistics_sectors$Avg_DP_Total, 
                               network_statistics_sectors$Graph, network_statistics_sectors$Avg_DP_Total)


p1 <- ggplot(data=network_statistics_sectors, aes(x=network_statistics_sectors$Graph, y=network_statistics_sectors$Avg_DP_Total)) + 
      geom_bar(stat="identity", fill="#0072B6") +
      geom_text(aes(label=round(network_statistics_sectors$Avg_DP_Total,2)), vjust=1.6, color="white", size=3.5) + 
      theme(legend.position="none", axis.title.x = element_blank()) + 
      ggtitle("Average data protection") + 
      ylab("Data protection")+
      geom_hline(yintercept = mean(network_statistics_sectors$Avg_DP_Total, na.rm = TRUE), color="grey30", size=1.5, alpha=0.5, linetype="dashed") +
      coord_cartesian(ylim=c(0,1)) +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
      panel.background = element_blank(), axis.line = element_line(colour = "black"),axis.ticks.x=element_blank(),axis.ticks.y=element_blank())

p2 <- ggplot(data=network_statistics_sectors, aes(x=network_statistics_sectors$Graph, y=network_statistics_sectors$Avg_DP_OM)) + 
      geom_bar(stat="identity", fill="#0072B6") +
      geom_text(aes(label=round(network_statistics_sectors$Avg_DP_OM,2)), vjust=1.6, color="white", size=3.5) + 
      theme(legend.position="none", axis.title.x = element_blank()) + 
      ggtitle("Average organisational measures") + 
      ylab("Data protection")+
      geom_hline(yintercept = mean(network_statistics_sectors$Avg_DP_OM, na.rm = TRUE), color="grey30", size=1.5, alpha=0.5, linetype="dashed") +
      coord_cartesian(ylim=c(0,1)) +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
      panel.background = element_blank(), axis.line = element_line(colour = "black"),axis.ticks.x=element_blank(),axis.ticks.y=element_blank())

p3 <- ggplot(data=network_statistics_sectors, aes(x=network_statistics_sectors$Graph, y=network_statistics_sectors$Avg_DP_PM)) + 
      geom_bar(stat="identity", fill="#0072B6") +
      geom_text(aes(label=round(network_statistics_sectors$Avg_DP_PM,2)), vjust=1.6, color="white", size=3.5) + 
      theme(legend.position="none", axis.title.x = element_blank()) + 
      ggtitle("Physical measures") + 
      ylab("Data protection")+
      geom_hline(yintercept = mean(network_statistics_sectors$Avg_DP_PM, na.rm = TRUE), color="grey30", size=1.5, alpha=0.5, linetype="dashed") +
      coord_cartesian(ylim=c(0,1)) +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
      panel.background = element_blank(), axis.line = element_line(colour = "black"),axis.ticks.x=element_blank(),axis.ticks.y=element_blank())

p4 <- ggplot(data=network_statistics_sectors, aes(x=network_statistics_sectors$Graph, y=network_statistics_sectors$Avg_DP_TM)) + 
      geom_bar(stat="identity", fill="#0072B6") +
      geom_text(aes(label=round(network_statistics_sectors$Avg_DP_TM,2)), vjust=1.6, color="white", size=3.5) + 
      theme(legend.position="none", axis.title.x = element_blank()) + 
      ggtitle("Technical measures") + 
      ylab("Data protection")+
      geom_hline(yintercept = mean(network_statistics_sectors$Avg_DP_TM, na.rm = TRUE), color="grey30", size=1.5, alpha=0.5, linetype="dashed") +
      coord_cartesian(ylim=c(0,1)) +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
      panel.background = element_blank(), axis.line = element_line(colour = "black"),axis.ticks.x=element_blank(),axis.ticks.y=element_blank())

p5 <- ggplot(data=network_statistics_sectors, aes(x=network_statistics_sectors$Graph, y=network_statistics_sectors$Avg_DP_RDS)) + 
      geom_bar(stat="identity", fill="#0072B6") +
      geom_text(aes(label=round(network_statistics_sectors$Avg_DP_RDS,2)), vjust=1.6, color="white", size=3.5) + 
      theme(legend.position="none", axis.title.x = element_blank()) + 
      ggtitle("Rights of data subject") + 
      ylab("Data protection")+
      geom_hline(yintercept = mean(network_statistics_sectors$Avg_DP_RDS, na.rm = TRUE), color="grey30", size=1.5, alpha=0.5, linetype="dashed") +
      coord_cartesian(ylim=c(0,1)) +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
      panel.background = element_blank(), axis.line = element_line(colour = "black"),axis.ticks.x=element_blank(),axis.ticks.y=element_blank())

p6 <- ggplot(data=network_statistics_sectors, aes(x=network_statistics_sectors$Graph, y=network_statistics_sectors$Avg_DP_ODS)) + 
      geom_bar(stat="identity", fill="#0072B6") +
      geom_text(aes(label=round(network_statistics_sectors$Avg_DP_ODS,2)), vjust=1.6, color="white", size=3.5) + 
      theme(legend.position="none", axis.title.x = element_blank()) + 
      ggtitle("Onward data sharing") + 
      ylab("Data protection")+
      geom_hline(yintercept = mean(network_statistics_sectors$Avg_DP_ODS, na.rm = TRUE), color="grey30", size=1.5, alpha=0.5, linetype="dashed") +
      coord_cartesian(ylim=c(0,1)) +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
      panel.background = element_blank(), axis.line = element_line(colour = "black"),axis.ticks.x=element_blank(),axis.ticks.y=element_blank())

multiplot(p1, p2, p3, p4, p5, p6, cols=2)

File <- paste("../not_shared/output/",country,"/data_protection/measures/contact_sheet_data_protection.png", sep="")
dir.create(dirname(File), showWarnings = FALSE)
ggsave(File, multiplot(p1, p2, p3, p4, p5, p6, cols=2), device = "png", scale = 1, width = 10, height = 6, units = "in", dpi = 300) 

################################################################################
############################ DP MEASURE LEVEL ##################################
################################################################################

folder_name <- paste("../not_shared/output/",country,"/data_protection/sectors/", sep="")
dir.create(dirname(folder_name), showWarnings = FALSE)

chart_export <- draw_bar_chart("All sectors data protection ratings by measure",                 #title
                                        "All_sectors_data_protection_ratings_by_measure",             #filename
                                        "Data protection rating",                                         #ylabel 
                                        "Data protection measure",                                        #xlabel
                                        network_statistics_measures,                                      #chart_data
                                        as.numeric(network_statistics_measures$all),                      #label_value
                                        row.names(network_statistics_measures),                           #xaxis
                                        as.numeric(network_statistics_measures$all),                       #yaxis
                                        FALSE                               
)

chart_export <- draw_measures_bar_chart("Basic Needs data protection ratings by measure",                 #title
                                        "Basic_Needs_data_protection_ratings_by_measure",             #filename
                                        "Data protection rating",                                         #ylabel 
                                        "Data protection measure",                                        #xlabel
                                        network_statistics_measures,                                      #chart_data
                                        as.numeric(network_statistics_measures$basic_needs),              #label_value
                                        row.names(network_statistics_measures),                           #xaxis
                                        as.numeric(network_statistics_measures$basic_needs)               #yaxis
)

chart_export <- draw_measures_bar_chart("Education data protection ratings by measure",                   #title
                                        "Education_data_protection_ratings_by_measure",               #filename
                                        "Data protection rating",                                         #ylabel 
                                        "Data protection measure",                                        #xlabel
                                        network_statistics_measures,                                      #chart_data
                                        as.numeric(network_statistics_measures$education),                #label_value
                                        row.names(network_statistics_measures),                           #xaxis
                                        as.numeric(network_statistics_measures$education)                 #yaxis
)

chart_export <- draw_measures_bar_chart("Food security data protection ratings by measure",               #title
                                        "Food_security_data_protection_ratings_by_measure",           #filename
                                        "Data protection rating",                                         #ylabel 
                                        "Data protection measure",                                        #xlabel
                                        network_statistics_measures,                                      #chart_data
                                        as.numeric(network_statistics_measures$food_security),            #label_value
                                        row.names(network_statistics_measures),                           #xaxis
                                        as.numeric(network_statistics_measures$food_security)             #yaxis
)

chart_export <- draw_measures_bar_chart("Health data protection ratings by measure",                      #title
                                        "Health_data_protection_ratings_by_measure",                  #filename
                                        "Data protection rating",                                         #ylabel 
                                        "Data protection measure",                                        #xlabel
                                        network_statistics_measures,                                      #chart_data
                                        as.numeric(network_statistics_measures$health),                   #label_value
                                        row.names(network_statistics_measures),                           #xaxis
                                        as.numeric(network_statistics_measures$health)                    #yaxis
)

chart_export <- draw_measures_bar_chart("Livelihoods data protection ratings by measure",                 #title
                                        "Livelihoods_data_protection_ratings_by_measure",             #filename
                                        "Data protection rating",                                         #ylabel 
                                        "Data protection measure",                                        #xlabel
                                        network_statistics_measures,                                      #chart_data
                                        as.numeric(network_statistics_measures$livelihoods),              #label_value
                                        row.names(network_statistics_measures),                           #xaxis
                                        as.numeric(network_statistics_measures$livelihoods)               #yaxis
)

chart_export <- draw_measures_bar_chart("Protection data protection ratings by measure",                  #title
                                        "Protection_data_protection_ratings_by_measure",              #filename
                                        "Data protection rating",                                         #ylabel 
                                        "Data protection measure",                                        #xlabel
                                        network_statistics_measures,                                      #chart_data
                                        as.numeric(network_statistics_measures$protection),               #label_value
                                        row.names(network_statistics_measures),                           #xaxis
                                        as.numeric(network_statistics_measures$protection)                #yaxis
)

chart_export <- draw_measures_bar_chart("Shelter data protection ratings by measure",                     #title
                                        "Shelter_data_protection_ratings_by_measure",                 #filename
                                        "Data protection rating",                                         #ylabel 
                                        "Data protection measure",                                        #xlabel
                                        network_statistics_measures,                                      #chart_data
                                        as.numeric(network_statistics_measures$shelter),                  #label_value
                                        row.names(network_statistics_measures),                           #xaxis
                                        as.numeric(network_statistics_measures$shelter)                   #yaxis
)

chart_export <- draw_measures_bar_chart("WASH data protection ratings by measure",                 #title
                                        "WASH_data_protection_ratings_by_measure",             #filename
                                        "Data protection rating",                                         #ylabel 
                                        "Data protection measure",                                        #xlabel
                                        network_statistics_measures,                                      #chart_data
                                        as.numeric(network_statistics_measures$wash),                      #label_value
                                        row.names(network_statistics_measures),                           #xaxis
                                        as.numeric(network_statistics_measures$wash)                       #yaxis
)

################################################################################
########################### ORGANISATION LEVEL #################################
################################################################################

