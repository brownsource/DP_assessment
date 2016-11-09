#### CREATES THE REQUIRED NODE AND EDGE TABLES FOR NETWORK ANALYSIS
#### YOU NEED TO SPECIFY THE COUNTRY AT THE START OF THE SCRIPT

### INPUT: 
## not_shared/output/[COUNTRY]/[COUNTRY]_nodes.csv
## not_shared/output/[COUNTRY]/[COUNTRY]_edges.csv

### OUTPUT:
## not_shared/output/[COUNTRY]/[COUNTRY]_nodes.csv
## not_shared/output/[COUNTRY]/[COUNTRY]_edges.csv
## not_shared/output/[COUNTRY]/[COUNTRY]_comments.csv

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
library(ggplot2)
library(png)


### SELECT WHICH COUNTRY
country <- "jordan"
# country <- "lebanon"

### LOAD THE DATA
survey_edges <- data.frame(read.csv(paste("../not_shared/output/", country, "/", country, "_edges.csv", sep=""),
                                    header = TRUE, 
                                    stringsAsFactors = FALSE))
survey_nodes <- data.frame(read.csv(paste("../not_shared/output/", country, "/", country, "_nodes.csv", sep=""),
                                    header = TRUE,
                                    stringsAsFactors = FALSE))


################################################################################
############################# CREATE GRAPH OBJECTS #############################
################################################################################

#Full country network
survey_network <- graph_from_data_frame(d=survey_edges, vertices = survey_nodes, directed = F)
survey_network <- simplify(survey_network, remove.multiple = F, remove.loops = T)

#Sector level networks
survey_network_basic_needs <- induced.subgraph(survey_network, which(V(survey_network)$Filter.Sector.Basic_needs=="True"))
survey_network_education <- induced.subgraph(survey_network, which(V(survey_network)$Filter.Sector.Education=="True"))
survey_network_food_security <- induced.subgraph(survey_network, which(V(survey_network)$Filter.Sector.Food_security=="True"))
survey_network_health <- induced.subgraph(survey_network, which(V(survey_network)$Filter.Sector.Health=="True"))
survey_network_livelihoods <- induced.subgraph(survey_network, which(V(survey_network)$Filter.Sector.Livelihoods=="True"))
survey_network_protection <- induced.subgraph(survey_network, which(V(survey_network)$Filter.Sector.Protection=="True"))
survey_network_shelter <- induced.subgraph(survey_network, which(V(survey_network)$Filter.Sector.Shelter=="True"))
survey_network_wash <- induced.subgraph(survey_network, which(V(survey_network)$Filter.Sector.WASH=="True"))

network_names <- c("survey_network_basic_needs",
                   "survey_network_education",
                   "survey_network_food_security",
                   "survey_network_health",
                   "survey_network_livelihoods",
                   "survey_network_protection",
                   "survey_network_shelter",
                   "survey_network_wash")

################################################################################
############################## CREATE DATA TABLES ##############################
################################################################################

########################### create sector level table ##########################

network_statistics_sectors <- data.frame(Graph=NA,
                                         Count_nodes=NA,
                                         Count_edges=NA,
                                         Density=NA,
                                         Average_degree=NA, #new
                                         Power_law=NA, #new
                                         Avg_Centrality_out=NA, #renamed
                                         Avg_Centrality_in=NA, #renamed
                                         Avg_Centrality_all=NA, #renamed
                                         Shortest_path=NA,
                                         Avg_path_length=NA, #renamed
                                         Diameter=NA,
                                         ClusteringCoefficient=NA, #New
                                         Avg_DP_OM=NA, #renamed
                                         Avg_DP_PM=NA, #renamed
                                         Avg_DP_TM=NA, #renamed
                                         Avg_DP_RDS=NA, #renamed
                                         Avg_DP_ODS=NA, #renamed
                                         Avg_DP_Total=NA) #renamed
network_statistics_sectors <- network_statistics_sectors[-c(1), ]

## function to populate table x= graph y=sectorname
populate_sector_table <- function(x,y) {
  table_row <- data.frame(
              Graph = y, #set Graph name
              Count_nodes = vcount(x), #Count graph nodes
              Count_edges = ecount(x), #Count graph edges
              Density = graph.density(x, loops = FALSE), # Density
              Centrality_out = mean(degree(x, mode="out")), # Average outbound degree centrality
              Centrality_in = mean(degree(x, mode="in")), # Average inbound degree centrality
              Centrality_all = mean(degree(x, mode="all")), # Average undirected degree centrality
              Shortest_path = NA, # Shortest path
              Avg_path_length = NA, # Average path length
              Diameter = diameter(x, directed=TRUE), # Diameter
              DP_OM = mean(V(x)$Format.Data_protection.Organisational_measures, na.rm=TRUE),
              DP_PM = mean(V(x)$Format.Data_protection.Physical_measures, na.rm=TRUE),
              DP_TM = mean(V(x)$Format.Data_protection.Technical_measures, na.rm=TRUE),
              DP_RDS = mean(V(x)$Format.Data_protection.Rights_of_data_subject_measures, na.rm=TRUE),
              DP_ODS = mean(V(x)$Format.Data_protection.Onward_sharing_measures, na.rm=TRUE),
              DP_Total = mean(V(x)$Format.Data_protection.Data_protection_rating, na.rm=TRUE)
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
write.csv(network_statistics_sectors, file = paste("../not_shared/output/", country, "/", country, "_network_statistics_sectors.csv", sep=""), row.names=FALSE)

########################### create organisation level table ##########################

network_statistics_organisations <- data.frame(Organisation=NA,
                                               #attributes,
                                               Degree=NA,
                                               Centrality_out=NA,
                                               Centrality_in=NA,
                                               Centrality_all=NA,
                                               Degree_centrality=NA,
                                               Closeness_centrality=NA,
                                               Betweenness_centrality=NA,
                                               Eigenvector_centrality=NA,
                                               DP_OM=NA,
                                               DP_PM=NA,
                                               DP_TM=NA,
                                               DP_RDS=NA,
                                               DP_ODS=NA,
                                               DP_Total=NA)
network_statistics_organisations <- network_statistics_organisations[-c(1), ]

########################### create data protection level table ##########################

#transpose the table and keep just values needed
names <- network_statistics_sectors$Graph
network_statistics_measures <- as.data.frame(t(network_statistics_sectors), stringsAsFactors = FALSE)
colnames(network_statistics_measures) <- names
network_statistics_measures <- network_statistics_measures[-1:-10,]


################################################################################
##############################              ####################################
################################################################################

folder_name <- paste("../not_shared/output/",country,"/data_protection/measures/", sep="")
dir.create(dirname(folder_name), showWarnings = FALSE)

## export charts
draw_bar_chart <- function(title, filename, ylabel, xlabel, chart_data, label_value, xaxis, yaxis){
  chart_export <- ggplot(data=chart_data, 
                         aes(x=reorder(xaxis,-yaxis), y=yaxis)) + 
    geom_bar(stat="identity",
             fill="#0072B6") +
    geom_text(aes(label=round(label_value,2)), vjust=1.6, color="white", size=3.5) + 
    #theme_minimal() + 
    theme(legend.position="none") + 
    ggtitle(title) + 
    ylab(xlabel)+
    xlab(ylabel) +
    geom_hline(yintercept = mean(yaxis, na.rm = TRUE), color="grey30", size=1.5, alpha=0.5, linetype="dashed") +
    coord_cartesian(ylim=c(0,1)) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"),axis.ticks.x=element_blank(),axis.ticks.y=element_blank())
  
  File <- paste("../not_shared/output/",country,"/data_protection/measures/",filename,".png", sep="")
  dir.create(dirname(File), showWarnings = FALSE)
  ggsave(File, 
         plot = chart_export, 
         device = "png", 
         scale = 1, 
         width = 8, 
         height = 6, 
         units = "in", 
         dpi = 300) 
  }

chart_export <- draw_bar_chart("Organisational data protection measures by sector",         #title
                               "Organisational_data_protection_measures_by_sector",         #filename
                               "Data protection rating",                                    #xlabel
                               "Sector",                                                    #ylabel  
                               network_statistics_sectors,                                  #chart_data
                               network_statistics_sectors$DP_OM,                            #label_value
                               network_statistics_sectors$Graph,                            #xaxis
                               network_statistics_sectors$DP_OM                             #yaxis
                               )

chart_export <- draw_bar_chart("Physical data protection measures by sector",
                               "Physical_data_protection_measures_by_sector",
                               "Data protection rating",
                               "Sector",
                               network_statistics_sectors, 
                               network_statistics_sectors$DP_PM, 
                               network_statistics_sectors$Graph, 
                               network_statistics_sectors$DP_PM 
)

chart_export <- draw_bar_chart("Technical data protection measures by sector",
                               "Technical_data_protection_measures_by_sector",
                               "Data protection rating",
                               "Sector",
                               network_statistics_sectors, 
                               network_statistics_sectors$DP_TM, 
                               network_statistics_sectors$Graph, 
                               network_statistics_sectors$DP_TM 
)

chart_export <- draw_bar_chart("Rights of data subject data protection measures by sector",
                               "Rights_of_data_subject_data_protection_measures_by_sector",
                               "Data protection rating",
                               "Sector",
                               network_statistics_sectors, 
                               network_statistics_sectors$DP_RDS, 
                               network_statistics_sectors$Graph, 
                               network_statistics_sectors$DP_RDS 
)

chart_export <- draw_bar_chart("Onward data sharing data protection measures by sector",
                               "Onward_data_sharing_data_protection_measures_by_sector",
                               "Data protection rating",
                               "Sector",
                               network_statistics_sectors, 
                               network_statistics_sectors$DP_ODS, 
                               network_statistics_sectors$Graph, 
                               network_statistics_sectors$DP_ODS 
)

chart_export <- draw_bar_chart("Average data protection measures by sector",
                               "Average_data_protection_measures_by sector",
                               "Data protection rating",
                               "Sector",
                               network_statistics_sectors, 
                               network_statistics_sectors$DP_Total, 
                               network_statistics_sectors$Graph, 
                               network_statistics_sectors$DP_Total 
)

################################################################################
############################ DP MEASURE LEVEL ##################################
################################################################################

folder_name <- paste("../not_shared/output/",country,"/data_protection/sectors/", sep="")
dir.create(dirname(folder_name), showWarnings = FALSE)

#Function to draw measures bar chart
draw_measures_bar_chart <- function(title, filename, ylabel, xlabel, chart_data, label_value, xaxis, yaxis){
  chart_export <- ggplot(data=chart_data, 
                         aes(x=reorder(xaxis,-yaxis), y=yaxis)) + 
    geom_bar(stat="identity",
             fill="#0072B6") +
    geom_text(aes(label=round(label_value,2)), vjust=1.6, color="white", size=3.5) + 
    #theme_minimal() + 
    theme(legend.position="none") + 
    ggtitle(title) + 
    ylab(xlabel)+
    xlab(ylabel) +
    geom_hline(yintercept = mean(yaxis, na.rm = TRUE), color="grey30", size=1.5, alpha=0.5, linetype="dashed") +
    coord_cartesian(ylim=c(0,1)) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"),axis.ticks.x=element_blank(),axis.ticks.y=element_blank())
  
  File <- paste("../not_shared/output/",country,"/data_protection/sectors/",filename,".png", sep="")
  dir.create(dirname(File), showWarnings = FALSE)
  ggsave(File, 
         plot = chart_export, 
         device = "png", 
         scale = 1, 
         width = 8, 
         height = 6, 
         units = "in", 
         dpi = 300) 
}


chart_export <- draw_measures_bar_chart("All sectors data protection ratings by measure",                 #title
                                        "All_sectors_data_protection_ratings_by_measure",             #filename
                                        "Data protection rating",                                         #ylabel 
                                        "Data protection measure",                                        #xlabel
                                        network_statistics_measures,                                      #chart_data
                                        as.numeric(network_statistics_measures$all),                      #label_value
                                        row.names(network_statistics_measures),                           #xaxis
                                        as.numeric(network_statistics_measures$all)                       #yaxis
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

survey_network

