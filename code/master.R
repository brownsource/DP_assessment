# Master script

####################################################################################################################
### INITIALISE THE WORKSPACE #######################################################################################
####################################################################################################################

rm(list = ls()) # Remove all the objects we created so far.

## Load the packages
if(!require(igraph)){
  install.packages("igraph")
}

## Load the libraries
library(igraph)

## Load the global fuctions
source("code/function_load_colors.R")

## LOAD THE DATA (options: "jordan", "lebanon") ###################################################################

source("code/function_load_data.R")

country <- "jordan"
survey_edges <- load_data(country, "edges")
survey_nodes <- load_data(country, "nodes")

## CREATE GRAPH OBJECTS ###########################################################################################

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
network_names <- c("survey_network", "survey_network_basic_needs", "survey_network_education", "survey_network_food_security",
                   "survey_network_health", "survey_network_livelihoods", "survey_network_protection", "survey_network_shelter",
                   "survey_network_wash")

### CREATE SECTOR LEVEL TABLE ######################################################################################

source("code/function_populate_sector_table.R")

network_statistics_sectors <- data.frame() 
network_statistics_sectors <- network_statistics_sectors[-c(1), ]

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
file_name <- paste("../not_shared/output/", country, "/outputs/country_level/", country, "_network_statistics_sectors.csv", sep="")
dir.create(dirname(file_name), showWarnings = FALSE)
write.csv(network_statistics_sectors, file = file_name, row.names=FALSE)

### create data protection level table #############################################################################

## Transpose the table and keep just values needed
# Create vector of sectors, which will become column headers
names <- network_statistics_sectors$sectors 
# Create a dataframe and transepose the network_statistics_sectors table
network_statistics_measures <- as.data.frame(t(network_statistics_sectors), stringsAsFactors = FALSE)
# paste column headers
colnames(network_statistics_measures) <- names
# remove colum header vector
rm(names)
# remove unneccessary fields
network_statistics_measures <- network_statistics_measures[-1:-12,]

## export the table as csv
file_name <- paste("../not_shared/output/", country, "/outputs/country_level/", country, "_network_statistics_dpmeasures.csv", sep="")
dir.create(dirname(file_name), showWarnings = FALSE)
write.csv(network_statistics_measures, file = file_name, row.names=FALSE)

### create organisation level tables #############################################################################

source("code/function_populate_organisation_table.R")

## call the function to populate for all sectors
populate_organisation_table(survey_network,"_network_statistics_organisations_all.csv")
file_name <- paste("../not_shared/output/", country, "/outputs/country_level/", country, "_network_statistics_organisations_all.csv", sep="")
dir.create(dirname(file_name), showWarnings = FALSE)
write.csv(network_statistics_measures, file = file_name, row.names=FALSE)

populate_organisation_table(survey_network_basic_needs,"_network_statistics_organisations_basic_needs.csv")
populate_organisation_table(survey_network_education,"_network_statistics_organisations_education.csv")
populate_organisation_table(survey_network_food_security,"_network_statistics_organisations_food_security.csv")
populate_organisation_table(survey_network_health,"_network_statistics_organisations_health.csv")
populate_organisation_table(survey_network_livelihoods,"_network_statistics_organisations_livelihoods.csv")
populate_organisation_table(survey_network_protection,"_network_statistics_organisations_protection.csv")
populate_organisation_table(survey_network_shelter,"_network_statistics_organisations_shelter.csv")
populate_organisation_table(survey_network_wash,"_network_statistics_organisations_wash.csv")

## TOP LEVEL FIGURES
# partners_surveyed <-
# policy_equivelance <-
# staff_adherence <-
# dp_rating <-
