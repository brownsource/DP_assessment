## function to populate table x= graph y=sectorname
populate_organisation_table <- function(x, y) {
  
  class(survey_network)
  temp <- as_data_frame(survey_network, what = "vertices")
  network_statistics_organisations <- temp
  network_statistics_organisations <- cbind(network_statistics_organisations, Degree = degree(x))
  network_statistics_organisations <- cbind(network_statistics_organisations, Centrality_out = degree(x, mode="out"))
  network_statistics_organisations <- cbind(network_statistics_organisations, Centrality_in = degree(x, mode="in"))
  network_statistics_organisations <- cbind(network_statistics_organisations, Centrality_all = degree(x, mode="all"))
  network_statistics_organisations <- cbind(network_statistics_organisations, Degree_centrality = NA)
  network_statistics_organisations <- cbind(network_statistics_organisations, Closeness_centrality = closeness(x))
  network_statistics_organisations <- cbind(network_statistics_organisations, Betweenness_centrality = betweenness(x))
  network_statistics_organisations <- cbind(network_statistics_organisations, Eigenvector_centrality = evcent(x)$vector)
  network_statistics_organisations <- cbind(network_statistics_organisations, Cluster_coefficent = transitivity(x, type="local"))
  
  # file_name <- paste("../not_shared/output/", country, "/outputs/tables/", country, "_network_statistics_dpmeasures.csv", sep="")
  # dir.create(dirname(file_name), showWarnings = FALSE)
  # write.csv(network_statistics_measures, file = file_name, row.names=FALSE)
  # 
  # folder_name <- paste("../not_shared/output/", country, "/stats/", country, y, sep="")
  # dir.create(dirname(folder_name), showWarnings = FALSE)
  # write.csv(network_statistics_organisations, file = paste("../not_shared/output/", country, "/stats/", country, y, sep=""), row.names=FALSE)    
  
}