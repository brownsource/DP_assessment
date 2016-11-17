## function to populate table x= graph y=sectorname
populate_sector_table <- function(x,y) {
  table_row <- data.frame(
    sectors = y, #set Graph name
    Count_nodes = vcount(x), #Count graph nodes
    Count_edges = ecount(x), #Count graph edges
    Density = graph.density(x, loops = FALSE), # Density
    # Average_degree=NA, #new
    # Power_law=NA, #new
    Avg_Centrality_out = mean(degree(x, mode="out")), # Average outbound degree centrality
    Avg_Centrality_in = mean(degree(x, mode="in")), # Average inbound degree centrality
    Avg_Centrality_all = mean(degree(x, mode="all")), # Average undirected degree centrality
    # Shortest_path = NA, # Shortest path
    # Avg_path_length = NA, # Average path length
    Diameter = diameter(x, directed=TRUE), # Diameter
    No_of_Clusters = clusters(x)$no,              
    ClusteringCoefficient = transitivity(x, type="global"),
    Edge_connectivity = edge_connectivity(x),
    Rreciprocity = reciprocity(x),
    
    Avg_DP_OM_policy = mean(V(x)$Format.Data_protection.Organisational_measures_policy, na.rm=TRUE),
    Avg_DP_PM_policy = mean(V(x)$Format.Data_protection.Physical_measures_policy, na.rm=TRUE),
    Avg_DP_TM_policy = mean(V(x)$Format.Data_protection.Technical_measures_policy, na.rm=TRUE),
    Avg_DP_RDS_policy = mean(V(x)$Format.Data_protection.Rights_of_data_subject_measures_policy, na.rm=TRUE),
    Avg_DP_ODS_policy = mean(V(x)$Format.Data_protection.Onward_sharing_measures_policy, na.rm=TRUE),
    Avg_DP_total_policy = mean(V(x)$Format.Data_protection.Data_protection_policy, na.rm=TRUE),
    
    Avg_DP_OM_adherence = mean(V(x)$Format.Data_protection.Organisational_measures_adherence, na.rm=TRUE),
    Avg_DP_PM_adherence = mean(V(x)$Format.Data_protection.Physical_measures_adherence, na.rm=TRUE),
    Avg_DP_TM_adherence = mean(V(x)$Format.Data_protection.Technical_measures_adherence, na.rm=TRUE),
    Avg_DP_RDS_adherence = mean(V(x)$Format.Data_protection.Rights_of_data_subject_measures_adherence, na.rm=TRUE),
    Avg_DP_ODS_adherence = mean(V(x)$Format.Data_protection.Onward_sharing_measures_adherence, na.rm=TRUE),
    Avg_DP_total_adherence = mean(V(x)$Format.Data_protection.Data_protection_adherence, na.rm=TRUE),
    
    Avg_DP_OM_rating = mean(V(x)$Format.Data_protection.Organisational_measures, na.rm=TRUE),
    Avg_DP_PM_rating = mean(V(x)$Format.Data_protection.Physical_measures, na.rm=TRUE),
    Avg_DP_TM_rating = mean(V(x)$Format.Data_protection.Technical_measures, na.rm=TRUE),
    Avg_DP_RDS_rating = mean(V(x)$Format.Data_protection.Rights_of_data_subject_measures, na.rm=TRUE),
    Avg_DP_ODS_rating = mean(V(x)$Format.Data_protection.Onward_sharing_measures, na.rm=TRUE),
    AVG_DP_total_rating = mean(V(x)$Format.Data_protection.Data_protection_rating, na.rm=TRUE)
  )
}