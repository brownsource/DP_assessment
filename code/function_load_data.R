### Loads the data for analysis

load_data <- function(x) {
  
  ## Load the data
  survey_edges <- data.frame(read.csv(paste("../not_shared/output/", x, "/", x, "_edges.csv", sep=""),header = TRUE, stringsAsFactors = FALSE))
  survey_nodes <- data.frame(read.csv(paste("../not_shared/output/", x, "/", x, "_nodes.csv", sep=""),header = TRUE, stringsAsFactors = FALSE))
  
}