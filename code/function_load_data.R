### Loads the data for analysis

load_data <- function(x,y) {
  
  ## Load the data
  data.frame(read.csv(paste("../not_shared/output/", x, "/inputs/", x, "_", y, ".csv", sep=""),header = TRUE, stringsAsFactors = FALSE))
  #survey_nodes <- data.frame(read.csv(paste("../not_shared/output/", x, "/", x, "_nodes.csv", sep=""),header = TRUE, stringsAsFactors = FALSE))
  
}