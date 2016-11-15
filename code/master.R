# Master script

####################################################################################################################
### INITIALISE THE WORKSPACE #######################################################################################
####################################################################################################################

rm(list = ls()) # Remove all the objects we created so far.

source("code/function_load_data.R")
source("code/function_load_colors.R")

# Load the data (options: "jordan", "lebanon")
country <- "jordan"
survey_edges <- load_data(country)
survey_nodes <- load_data(country)


countries <- c("jordan","lebanon")
partners_surveyed <-
policy_equivelance <-
staff_adherence <-
dp_rating <-
