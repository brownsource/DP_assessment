# INITIALIZE
# Create a data.frame to save the cleaned EDGE values to
edges2 <- data.frame(matrix(ncol = 5))
column_names_edges <- c(
  "Source", 
  "Target",
  "Label",
  "Type",
  "Weight"
)
colnames(edges2) <- column_names_edges
# Remove the column name vector
rm(column_names_edges)
edges2 <- edges2[-c(1), ]

# REPEAT
for (i in 1:nrow(kobo_import)){
  # If the value of the cell is TRUE
  TargetOrgs <- c(ifelse (kobo_import[i,16:76] == "True", 
                          # Extract the Organisation ID from the header
                          as.numeric(substr(c(colnames(kobo_import[16:76])), 61, 64)),
                          # Or write NA
                          NA))
  length(TargetOrgs)
  # Remove the NA
  TargetOrgs <-TargetOrgs[!is.na(TargetOrgs)]
  length(TargetOrgs)
  #TargetOrgs
  #length(TargetOrgs)
  if(length(TargetOrgs) != 0) { 
         SourceOrgs <- rep(kobo_import[i,1], length(TargetOrgs))
         #SourceOrgs
         EdgeFrame <-data.frame(Source = SourceOrgs, Target = TargetOrgs, Label = NA, Type = "Undirected", Weight = 1)
         #EdgeFrame
         edges2 <- rbind(edges2, EdgeFrame)
  }
}

kobo_import[4,16:76]



edges2 <- rbind(EdgeFrame,c(SourceOrgs, TargetOrgs, NA, NA, NA))
edges2



#A loop for each row in the import sheet
for (i in 1:nrow(kobo_import)) {
  print(kobo_import$group_organisational_ties.Org_ties_Source_Organisation)
  
}

#Identify colums where TRUE




#Extracting the Org ID from the Column names
substr(c(colnames(kobo_import[15])), 61, 64)
as.numeric(substr(c(colnames(kobo_import[16:76])), 61, 64))

#Transposing the Org IDs

#Creating a matrix with the source and target Org IDs
