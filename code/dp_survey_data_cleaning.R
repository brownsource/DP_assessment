#### CREATES THE REQUIRED NODE AND EDGE TABLES FOR NETWORK ANALYSIS
#### YOU NEED TO SPECIFY THE COUNTRY AT THE START OF THE SCRIPT

### BEFORE YOU BEGIN:
## Input file should be checked for:
## 1. If the source organisation is new and not in the partner_list
## 2. If the downstream organisations are new and not in the partner_list
## 3. If the upstream organisations are new and not in the partner_list
## In all instances the partner_list should be updated


### INPUT: 
## not_shared/data/[COUNTRY]/kobo_import_DPA_[COUNTRY].csv
## not_shared/data/jordan_partner_list.csv

### OUTPUT:
## not_shared/output/[COUNTRY]/[COUNTRY]_nodes.csv
## not_shared/output/[COUNTRY]/[COUNTRY]_edges.csv
## not_shared/output/[COUNTRY]/[COUNTRY]_comments.csv

### CLEAR WORKSPACE
rm(list = ls()) # Remove all the objects we created so far.

### INSTALL PACKAGES IF REQUIRED
if(!require(dplyr)){
  install.packages("dplyr")
}

### INSTALL LIBRARIES IF REQUIRED
### NEED TO CHANGE THIS BLOCK OF CODE
library(dplyr)

### SELECT WHICH COUNTRY
country <- "jordan"
# country <- "lebanon"

### IMPORT THE DATA AND REMOVE N/A
# Import survey data
kobo_survey_import <- read.csv(paste("../not_shared/data/", country, "/kobo_import_DPA_", country, ".csv", sep=""),
                        header = TRUE,
                        stringsAsFactors = FALSE)
kobo_survey_import[kobo_survey_import == "n/a"] <- ""
kobo_survey_import[kobo_survey_import == "N/A"] <- ""

# Import partner list to retrieve partner names
partner_list <- read.csv(paste("../not_shared/data/", country, "/", country, "_partner_list.csv", sep=""), 
                         header = TRUE, 
                         stringsAsFactors = FALSE)

################################################################################
### PREPARE THE COMMENTS DATA ##################################################
################################################################################

# Create a data.frame
comments <- data.frame(matrix(ncol = 8, nrow = nrow(kobo_survey_import)))
comments_labels <- c(
  "id",
  "label",
  "organisation_name",
  "participant_name",
  "email_address",
  "summary_request",
  "comment",
  "own_case_management")
colnames(comments) <- comments_labels
rm(comments_labels)
# import data (matching some with the partner_list table)
comments$id <- kobo_survey_import$GROUP_organization_details.organizational_connections_source_organisation
comments$label <- partner_list$Acronym[match(comments$id,partner_list$ID)]
comments$organisation_name <- partner_list$Name[match(comments$id,partner_list$ID)]
comments$participant_name <- kobo_survey_import$GROUP_participation_agreement.participation_name
comments$email_address <- kobo_survey_import$GROUP_participation_agreement.participation_email
comments$summary_request <- kobo_survey_import$GROUP_participation_agreement.summary_request_yn
comments$comment <- kobo_survey_import$GROUP_comments.comments
comments$own_case_management <- kobo_survey_import$GROUP_organization_details.diagnostics_own_case_management_name

################################################################################
### PREPARE THE NODES DATA #####################################################
################################################################################

nodes <- data.frame(matrix(ncol = 46, nrow = nrow(kobo_survey_import)))
nodes_labels <- c(
  "Id",
  "Label",
  "Organisation_name",
  "Type",
  "Replied_or_nominated",
  "Filter.Sector.Basic_needs",
  "Filter.Sector.Education",
  "Filter.Sector.Food_security",
  "Filter.Sector.Health",
  "Filter.Sector.Livelihoods",
  "Filter.Sector.Protection",
  "Filter.Sector.Shelter",
  "Filter.Sector.WASH",
  "Filter.Location.camp_based",
  "Filter.Location.non_camp_based",
  "Format.Data_protection.Organisational_measures",
  "Format.Data_protection.Physical_measures",
  "Format.Data_protection.Technical_measures",
  "Format.Data_protection.Rights_of_data_subject_measures",
  "Format.Data_protection.Onward_sharing_measures",
  "Format.Data_protection.Data_protection_rating",
  "Diagnostic.Satisfaction",
  "Diagnostic.Own_case_management",
  "Diagnostic.Frequency",
  "Diagnostic.Transfer_method.Printed_material",
  "Diagnostic.Transfer_method.Unencrypted_via_email_USB",
  "Diagnostic.Transfer_method.Encrypted_via_email_USB",
  "Diagnostic.Transfer_method.FTP",
  "Diagnostic.Transfer_method.Secure_FTP",
  "Diagnostic.Transfer_method.Direct_access_through_web_interface",
  "Diagnostic.Transfer_method.API",
  "Diagnostic.Agree_with_statement.Happy_with_format",
  "Diagnostic.Agree_with_statement.Happy_with_turnaround",
  "Diagnostic.Agree_with_statement.Happy_with_workflow",
  "Diagnostic.Agree_with_statement.Happy_with_accuracy",
  "Diagnostic.Dont_follow_policy.Lack_of_awareness_of_policy_existance",
  "Diagnostic.Dont_follow_policy.Lack_of_awareness_of_policy_details",
  "Diagnostic.Dont_follow_policy.Not_enough_time_to_follow_protocol",
  "Diagnostic.Dont_follow_policy.Not_enough_resource_to_follow_protocol",
  "Diagnostic.Dont_follow_policy.Valid_and_exceptional_circumstances_but_without_approval",
  "Diagnostic.Do_follow_policy.Awareness_of_policy_existance",
  "Diagnostic.Do_follow_policy.Awareness_of_policy_details",
  "Diagnostic.Do_follow_policy.Enough_time_to_follow_protocol",
  "Diagnostic.Do_follow_policy.Enough_resource_to_follow_protocol",
  "Diagnostic.Do_follow_policy.Valid_and_exceptional_circumstances_but_with_approval",
  "Diagnostic.Do_follow_policy.Normative_behaviour"
  )
colnames(nodes) <- nodes_labels
rm(nodes_labels)
# import data (matching some with the partner_list table)
nodes$Id <- kobo_survey_import$GROUP_organization_details.organizational_connections_source_organisation

nodes$Label <- partner_list$Acronym[match(nodes$Id,partner_list$ID)]
nodes$Organisation_name <- partner_list$Name[match(nodes$Id,partner_list$ID)]
nodes$Type <- partner_list$Type[match(nodes$Id,partner_list$ID)]

nodes$Replied_or_nominated <- rep(c("Replied"), nrow(kobo_survey_import))

nodes$Filter.Sector.Basic_needs <- kobo_survey_import$GROUP_organization_details.organizational_connections_sectors.basic_needs
nodes$Filter.Sector.Education <- kobo_survey_import$GROUP_organization_details.organizational_connections_sectors.education
nodes$Filter.Sector.Food_security <- kobo_survey_import$GROUP_organization_details.organizational_connections_sectors.food_security
nodes$Filter.Sector.Health <- kobo_survey_import$GROUP_organization_details.organizational_connections_sectors.health
nodes$Filter.Sector.Livelihoods <- kobo_survey_import$GROUP_organization_details.organizational_connections_sectors.livelihoods
nodes$Filter.Sector.Protection <- kobo_survey_import$GROUP_organization_details.organizational_connections_sectors.protection
nodes$Filter.Sector.Shelter <- kobo_survey_import$GROUP_organization_details.organizational_connections_sectors.shelter
nodes$Filter.Sector.WASH <- kobo_survey_import$GROUP_organization_details.organizational_connections_sectors.wash

nodes$Filter.Location.camp_based <- kobo_survey_import$GROUP_organization_details.organizational_connections_location.camp_based
nodes$Filter.Location.non_camp_based <- kobo_survey_import$GROUP_organization_details.organizational_connections_location.non_camp_based

nodes$Diagnostic.Satisfaction <- kobo_survey_import$GROUP_diagnostics.diagnostics_satisfaction
nodes$Diagnostic.Own_case_management <- kobo_survey_import$GROUP_organization_details.diagnostics_own_case_management
nodes$Diagnostic.Frequency <- kobo_survey_import$GROUP_diagnostics.diagnostics_frequency

nodes$Diagnostic.Transfer_method.Printed_material <- kobo_survey_import$GROUP_diagnostics.diagnostics_transfer_method.printed_material
nodes$Diagnostic.Transfer_method.Unencrypted_via_email_USB <- kobo_survey_import$GROUP_diagnostics.diagnostics_transfer_method.unencrypted
nodes$Diagnostic.Transfer_method.Encrypted_via_email_USB <- kobo_survey_import$GROUP_diagnostics.diagnostics_transfer_method.encrypted
nodes$Diagnostic.Transfer_method.FTP <- kobo_survey_import$GROUP_diagnostics.diagnostics_transfer_method.ftp
nodes$Diagnostic.Transfer_method.Secure_FTP <- kobo_survey_import$GROUP_diagnostics.diagnostics_transfer_method.secure_ftp
nodes$Diagnostic.Transfer_method.Direct_access_through_web_interface <- kobo_survey_import$GROUP_diagnostics.diagnostics_transfer_method.direct_access
nodes$Diagnostic.Transfer_method.API <- kobo_survey_import$GROUP_diagnostics.diagnostics_transfer_method.api
nodes$Diagnostic.Agree_with_statement.Happy_with_format <- kobo_survey_import$GROUP_diagnostics.SUBGROUP_diagnostics_statements.diagnostics_statements_format
nodes$Diagnostic.Agree_with_statement.Happy_with_turnaround <- kobo_survey_import$GROUP_diagnostics.SUBGROUP_diagnostics_statements.diagnostics_statements_timely
nodes$Diagnostic.Agree_with_statement.Happy_with_workflow <- kobo_survey_import$GROUP_diagnostics.SUBGROUP_diagnostics_statements.diagnostics_statements_workflow
nodes$Diagnostic.Agree_with_statement.Happy_with_accuracy <- kobo_survey_import$GROUP_diagnostics.SUBGROUP_diagnostics_statements.diagnostics_statements_accurate

nodes$Diagnostic.Dont_follow_policy.Lack_of_awareness_of_policy_existance <- kobo_survey_import$GROUP_diagnostics.diagnostics_dont_follow_policy.Lack_of_awareness_of_policy_existance
nodes$Diagnostic.Dont_follow_policy.Lack_of_awareness_of_policy_details <- kobo_survey_import$GROUP_diagnostics.diagnostics_dont_follow_policy.Lack_of_awareness_of_policy_details
nodes$Diagnostic.Dont_follow_policy.Not_enough_time_to_follow_protocol <- kobo_survey_import$GROUP_diagnostics.diagnostics_dont_follow_policy.Not_enough_time_to_follow_protocol
nodes$Diagnostic.Dont_follow_policy.Not_enough_resource_to_follow_protocol <- kobo_survey_import$GROUP_diagnostics.diagnostics_dont_follow_policy.Not_enough_resource_to_follow_protocol
nodes$Diagnostic.Dont_follow_policy.Valid_and_exceptional_circumstances_but_without_approval <- kobo_survey_import$GROUP_diagnostics.diagnostics_dont_follow_policy.Valid_and_exceptional_circumstances_but_without_approval
nodes$Diagnostic.Do_follow_policy.Awareness_of_policy_existance <- kobo_survey_import$GROUP_diagnostics.diagnostics_do_follow_policy.Awareness_of_policy_existance
nodes$Diagnostic.Do_follow_policy.Awareness_of_policy_details <- kobo_survey_import$GROUP_diagnostics.diagnostics_do_follow_policy.Awareness_of_policy_details
nodes$Diagnostic.Do_follow_policy.Enough_time_to_follow_protocol <- kobo_survey_import$GROUP_diagnostics.diagnostics_do_follow_policy.Enough_time_to_follow_protocol
nodes$Diagnostic.Do_follow_policy.Enough_resource_to_follow_protocol <- kobo_survey_import$GROUP_diagnostics.diagnostics_do_follow_policy.Enough_resource_to_follow_protocol
nodes$Diagnostic.Do_follow_policy.Valid_and_exceptional_circumstances_but_with_approval <- kobo_survey_import$GROUP_diagnostics.diagnostics_do_follow_policy.Valid_and_exceptional_circumstances_but_with_approval
nodes$Diagnostic.Do_follow_policy.Normative_behaviour <- kobo_survey_import$GROUP_diagnostics.diagnostics_do_follow_policy.Normative_behaviour


# Calculate ORGANISATIONAL MEASURES values
nodes$Format.Data_protection.Organisational_measures <- ((
  kobo_survey_import$GROUP_data_protection.SUBGROUP_organizational_measures.dp_om_focal_point +
  kobo_survey_import$GROUP_data_protection.SUBGROUP_organizational_measures.dp_om_have_policy +
  kobo_survey_import$GROUP_data_protection.SUBGROUP_organizational_measures.dp_om_have_code_of_conduct +
  kobo_survey_import$GROUP_data_protection.SUBGROUP_organizational_measures.dp_om_data_breach_process) *
  kobo_survey_import$GROUP_data_protection.SUBGROUP_organizational_measures.dp_om_staff_rating
) / 20

# Calculate PHYSICAL MEASURES values
nodes$Format.Data_protection.Physical_measures <- ((
  kobo_survey_import$GROUP_data_protection.SUBGROUP_physical_measures.dp_pm_stored_safe + 
  kobo_survey_import$GROUP_data_protection.SUBGROUP_physical_measures.dp_pm_offices_locked + 
  kobo_survey_import$GROUP_data_protection.SUBGROUP_physical_measures.dp_pm_staff_reminder + 
  kobo_survey_import$GROUP_data_protection.SUBGROUP_physical_measures.dp_pm_safe_paper_waste + 
  kobo_survey_import$GROUP_data_protection.SUBGROUP_physical_measures.dp_pm_visitors + 
  kobo_survey_import$GROUP_data_protection.SUBGROUP_physical_measures.dp_pm_usb + 
  kobo_survey_import$GROUP_data_protection.SUBGROUP_physical_measures.dp_pm_filing_cabinets) * 
  kobo_survey_import$GROUP_data_protection.SUBGROUP_physical_measures.dp_pm_staff_rating
) / 35

# Calculate TECHNICAL MEASURES values
nodes$Format.Data_protection.Technical_measures <- ((
  kobo_survey_import$GROUP_data_protection.SUBGROUP_technical_measures.dp_tm_passwords +
  kobo_survey_import$GROUP_data_protection.SUBGROUP_technical_measures.dp_tm_tiered_access +
  kobo_survey_import$GROUP_data_protection.SUBGROUP_technical_measures.dp_tm_backups +
  kobo_survey_import$GROUP_data_protection.SUBGROUP_technical_measures.dp_tm_personal_account_awareness) *
  kobo_survey_import$GROUP_data_protection.SUBGROUP_technical_measures.dp_tm_staff_rating
) / 20

# Calculate RIGHTS OF DATA SUBJECT MEASURES values
nodes$Format.Data_protection.Rights_of_data_subject_measures <- 
  ifelse (kobo_survey_import$GROUP_data_protection.SUBGROUP_rights_of_data_subjects.dp_rds_data_collection_yn == 0, NA,
          ((as.numeric(kobo_survey_import$GROUP_data_protection.SUBGROUP_rights_of_data_subjects.dp_rds_informed) +
            as.numeric(kobo_survey_import$GROUP_data_protection.SUBGROUP_rights_of_data_subjects.dp_rds_consent) +
            as.numeric(kobo_survey_import$GROUP_data_protection.SUBGROUP_rights_of_data_subjects.dp_rds_opportunity_to_object) +
            as.numeric(kobo_survey_import$GROUP_data_protection.SUBGROUP_rights_of_data_subjects.dp_rds_aware_of_rights)) *
            as.numeric(kobo_survey_import$GROUP_data_protection.SUBGROUP_rights_of_data_subjects.dp_rds_staff_rating)
          ) /20
  )

# Calculate ONWARD DATA SHARING MEASURES values

temp <- data.frame(
  "tempController1" = kobo_survey_import$GROUP_data_protection.SUBGROUP_onward_data_sharing.dp_ods_yn, 
  "tempA" = kobo_survey_import$GROUP_data_protection.SUBGROUP_onward_data_sharing.dp_ods_outside_of_country, 
  "tempController2" = kobo_survey_import$GROUP_data_protection.SUBGROUP_onward_data_sharing.dp_ods_national_legislation_yn,
  "tempB"= kobo_survey_import$GROUP_data_protection.SUBGROUP_onward_data_sharing.dp_ods_national_legislation_risk_mitigation, 
  "tempC" = kobo_survey_import$GROUP_data_protection.SUBGROUP_onward_data_sharing.dp_ods_law_enforcement,
  "tempController3" = kobo_survey_import$GROUP_data_protection.SUBGROUP_onward_data_sharing.dp_ods_third_party_yn,
  "tempD" = kobo_survey_import$GROUP_data_protection.SUBGROUP_onward_data_sharing.dp_ods_third_party_provision_for_deletion,
  "tempE" = kobo_survey_import$GROUP_data_protection.SUBGROUP_onward_data_sharing.dp_ods_third_party_archived_securely, 
  "tempMultiplier" = kobo_survey_import$GROUP_data_protection.SUBGROUP_onward_data_sharing.dp_ods_staff_rating
  )

result <- ifelse(temp$tempController1 == "", NA, {
              rowSums(cbind(
                  c(as.numeric(as.character(temp$tempA))), 
                  c(ifelse(temp$tempController2 == 0, NA, {
                    as.numeric(as.character(temp$tempB))
                  })),
                  c(as.numeric(as.character(temp$tempC))),
                  c(ifelse(temp$tempController3 == 0, NA, {
                    as.numeric(as.character(temp$tempD)) +
                    as.numeric(as.character(temp$tempE))
                  }))),
              na.rm = TRUE)
          })
result <- result * as.numeric(as.character(temp$tempMultiplier))

maxmultiplier <-  ifelse(temp$tempController1 == "", NA, {
                2+
                ifelse(temp$tempController2 == 1,1,0)+
                ifelse(temp$tempController3 == 1,2,0)
              })*5

nodes$Format.Data_protection.Onward_sharing_measures <- result/maxmultiplier              

# Calculate final DATA PROTECTION RATING value
nodes$Format.Data_protection.Data_protection_rating <- rowMeans(nodes[,16:20], na.rm=TRUE)

################################################################################
### PREPARE THE EDGES DATA #####################################################
################################################################################

# Create a data.frame to save the cleaned EDGE values to
edges <- data.frame(matrix(ncol = 5))
column_names_edges <- c(
  "from", 
  "to",
  "label",
  "type",
  "weight"
)
colnames(edges) <- column_names_edges
edges <- edges[-c(1), ]
# Remove the column name vector
rm(column_names_edges)

# IDENTIFY DOWNSTREAM ORGANISATION CONNECTIONS
# Repeat for each row in kobo_import
for (i in 1:nrow(kobo_survey_import)){
  
  # If the value of the cell is TRUE
  TargetOrgs <- c(ifelse (kobo_survey_import[i,57:147] == "True",
                          # Extract the Organisation ID from the header
                          as.numeric(substr(c(colnames(kobo_survey_import[57:147])), 76, 78)), #would like to use nchar to get the string length
                          # Or write NA
                          NA))
  # Remove the NA
  TargetOrgs <- TargetOrgs[!is.na(TargetOrgs)]
  # If thelength of TargetOrgs is NOT 0 then Transpose the Organisatonis to the Edges table with the Source ID as the current Row's Organisation
  if(length(TargetOrgs) != 0) { 
    # Create a vector with the source organisation
    SourceOrgs <- rep(as.character(kobo_survey_import[i,4]), length(TargetOrgs))
    # Create a vector with target organisations and some other fixed variables
    EdgeFrame <-data.frame(from = SourceOrgs, to = TargetOrgs, label = NA, type = "Directed", weight = 1)
    # Append the values to the edges table
    edges <- rbind(edges, EdgeFrame)
  }
}
rm(EdgeFrame)

# IDENTIFY UPSTREAM ORGANISATION CONNECTIONS

# Repeat for each row in kobo_import
for (i in 1:nrow(kobo_survey_import)){
  
  # If the value of the cell is TRUE
  SourceOrgs <- c(ifelse (kobo_survey_import[i,151:241] == "True",
                          # Extract the Organisation ID from the header
                          as.numeric(substr(c(colnames(kobo_survey_import[57:147])), 76, 78)),
                          # Or write NA
                          NA))
  # Remove the NA
  SourceOrgs <- SourceOrgs[!is.na(SourceOrgs)]
  # If thelength of TargetOrgs is NOT 0 then Transpose the Organisatonis to the Edges table with the Source ID as the current Row's Organisation
  if(length(SourceOrgs) != 0) { 
    # Create a vector with the source organisation
    TargetOrgs <- rep(as.character(kobo_survey_import[i,4]), length(SourceOrgs))
    # Create a vector with target organisations and some other fixed variables
    EdgeFrame <-data.frame(from = as.character(SourceOrgs), to = TargetOrgs, label = NA, type = "Directed", weight = 1)
    # Append the values to the edges table
    edges <- rbind(edges, EdgeFrame)
  }
}
rm(EdgeFrame)

################################################################################
### ADD EXTRA TO NODES #########################################################
################################################################################


# ADD orgainsitions to the node table that were listed in the edge extraction
# Create a list of all of the nominated nodes
nominated_nodes <- data.frame(Id = unique(edges$from))
nominated_nodes <- cbind(nominated_nodes, 
                         Label = partner_list$Acronym[match(nominated_nodes$Id,partner_list$ID)], 
                         Organisation_name = partner_list$Name[match(nominated_nodes$Id,partner_list$ID)],
                         Type = partner_list$Type[match(nominated_nodes$Id,partner_list$ID)],
                         Replied_or_nominated = "Nominated")

# Remove the nodes that already exist in the nodes table
nominated_nodes <- nominated_nodes[!nominated_nodes$Id %in% nodes$Id,]
# Append remaining nominated nodes to the nodes table
nodes$Id <- as.character(nodes$Id)
nominated_nodes$Id <- as.character(nominated_nodes$Id)
nodes <- bind_rows(nodes, nominated_nodes)

nominated_nodes <- data.frame(Id = unique(edges$to))
nominated_nodes <- cbind(nominated_nodes, 
                         Label = partner_list$Acronym[match(nominated_nodes$Id,partner_list$ID)], 
                         Organisation_name = partner_list$Name[match(nominated_nodes$Id,partner_list$ID)],
                         Type = partner_list$Type[match(nominated_nodes$Id,partner_list$ID)],
                         Replied_or_nominated = "Nominated")

# Remove the nodes that already exist in the nodes table
nominated_nodes <- nominated_nodes[!nominated_nodes$Id %in% nodes$Id,]
# Append remaining nominated nodes to the nodes table
nodes$Id <- as.character(nodes$Id)
nominated_nodes$Id <- as.character(nominated_nodes$Id)
nodes <- bind_rows(nodes, nominated_nodes) 
  
################################################################################
### OUTPUT TO CSV ##############################################################
################################################################################

write.csv(comments, file = paste("../not_shared/output/", country, "/", country, "_comments.csv", sep=""), row.names=FALSE)
write.csv(nodes, file = paste("../not_shared/output/", country, "/", country, "_nodes.csv", sep=""), row.names=FALSE)
write.csv(edges, file = paste("../not_shared/output/", country, "/", country, "_edges.csv", sep=""), row.names=FALSE)

################################################################################
### HOUSEKEEPING ###############################################################
################################################################################

rm(list = ls()) # Remove all the objects we created so far.
