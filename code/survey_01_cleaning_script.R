# DATA CLEANING SCRIPT TO RE-FORMAT EXPORTED KOBO DATA TO A SOCIAL NETWORK ANALYSIS FORMAT

# INPUTS: 
#  - data/kobo_import_v02.csv
#  - data/Partner_List_Jordan_2016_10_16.csv
# PROCESSES:
#  - Import the data
#  - Create the two output data.frames
#  - Copy the data to the nodes data.frame
#  - Calculate data protection indicators
#  - Strip out target organisations into the edges data.frame
#  - Save the data.frames as files for analysis
#  - Drop old data frames
# OUTPUTS: 
#  - output/jordan_nodes.csv
#  - output/jordan_edges.csv

# BEFORE STARTING
# 1. Download the data from Kobo as .CSV
# 2. Rename it "kobo_import.csv"
# 3. Save the file in the "data" folder
# 4. Check if any addtional organisations were listed as responses to the following questions
#    -  group_organisational_ties/Org_ties_Source_Organisation_other
#    -  group_organisational_ties/Org_ties_Target_Organisations_DSA/other
#    -  group_organisational_ties/Org_ties_Target_Organisations_DSA_other
#    -  group_organisational_ties/Org_ties_Target_Organisations_Transfer/other
#    -  group_organisational_ties/Org_ties_Target_Organisations_Transfer_other
# 5. Update the "partner_list.csv" file with any new organisations

#install.packages("dplyr")
library(dplyr)

#  IMPORT THE DATA

# Load kobo_import.csv file into R as a data.frame
kobo_import <- read.csv("../not_shared/data/kobo_import_v02.csv", header = TRUE, stringsAsFactors = FALSE)
kobo_import[kobo_import == "n/a"] <- ""
kobo_import[kobo_import == "N/A"] <- ""

# Load partner_list_cleaned_jordan.csv file into R as a data.frame
partner_list <- read.csv("../not_shared/data/Partner_List_Jordan_2016_10_16.csv", header = TRUE, stringsAsFactors = FALSE)

#  CREATE THE OUTPUT DATA.FRAMES

# Create a data.frame to save the cleaned NODE values to
nodes <- data.frame(matrix(ncol = 34, nrow = nrow(kobo_import)))
column_names_nodes <- c(
              "Id",
              "Label",
              "Organisation_name",
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
              "Format.Satisfaction",
              "Format.Own_case_management",
              "Format.Frequency",
              "Format.Transfer_method.Printed_material",
              "Format.Transfer_method.Unencrypted_via_email_USB",
              "Format.Transfer_method.Encrypted_via_email_USB",
              "Format.Transfer_method.FTP",
              "Format.Transfer_method.Secure_FTP",
              "Format.Transfer_method.Direct_access_through_web_interface",
              "Format.Transfer_method.API",
              "Format.Agree_with_statement.Happy_with_format",
              "Format.Agree_with_statement.Happy_with_turnaround",
              "Format.Agree_with_statement.Happy_with_workflow",
              "Format.Agree_with_statement.Happy_with_accuracy"
)
colnames(nodes) <- column_names_nodes
# Remove the column name vector
rm(column_names_nodes)

# Create a data.frame to save the cleaned EDGE values to
edges <- data.frame(matrix(ncol = 5))
column_names_edges <- c(
              "Source", 
              "Target",
              "Label",
              "Type",
              "Weight"
)
colnames(edges) <- column_names_edges
edges <- edges[-c(1), ]
# Remove the column name vector
rm(column_names_edges)

# Create a data.frame to save the COMMENTS to
comments <- data.frame(matrix(ncol = 8, nrow = nrow(kobo_import)))
column_names_comments <- c( 
                "Id",
                "Label",
                "Organisation_name",
                "Participant_name",
                "Email_address",
                "summary_request",
                "Comment",
                "Own_case_management")
colnames(comments) <- column_names_comments
# Remove the column name vector
rm(column_names_comments)

#  COPY THE DATA TO THE NODES DATA.FRAME

# Direct copying from one data.frame to another
nodes$Id <- kobo_import$GROUP_organizational_ties.Organizational_connections_Source_Organisation

nodes$Label <- partner_list$Acronym[match(nodes$Id,partner_list$ID)]
nodes$Organisation_name <- partner_list$Name[match(nodes$Id,partner_list$ID)]

nodes$Replied_or_nominated <- rep(c("Replied"), nrow(kobo_import))

nodes$Filter.Sector.Basic_needs <- kobo_import$GROUP_organizational_ties.Organizational_connections_Sectors.basic_needs
nodes$Filter.Sector.Education <- kobo_import$GROUP_organizational_ties.Organizational_connections_Sectors.education
nodes$Filter.Sector.Food_security <- kobo_import$GROUP_organizational_ties.Organizational_connections_Sectors.food_security
nodes$Filter.Sector.Health <- kobo_import$GROUP_organizational_ties.Organizational_connections_Sectors.health
nodes$Filter.Sector.Livelihoods <- kobo_import$GROUP_organizational_ties.Organizational_connections_Sectors.livelihoods
nodes$Filter.Sector.Protection <- kobo_import$GROUP_organizational_ties.Organizational_connections_Sectors.protection
nodes$Filter.Sector.Shelter <- kobo_import$GROUP_organizational_ties.Organizational_connections_Sectors.shelter
nodes$Filter.Sector.WASH <- kobo_import$GROUP_organizational_ties.Organizational_connections_Sectors.wash

nodes$Filter.Location.camp_based <- kobo_import$GROUP_organizational_ties.Organizational_connections_Location.camp_based
nodes$Filter.Location.non_camp_based <- kobo_import$GROUP_organizational_ties.Organizational_connections_Location.non_camp_based

nodes$Format.Satisfaction <- kobo_import$GROUP_diagnostics.diagnostics_satisfaction

nodes$Format.Own_case_management <- kobo_import$GROUP_diagnostics.diagnostics_own_case_management

nodes$Format.Frequency <- kobo_import$GROUP_diagnostics.diagnostics_frequency

nodes$Format.Transfer_method.Printed_material <- kobo_import$GROUP_diagnostics.diagnostics_transfer_method.printed_material
nodes$Format.Transfer_method.Unencrypted_via_email_USB <- kobo_import$GROUP_diagnostics.diagnostics_transfer_method.unencrypted
nodes$Format.Transfer_method.Encrypted_via_email_USB <- kobo_import$GROUP_diagnostics.diagnostics_transfer_method.encrypted
nodes$Format.Transfer_method.FTP <- kobo_import$GROUP_diagnostics.diagnostics_transfer_method.ftp
nodes$Format.Transfer_method.Secure_FTP <- kobo_import$GROUP_diagnostics.diagnostics_transfer_method.secure_ftp
nodes$Format.Transfer_method.Direct_access_through_web_interface <- kobo_import$GROUP_diagnostics.diagnostics_transfer_method.direct_access
nodes$Format.Transfer_method.API <- kobo_import$GROUP_diagnostics.diagnostics_transfer_method.api

nodes$Format.Agree_with_statement.Happy_with_format <- kobo_import$GROUP_diagnostics.SUBGROUP_diagnostics_statements.diagnostics_statements_format
nodes$Format.Agree_with_statement.Happy_with_turnaround <- kobo_import$GROUP_diagnostics.SUBGROUP_diagnostics_statements.diagnostics_statements_timely
nodes$Format.Agree_with_statement.Happy_with_workflow <- kobo_import$GROUP_diagnostics.SUBGROUP_diagnostics_statements.diagnostics_statements_workflow
nodes$Format.Agree_with_statement.Happy_with_accuracy <- kobo_import$GROUP_diagnostics.SUBGROUP_diagnostics_statements.diagnostics_statements_accurate

#  CALCULATE THE DATA PROTECTION INDICATORS

# Calculate ORGANISATIONAL MEASURES values
nodes$Format.Data_protection.Organisational_measures <- ((
    kobo_import$GROUP_data_protection.SUBGROUP_organizational_measures.DP_OM_focal_point +
    kobo_import$GROUP_data_protection.SUBGROUP_organizational_measures.DP_OM_have_policy +
    kobo_import$GROUP_data_protection.SUBGROUP_organizational_measures.DP_OM_have_code_of_conduct +
    kobo_import$GROUP_data_protection.SUBGROUP_organizational_measures.DP_OM_data_breach_process) *
    kobo_import$GROUP_data_protection.SUBGROUP_organizational_measures.DP_OM_staff_rating
) / 16

# Calculate PHYSICAL MEASURES values
nodes$Format.Data_protection.Physical_measures <- ((
  kobo_import$GROUP_data_protection.SUBGROUP_physical_measures.DP_PM_stored_safe + 
    kobo_import$GROUP_data_protection.SUBGROUP_physical_measures.DP_PM_offices_locked + 
    kobo_import$GROUP_data_protection.SUBGROUP_physical_measures.DP_PM_staff_reminder + 
    kobo_import$GROUP_data_protection.SUBGROUP_physical_measures.DP_PM_safe_paper_waste + 
    kobo_import$GROUP_data_protection.SUBGROUP_physical_measures.DP_PM_visitors + 
    kobo_import$GROUP_data_protection.SUBGROUP_physical_measures.DP_PM_USB + 
    kobo_import$GROUP_data_protection.SUBGROUP_physical_measures.DP_PM_filing_cabinets) * 
    kobo_import$GROUP_data_protection.SUBGROUP_physical_measures.DP_PM_staff_rating
) / 28

# Calculate TECHNICAL MEASURES values
nodes$Format.Data_protection.Technical_measures <- ((
  kobo_import$GROUP_data_protection.SUBGROUP_technical_measures.DP_TM_passwords +
    kobo_import$GROUP_data_protection.SUBGROUP_technical_measures.DP_TM_tiered_access +
    kobo_import$GROUP_data_protection.SUBGROUP_technical_measures.DP_TM_backups +
    kobo_import$GROUP_data_protection.SUBGROUP_technical_measures.DP_TM_personal_account_awareness) *
    kobo_import$GROUP_data_protection.SUBGROUP_technical_measures.DP_TM_staff_rating
) / 16

# Calculate RIGHTS OF DATA SUBJECT MEASURES values

# Perform calculation
nodes$Format.Data_protection.Rights_of_data_subject_measures <- 
    ifelse (kobo_import$GROUP_data_protection.SUBGROUP_rights_of_data_subjects.DP_RDS_data_collection_YN == 0, "",
            ((as.numeric(kobo_import$GROUP_data_protection.SUBGROUP_rights_of_data_subjects.DP_RDS_informed) +
            as.numeric(kobo_import$GROUP_data_protection.SUBGROUP_rights_of_data_subjects.DP_RDS_consent) +
            as.numeric(kobo_import$GROUP_data_protection.SUBGROUP_rights_of_data_subjects.DP_RDS_opportunity_to_object) +
            as.numeric(kobo_import$GROUP_data_protection.SUBGROUP_rights_of_data_subjects.DP_RDS_aware_of_rights)) *
            as.numeric(kobo_import$GROUP_data_protection.SUBGROUP_rights_of_data_subjects.DP_RDS_staff_rating)
            ) /16
    )

# Calculate ONWARD DATA SHARING MEASURES values
# kobo_import$GROUP_data_protection.SUBGROUP_onward_data_sharing.DP_ODS_YN <- as.numeric(kobo_import$GROUP_data_protection.SUBGROUP_onward_data_sharing.DP_ODS_YN)
# kobo_import$GROUP_data_protection.SUBGROUP_onward_data_sharing.DP_ODS_outside_of_country <- as.numeric(kobo_import$GROUP_data_protection.SUBGROUP_onward_data_sharing.DP_ODS_outside_of_country)
# kobo_import$GROUP_data_protection.SUBGROUP_onward_data_sharing.DP_ODS_national_legislation_YN <- as.numeric(kobo_import$GROUP_data_protection.SUBGROUP_onward_data_sharing.DP_ODS_national_legislation_YN)
# kobo_import$GROUP_data_protection.SUBGROUP_onward_data_sharing.DP_ODS_national_legislation_risk_mitigation <- as.numeric(kobo_import$GROUP_data_protection.SUBGROUP_onward_data_sharing.DP_ODS_national_legislation_risk_mitigation)
# kobo_import$GROUP_data_protection.SUBGROUP_onward_data_sharing.DP_ODS_law_enforcement <- as.numeric(kobo_import$GROUP_data_protection.SUBGROUP_onward_data_sharing.DP_ODS_law_enforcement)
# kobo_import$GROUP_data_protection.SUBGROUP_onward_data_sharing.DP_ODS_third_party_YN <- as.numeric(kobo_import$GROUP_data_protection.SUBGROUP_onward_data_sharing.DP_ODS_third_party_YN)
# kobo_import$GROUP_data_protection.SUBGROUP_onward_data_sharing.DP_ODS_third_party_provision_for_deletion <- as.numeric(kobo_import$GROUP_data_protection.SUBGROUP_onward_data_sharing.DP_ODS_third_party_provision_for_deletion)
# kobo_import$GROUP_data_protection.SUBGROUP_onward_data_sharing.DP_ODS_third_party_archived_securely <- as.numeric(kobo_import$GROUP_data_protection.SUBGROUP_onward_data_sharing.DP_ODS_third_party_archived_securely)
# kobo_import$GROUP_data_protection.SUBGROUP_onward_data_sharing.DP_ODS_staff_rating <- as.numeric(kobo_import$GROUP_data_protection.SUBGROUP_onward_data_sharing.DP_ODS_staff_rating)
# 
# nodes$Format.Data_protection.Onward_sharing_measures <-
#   ifelse(kobo_import$GROUP_data_protection.SUBGROUP_onward_data_sharing.DP_ODS_YN == "", "", 
#          ifelse(kobo_import$GROUP_data_protection.SUBGROUP_onward_data_sharing.DP_ODS_YN == 0, 0, {
#                 temp <- data.frame(c(kobo_import$GROUP_data_protection.SUBGROUP_onward_data_sharing.DP_ODS_outside_of_country), 
#                                    c(kobo_import$GROUP_data_protection.SUBGROUP_onward_data_sharing.DP_ODS_national_legislation_risk_mitigation),
#                                    c(kobo_import$GROUP_data_protection.SUBGROUP_onward_data_sharing.DP_ODS_law_enforcement),
#                                    c(kobo_import$GROUP_data_protection.SUBGROUP_onward_data_sharing.DP_ODS_third_party_provision_for_deletion),
#                                    c(kobo_import$GROUP_data_protection.SUBGROUP_onward_data_sharing.DP_ODS_third_party_archived_securely))
#                 rowSums(temp, na.rm = TRUE) *
#                   kobo_import$GROUP_data_protection.SUBGROUP_onward_data_sharing.DP_ODS_staff_rating
#          }
#          )
#  )

#### ((5 - sum(is.na(temp[1,])))*4)


# Calculate final DATA PROTECTION RATING value
# nodes$Format.Data_protection.Data_protection_rating <- rowMeans(nodes[,15:19], na.rm=TRUE)

#  COPY THE COMMENTS TO THE COMMENTS DATA.FRAME

comments$Id <- kobo_import$GROUP_organizational_ties.Organizational_connections_Source_Organisation
comments$Label <- partner_list$Acronym[match(nodes$Id,partner_list$ID)]
comments$Organisation_name <- partner_list$Name[match(nodes$Id,partner_list$ID)]
comments$Participant_name <- kobo_import$GROUP_participation_agreement.participation_name
comments$Email_address <- kobo_import$GROUP_participation_agreement.participation_email
comments$summary_request <- kobo_import$GROUP_data_protection_intro.Summary_request_YN
comments$Comment <- kobo_import$GROUP_comments.comments
comments$Own_case_management <- kobo_import$GROUP_diagnostics.diagnostics_own_case_management_name

# IDENTIFY DOWNSTREAM ORGANISATION CONNECTIONS

# Repeat for each row in kobo_import
for (i in 1:nrow(kobo_import)){

    # If the value of the cell is TRUE
  TargetOrgs <- c(ifelse (kobo_import[i,14:98] == "True",
             # Extract the Organisation ID from the header
             as.numeric(substr(c(colnames(kobo_import[14:98])), 79, 81)),
             # Or write NA
             NA))
  # Remove the NA
  TargetOrgs <- TargetOrgs[!is.na(TargetOrgs)]
  # If thelength of TargetOrgs is NOT 0 then Transpose the Organisatonis to the Edges table with the Source ID as the current Row's Organisation
  if(length(TargetOrgs) != 0) { 
            # Create a vector with the source organisation
            SourceOrgs <- rep(kobo_import[i,1], length(TargetOrgs))
            # Create a vector with target organisations and some other fixed variables
            EdgeFrame <-data.frame(Source = SourceOrgs, Target = TargetOrgs, Label = NA, Type = "Directed", Weight = 1)
            # Append the values to the edges table
            edges <- rbind(edges, EdgeFrame)
  }
}
rm(EdgeFrame)

# IDENTIFY UPSTREAM ORGANISATION CONNECTIONS

# Repeat for each row in kobo_import
for (i in 1:nrow(kobo_import)){
  
  # If the value of the cell is TRUE
  SourceOrgs <- c(ifelse (kobo_import[i,102:186] == "True",
                          # Extract the Organisation ID from the header
                          as.numeric(substr(c(colnames(kobo_import[102:186])), 77, 79)),
                          # Or write NA
                          NA))
  # Remove the NA
  SourceOrgs <- SourceOrgs[!is.na(SourceOrgs)]
  # If thelength of TargetOrgs is NOT 0 then Transpose the Organisatonis to the Edges table with the Source ID as the current Row's Organisation
  if(length(SourceOrgs) != 0) { 
    # Create a vector with the source organisation
    TargetOrgs <- rep(kobo_import[i,1], length(SourceOrgs))
    # Create a vector with target organisations and some other fixed variables
    EdgeFrame <-data.frame(Source = SourceOrgs, Target = TargetOrgs, Label = NA, Type = "Directed", Weight = 1)
    # Append the values to the edges table
    edges <- rbind(edges, EdgeFrame)
  }
}
rm(EdgeFrame)

# ADD orgainsitions to the node table that were listed in the edge extraction
# Create a list of all of the nominated nodes
nominated_nodes <- data.frame(Id = unique(c(edges[,1],edges[,2])))
nominated_nodes <- cbind(nominated_nodes, Label = partner_list$Acronym[match(nominated_nodes$Id,partner_list$ID)], 
                              Organisation_name = partner_list$Name[match(nominated_nodes$Id,partner_list$ID)], 
                              Replied_or_nominated = "Nominated")

# Remove the nodes that already exist in the nodes table
nominated_nodes <- nominated_nodes[!nominated_nodes$Id %in% nodes$Id,]
# Append remaining nominated nodes to the nodes table
nodes <- bind_rows(nodes, nominated_nodes)

#  SAVE THE DATA.FRAMES AS FILES FOR ANALYSIS
write.csv(nodes, file = "../not_shared/output/nodes.csv")
write.csv(edges, file = "../not_shared/output/edges.csv")
write.csv(comments, file = "../not_shared/output/comments.csv")

#  DROP USED DATA FRAMES
# rm(i, SourceOrgs, TargetOrgs)
# rm(comments, edges, kobo_import, nodes, partner_list, nominated_nodes)
