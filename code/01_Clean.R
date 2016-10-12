# DATA CLEANING SCRIPT TO RE-FORMAT EXPORTED KOBO DATA TO A SOCIAL NETWORK ANALYSIS FORMAT

# INPUTS: 
#  - data/kobo_Import.csv
#  - data/partner_list.csv
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

#  IMPORT THE DATA

# Load kobo_import.csv file into R as a data.frame
kobo_import <- read.csv("../not_shared/data/kobo_import.csv", header = TRUE, stringsAsFactors = FALSE)

# Load partner_list_cleaned_jordan.csv file into R as a data.frame
partner_list <- read.csv("../not_shared/data/partner_list_cleaned_jordan.csv", header = TRUE, stringsAsFactors = FALSE)

#  CREATE THE TWO OUTPUT DATA.FRAMES

# Create a data.frame to save the cleaned NODE values to
nodes <- data.frame(matrix(ncol = 33, nrow = nrow(kobo_import)))
column_names_nodes <- c(
              "Id",
              "Label",
              "Organisation_name",
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
              "Format.Data_protection.Onward_sharing_measures",
              "Format.Data_protection.Data_protection_rating",
              "Format.Satisfaction",
              "Format.Own_case_management",
              "Misc.Own_case_management_name",
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
# Remove the column name vector
rm(column_names_edges)

# Create a data.frame to save the COMMENTS to
comments <- data.frame(matrix(ncol = 6, nrow = nrow(kobo_import)))
column_names_comments <- c( 
                "Id",
                "Label",
                "Organisation_name",
                "Participant_name",
                "Email_address",
                "Comment")
colnames(comments) <- column_names_comments
# Remove the column name vector
rm(column_names_comments)

#  COPY THE DATA TO THE NODES DATA.FRAME

# Direct copying from one data.frame to another
nodes$Id <- kobo_import$group_organisational_ties.Org_ties_Source_Organisation

nodes$Filter.Sector.Basic_needs <- kobo_import$group_organisational_ties.Org_ties_Sectors.basic_needs
nodes$Filter.Sector.Education <- kobo_import$group_organisational_ties.Org_ties_Sectors.education
nodes$Filter.Sector.Food_security <- kobo_import$group_organisational_ties.Org_ties_Sectors.food_security
nodes$Filter.Sector.Health <- kobo_import$group_organisational_ties.Org_ties_Sectors.health
nodes$Filter.Sector.Livelihoods <- kobo_import$group_organisational_ties.Org_ties_Sectors.livelihoods
nodes$Filter.Sector.Protection <- kobo_import$group_organisational_ties.Org_ties_Sectors.protection
nodes$Filter.Sector.Shelter <- kobo_import$group_organisational_ties.Org_ties_Sectors.shelter
nodes$Filter.Sector.WASH <- kobo_import$group_organisational_ties.Org_ties_Sectors.wash

nodes$Filter.Location.camp_based <- kobo_import$group_organisational_ties.Org_ties_Camp_non_camp.camp_based
nodes$Filter.Location.non_camp_based <- kobo_import$group_organisational_ties.Org_ties_Camp_non_camp.non_camp_based

nodes$Format.Satisfaction <- kobo_import$group_diagnostics.diagnostics_satisfaction

nodes$Format.Own_case_management <- kobo_import$group_diagnostics.diagnostics_own_case_management
nodes$Misc.Own_case_management_name <- kobo_import$group_diagnostics.diagnostics_own_case_management_name

nodes$Format.Frequency <- kobo_import$group_diagnostics.diagnostics_frequency
nodes$Format.Transfer_method.Printed_material <- kobo_import$group_diagnostics.diagnostics_transfer_method.printed_material
nodes$Format.Transfer_method.Unencrypted_via_email_USB <- kobo_import$group_diagnostics.diagnostics_transfer_method.unencrypted
nodes$Format.Transfer_method.Encrypted_via_email_USB <- kobo_import$group_diagnostics.diagnostics_transfer_method.encrypted
nodes$Format.Transfer_method.FTP <- kobo_import$group_diagnostics.diagnostics_transfer_method.ftp
nodes$Format.Transfer_method.Secure_FTP <- kobo_import$group_diagnostics.diagnostics_transfer_method.secure_ftp
nodes$Format.Transfer_method.Direct_access_through_web_interface <- kobo_import$group_diagnostics.diagnostics_transfer_method.direct_access
nodes$Format.Transfer_method.API <- kobo_import$group_diagnostics.diagnostics_transfer_method.api

nodes$Format.Agree_with_statement.Happy_with_format <- kobo_import$group_diagnostics.diagnostics_statements_header.diagnostics_statements_format
nodes$Format.Agree_with_statement.Happy_with_turnaround <- kobo_import$group_diagnostics.diagnostics_statements_header.diagnostics_statements_timely
nodes$Format.Agree_with_statement.Happy_with_workflow <- kobo_import$group_diagnostics.diagnostics_statements_header.diagnostics_statements_workflow
nodes$Format.Agree_with_statement.Happy_with_accuracy <- kobo_import$group_diagnostics.diagnostics_statements_header.diagnostics_statements_accurate

# Retrive the correct ORGANISATION ACRONYMS and NAMES from the partner_list table
nodes$Label <- partner_list$Country_acronym[match(nodes$Id,partner_list$ID)]
nodes$Organisation_name <- partner_list$Country_name[match(nodes$Id,partner_list$ID)]

#  CALCULATE THE DATA PROTECTION INDICATORS

# Calculate ORGANISATIONAL MEASURES values
#Change "n/a" to NA
kobo_import$group_data_protection.group_organisational_measures.DP_OM_Have_policy_yes_match_UNHCR[kobo_import$group_data_protection.group_organisational_measures.DP_OM_Have_policy_yes_match_UNHCR == "n/a"] <- NA
#Change "n/a" to 0
kobo_import$group_data_protection.group_organisational_measures.DP_OM_Have_policy_yes_match_UNHCR[is.na(kobo_import$group_data_protection.group_organisational_measures.DP_OM_Have_policy_yes_match_UNHCR)] <- 0
#Calculate rating
nodes$Format.Data_protection.Organisational_measures <- (
  as.numeric(kobo_import$group_data_protection.group_organisational_measures.DP_OM_Have_policy) * 
    as.numeric(kobo_import$group_data_protection.group_organisational_measures.DP_OM_Have_policy_yes_match_UNHCR)
) / 3

# Calculate PHYSICAL MEASURES values
nodes$Format.Data_protection.Physical_measures <- ((
  kobo_import$group_data_protection.group_physical_measures.regarding_physical_measures.DP_PM_stored_safe + 
    kobo_import$group_data_protection.group_physical_measures.regarding_physical_measures.DP_PM_offices_locked + 
    kobo_import$group_data_protection.group_physical_measures.regarding_physical_measures.DP_PM_staff_reminder + 
    kobo_import$group_data_protection.group_physical_measures.regarding_physical_measures.DP_PM_safe_paper_waste + 
    kobo_import$group_data_protection.group_physical_measures.regarding_physical_measures.DP_PM_visitors + 
    kobo_import$group_data_protection.group_physical_measures.regarding_physical_measures.DP_PM_USB + 
    kobo_import$group_data_protection.group_physical_measures.regarding_physical_measures.DP_PM_filing_cabinets) * 
    kobo_import$group_data_protection.group_physical_measures.regarding_physical_measures.DP_PM_staff_rating
) / 28

# Calculate TECHNICAL MEASURES values
nodes$Format.Data_protection.Technical_measures <- ((
  kobo_import$group_data_protection.group_technical_measures.regarding_technical_measures_header.DP_TM_passwords +
    kobo_import$group_data_protection.group_technical_measures.regarding_technical_measures_header.DP_TM_tiered_access +
    kobo_import$group_data_protection.group_technical_measures.regarding_technical_measures_header.DP_TM_backups +
    kobo_import$group_data_protection.group_technical_measures.regarding_technical_measures_header.DP_TM_personal_account_awareness) *
    kobo_import$group_data_protection.group_technical_measures.regarding_technical_measures_header.DP_TM_staff_rating
) / 16

# Calculate ONWARD DATA SHARING MEASURES values
nodes$Format.Data_protection.Onward_sharing_measures <- 
  ifelse (kobo_import$group_data_protection.group_onward_data_sharing.DP_ODS_agreements_third_parties == "N/A", "",
             ifelse (kobo_import$group_data_protection.group_onward_data_sharing.DP_ODS_agreements_third_parties == 0, 0,
                     (as.numeric(kobo_import$group_data_protection.group_onward_data_sharing.DP_ODS_agreements_third_parties) * 
                      as.numeric(kobo_import$group_data_protection.group_onward_data_sharing.DP_ODS_third_party_screening) * 
                      as.numeric(kobo_import$group_data_protection.group_onward_data_sharing.DP_ODS_partner_rating)) / 4 
              )
          )
nodes$Format.Data_protection.Onward_sharing_measures <- as.numeric(nodes$Format.Data_protection.Onward_sharing_measures)

# Calculate final DATA PROTECTION RATING value
nodes$Format.Data_protection.Data_protection_rating <- rowMeans(nodes[,14:17], na.rm=TRUE)

#  COPY THE COMMENTS TO THE COMMENTS DATA.FRAME

comments$Id <- kobo_import$group_organisational_ties.Org_ties_Source_Organisation
comments$Label <- partner_list$Country_acronym[match(comments$Id,partner_list$ID)]
comments$Organisation_name <- partner_list$Country_name[match(comments$Id,partner_list$ID)]
comments$Participant_name <- kobo_import$group_participation_agreement.participation_name
comments$Email_address <- kobo_import$group_participation_agreement.participation_email
comments$Comment <- kobo_import$group_comments.comments

#  STRIP OUT ORGANISATIONS INTO THE EDGES DATA.FRAME

#  SAVE THE DATA.FRAMES AS FILES FOR ANALYSIS

write.csv(nodes, file = "../not_shared/output/nodes.csv")
# write.csv(edges, file = "../not_shared/output/edges.csv")
write.csv(comments, file = "../not_shared/output/comments.csv")

#  DROP USED DATA FRAMES