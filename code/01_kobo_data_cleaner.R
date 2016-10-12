# DATA CLEANING SCRIPT TO RE-FORMAT EXPORTED KOBO DATA TO A SOCIAL NETWORK ANALYSIS FORMAT

# Inputs: kobo_Import.csv, partner_list.csv
# Outputs: jordan_nodes.csv, jordan_edges.csv

# Before starting
# 1. Download the data from Kobo as .CSV
# 2. Rename it "kobo_import.csv"
# 3. Save the file in the correct Working directory

# Set the working directory to the location you saved the kobo data
# setwd("Documents/Projects/2016 06 - UNHCR Periklis/Surveys/Jordan/03_Kobo_data_cleaner/")

# Load kobo_import.csv file into R as a data.frame
kobo_import <- read.csv("data/kobo_import.csv", header = TRUE, stringsAsFactors = FALSE)

# Create a data.frame to save the cleaned NODE values to
kobo_cleaned_nodes <- data.frame(matrix(ncol = 36, nrow = nrow(kobo_import)))
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
                      "Format.Transfer_method.Other",
                      "Format.Agree_with_statement.Happy_with_format",
                      "Format.Agree_with_statement.Happy_with_turnaround",
                      "Format.Agree_with_statement.Happy_with_workflow",
                      "Format.Agree_with_statement.Happy_with_accuracy",
                      "Misc.Participant_name",
                      "Misc.Participant_email"
)
colnames(kobo_cleaned_nodes) <- column_names_nodes

# Load partner_list_cleaned_jordan.csv file into R as a data.frame
partner_list <-
  read.csv(
    "data/partner_list_cleaned_jordan.csv", header = TRUE, stringsAsFactors = FALSE
  )

# Copy the organisation IDs of the participant's organisation
# !!!! NEED TO ADD ERROR HANDLING
# !!!! IF IT'S MISSING THE USER NEEDS TO ADD AN ORGANISATION TO THE PARTNER LIST AND REFRESH
kobo_cleaned_nodes$Id <- kobo_import$group_organisational_ties.Org_ties_Source_Organisation

# Retrive the correct ORGANISATION ACRONYMS and NAMES from the partner_list table
# !!!! NEED TO ADD ERROR HANDLING
# !!!! IF IT'S MISSING THE USER NEEDS TO ADD AN ORGANISATION TO THE PARTNER LIST AND REFRESH
kobo_cleaned_nodes$Label <- partner_list$Country_acronym[match(kobo_cleaned_nodes$Id,partner_list$ID)]
kobo_cleaned_nodes$Organisation_name <- partner_list$Country_name[match(kobo_cleaned_nodes$Id,partner_list$ID)]

# Copy the SECTORS the organisation operates in
# !!!! NEED TO ADD ERROR HANDLING
# !!!! IF THE USER ADDS OTHER THIS FIELD SHOULD BE CLEANED AND UPADTED
kobo_cleaned_nodes$Filter.Sector.Basic_needs <- kobo_import$group_organisational_ties.Org_ties_Sectors.basic_needs
kobo_cleaned_nodes$Filter.Sector.Education <- kobo_import$group_organisational_ties.Org_ties_Sectors.education
kobo_cleaned_nodes$Filter.Sector.Food_security <- kobo_import$group_organisational_ties.Org_ties_Sectors.food_security
kobo_cleaned_nodes$Filter.Sector.Health <- kobo_import$group_organisational_ties.Org_ties_Sectors.health
kobo_cleaned_nodes$Filter.Sector.Livelihoods <- kobo_import$group_organisational_ties.Org_ties_Sectors.livelihoods
kobo_cleaned_nodes$Filter.Sector.Protection <- kobo_import$group_organisational_ties.Org_ties_Sectors.protection
kobo_cleaned_nodes$Filter.Sector.Shelter <- kobo_import$group_organisational_ties.Org_ties_Sectors.shelter
kobo_cleaned_nodes$Filter.Sector.WASH <- kobo_import$group_organisational_ties.Org_ties_Sectors.wash

# Copy the OPERATION LOCATION the organisation operates in
kobo_cleaned_nodes$Filter.Location.camp_based <- kobo_import$group_organisational_ties.Org_ties_Camp_non_camp.camp_based
kobo_cleaned_nodes$Filter.Location.non_camp_based <- kobo_import$group_organisational_ties.Org_ties_Camp_non_camp.non_camp_based

# Copy the DIAGNOSTIC INDICATORS
kobo_cleaned_nodes$Format.Satisfaction <- kobo_import$group_diagnostics.diagnostics_satisfaction
kobo_cleaned_nodes$Format.Own_case_management <- kobo_import$group_diagnostics.diagnostics_own_case_management
kobo_cleaned_nodes$Misc.Own_case_management_name <- kobo_import$group_diagnostics.diagnostics_own_case_management_name
kobo_cleaned_nodes$Format.Frequency <- kobo_import$group_diagnostics.diagnostics_frequency
kobo_cleaned_nodes$Format.Transfer_method.Printed_material <- kobo_import$group_diagnostics.diagnostics_transfer_method.printed_material
kobo_cleaned_nodes$Format.Transfer_method.Unencrypted_via_email_USB <- kobo_import$group_diagnostics.diagnostics_transfer_method.unencrypted
kobo_cleaned_nodes$Format.Transfer_method.Encrypted_via_email_USB <- kobo_import$group_diagnostics.diagnostics_transfer_method.encrypted
kobo_cleaned_nodes$Format.Transfer_method.FTP <- kobo_import$group_diagnostics.diagnostics_transfer_method.ftp
kobo_cleaned_nodes$Format.Transfer_method.Secure_FTP <- kobo_import$group_diagnostics.diagnostics_transfer_method.secure_ftp
kobo_cleaned_nodes$Format.Transfer_method.Direct_access_through_web_interface <- kobo_import$group_diagnostics.diagnostics_transfer_method.direct_access
kobo_cleaned_nodes$Format.Transfer_method.API <- kobo_import$group_diagnostics.diagnostics_transfer_method.api
# !! NEED TO HANDLE IF OTHER SELECTED
kobo_cleaned_nodes$Format.Agree_with_statement.Happy_with_format <- kobo_import$group_diagnostics.diagnostics_statements_header.diagnostics_statements_format
kobo_cleaned_nodes$Format.Agree_with_statement.Happy_with_turnaround <- kobo_import$group_diagnostics.diagnostics_statements_header.diagnostics_statements_timely
kobo_cleaned_nodes$Format.Agree_with_statement.Happy_with_workflow <- kobo_import$group_diagnostics.diagnostics_statements_header.diagnostics_statements_workflow
kobo_cleaned_nodes$Format.Agree_with_statement.Happy_with_accuracy <- kobo_import$group_diagnostics.diagnostics_statements_header.diagnostics_statements_accurate
kobo_cleaned_nodes$Misc.Participant_name <- kobo_import$group_participation_agreement.participation_name
kobo_cleaned_nodes$Misc.Participant_email <- kobo_import$group_participation_agreement.participation_email

# Calculate ORGANISATIONAL MEASURES values
kobo_cleaned_nodes$Format.Data_protection.Organisational_measures <- (
  kobo_import$group_data_protection.group_organisational_measures.DP_OM_Have_policy * 
  kobo_import$group_data_protection.group_organisational_measures.DP_OM_Have_policy_yes_match_UNHCR
  ) / 3

# Calculate PHYSICAL MEASURES values
kobo_cleaned_nodes$Format.Data_protection.Physical_measures <- ((
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
kobo_cleaned_nodes$Format.Data_protection.Technical_measures <- ((
  kobo_import$group_data_protection.group_technical_measures.regarding_technical_measures_header.DP_TM_passwords +
  kobo_import$group_data_protection.group_technical_measures.regarding_technical_measures_header.DP_TM_tiered_access +
  kobo_import$group_data_protection.group_technical_measures.regarding_technical_measures_header.DP_TM_backups +
  kobo_import$group_data_protection.group_technical_measures.regarding_technical_measures_header.DP_TM_personal_account_awareness) *
  kobo_import$group_data_protection.group_technical_measures.regarding_technical_measures_header.DP_TM_staff_rating
  ) / 16

# Calculate ONWARD DATA SHARING MEASURES values
# !! 
# kobo_cleaned_nodes$Format.Data_protection.Onward_sharing_measures <- ((
#  kobo_import$group_data_protection.group_onward_data_sharing.DP_ODS_agreements_third_parties
#  kobo_import$group_data_protection.group_onward_data_sharing.DP_ODS_third_party_screening
#  kobo_import$group_data_protection.group_onward_data_sharing.DP_ODS_partner_rating  
# ))

# Calculate final DATA PROTECTION RATING value
# If there is no value in the onward data sharing then dont include in mean
# kobo_cleaned_nodes$Format.Data_protection.Data_protection_rating <- average

write.csv(kobo_cleaned_nodes, file = "output/Jordan_nodes.csv")

# Create a data.frame to save the cleaned EDGE values to
kobo_cleaned_edges <- data.frame(matrix(ncol = 5))
column_names_edges <- c("Source", 
                  "Target",
                  "Label",
                  "Type",
                  "Weight")
colnames(kobo_cleaned_edges) <- column_names_edges
