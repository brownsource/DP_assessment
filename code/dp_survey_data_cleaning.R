#### CREATES THE REQUIRED NODE AND EDGE TABLES FOR NETWORK ANALYSIS
#### YOU NEED TO SPECIFY THE COUNTRY AT THE START OF THE SCRIPT

### INPUT: 
## not_shared/data/[COUNTRY]/kobo_import_DPA_[COUNTRY].csv
## not_shared/data/jordan_partner+list.csv

### OUTPUT:
## not_shared/output/[COUNTRY]/[COUNTRY]_nodes.csv
## not_shared/output/[COUNTRY]/[COUNTRY]_edges.csv
## not_shared/output/[COUNTRY]/[COUNTRY]_comments.csv

### CLEAR WORKSPACE
rm(list = ls()) # Remove all the objects we created so far.

### INSTALL PACKAGES IF REQUIRED
if(!require(igraph)){
  install.packages("igraph")
}
if(!require(dplyr)){
  install.packages("dplyr")
}

### INSTALL LIBRARIES IF REQUIRED
### NEED TO CHANGE THIS BLOCK OF CODE
library(igraph)
library(plyr)

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

### CREATE AND EXPORT THE NODES DATA
nodes <- data.frame(matrix(ncol = 34, nrow = nrow(kobo_survey_import)))
nodes_labels <- c(
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
colnames(nodes) <- nodes_labels
rm(nodes_labels)
# import data (matching some with the partner_list table)
nodes$Id <- kobo_survey_import$GROUP_organization_details.organizational_connections_source_organisation

nodes$Label <- partner_list$Acronym[match(nodes$Id,partner_list$ID)]
nodes$Organisation_name <- partner_list$Name[match(nodes$Id,partner_list$ID)]

nodes$Replied_or_nominated <- rep(c("Replied"), nrow(kobo_survey_import))

nodes$Filter.Sector.Basic_needs <- kobo_survey_import$GROUP_organization_details.organizational_connections_sectors.basic_needs
nodes$Filter.Sector.Education <- kobo_survey_import$GROUP_organization_details.organizational_connections_sectors.education
nodes$Filter.Sector.Food_security <- kobo_survey_import$GROUP_organizational_ties.Organizational_connections_Sectors.food_security
nodes$Filter.Sector.Health <- kobo_survey_import$GROUP_organization_details.organizational_connections_sectors.health
nodes$Filter.Sector.Livelihoods <- kobo_survey_import$GROUP_organization_details.organizational_connections_sectors.livelihoods
nodes$Filter.Sector.Protection <- kobo_survey_import$GROUP_organization_details.organizational_connections_sectors.protection
nodes$Filter.Sector.Shelter <- kobo_survey_import$GROUP_organization_details.organizational_connections_sectors.shelter
nodes$Filter.Sector.WASH <- kobo_survey_import$GROUP_organization_details.organizational_connections_sectors.wash

nodes$Filter.Location.camp_based <- kobo_survey_import$GROUP_organization_details.organizational_connections_location.camp_based
nodes$Filter.Location.non_camp_based <- kobo_survey_import$GROUP_organization_details.organizational_connections_location.non_camp_based

nodes$Format.Satisfaction <- kobo_survey_import$GROUP_diagnostics.diagnostics_satisfaction
nodes$Format.Own_case_management <- kobo_survey_import$GROUP_organization_details.diagnostics_own_case_management
nodes$Format.Frequency <- kobo_survey_import$GROUP_diagnostics.diagnostics_frequency

nodes$Format.Transfer_method.Printed_material <- kobo_survey_import$GROUP_diagnostics.diagnostics_transfer_method.printed_material
nodes$Format.Transfer_method.Unencrypted_via_email_USB <- kobo_survey_import$GROUP_diagnostics.diagnostics_transfer_method.unencrypted
nodes$Format.Transfer_method.Encrypted_via_email_USB <- kobo_survey_import$GROUP_diagnostics.diagnostics_transfer_method.encrypted
nodes$Format.Transfer_method.FTP <- kobo_survey_import$GROUP_diagnostics.diagnostics_transfer_method.ftp
nodes$Format.Transfer_method.Secure_FTP <- kobo_survey_import$GROUP_diagnostics.diagnostics_transfer_method.secure_ftp
nodes$Format.Transfer_method.Direct_access_through_web_interface <- kobo_survey_import$GROUP_diagnostics.diagnostics_transfer_method.direct_access
nodes$Format.Transfer_method.API <- kobo_survey_import$GROUP_diagnostics.diagnostics_transfer_method.api
nodes$Format.Agree_with_statement.Happy_with_format <- kobo_survey_import$GROUP_diagnostics.SUBGROUP_diagnostics_statements.diagnostics_statements_format
nodes$Format.Agree_with_statement.Happy_with_turnaround <- kobo_survey_import$GROUP_diagnostics.SUBGROUP_diagnostics_statements.diagnostics_statements_timely
nodes$Format.Agree_with_statement.Happy_with_workflow <- kobo_survey_import$GROUP_diagnostics.SUBGROUP_diagnostics_statements.diagnostics_statements_workflow
nodes$Format.Agree_with_statement.Happy_with_accuracy <- kobo_survey_import$GROUP_diagnostics.SUBGROUP_diagnostics_statements.diagnostics_statements_accurate

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

# Perform calculation
nodes$Format.Data_protection.Rights_of_data_subject_measures <- 
  ifelse (kobo_survey_import$GROUP_data_protection.SUBGROUP_rights_of_data_subjects.dp_rds_data_collection_yn == 0, "",
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
# nodes$Format.Data_protection.Data_protection_rating <- rowMeans(nodes[,15:19], na.rm=TRUE)

# output to CSV
write.csv(comments, file = paste("../not_shared/output/", country, "/", country, "_nodes.csv", sep=""))

### CREATE AND EXPORT THE COMMENTS DATA
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
# output to CSV
write.csv(comments, file = paste("../not_shared/output/", country, "/", country, "_comments.csv", sep=""))

