# merge_data_sets.R: Load and merge project and organisation xlsx files from CORDIS for
#                    both the H2020 (2014-2020) and HORIZON EUROPE (2021-2027) programs

# Map program labels to their respective subdirectory names in Data/Raw
PROGRAM_DIRECTORIES <- list(
  Horizon2014to2020   = list(
    projects      = "cordis-h2020projects-xlsx",
    organisations = "cordis-h2020projects-xlsx"
  ),
  Horizon2021to2027 = list(
    projects      = "cordis-HORIZONprojects-xlsx",
    organisations = "cordis-HORIZONprojects-xlsx"
  )
)

# Load and combine different program data for projects
# Note: primarily columns from 'objective' onward may be misaligned for a subset of rows 
# due to inconsistent field counts in the exported CORDIS file. These columns are not used
# in the network analysis and the issue is therefore not corrected here.
projects_list <- lapply(names(PROGRAM_DIRECTORIES), function(prog) {
  load_xlsx(subdirectory = PROGRAM_DIRECTORIES[[prog]]$projects,
            filename     = "project.xlsx")
})
projects <- rbindlist(projects_list, use.names = TRUE, fill = TRUE)
message("Projects loaded: ", nrow(projects), " rows across both programs")

# Load and combine different program data for organisations
organisations_list <- lapply(names(PROGRAM_DIRECTORIES), function(prog) {
  load_xlsx(subdirectory = PROGRAM_DIRECTORIES[[prog]]$organisations,
            filename     = "organization.xlsx")
})
organisations <- rbindlist(organisations_list, use.names = TRUE, fill = TRUE)
message("Organisations loaded: ", nrow(organisations), " rows across both programs")

# Join organisations to projects via ProjectID: organisations.projectID <=> projects.id
# Use left join to keep all organisation rows and add project-level attributes
cordis <- organisations[projects, on = .(projectID = id), nomatch = NA]
message("Joined dataset: ", nrow(cordis), " rows, ", ncol(cordis), " columns")


# --- Sanity checks: ---------------------------------------------------------------------
message("\n--- Sanity checks --- \nRows per program:")
print(cordis[, .N, by = frameworkProgramme])

# Try to find out more about the malformed frameworkProgramme entries
# TODO: HOW TO PROCEED WITH THESE DEVIATIONS? EXCLUDE THEM COMPLETELY OR CHANGE PROGRAM
#       MANUALLY AND IGNORE WRONG VALUES IN OTHER COLUMNS (OR MANUALLY FIX THEM)?
View(cordis[!frameworkProgramme %in% c("H2020", "HORIZON")])
# Check whether present deviations for projectIDs cover all entries under respective ID
cordis[!frameworkProgramme %in% c("H2020", "HORIZON"), .N, by = projectID]
malformed_frameworkProgramme_projIDs <-
  unique(cordis[!frameworkProgramme %in% c("H2020", "HORIZON"), projectID])
cordis[projectID %in% malformed_frameworkProgramme_projIDs, .N, by = projectID]
# Yes, projects with malformed framework are fully malformed, no additional correct entries!

message("Missing projectIDs in organisations: ", organisations[is.na(projectID), .N])

message("Projects with no matching organisation: ",
        projects[!id %in% organisations$projectID, .N])

message("\nColumns in final dataset: ", paste(names(cordis), collapse = ", "))