# download_and_merge_data.R: Import data sets needed for project from openly-accessible EU
#                            portal CORDIS by either downloading them directly from portal
#                            or downloading frozen snapshot from Sync&Share. Afterwards,
#                            load and merge .xlsx files into one data set containing project
#                            and organisation data for H2020 (2014-2020) and HORIZON EUROPE
#                            (2021-2027) programmes.

cat("\n=====================================================================\n")
cat("                        Data Source Selection                        \n")
cat("=====================================================================\n")
cat("     [1] Latest data directly from CORDIS                            \n")
cat("     [2] Frozen snapshot (as of end of June 2026, via Sync&Share)    \n")
cat("=====================================================================\n")

# Make the user choose the source for CORDIS data
user_input <- readline("Please enter 1 or 2: ")

# Configure the URLs required to download depending on user's choice
if (user_input == "1") {
  # Choose the URLs required to download directly from CORDIS
  urls <- list(
    Horizon2014to2020 = "https://cordis.europa.eu/data/cordis-h2020projects-xlsx.zip",
    Horizon2021to2027 = "https://cordis.europa.eu/data/cordis-HORIZONprojects-xlsx.zip"
  )

  # Download the respective .zip files and unzip them accordingly
  programme_directories <- list()
  for (programme in names(urls)) {
    download_and_unzip(urls[[programme]], PATHS$DATA_RAW)
    programme_directories[[programme]] <- tools::file_path_sans_ext(basename(urls[[programme]]))
  }

} else if (user_input == "2") {
  # Ask user for linkID (kept out of GitHub repository on purpose for data safety)
  linkID <- readline("Please enter the Sync&Share linkID (provided upon request): ")

  # Download and unzip single outer archive containing frozen data snapshot
  download_and_unzip(
    paste0("https://syncandshare.lrz.de/dl/", linkID, "/cordis-frozen-snapshot.zip"),
    PATHS$DATA_RAW
  )

  # Move the two extracted programme subfolder up one level, out of the temporary folder
  # 'cordis-frozen-snapshot' and then remove the folder
  directory_outer <- file.path(PATHS$DATA_RAW, "cordis-frozen-snapshot")
  for (subfolder in c("cordis-h2020projects-xlsx", "cordis-HORIZONprojects-xlsx")) {
    target <- file.path(PATHS$DATA_RAW, subfolder)

    # Remove any pre-existing folder at target destination to allow overwrite on re-runs
    if (dir.exists(target)) unlink(target, recursive = TRUE)
    file.rename(file.path(directory_outer, subfolder), target)
  }
  unlink(directory_outer, recursive = TRUE)

  # Both programme subfolder now available, named as usual
  programme_directories <- list(
    Horizon2014to2020 = "cordis-h2020projects-xlsx",
    Horizon2021to2027 = "cordis-HORIZONprojects-xlsx"
  )

} else {
  stop("Invalid choice. Please enter 1 or 2!")
}

message("All files downloaded and extracted.")

# Load and combine different program data for projects
# Note: primarily columns from 'objective' onward may be misaligned for a subset of rows
# due to inconsistent field counts in the exported CORDIS file. These columns are not used
# in the network analysis and the issue is therefore not corrected here.
projects_list <- lapply(names(programme_directories), function(prog) {
  load_xlsx(subdirectory = programme_directories[[prog]], filename = "project.xlsx")
})
names(projects_list) <- c("H2020", "HORIZON")

# Report number of projects per programme before merging
n_projects_per_programme <- sapply(projects_list, nrow)
message("Projects per programme: ",
        paste(names(n_projects_per_programme), n_projects_per_programme,
              sep = " = ", collapse = ", "))

projects <- rbindlist(projects_list, use.names = TRUE, fill = TRUE)
message("Projects loaded: ", nrow(projects), " rows across both programs")

# Load and combine different program data for organisations
organisations_list <- lapply(names(programme_directories), function(prog) {
  load_xlsx(subdirectory = programme_directories[[prog]], filename = "organization.xlsx")
})
names(organisations_list) <- c("H2020", "HORIZON")

# Report number of unique organisations per programme before merging
n_organisations_per_programme <- sapply(organisations_list,
                                        function(dt) uniqueN(dt$organisationID))
message("Unique organisations per programme: ",
        paste(names(n_organisations_per_programme), n_organisations_per_programme,
              sep = " = ", collapse = ", "))

# Report overlap of how many organisations in appear in both programmes
n_overlap <- length(intersect(unique(organisations_list[["H2020"]]$organisationID),
                              unique(organisations_list[["HORIZON"]]$organisationID)))
message("Organisations appearing in both programmes: ", n_overlap, " (",
        round(n_overlap /
                length(unique(organisations_list[["HORIZON"]]$organisationID)) * 100, 2),
              " % of HORIZON organisations)")
# Note: rough early-stage overview count on uncleaned organisationIDs, not directly
#       comparable with giant-component-based persistence analysis done in 'network_roles.R'

organisations <- rbindlist(organisations_list, use.names = TRUE, fill = TRUE)
message("Organisations loaded: ", nrow(organisations), " rows across both programs")

# Join organisations to projects via ProjectID: organisations.projectID <=> projects.id
# Use left join to keep all organisation rows and add project-level attributes
cordis <- projects[organisations, on = .(id = projectID), nomatch = NA]
setnames(cordis, old = "id", new = "projectID")
message("Joined dataset: ", nrow(cordis), " rows, ", ncol(cordis), " columns")

# Check for any malformed frameworkProgramme entries
print(cordis[, .N, by = frameworkProgramme])
message("Data contains ",
        nrow(cordis[!frameworkProgramme %in% c("H2020", "HORIZON"), .N, by = projectID]),
        " entries with malformed frameworkProgramme.\n(Snapshot does not contain malformed",
        " entries. If any for latest CORDIS data, these are excluded.)")

# Convert variable frameworkProgramme to Factor
cordis[, frameworkProgramme := factor(frameworkProgramme, levels = c("H2020", "HORIZON"))]
cordis <- cordis[!is.na(frameworkProgramme)]

# Save the joined and individual programme-networks
saveRDS(cordis, file.path(PATHS$DATA_INT, "cordis.RDS"))
message("\nColumns in final dataset: ", paste(names(cordis), collapse = ", "))
