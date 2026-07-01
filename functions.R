# functions.R: contains all the self-written functions used in the project

# --- List of functions ------------------------------------------------------------------
#' 1. unzip_recursive
#' 2. download_and_unzip
#' 3. load_xlsx
#' 4. build_collaboration_network

# --- Function 1 -------------------------------------------------------------------------
#' @description
#' Recursively unzips .zip files to destination directory and removes the .zip afterwards.
#' If any nested .zip files are found among the extracted contents, those are unzipped in
#' the same way until no .zip files remain.
#'
#' Inputs:
#' @param path        Character string. Full path to the .zip file to extract.
#' @param destination Character string. Path to directory where extracted files are placed.
#'
#' Output:
#' @returns No return value. Called for extracting files and removing zips.

unzip_recursive <- function(zip_path, destination) {
  # Check for valid input
  require(checkmate)
  assertString(zip_path)
  assertString(destination)

  # Unzip the original .zip files imported from the portal and remove afterwards
  unzip(zip_path, exdir = destination)
  file.remove(zip_path)

  # Check for any nested zips and unzip those too
  nested_zips <- list.files(
    destination, pattern = "\\.zip$", full.names = TRUE, recursive = TRUE
    )
  for (zip in nested_zips) {
    message("Unzipping nested zip: ", basename(zip))
    unzip_recursive(zip, dirname(zip))
  }
}

# --- Function 2 -------------------------------------------------------------------------
#' @description
#' Downloads a .zip file from given URL to destination directory, then extracts its
#' contents recursively using 'unzip_recursive()'.
#'
#' Inputs:
#' @param url         Character string. URL of the .zip file to download.
#' @param destination Character string. Path to the directory where downloaded files are
#'                    saved and extracted into.
#'
#' Output:
#' @returns No return value. Called for downloading and extracting files.

download_and_unzip <- function(url, destination) {
  # Check for valid input
  require(checkmate)
  assertString(url)
  assertString(destination)

  # Create a subdirectory named after the .zip file
  zip_name <- tools::file_path_sans_ext(basename(url))  # e.g. "cordis-h2020projects-xlsx"
  subdirectory <- file.path(destination, zip_name)
  dir.create(subdirectory, showWarnings = FALSE)

  # Download the files via URLs
  zip_path <- file.path(destination, basename(url))
  message("Downloading: ", basename(url))
  downloaded <- GET(url,
                    config(http_version = 2),  # force HTTP/1.1 for CORDIS compatibility
                    write_disk(zip_path, overwrite = TRUE),
                    # Attention: Switched overwrite from TRUE to FALSE on July __ 2026 to
                    #            use up-to-date version for analysis. If latest version is
                    #            desired, just switch back to TRUE which makes the files
                    #            to be overwritten once new version is available.
                    progress())
  if (http_error(downloaded)) {
    stop("Failed to download: ", url, "\nStatus: ", status_code(downloaded))
  }

  # Recursively repeat unzipping
  message("Unzipping: ", basename(zip_path))
  unzip_recursive(zip_path, subdirectory)
  message("Done: ", basename(url))
}

# --- Function 3 -------------------------------------------------------------------------
#' @description
#' Loads a single .xlsx file from CORDIS program subdirectory using 'readxl::read_excel'.
#' Excel's format guarantees row integrity regardless of column count variation, just adds
#' NAs for empty cells in their correct column position.
#'
#' @param subdirectory  Character string. Subdirectory name within raw data directory.
#' @param filename      Character string. CSV filename to load.
#'
#' @returns A data.table of the referenced data.

load_xlsx <- function(subdirectory, filename) {
  # Check for valid input
  require(checkmate)
  require(readxl)
  assertString(subdirectory)
  assertString(filename)

  # Check for paths to load .csv files from
  path <- file.path(PATHS$DATA_DIR, subdirectory, filename)
  if (!file.exists(path)) {
    stop("File not found: ", path)
  }
  message("Loading: ", path)
  
  # Read .xlsx files with 'read_excel()' for more tolerant handling of malformed format
  # and convert to data.table objects afterwards
  dt <- setDT(read_excel(path))
  return(dt)
}

# --- Function 4 -------------------------------------------------------------------------
#' @description
#' Builds a unimodal (organisation x organisation), undirected collaboration network from
#' a data.table object. Nodes represent the organisations, edges connect those nodes that
#' have co-participated in at least one project, weighted by the number of shared projects.
#'
#' @param dt data.table object. Must contain at least columns projectID, organisationID,
#'                              and role.
#'
#' @returns A list with two igraph objects, one weighted ($weighted) and one unweighted
#'          ($unweighted) network.

build_collaboration_network <- function(dt) {
  # Check for valid input
  require(checkmate)
  require(igraph)
  assertDataTable(dt)
  assertNames(names(dt), must.include = c("projectID", "organisationID", "role"))

  # Build the participation table of organisations: Retain only the columns needed for
  # constructing the uni-modal network plus columns used as node-level attributes
  network <- dt[, .(projectID, organisationID, role)]
  # TODO: ADD MORE ATTRIBUTES IF NEEDED, REMEMBER TO UPDATE FUNCTION DESCRIPTION ACCORDINGLY

  # Self-join the network to get all pairs of co-participating organisations. Only the pairs
  # where organisationID < i.organisationID are kept to avoid duplicates in undirected graph
  network <- network[network, on = .(projectID), nomatch = NULL, allow.cartesian = TRUE]
  network <- network[organisationID < i.organisationID]
  setnames(network, old = c("organisationID", "i.organisationID"), new = c("from", "to"))

  # Aggregate edges to weighted edges by number of shared projects
  edges <- network[, .(weight = .N), by = c("from", "to")]

  # Extract node attributes information (using 'uniqueN()' instead of .N as an organisation
  # can theoretically perform different roles in the same project)
  nodes <- dt[, .(
    n_proj  = uniqueN(projectID),
    n_coord = sum(role == "coordinator")
  ), by = organisationID]

  # Make igraph network objects
  graph_weighted <- graph_from_data_frame(edges, directed = FALSE, vertices = nodes)
  graph_unweighted <- delete_edge_attr(graph_weighted, "weight")

  # Return list of the two igraph objects
  list(weighted = graph_weighted, unweighted = graph_unweighted)
}
