# functions.R: contains all the self-written functions used in the project

# --- List of functions ------------------------------------------------------------------
#' 1. unzip_recursive
#' 2. download_and_unzip
#' 3. load_xlsx
#' 4. build_collaboration_network
#' 5. expand_subgraph_to_full_graph
#' 6. checkpoint_RDS

# --- Function 1 -------------------------------------------------------------------------
#' @description
#' Recursively unzips .zip files to destination directory and removes the .zip afterwards.
#' If any nested .zip files are found among the extracted contents, those are unzipped in
#' the same way until no .zip files remain.
#'
#' Inputs:
#' @param path Character string. Full path to the .zip file to extract.
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
#' @param url Character string. URL of the .zip file to download.
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
#' Inputs:
#' @param subdirectory Character string. Subdirectory name within raw data directory.
#' @param filename Character string. CSV filename to load.
#'
#' Output:
#' @returns A data.table of the referenced data.

load_xlsx <- function(subdirectory, filename) {
  # Check for valid input
  require(checkmate)
  require(readxl)
  assertString(subdirectory)
  assertString(filename)

  # Check for paths to load .csv files from
  path <- file.path(PATHS$DATA_RAW, subdirectory, filename)
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
#' Inputs:
#' @param dt data.table object. Must contain at least columns projectID, organisationID,
#'                              and role.
#'
#' Output:
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

# --- Function 5 -------------------------------------------------------------------------
#' @description
#' Expands a named numeric vector of values of a subgraph onto a full graph's nodes by
#' filling in NAs for any nodes not present in the subgraph result.
#'
#' Inputs:
#' @param subgraph_values Named numeric vector. Resulting values computed for a subgraph.
#'                        Names must be names of nodes matching a subset of 'names_full'.
#' @param names_full Character vector. Complete set of names of nodes for full graph.
#'
#' Output:
#' @returns A named numeric vector of 'length(names_full)'. Contains 'subgraph_values' and
#'          NAs for elements absent from 'subgraph_values'

expand_subgraph_to_full_graph <- function(subgraph_values, names_full) {
  # Check for valid input
  require(checkmate)
  assertNumeric(subgraph_values)
  assertCharacter(names(subgraph_values), unique = TRUE)
  assertCharacter(names_full, unique = TRUE)

  # Create vector of NAs of length of the full graph (use NA_real_ to avoid type coercion)
  graph_full <- rep(NA_real_, length(names_full))

  # Assign the full NA-graph the names from the graph inserted into the function
  names(graph_full) <- names_full

  # Substitute the NA-values with subgraph-values for the name-matching positions
  graph_full[names(subgraph_values)] <- subgraph_values

  # Return a vector of the values of the full graph containing subgraph values and
  # filled-up NAs for nodes not in the subgraph
  return(graph_full)
}

# --- Function 6 -------------------------------------------------------------------------
#' @description
#' Loads either an existing .RDS checkpoint if it exists, or otherwise performs function
#' inserted in 'func_to_compute()' and saves the result to programme-specific subdirectory
#' (created if needed). The file is saved under the name concatenated out of 'filename'
#' and '.RDS'. This function is intended to avoid expensive re-computations on every rerun
#' once the files are already created.
#'
#' Inputs:
#' @param filename Character string. Name used for the checkpoint .RDS file.
#' @param prog Character string. Identifies the framework programme and is used for the
#'             subdirectory name under 'Data/Intermediate'.
#' @param func_to_compute Function with no arguments, contains computation whose result
#'                        is to be saved. Only called if no cached checkpoint is found.
#' @param recompute Logical. Default is FALSE; if TRUE, ignores any existing checkpoint
#'                  files and calls 'func_to_compute()' regardless and overwrites cached
#'                  file. Use this after changes previous to position of function call
#'                  (e.g. network construction) to avoid working with outdated results.
#'
#' Output:
#' @returns Either the loaded, already existing file or the newly computed result of the
#'          function within 'func_to_compute()'.

checkpoint_RDS <- function(filename, prog, func_to_compute, recompute = FALSE) {
  # Check for valid input
  require(checkmate)
  assertString(filename)
  assertString(prog)
  assertFunction(func_to_compute, nargs = 0)
  assertFlag(recompute)

  # Initialize paths for reference
  subdirectory <- file.path(PATHS$DATA_INT, prog)
  path <- file.path(subdirectory, paste0(filename, ".RDS"))
  
  # Load check-pointed RDS if available (i.e. already saved) and recompute is not forced
  if(file.exists(path) & !recompute) {
    # Retrieve file information of when the existing file was last modified
    created_at <- format(file.info(path)$mtime, format = "%d %b %Y, %H:%M:%S")

    # Inform the user that an already existing file is being loaded
    message("Loading cached checkpoint: ", file.path(prog, paste0(filename, ".RDS")),
            " (saved: ", created_at, ")")
    return(readRDS(path))
  }

  # Otherwise, perform the inserted computation function, then save the result to new file
  dir.create(subdirectory, showWarnings = FALSE)
  result <- func_to_compute()
  saveRDS(result, path)
  message("Checkpoint saved: ", file.path(prog, paste0(filename, ".RDS")))
  return(result)
}

# --- Function 7 -------------------------------------------------------------------------
#' @description
#' Repeatedly generates random graphs using a function to generate the graph, which is
#' supplied by the user. Then computes the global clustering coefficient and average path
#' length for the giant component of each simulated graph. The resulting values can be
#' used to compare an empirical network with these reference models.
#'
#' Inputs:
#' @param func_to_generate_graph Function with no arguments. Must return an igraph object
#'                               representing on randomly generated (= simulated) graph.
#' @param n_simulation Numeric scalar. Default is 100; describes the number of iterations,
#'                     i.e. number of random graphs to generate.
#'
#' Output:
#' @returns A data.table object with one row for the respective measures per simulation

simulate_random_graph <- function(func_to_generate_graph, n_simulation = 100) {
  # Check for valid input
  require(checkmate)
  assertFunction(func_to_generate_graph, nargs = 0)
  assertCount(n_simulation)

  # Pre-allocate placeholder vectors to be filled in loop due to performance reasons
  clustering <- numeric(length = n_simulation)
  pathlength <- numeric(length = n_simulation)

  # Each iteration, compute measures once for simulated random graph and store in slot i
  # of the placeholder
  for (i in seq_len(n_simulation)) {
    # Generate random graph as reference model
    g_random <- func_to_generate_graph()

    # Reduce to giant component, as measures well-defined solely on connected graph
    g_random_giant_comp <- largest_component(g_random)

    # Compute measures (clustering coefficient, average path length)
    clustering[i] <- transitivity(g_random_giant_comp, type = "global")
    pathlength[i] <- mean_distance(g_random_giant_comp, directed = FALSE)
  }

  # Return data.table object with the fully-filled placeholder vector
  data.table(clustering = clustering, pathlength = pathlength)
}
