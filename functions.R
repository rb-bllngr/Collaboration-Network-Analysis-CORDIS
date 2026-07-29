# functions.R: contains all the self-written, functions (re-)used in the project

# --- List of functions ------------------------------------------------------------------
#' 01. unzip_recursive
#' 02. download_and_unzip
#' 03. load_xlsx
#' 04. build_collaboration_network
#' 05. expand_subgraph_to_full_graph
#' 06. checkpoint_RDS
#' 07. simulate_random_graph
#' 08. build_graph_fundamentals
#' 09. build_country_plot_map
#' 10. build_country_plot_abstract

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
#'                              role, and country.
#'
#' Output:
#' @returns A list with two igraph objects, one weighted ($weighted) and one unweighted
#'          ($unweighted) network.

build_collaboration_network <- function(dt) {
  # Check for valid input
  require(checkmate)
  require(igraph)
  assertDataTable(dt)
  assertNames(names(dt),
              must.include = c("projectID", "organisationID", "role", "country"))

  # Build the participation table of organisations: Retain only the columns needed for
  # constructing the uni-modal network plus columns used as node-level attributes
  network <- dt[, .(projectID, organisationID)]

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
    n_coord = sum(role == "coordinator"),
    country = first(country)
  ), by = organisationID]

  # Run diagnostics on country information: flag organisations whose rows differ on country
  if (dt[, uniqueN(country), by = organisationID][V1 > 1, .N] > 0) {
    message(dt[, uniqueN(country), by = organisationID][V1 > 1, .N],
            " organisations have inconsistent country attributes across rows. First non-",
            "missing valuse used per organisation.")
  }

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

# --- Function 8 -------------------------------------------------------------------------
#' @description
#' Builds the fundamental data of a country-pair collaboration network, consisting of the
#' union of the maximum spanning tree AND the N-1 overall strongest links in the network.
#' Additionally, it establishes a node-level table containing information about geolocation,
#' EU-membership, and degree within the country graph. Optionally can restrict the data to
#' EU-member countries only before taking the corresponding edges from the network.
#'
#' Inputs:
#' @param dt_nodes data.table object. Contains country-level coordinates, averaged across
#'                 all organisations within that country. Must contain at least columns
#'                 programme, country, latitude, and longitude.
#' @param dt_edges data.table object. Contains country-pair edges across all programmes.
#'                 Must contain at least columns programme, country_i, country_j, and
#'                 n_organisation_pairs.
#' @param prog Character string. Either 'H2020' or 'HORIZON'; identifies programme being
#'             looked at and used to subset input data.tables.
#' @param eu Logical. Default FALSE; if TRUE, restricts edges to country-pairs where both
#'           countries are EU members.
#'
#' Output:
#' @returns A list with two data.table objects for information on the fundamental edges
#'          with country-pair attributes ($edges) and node-level information ($nodes).

build_graph_fundamentals <- function(dt_nodes, dt_edges, prog, eu = FALSE) {
  # Check for valid input
  require(checkmate)
  require(igraph)
  assertDataTable(dt_nodes)
  assertNames(names(dt_nodes),
              must.include = c("programme", "country", "latitude", "longitude"))
  assertDataTable(dt_edges)
  assertNames(names(dt_edges),
              must.include = c("programme", "country_i", "country_j", "n_organisation_pairs"))
  assertChoice(prog, choices = c("H2020", "HORIZON"))
  assertFlag(eu)

  # Subset both input data.table objects to current programme and exclude same-country pairs
  dt_edges_prog <- dt_edges[(programme == prog) & (country_i != country_j)]
  dt_nodes_prog <- dt_nodes[programme == prog]

  # Set the ACTUAL EU-membership for the programme (used for coloring), but restrict the
  # displayed countries to the EU-28 countries REGARDLESS of the current programme if the
  # EU flag is applied, so both H2020 and HORIZON show the same set of countries
  countries_EU <- if (prog == "H2020") names(eu28) else names(eu27)
  if(eu) {
    dt_edges_prog <- dt_edges_prog[(country_i %in% names(eu28)) &
                                     (country_j %in% names(eu28))]
  }

  # Reduce data to countries with available averaged-geolocation data and inform about
  # countries being excluded due to fully missing geolocations
  countries_prog <- sort(unique(c(dt_edges_prog$country_i, dt_edges_prog$country_j)))
  countries_missing <- setdiff(countries_prog, dt_nodes_prog$country)
  if(length(countries_missing) > 0) {
    message("For ", prog, ", exclude countries without geolocation for any organisation: ",
            paste(countries_missing, collapse = ", "))
  }

  # Exclude these countries from visualisation of edges
  dt_edges_prog <- dt_edges_prog[(country_i %in% dt_nodes_prog$country) &
                                   (country_j %in% dt_nodes_prog$country)]
  
  # Exclude the same countries from the node-level data
  countries_prog <- sort(unique(c(dt_edges_prog$country_i, dt_edges_prog$country_j)))
  dt_nodes_prog <- dt_nodes_prog[country %in% countries_prog]

  # Build the graph network from the centroid and organisation-pair data
  graph_country <- graph_from_data_frame(
    # Data Frame containing edgelist in the first two columns with additional columns as
    # edge attributes (here: weight)
    dt_edges_prog[, .(country_i, country_j, n_organisation_pairs)],
    directed = FALSE,
    vertices = dt_nodes_prog[, .(country, latitude, longitude)]
  )

  # Assemble all node information available
  dt_nodes_prog[, isEU := country %in% countries_EU]
  dt_nodes_prog[, degree := degree(graph_country)[country]]

  # Create the maximum spanning tree (MST). Because igraph's function 'mst()' computes
  # the minimum spanning tree, weights are negated to produce maximum instead
  graph_mst <- mst(graph_country, weights = -E(graph_country)$n_organisation_pairs)
  dt_mst <- as.data.table(as_data_frame(graph_mst, what = "edges"))
  dt_mst <- dt_mst[, .(country_i = pmin(from, to), country_j = pmax(from, to))]

  # Additionally, take the N-1 overall strongest links between countries, independent of
  # the maximum spanning tree
  dt_strongest_Nminus1 <- dt_edges_prog[order(-n_organisation_pairs)][
    1:(vcount(graph_country) - 1), .(country_i, country_j)
  ]

  # Collect results for both ways of determining most 'important' edges and merge with edge
  # information for these edges
  dt_connections <- merge(unique(rbind(dt_mst, dt_strongest_Nminus1)),
                          dt_edges_prog, by = c("country_i", "country_j"))

  list(nodes = dt_nodes_prog, edges = dt_connections)
}

# --- Function 9 -------------------------------------------------------------------------
#' @description 
#' Build geographical visualisation out of foundational node and edge data.
#'
#' Inputs:
#' @param dt_nodes data.table object. Must contain at least country, latitude, longitude,
#'                 isEU, and degree (as returned by 'build_graph_fundamentals$nodes').
#' @param dt_edges data.table object. Must contain at least columns country_i, country_j,
#'                 and n_organisation_pairs (as returned by 'build_graph_fundamentals$edges').
#' @param world data.frame. Data to visualise map as given by 'ggplot2::map_data("world")'.
#'
#' Output:
#' @returns A ggplot object showing the geographical representation of the inputted data
#'          layered over world map outline.

build_country_plot_map <- function(dt_nodes, dt_edges, world) {
  # Check for valid input
  require(checkmate)
  assertDataTable(dt_nodes)
  assertNames(names(dt_nodes),
              must.include = c("country", "latitude", "longitude", "degree"))
  assertDataTable(dt_edges)
  assertNames(names(dt_edges),
              must.include = c("country_i", "country_j", "n_organisation_pairs"))

  # Merge node and edge information into one data.table
  dt_map <- merge(dt_edges, dt_nodes[, .(country, latitude, longitude, isEU)],
                  by.x = "country_i", by.y = "country")
  dt_map <- merge(dt_map, dt_nodes[, .(country, latitude, longitude, isEU)],
                  by.x = "country_j", by.y = "country")
  setnames(dt_map,
           old = c("latitude.x", "longitude.x", "isEU.x",
                   "latitude.y", "longitude.y", "isEU.y"),
           new = c("latitude_i", "longitude_i", "isEU_i",
                   "latitude_j", "longitude_j", "isEU_j"))

  # Map the colorblind-friendly coloring scale onto EU membership status
  palette_isEU <- c("FALSE" = colorblindfriendly()[1], "TRUE" = colorblindfriendly()[2])

  # Plot world map with organisations aggregated to countries on the map
  ggplot() +
    geom_polygon(data = world, aes(x = long, y = lat, group = group),
                 fill = lmu_colors$white, colour = lmu_default_color(), linewidth = 0.2) +
    # Alternatively, choose geom_segment for straight lines instead of curves!
    geom_curve(data = dt_map,
               aes(x = longitude_i, y = latitude_i, xend = longitude_j, yend = latitude_j,
                   linewidth = n_organisation_pairs),
               color = ifelse((dt_map$isEU_i == TRUE) & (dt_map$isEU_j == TRUE),
                              palette_isEU[["TRUE"]],  # Edge between two EU members
                              palette_isEU[["FALSE"]]),  # Edge at least one non-EU
               curvature = 0.1, alpha = 0.5, show.legend = FALSE) +
    geom_point(data = dt_nodes,
               aes(x = longitude, y = latitude, size = degree, color = isEU, fill = isEU),
               shape = 21) +
    scale_fill_manual(values = scales::alpha(palette_isEU, alpha = 0.5), guide = "none") +
    scale_color_manual(
      values = palette_isEU,
      labels = c("FALSE" = "Kein Mitglied der Europäischen Union",
                 "TRUE" = "Mitglied der Europäischen Union"),
      guide = guide_legend(position = "top", override.aes = list(shape = 16, size = 3))) +
    scale_size_continuous(range = c(0.5, 5), guide = guide_legend(position = "bottom")) +
    scale_linewidth_continuous(range = c(0.1, 2), guide = "none") +
    labs(size = "Grad (Anzahl an Ländern)", color = NULL) +
    theme_void() +
    theme(legend.title = element_text(vjust = 0.6),
          plot.background = element_rect(fill = lmu_colors$white, color = NA),
          panel.background = element_rect(fill = lmu_colors$white, color = NA))
}

# --- Function 10 ------------------------------------------------------------------------
#' @description
#' Build abstract graph layout out of foundational node and edge data, using force-directed
#' (Fruchterman-Reingold) positioning.
#'
#' Inputs:
#' @param dt_nodes data.table object. Must contain at least columns country, isEU, and
#'                 degree (as returned by 'build_graph_fundamentals$nodes').
#' @param dt_edges data.table object. Must contain at least columns country_i, country_j,
#'                 and n_organisation_pairs (returned by 'build_graph_fundamentals$edges').
#' @param seed Numeric scalar. Default 20260916 (= date of submission). Due to Fruchterman-
#'             Reingold layout being stochastic, fixing the seed ensures reproducibility.
#'
#' Output:
#' @returns A ggplot object showing an abstract, force-directed representation of the 
#'          inputted data.

build_country_plot_abstract <- function(dt_nodes, dt_edges, seed = 20260916) {
  # Check for valid input
  require(checkmate)
  require(ggraph)
  require(tidygraph)
  assertDataTable(dt_nodes)
  assertNames(names(dt_nodes), must.include = c("country", "isEU", "degree"))
  assertDataTable(dt_edges)
  assertNames(names(dt_edges),
              must.include = c("country_i", "country_j", "n_organisation_pairs"))
  assertCount(seed)

  # Add an edge-level EU membership flag analogous to 'build_country_plot_map()'
  dt_edges_flag <- merge(dt_edges, dt_nodes[, .(country, isEU)],
                         by.x = "country_i", by.y = "country")
  dt_edges_flag <- merge(dt_edges_flag, dt_nodes[, .(country, isEU)],
                         by.x = "country_j", by.y = "country")
  setnames(dt_edges_flag, old = c("isEU.x", "isEU.y"), new = c("isEU_i", "isEU_j"))
  dt_edges_flag[, both_EU := (isEU_i == TRUE) & (isEU_j == TRUE)]

  # Build the graph network from the node and edge data
  graph_abstract <- graph_from_data_frame(
    dt_edges_flag[, .(country_i, country_j, n_organisation_pairs, both_EU)],
    directed = FALSE,
    vertices = dt_nodes[, .(country, isEU, degree)]
  )

  # Map the colorblind-friendly coloring scale onto EU membership status
  palette_isEU <- c("FALSE" = colorblindfriendly()[1], "TRUE" = colorblindfriendly()[2])

  # Plot abstract graph layout (i.e. Fruchterman-Reingold) using seed for reproducibility
  set.seed(seed)
  ggraph(graph_abstract, layout = "fr") +
    geom_edge_link(aes(edge_width = n_organisation_pairs, edge_colour = both_EU), alpha = 0.5) +
    geom_node_point(aes(size = degree, color = isEU, fill = isEU), shape = 21) +
    scale_edge_color_manual(values = palette_isEU, guide = "none") +
    scale_fill_manual(values = scales::alpha(palette_isEU, alpha = 0.5), guide = "none") +
    scale_color_manual(
      values = palette_isEU,
      labels = c("FALSE" = "Kein Mitglied der Europäischen Union",
                 "TRUE" = "Mitglied der Europäischen Union"),
      guide = guide_legend(position = "top", override.aes = list(shape = 16, size = 3))) +
    scale_size_continuous(range = c(0.5, 5), guide = guide_legend(position = "bottom")) +
    scale_edge_width_continuous(range = c(0.1, 2), guide = "none") +
    labs(size = "Grad (Anzahl an Ländern)", color = NULL) +
    theme_void() +
    theme(legend.title = element_text(vjust = 0.6),
          plot.background = element_rect(fill = lmu_colors$white, color = NA),
          panel.background = element_rect(fill = lmu_colors$white, color = NA))
}