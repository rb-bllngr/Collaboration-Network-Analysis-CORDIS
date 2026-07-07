# network_centrality.R: Compute degree, betweenness, closeness, and eigenvector centrality
#                       for the H2020 and the HORIZON EUROPE collaboration networks each.

# Load networks for each programme (take both, the weighted and unweighted versions, as
# they are required for different centrality measure calculations)
h2020_weighted <- readRDS(file.path(PATHS$DATA_INT, "network_h2020_weighted.RDS"))
h2020_unweighted <- readRDS(file.path(PATHS$DATA_INT, "network_h2020_unweighted.RDS"))
horizon_weighted <- readRDS(file.path(PATHS$DATA_INT, "network_horizon_weighted.RDS"))
horizon_unweighted <- readRDS(file.path(PATHS$DATA_INT, "network_horizon_unweighted.RDS"))

# Save the networks to a list for quick access
networks <- list(H2020 = list(weighted = h2020_weighted, unweighted = h2020_unweighted),
                 HORIZON = list(weighted = horizon_weighted, unweighted = horizon_unweighted))

# Initialize list for results
results <- list()

# Decide whether checkpoint .RDS files for expensive intermediate computations should be
# recomputed --> set TRUE for re-computation!
recompute <- FALSE

for (prog in c("H2020", "HORIZON")) {
  # Initialize time to track how much time the full for-loop takes up
  message("\n --- Programme: ", prog, " ---")
  time_start <- Sys.time()

  # Extract weighted and unweighted graph for iteration-specific network
  g_weighted <- networks[[prog]]$weighted
  g_unweighted <- networks[[prog]]$unweighted

  # Convert weight into distance-based logic:
  # stronger collaboration = higher shared number of projects = shorter distance
  dist <- 1 / E(g_weighted)$weight

  # Identify which nodes belong to giant component and create reduced graph versions
  giant_comp_weighted <- largest_component(g_weighted)
  giant_comp_unweighted <- largest_component(g_unweighted)

  # Sanity check: Do weighted and unweighted giant component really contain same subset
  # of organisations?
  if(!identical(sort(V(giant_comp_weighted)$name), sort(V(giant_comp_unweighted)$name))) {
    stop("Caution: weighted and unweighted giant components differ in programme ", prog)
  } 

  # Compute degree centrality, manually normalised via dividing by (n - 1), i.e. every node
  # can be connected to every other node but not itself. Remains well-defined across
  # disconnected components, therefore computed on full graph (computed on unweighted graph
  # only, as weighted degree = strength --> already covered in 'network_degrees.R')
  message("--- Degree centrality ...")
  centr_degrees <- degree(g_unweighted)
  centr_degrees_norm <- centr_degrees / (vcount(g_unweighted) - 1)

  # Compute betweenness centrality, normalised by using function argument, for unweighted
  # and weighted (= inverse-weight distance) version. Remains well-defined across
  # disconnected components, therefore computed on full graph.
  message("--- Betweenness centrality (weighted)...")
  centr_between_weighted <- checkpoint_RDS("centr_between_weighted", prog, function() {
    betweenness(g_weighted, weights = dist, normalized = FALSE)
  }, recompute = recompute)

  centr_between_weighted_norm <- checkpoint_RDS("centr_between_weighted_norm", prog, function() {
    betweenness(g_weighted, weights = dist, normalized = TRUE)
  }, recompute = recompute)

  message("--- Betweenness centrality (unweighted)...")
  centr_between_unweighted <- checkpoint_RDS("centr_between_unweighted", prog, function() {
    betweenness(g_unweighted, normalized = FALSE)
  }, recompute = recompute)

  centr_between_unweighted_norm <- checkpoint_RDS("centr_between_unweighted_norm", prog, function() {
    betweenness(g_unweighted, normalized = TRUE)
  }, recompute = recompute)

  # Analogously to above, convert weight into distance-based, but for giant component only
  dist_giant <- 1 / E(giant_comp_weighted)$weight

  # Compute closeness centrality, but only well-defined on a connected graph, as closeness
  # will interpret nodes in tiny components as artificially "close"; for unweighted and
  # weighted (= inverse-weight distance for giant component) version.
  message("--- Closeness centrality ...")
  giant_comp_close_weighted <- checkpoint_RDS("centr_close_weighted", prog, function() {
    closeness(giant_comp_weighted, weights = dist_giant, normalized = TRUE)
  }, recompute = recompute)
    
  giant_comp_close_unweighted <- checkpoint_RDS("centr_close_unweighted", prog, function() {
    closeness(giant_comp_unweighted, normalized = TRUE)
  }, recompute = recompute)

  # Compute eigenvector centrality, but eigenvector will typically give nodes outside giant
  # component around zero centrality (even though they might be structurally important only
  # due to drive by dominant eigenvalue); for unweighted and weighted (= strengths) version.
  message("--- Computing eigenvector centrality ...")
  giant_comp_eigenv_weighted <- eigen_centrality(giant_comp_weighted,
                                                 weights = E(giant_comp_weighted)$weight)$vector
  giant_comp_eigenv_unweighted <- eigen_centrality(giant_comp_unweighted,
                                                   weights = NA)$vector

  # Map the results for the giant-component measures back to the full graph by adding NAs
  # to nodes outside the giant component via function 'expand_subgraph_to_full_graph()'
  centr_close_weighted <- expand_subgraph_to_full_graph(giant_comp_close_weighted,
                                                        V(g_weighted)$name)
  centr_close_unweighted <- expand_subgraph_to_full_graph(giant_comp_close_unweighted,
                                                          V(g_unweighted)$name)
  centr_eigenv_weighted <- expand_subgraph_to_full_graph(giant_comp_eigenv_weighted,
                                                         V(g_weighted)$name)
  centr_eigenv_unweighted <- expand_subgraph_to_full_graph(giant_comp_eigenv_unweighted,
                                                           V(g_unweighted)$name)

  # Assemble all calculated results in one data.table per programme
  results[[prog]] <- data.table(
    programme = prog,
    organisationID = V(g_unweighted)$name,
    in_giant_comp = V(g_unweighted)$name %in% V(giant_comp_unweighted)$name,

    degree = centr_degrees,
    degree_norm = centr_degrees_norm,

    betweenness_weighted = centr_between_weighted,
    betweenness_weighted_norm = centr_between_weighted_norm,
    betweenness_unweighted = centr_between_unweighted,
    betweenness_unweighted_norm = centr_between_unweighted_norm,

    closeness_weighted = centr_close_weighted,
    closeness_unweighted = centr_close_unweighted,

    eigenvector_weighted = centr_eigenv_weighted,
    eigenvector_unweighted = centr_eigenv_unweighted
  )

  message("Programme ", prog, " done in: ",
          round(difftime(Sys.time(), time_start, units = "secs"), 1), " seconds")
}

# Combine programme-specific computations into one data.table
dt_centrality <- rbindlist(results)

# Sanity checks:
message("\n --- Sanity checks ---")
print(dt_centrality[, .(
  n_nodes = .N,
  n_giant_comp = sum(in_giant_comp),
  pct_giant_comp = mean(in_giant_comp) * 100
), by = programme])