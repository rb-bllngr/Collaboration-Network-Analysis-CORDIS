# build_networks.R: Construct one-mode (organisation x organisation) undirected collaboration
#                   network from CORDIS for full data and individual programmes.

# Load the data sets and filter for programmes
cordis <- readRDS(file.path(PATHS$DATA_INT, "cordis_population.RDS"))
h2020 <- cordis[frameworkProgramme == "H2020"]
horizon <- cordis[frameworkProgramme == "HORIZON"]

# Check for organisation-project row duplication before network construction to prevent
# edge weight inflation
duplicates <- cordis[, .N, by = .(projectID, organisationID)][N > 1, .(projectID, organisationID)]
duplicates_rows <- cordis[duplicates, on = .(projectID, organisationID)]
columns_to_check <- setdiff(names(cordis), c("projectID", "organisationID"))
columns_varying <- sapply(columns_to_check, function(col) {
  duplicates_rows[, uniqueN(get(col)), by = .(projectID, organisationID)][, any(V1 > 1)]
})
message("Changes for duplicates can appear in: ",
        paste0(names(columns_varying)[columns_varying], collapse = ", "))

# Check whether any organisation-project pair has >1 row where role is coordinator
duplicates_rows[role == "coordinator", .N, by = .(projectID, organisationID)][N > 1]
# Note: No overcounts happening!

# # One-time validation of effect from duplicated changed to de-duplicated version:
# # Temporarily define old (pre-fix) version of function No. 4 in 'functions.R'
# build_collaboration_network_old <- function(dt) {
#   network <- dt[, .(projectID, organisationID)]  # no 'unique()' here!
#   network <- network[network, on = .(projectID), nomatch = NULL, allow.cartesian = TRUE]
#   network <- network[organisationID < i.organisationID]
#   setnames(network, old = c("organisationID", "i.organisationID"), new = c("from", "to"))
#   edges <- network[, .(weight = .N), by = c("from", "to")]
#   nodes <- dt[, .(
#     n_proj  = uniqueN(projectID),
#     n_coord = sum(role == "coordinator"),
#     country = first(country)
#   ), by = organisationID]
# 
#   graph_from_data_frame(edges, directed = FALSE, vertices = nodes)
# }
# 
# # Build both versions for comparison
# graph_old <- build_collaboration_network_old(cordis)  # see above
# graph_new <- build_collaboration_network(cordis)$weighted  # fixed version
# 
# # Compare versions
# message("Edges before: ", ecount(graph_old), " / Edges after: ", ecount(graph_new))
# message("Total edge weight before: ", sum(E(graph_old)$weight),
#         " / Total edge weight after: ", sum(E(graph_new)$weight))
# dt_compare <- merge(
#   data.table(organisationID = V(graph_old)$name, degree_old = degree(graph_old)),
#   data.table(organisationID = V(graph_new)$name, degree_new = degree(graph_new)),
#   by = "organisationID"
# )
# n_affected <- dt_compare[degree_old != degree_new, .N]
# message(n_affected, " organisations had change in degree.")
# print(dt_compare[degree_old != degree_new][order(-abs(degree_old - degree_new))][1:10])

# Build weighted networks for the full CORDIS data set and each individual programme
programmes <- list(cordis = cordis, h2020 = h2020, horizon = horizon)
networks <- lapply(programmes, build_collaboration_network)

# Save the two versions (unweighted and weighted) for each network to one .RDS file
# containing a list of the networks, named consistently
for(name in names(networks)) {
  saveRDS(networks[[name]], file.path(PATHS$DATA_INT, paste0("network_", name, ".RDS")))
}

# Sanity checks for CORDIS network
graph_weighted <- networks[["cordis"]]$weighted
graph_unweighted <- networks[["cordis"]]$unweighted
message("Nodes: ", vcount(graph_weighted))
message("Edges: ", ecount(graph_weighted))
message("Weighted graph is weighted: ", is_weighted(graph_weighted))
message("Unweighted graph is unweighted: ", !is_weighted(graph_unweighted))
message("Edge weight range: ", min(E(graph_weighted)$weight),
        " to ", max(E(graph_weighted)$weight))
message("Isolated nodes: ", sum(degree(graph_weighted) == 0))
message("Graph is connected: ", is_connected(graph_weighted))
message("Number of components: ", components(graph_weighted)$no)
message("Size of largest component: ", max(components(graph_weighted)$csize))
message("Node attributes: ", paste(vertex_attr_names(graph_weighted), collapse = ", "))
message("Edge attributes: ", paste(edge_attr_names(graph_weighted), collapse = ", "))
