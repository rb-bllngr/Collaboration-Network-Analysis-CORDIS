# build_network.R: Construct one-mode (organisation x organisation) undirected collaboration
#                  network from CORDIS for full data and individual programmes.

# Load the data sets
cordis <- readRDS(file.path(PATHS$DATA_INT, "cordis.RDS"))
h2020 <- readRDS(file.path(PATHS$DATA_INT, "h2020.RDS"))
horizon <- readRDS(file.path(PATHS$DATA_INT, "horizon.RDS"))

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
message("\n --- Sanity checks ---")
message("Nodes: ", vcount(graph_weighted))
message("Edges: ", ecount(graph_weighted))
message("Weighted graph is weigthed: ", is_weighted(graph_weighted))
message("Unweighted graph is unweigthed: ", !is_weighted(graph_unweighted))
message("Edge weight range: ", min(E(graph_weighted)$weight),
        " to ", max(E(graph_weighted)$weight))
message("Isolated nodes: ", sum(degree(graph_weighted) == 0))
message("Graph is connected: ", is_connected(graph_weighted))
message("Number of components: ", components(graph_weighted)$no)
message("Size of largest component: ", max(components(graph_weighted)$csize))
message("Node attributes: ", paste(vertex_attr_names(graph_weighted), collapse = ", "))
message("Edge attributes: ", paste(edge_attr_names(graph_weighted), collapse = ", "))
