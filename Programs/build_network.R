# build_network.R: Construct one-mode (organisation x organisation) undirected collaboration
#                  network from CORDIS. Nodes are the organisations, edges connect those
#                  that co-participated in at least one project.

# Load the data set
cordis <- readRDS(file.path(PATHS$INTERMEDIATE_DIR, "cordis.RDS"))

# Build the participation table of organisations: Retain only the columns needed for
# constructing the uni-modal network plus columns used as node-level attributes
network <- cordis[, .(organisationID, projectID, role)]  # TODO: add more attributes if needed

# Self-join the network to get all pairs of co-participating organisations. Only the pairs
# where organisationID < i.organisationID are kept to avoid duplicates in undirected graph
network <- network[network, on = .(projectID), nomatch = NULL, allow.cartesian = TRUE]
network <- network[organisationID < i.organisationID]
setnames(network, old = c("organisationID", "i.organisationID"),
                  new = c("from", "to"))

# Aggregate edges to weighted edges by number of shared projects
edges <- network[, .(weight = .N), by = c("from", "to")]

# Extract node attributes information (using 'uniqueN()' instead of .N as an organisation
# can theoretically perform different roles in the same project)
nodes <- cordis[, .(
  n_proj  = uniqueN(projectID),
  n_coord = sum(role == "coordinator")
), by = organisationID]

# Make igraph network objects
graph_weighted <- graph_from_data_frame(edges, directed = FALSE, vertices = nodes)
graph_unweighted <- delete_edge_attr(graph_weighted, "weight")

# Save the two network versions for future use
saveRDS(graph_weighted, file.path(PATHS$INTERMEDIATE_DIR, "network_unimodal_weighted.RDS"))
saveRDS(graph_unweighted, file.path(PATHS$INTERMEDIATE_DIR, "network_unimodal_unweighted.RDS"))

# --- Sanity checks: ---------------------------------------------------------------------
message("\n --- Sanity checks ---")
message("Nodes: ", vcount(graph_weighted))
message("Edges: ", ecount(graph_weighted))
message("Weighted graph is weigthed: ", is_weighted(graph_weighted))
message("Unweighted graph is unweigthed: ", !is_weighted(graph_unweighted))
message("Edge weight range: ", min(E(graph_weighted)$weight),
        " to ", max(E(graph_weighted)$weight))
message("Isolated nodes: ", sum(degree(graph_weighted) == 0))
# TODO: SHOULD WE EXCLUDE THE ISOLATED NODES AS WE ARE LOOKING AT COLLABORATIONS OR KEEP
# THEM (AS DEGREE OF 0 IMPLIES ORGANISATION IS SOLE ORGANISATION ON THE PROJECT)

message("Graph is connected: ", is_connected(graph_weighted))
message("Number of components: ", components(graph_weighted)$no)
message("Size of largest component: ", max(components(graph_weighted)$csize))
message("Node attributes: ", paste(vertex_attr_names(graph_weighted), collapse = ", "))
message("Edge attributes: ", paste(edge_attr_names(graph_weighted), collapse = ", "))