# visualise_basic_networks.R: Visualize the collaboration network as world map using the
#                             organisations' geolocation data for node placement.

# Load the geolocation data
dt_geo <- readRDS(file.path(PATHS$DATA_INT, "geodata.RDS"))

# Load world map to visualise geograhically on
world <- map_data("world")

# Load both weighted programme-networks
networks <- list(H2020 = readRDS(file.path(PATHS$DATA_INT, "network_h2020.RDS"))$weighted,
                 HORIZON = readRDS(file.path(PATHS$DATA_INT, "network_horizon.RDS"))$weighted)
programmes <- names(networks)

# Plot the networks on geographical map as first overview per programme
for (prog in programmes) {
  graph_weighted <- networks[[prog]]

  # Merge all organisations present as graph vertices with those available still in geodata
  dt_nodes <- dt_geo[organisationID %in% V(graph_weighted)$name]
  message(prog, ": ", vcount(graph_weighted) - nrow(dt_nodes), " organisations excluded ",
          "from map overview visualisation due to missing geolocation.")

  # Convert edges from igraph object to data.table object and connect the organisations
  # according to their edge data
  dt_edges <- as.data.table(as_data_frame(graph_weighted, what = "edges"))
  dt_edges <- merge(dt_edges, dt_nodes, by.x = "from", by.y = "organisationID")
  setnames(dt_edges, old = c("latitude", "longitude"), new = c("lat_from", "long_from"))
  dt_edges <- merge(dt_edges, dt_nodes, by.x = "to", by.y = "organisationID")
  setnames(dt_edges, old = c("latitude", "longitude"), new = c("lat_to", "long_to"))

  # Plot the networks each as world map
  plot_map <- ggplot() +
    geom_polygon(data = world, aes(x = long, y = lat, group = group),
                 fill = lmu_colors$white, color = lmu_default_color(), linewidth = 0.1) +
    geom_segment(data = dt_edges, aes(x = long_from, y = lat_from,
                                      xend = long_to, yend = lat_to, linewidth = weight),
                 color = "grey85", alpha = 0.08) +
    geom_point(data = dt_nodes, aes(x = longitude, y = latitude),
               color = programme_colors[[prog]], size = 0.3, alpha = 0.5) +
    scale_linewidth_continuous(range = c(0.1, 1), guide = "none") +
    coord_fixed(1.1) +
    theme_void() +
    theme(
      plot.background = element_rect(fill = lmu_colors$white, color = NA),
      panel.background = element_rect(fill = lmu_colors$white, color = NA)
    )

  save_plot_lmu(plot_map, paste0("network_map_", tolower(prog), ".png"))
}


# # One-time visualisation of egocentric network for Ludwig-Maximilians-Universität München
# # Set network to inspect for LMU
# network_LMU <- "H2020"  # or "HORIZON", depending on network of interest!
# graph_weighted <- networks[[network_LMU]]
# # Reduce to LMUs node to be the focal one
# focal_node <- which(V(graph_weighted)$name == "999978433")
# neighbour_nodes <- ego(graph_weighted, order = 1, nodes = focal_node)[[1]]
# neighbour_ids <- V(graph_weighted)$name[neighbour_nodes]
# dt_nodes_ego <- dt_geo[organisationID %in% neighbour_ids]
# dt_edges_ego <- as.data.table(as_data_frame(graph_weighted, what = "edges"))
# dt_edges_ego <- dt_edges_ego[(from %in% neighbour_ids) & (to %in% neighbour_ids)]
# dt_edges_ego <- merge(dt_edges_ego, dt_nodes_ego, by.x = "from", by.y = "organisationID")
# setnames(dt_edges_ego, old = c("latitude", "longitude"), new = c("lat_from", "long_from"))
# dt_edges_ego <- merge(dt_edges_ego, dt_nodes_ego, by.x = "to", by.y = "organisationID")
# setnames(dt_edges_ego, old = c("latitude", "longitude"), new = c("lat_to", "long_to"))
# 
# ggplot() +
#   geom_polygon(data = world, aes(x = long, y = lat, group = group),
#                fill = lmu_colors$white, color = lmu_colors$black, linewidth = 0.1) +
#   geom_segment(data = dt_edges_ego, aes(x = long_from, y = lat_from,
#                                         xend = long_to, yend = lat_to, linewidth = weight),
#                color = "grey85", alpha = 0.1) +
#   geom_point(data = dt_nodes_ego, aes(x = longitude, y = latitude),
#              color = programme_colors[[network_LMU]], size = 0.5, alpha = 0.5) +
#   scale_linewidth_continuous(range = c(0.1, 1), guide = "none") +
#   coord_fixed(1.1) +
#   theme_void() +
#   theme(
#     plot.background = element_rect(fill = lmu_colors$white, color = NA),
#     panel.background = element_rect(fill = lmu_colors$white, color = NA)
#   )
