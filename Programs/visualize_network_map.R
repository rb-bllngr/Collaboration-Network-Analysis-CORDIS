# visualize_network_map.R: Visualize the collaboration network as world map using the
#                          organisations' geolocation data for node placement.

# Load the data set and network object
cordis <- readRDS(file.path(PATHS$INTERMEDIATE_DIR, "cordis.RDS"))
graph_weighted <- readRDS(file.path(PATHS$INTERMEDIATE_DIR, "network_unimodal_weighted.RDS"))

# Check whether any missing data for geolocation
message("Missing geolocations: ", nrow(cordis[is.na(geolocation) | geolocation == "",
                                              .N, by = organisationID]))

# Remove the 3946 organizations for which there is no geolocation data available
nodes <- cordis[!is.na(geolocation) & geolocation != "",
                     # Handling the case where an organisation appears in multiple rows
                     .(geolocation = first(geolocation)), by = organisationID]

# Divide geolocation into latitude and longitude for each organisation
nodes[, c("latitude", "longitude") := tstrsplit(geolocation, ",", type.convert = TRUE)]
nodes[, geolocation := NULL]

# Convert edges from igraph object to data.table object and connect the organisations
# according to their edge data
edges <- as.data.table(as_data_frame(graph_weighted, what = "edges"))
edges <- merge(edges, nodes, by.x = "from", by.y = "organisationID")
setnames(edges, old = c("latitude", "longitude"), new = c("lat_from", "long_from"))
edges <- merge(edges, nodes, by.x = "to", by.y = "organisationID")
setnames(edges, old = c("latitude", "longitude"), new = c("lat_to", "long_to"))

# Plot the network as world map
world <- map_data("world")

plot_map <- ggplot() +
  geom_polygon(data = world,
               aes(x = long, y = lat, group = group), fill = "white", color = "black", linewidth = 0.1) +
  geom_point(data = nodes, aes(x = longitude, y = latitude),
             color = "blue", # RColorBrewer::brewer.pal(3, "Set2")[3],
             size = 0.2, alpha = 0.5) +
  geom_segment(data = edges, aes(x = long_from, y = lat_from, xend = long_to, yend = lat_to),
               color = "red", #RColorBrewer::brewer.pal(3, "Set2")[2],
               alpha = 0.0075, linewidth = 0.1) +
  coord_fixed(1.1) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )
save_plot_lmu(plot_map, "network_unimodal_weighted_map.png")
