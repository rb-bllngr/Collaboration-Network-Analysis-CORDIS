# network_mds.R: Multidimensional Scaling (MDS) of country-level collaboration structure
#                as complementary visualisation to the geographic illustration done in
#                'network_countries.R'. Restricted to country level as organisation-level
#                MDS was judged computationally infeasible for this network size.

# Load the pairing country edge data
dt_country_pairs <- readRDS(file.path(PATHS$DATA_INT, "country_pairs.RDS"))
programmes <- unique(dt_country_pairs$programme)

# Initialize resulting lists for the application of the Multidimensional Scaling algorithm
results_mds <- list()
results_weight_comparison <- list()

# Iterate over both programmes and perform Multidimensional Scaling on network data
for(prog in programmes) {
  message("\n --- Programme: ", prog, " ---")

  # Restrict country pairings to EU plus associated countries for this programme, analogous
  # to the one already used for geographic network in 'network_countries.R'
  countries_EU <- if (prog == "H2020") names(eu28) else names(eu27)
  countries <- c(countries_EU, names(associated_countries[[prog]]))
  dt_edges <- dt_country_pairs[(programme == prog) &
                                 (country_i %in% countries) &
                                 (country_j %in% countries) &
                                 (country_i != country_j)]

  # Build the full weighted country-level graph with all edges (i.e. differing from geographic
  # version, which was build upon MST/N-1 strongest connections). Supply an explicit vertex
  # list so every intended country becomes a node, letting check for connected graph below
  # actually check for connectivity instead of silently dropping countries with zero edges
  graph_mds <- graph_from_data_frame(
    dt_edges[, .(country_i, country_j, sum_weight)],
    directed = FALSE,
    vertices = data.table(name = countries)
  )

  # Check whether MDS is applied onto fully connected graph, as algorithm needs complete,
  # finite distance matrix
  if(is_connected(graph_mds) == FALSE) {
    stop("Graph designed for MDS not fully connected for programme ", prog,
         ". MDS distance matrix would contain non-finite elements.")
  }

  # Convert edge weights into distance measure: more shared projects = shorter distance.
  # Tries out 1/weight (same convention as done elsewhere throughout other scripts) and 
  # 1/(1 + log(weight)), as 1/weight was tried initially and produces an extremely heavy-
  # tailed distribution that places the EU countries in near-zero separation.
  # Note: 1/(1 + log(weight)) produces even worse GOF, keep both variants for comparability
  distances_mds <- list(
    inverse = 1 / E(graph_mds)$sum_weight,
    logarithmic = 1 / (1 + log(E(graph_mds)$sum_weight))
  )

  fits_comparison <- lapply(distances_mds, function(distance_mds) {
    # Compute the full shortest-paths distance matrix using Djikstra algorithm for weighted
    # graph and run classical MDS
    distance_matrix <- distances(graph_mds, weights = distance_mds)
    mds_coordinates <- cmdscale(as.dist(distance_matrix), k = 2, eig = TRUE)
  })

  # Assemble diagnostics for both weighing versions
  results_weight_comparison[[prog]] <-
    rbindlist(lapply(names(fits_comparison), function (weight) {
      fit <- fits_comparison[[weight]]

      data.table(
        programme = prog,
        weight = weight,
        GOF = round(fit$GOF[1], 4),
        eigenvalue1 = round(fit$eig[1], 4),
        eigenvalue2 = round(fit$eig[2], 4),
        eigenvalue3 = round(fit$eig[3], 4),
        share_top2_dims = round(sum(fit$eig[1:2]) / sum(pmax(fit$eig, 0)), 4),
        eigenvalue_min = round(min(fit$eig), 4)
      )
    }))

  # Use the inverse weighing for computation illustrated further down
  mds_coordinates <- fits_comparison$inverse

  # Report the goodness-of-fit (GOF) criterion and other measures returned by 'cmdscale()'
  message("Goodness-of-fit (GOF), 1/weight-variant: ", round(mds_coordinates$GOF[1], 4))

  # Assemble results into data.table to have all information necessary for visualisation
  results_mds[[prog]] <- data.table(
    programme = prog,
    country = rownames(mds_coordinates$points),
    dim1 = mds_coordinates$points[, 1],
    dim2 = mds_coordinates$points[, 2],
    isEU = rownames(mds_coordinates$points) %in% countries_EU
  )
}

# Combine programme-specific computations into one data.table
dt_mds <- rbindlist(results_mds)
dt_weight_comparison <- rbindlist(results_weight_comparison)
print(dt_weight_comparison)

# Add labelling names for visualisation purposes to MDS data
dt_mds[, country_name := mapply(function(country, prog) {
  labels_prog <- country_labels_EU_plus_associated[[prog]]
  fifelse(country %in% names(labels_prog), labels_prog[[country]], country)
}, country, programme)]

# Visualise the MDS data, faceted by programme in direct comparison to geographic version
plot_mds <-
  ggplot(dt_mds, aes(x = dim1, y = dim2, color = isEU)) +
  geom_point(size = 4, alpha = 0.5) +
  geom_text_repel(aes(label = country_name), size = 5, color = lmu_default_color(),
                  segment.color = NA, max.overlaps = 10) +
  scale_color_manual(values = colorblindfriendly(),
                     labels = c("Kein Mitglied der Europäischen Union",
                                "Mitglied der Europäischen Union")) +
  labs(x = "Dimension 1", y = "Dimension 2", color = NULL) +
  facet_wrap(~ programme) +
  theme_lmu() +
  theme(legend.position = "bottom",
        panel.spacing.x = unit(1.5, "lines"),
        plot.margin = margin(r = 20, l = 20)) +
  guides(color = guide_legend(override.aes = list(alpha = 1)))
save_plot_lmu(plot_mds, "mds_inverse_weight.png")

# Compute axis limits from EU-members only with small padding margin instead of hardcoding
# to keep it correct in case of input changes during re-run
padding_factor <- 8
dt_zoom <- dt_mds[isEU == TRUE, .(
  xlim = max(abs(dim1)) * padding_factor,
  ylim = max(abs(dim2)) * padding_factor
)]
# Note: if programme-specific zooms wanted, need for-loop for 'coord_cartesian()' to work

# Filter data to points still in zoomed range to avoid labels being shown for points outside
# of the viewing window
dt_mds_zoomed <- dt_mds[(abs(dim1) <= dt_zoom$xlim) & (abs(dim2) <= dt_zoom$ylim)]

# Build zoomed in plot to highlight the overlapping part of first MDS visualisation
plot_mds_zoomed <-
  ggplot(dt_mds_zoomed, aes(x = dim1, y = dim2, color = isEU)) +
  geom_point(size = 4, alpha = 0.5) +
  geom_text_repel(aes(label = country_name), size = 5, color = lmu_default_color(),
                  segment.color = NA, max.overlaps = 10) +
  scale_color_manual(values = colorblindfriendly(),
                     labels = c("Kein Mitglied der Europäischen Union",
                                "Mitglied der Europäischen Union")) +
  coord_cartesian(xlim = c(-dt_zoom$xlim, dt_zoom$xlim),
                  ylim = c(-dt_zoom$ylim, dt_zoom$ylim)) +
  labs(x = "Dimension 1", y = "Dimension 2", color = NULL) +
  facet_wrap(~ programme) +
  theme_lmu() +
  theme(legend.position = "bottom",
        panel.spacing.x = unit(1.5, "lines"),
        plot.margin = margin(r = 20, l = 20)) +
  guides(color = guide_legend(override.aes = list(alpha = 1)))
save_plot_lmu(plot_mds_zoomed, "mds_inverse_weight_zoomed.png")
