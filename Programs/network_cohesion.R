# network_cohesion.R: Compute component structure, density, clustering and small-world
#                     coefficient, as well as average path length for each of the H2020
#                     and HORIZON EUROPE collaboration networks.

# Load networks for each programme (take both, the weighted and unweighted versions, as
# they are required for different cohesion measures) and save for quick access
networks <- list(H2020 = readRDS(file.path(PATHS$DATA_INT, "network_h2020.RDS")),
                 HORIZON = readRDS(file.path(PATHS$DATA_INT, "network_horizon.RDS")))
programmes <- names(networks)

# Initialize lists for results of each of the cohesion measures
results_components <- list()
results_density <- list()
results_cores <- list()
results_clustering <- list()
results_pathlength <- list()
results_smallworld <- list()
results_smallworld_simulation <- list()

# Decide whether checkpoint .RDS files for expensive intermediate computations should be
# recomputed --> set TRUE for re-computation! (here: average path length)
recompute <- FALSE

# Compute different cohesion measures for each of the programme-networks
for (prog in programmes) {
  # Initialize time to track how much time the full for-loop takes up
  message("\n --- Programme: ", prog, " ---")
  time_start <- Sys.time()

  # Extract weighted and unweighted graph for iteration-specific network
  g_weighted <- networks[[prog]]$weighted
  g_unweighted <- networks[[prog]]$unweighted

  # Component structure: identify components in the network and reduce to giant component
  # (analogous to 'network_centrality.R'); purely topological, therefore unweighted.
  message("--- Component structure ...")
  results_components[[prog]] <- data.table(
    programme = prog,
    componentID = seq_len(components(g_unweighted)$no),
    size = components(g_unweighted)$csize
  )
  giant_comp_weighted <- largest_component(g_weighted)
  giant_comp_unweighted <- largest_component(g_unweighted)

  # Density: computed for both, full and giant-component graph; unweighted as density
  # only compares number of edges with possible edges, regardless of edge weights.
  message("--- Density ...")
  results_density[[prog]] <- data.table(
    programme = prog,
    density_full_graph = edge_density(g_unweighted),
    density_giant_comp = edge_density(giant_comp_unweighted)
  )

  # k-Cores:
  message("--- k-Cores ...")
  results_cores[[prog]] <- data.table(
    programme = prog,
    organisationID = V(g_unweighted)$name,
    degree = degree(g_unweighted),
    coreness = coreness(g_unweighted)
  )

  # Clustering coefficient:
  message("--- Clustering coefficient ...")
  results_clustering[[prog]] <- data.table(
    programme = prog,
    organisationID = V(g_unweighted)$name,
    degree = degree(g_unweighted),
    # Compute coefficient for different neighborhoods, not including nodes of degree < 2,
    # which get NaN instead of zero
    coeff_global = transitivity(g_unweighted, type = "global"),
    coeff_local_unweighted = transitivity(g_unweighted, type = "local", isolates = "NaN"),
    coeff_local_weighted = transitivity(g_weighted, type = "weighted", isolates = "NaN")
  )
  # Note: for definition of weighted (or so-called Barrat) variant, see ?transitivity

  # Average path length:
  message("--- Average path length ...")
  # Caution, as this section is computationally expensive (takes approximately ___ minutes
  # to be re-computed), computation is skipped to use checkpoint .RDS if already available
  results_pathlength[[prog]] <- data.table(
    programme = prog,
    # Compute on giant-component, as only well-defined on connected graph
    avg_unweighted = checkpoint_RDS("avgpathlength_unweighted", prog, function() {
      mean_distance(giant_comp_unweighted, directed = FALSE)
      }, recompute = recompute),

    avg_weighted = checkpoint_RDS("avgpathlength_weighted", prog, function() {
      mean_distance(giant_comp_weighted, weights = 1 / E(giant_comp_weighted)$weight, directed = FALSE)
      }, recompute = recompute)
  )

  # Small-world coefficient:
  message("--- Small-world coefficient (Monte Carlo simulation) ...")
  # Monte Carlo simulation to compare the real graph against two reference models, i.e.
  #   - Erdös-Rényi graph (with same number of edges and nodes; ignores degree heterogeneity)
  #   - configuration model graph (specifically Viger-Latapy; same degree sequence)
  # Both are computed to see whether potential small-world property withstands even
  # stricter small-world test condition
  m_edges <- ecount(giant_comp_unweighted)
  n_nodes <- vcount(giant_comp_unweighted)
  degree_sequence <- degree(giant_comp_unweighted)

  # Generate random graph reference models and compute measure on them, respectively (use
  # default iteration runs of n_simulation = 100)
  simulated_ErdosRenyi <- checkpoint_RDS("smallworld_ErdosRenyi", prog, function() {
    simulate_random_graph(function() sample_gnm(n = n_nodes, m = m_edges, directed = FALSE))
  }, recompute = recompute)
  simulated_VigerLatapy <- checkpoint_RDS("smallworld_VigerLatapy", prog, function() {
    simulate_random_graph(function() sample_degseq(degree_sequence, method = "vl"))
  }, recompute = recompute)

  results_smallworld_simulation[[prog]] <- list(ErdosRenyi = simulated_ErdosRenyi,
                                                VigerLatapy = simulated_VigerLatapy)

  # Assign respective values to needed formula variables (cf. Humphries & Gurney, 2008)
  C_observed <- transitivity(giant_comp_unweighted, type = "global")
  C_random_ErdosRenyi <- mean(simulated_ErdosRenyi$clustering)
  C_random_VigerLatapy <- mean(simulated_VigerLatapy$clustering)
  L_observed <- results_pathlength[[prog]]$avg_unweighted
  L_random_ErdosRenyi <- mean(simulated_ErdosRenyi$pathlength)
  L_random_VigerLatapy <- mean(simulated_VigerLatapy$pathlength)

  # Compute small-world coefficient
  S_ErdosRenyi <- (C_observed / C_random_ErdosRenyi) / (L_observed / L_random_ErdosRenyi)
  S_VigerLatapy <- (C_observed / C_random_VigerLatapy) / (L_observed / L_random_VigerLatapy)

  # Summarize to one data.table object
  results_smallworld[[prog]] <- data.table(
    programme = prog,
    clustering_observed = C_observed,
    clustering_ER = C_random_ErdosRenyi,
    clustering_VL = C_random_VigerLatapy,
    pathlength_observed = L_observed,
    pathlength_ER = L_random_ErdosRenyi,
    pathlength_VL = L_random_VigerLatapy,
    smallworld_ER = S_ErdosRenyi,
    smallworld_VL = S_VigerLatapy
  )

  message("Programme ", prog, " done in: ",
          round(difftime(Sys.time(), time_start, units = "secs"), 1), " seconds")
}

# Combine programme-specific computations into one data.table each
dt_components <- rbindlist(results_components)
dt_density <- rbindlist(results_density)
dt_cores <- rbindlist(results_cores)
dt_clustering <- rbindlist(results_clustering)
dt_pathlength <- rbindlist(results_pathlength)
dt_smallworld <- rbindlist(results_smallworld)


# Component structure: summary
dt_components_summary <- dt_components[, .(
  n_components = .N,
  giant_comp_size = max(size),
  giant_comp_pct = (max(size) / sum(size)) * 100
), by = programme]
print(dt_components_summary)

# Component structure: size distribution
plot_components_hist <-
  ggplot(dt_components[, .N, by = .(programme, size)],
         aes(x = size, y = N, fill = programme, color = programme)) +
  # Prevent transformation of y-values to infinite values by only visualizing the count
  # of actual appearances via 'geom_col()' instead of 'geom_histogram()'
  geom_col(position = "identity", alpha = 0.75) +
  scale_x_log10() +
  scale_y_log10(limits = c(1, 10000)) +
  scale_fill_manual(values = colorblindfriendly()) +
  scale_color_manual(values = colorblindfriendly(), guide = "none") +
  labs(x = "Größe der Komponente [log10]", y = "Anzahl an Komponenten [log10]",
       fill = "EU-Förderprogramm") +
  theme_lmu() +
  theme(legend.position = "top")
save_plot_lmu(plot_components_hist, "cohesion_components_histogram.png")
# Note: minimum and maximum bars represent the following size and count, respectively
dt_components[, .N, by = .(programme, size)][,
  .SD[c(which.min(size), which.max(size))], by = programme
  ]

# Density
print(dt_density)

# k-Cores: summary
dt_cores_summary <- dt_cores[, .(
  cores_mean = mean(coreness),
  cores_median = median(coreness),
  # Maximum core number, i.e. largest value k for which a non-empty k-core exists
  degenerate = max(coreness)
), by = programme]
print(dt_cores_summary)

# Scatter plot of coreness vs. degree
plot_coreness_degree <-
  ggplot(dt_cores[degree != 0], aes(x = degree, y = coreness)) +
  geom_point(size = 0.5, alpha = 0.1) +
  scale_x_log10() +
  labs(x = "Grad [log10]", y = "Zugehörigkeit zu einem k-Kern") +
  facet_wrap(~ programme) +
  theme_lmu()
save_plot_lmu(plot_coreness_degree, "cohesion_coreness_degree.png")

# Clustering coefficient: Validate weighted vs. unweighted local coefficient analogous to
# the centrality validation
dt_clustering_corr <- rbindlist(lapply(programmes, function(prog) {
  # Extract programme-specific values and remove NaNs
  dt_temp<- dt_clustering[(programme == prog) &
                            !is.na(coeff_local_unweighted) &
                            !is.na(coeff_local_weighted)]
  # Compute Spearman correlation between the two versions and add to data table overview
  data.table(
    programme = prog,
    spearman = round(cor(dt_temp$coeff_local_unweighted, dt_temp$coeff_local_weighted,
                         method = "spearman"), digits = 4)
  )
}))
print(dt_clustering_corr)
# Yes, extremely high correlation within both programmes (0.9990 and 0.9994)!

# Scatter plot of local clustering coefficient vs. degree
plot_clustering_degree <-
  # Restrict network to nodes of degree > 1, as clustering is rather meaningless below that
  ggplot(dt_clustering[degree > 1], aes(x = degree, y = coeff_local_unweighted)) +
  geom_point(size = 0.5, alpha = 0.1) +
  scale_x_log10(limits = c(1, 10000)) +
  labs(x = "Grad [log10]", y = "Lokaler Clustering-Koeffizient") +
  facet_wrap(~ programme) +
  theme_lmu()
save_plot_lmu(plot_clustering_degree, "cohesion_clustering_degree.png")

# Average path length and small-world coefficient
print(dt_pathlength)
print(dt_smallworld)

# Small-world coefficient comparison of observed values with reference graphs for both,
# the clustering coefficient and average path length
# TODO: 