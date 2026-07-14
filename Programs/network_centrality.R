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
programmes <- c("H2020", "HORIZON")

# Initialize list for results
results <- list()

# Decide whether checkpoint .RDS files for expensive intermediate computations should be
# recomputed --> set TRUE for re-computation! (here: betweenness and closeness centrality)
recompute <- FALSE

for (prog in programmes) {
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

# Derive some summary statistics on centrality measurements
dt_summary <- dt_centrality[, .(
  n_nodes = .N,
  n_giant_comp = sum(in_giant_comp),
  pct_giant_comp = mean(in_giant_comp) * 100,

  degree_mean = mean(degree),
  between_weighted_mean = mean(betweenness_weighted_norm),
  close_weighted_mean = mean(closeness_weighted, na.rm = TRUE),
  close_unweighted_mean = mean(closeness_unweighted, na.rm = TRUE),
  eigenv_weighted_mean = mean(eigenvector_weighted, na.rm = TRUE),
  eigenv_unweighted_mean = mean(eigenvector_unweighted, na.rm = TRUE)
), by = programme]
print(dt_summary)

# Spearman correlation between weighted and unweighted version of each of the centrality
# measures per programme (constrained to giant component for comparability with closeness
# and eigenvector centrality)
correlation <- list()

dt_correlation <- rbindlist(lapply(programmes, function(prog) {
  # Reduce data set to giant component subset within each programme
  dt_giant_comp <- dt_centrality[(programme == prog) & (in_giant_comp == TRUE)]

  # Compute correlation within each measurement and save to data.table for overview
  data.table(
    programme = prog,
    betweenness = round(
      cor(dt_giant_comp$betweenness_weighted, dt_giant_comp$betweenness_unweighted,
          method = "spearman"), digits = 4),
    closeness = round(
      cor(dt_giant_comp$closeness_weighted, dt_giant_comp$closeness_unweighted,
          method = "spearman"), digits = 4),
    eigenvector = round(
      cor(dt_giant_comp$eigenvector_weighted, dt_giant_comp$eigenvector_unweighted,
          method = "spearman"), digits = 4)
  )
}))
print(dt_correlation)

# Further check correlation results: equivalence at top ranks? Run the check for closeness,
# betweenness, and eigenvector centrality for context
# Note: confidence interval via bootstrapping would be highly driven by size of n, which
# is rather high here, making the confidence interval narrow around point estimate.
top_ranks_n <- c(25, 50, 100, 250, 1000)
measures <- list(betweenness = c("betweenness_weighted", "betweenness_unweighted"),
                 closeness = c("closeness_weighted", "closeness_unweighted"),
                 eigenvector = c("eigenvector_weighted", "eigenvector_unweighted"))

dt_giant_comp <- dt_centrality[in_giant_comp == TRUE]  # Reset to non-programme-specific

dt_overlap <- rbindlist(lapply(programmes, function(prog) {
  dt_prog <- dt_giant_comp[programme == prog]

  rbindlist(lapply(top_ranks_n, function(n) {
    rbindlist(lapply(names(measures), function(measure) {
      ranks_weighted <- dt_prog[order(dt_prog[[measures[[measure]][1]]],
                                      decreasing = TRUE)][seq_len(n), organisationID]
      ranks_unweighted <- dt_prog[order(dt_prog[[measures[[measure]][2]]],
                                        decreasing = TRUE)][seq_len(n), organisationID]

      data.table(
        programme = prog,
        measure = measure,
        top_ranks_n = n,
        overlap = length(intersect(ranks_weighted, ranks_unweighted)) / n
      )
    }))
  }))
}))
print(dt_overlap[order(programme, measure, top_ranks_n)])

# Four-way correlation among all centrality measures
dt_fourway_long <- rbindlist(lapply(programmes, function(prog) {
  # Assemble all normalised and unweighted measure variants for the four-way-comparison
  dt_fourway <- dt_centrality[(programme == prog) & (in_giant_comp == TRUE),
                              .(degree_norm,
                                betweenness_unweighted_norm,
                                closeness_unweighted,
                                eigenvector_unweighted)]
  
  message("\n Spearman correlation for ", prog, ":")
  correlation_matrix_fourway <- cor(dt_fourway, method = "spearman")
  print(correlation_matrix_fourway)
  
  # Reshape the correlation matrix for visualisation purposes by converting to data.table
  # with row names explicitly kept as column, then transform from wide to long format
  dt_fourway_wide <- as.data.table(correlation_matrix_fourway, keep.rownames = "centrality1")
  dt_fourway_corr <- melt(dt_fourway_wide,
                          id.vars = "centrality1",
                          variable.name = "centrality2",
                          value.name = "corr")
  dt_fourway_corr[, programme := prog]
  
  # Order the centrality measure, so they appear in determined order in plots
  centr_order <- c("degree_norm", "betweenness_unweighted_norm", "closeness_unweighted", "eigenvector_unweighted")
  dt_fourway_corr[, ":="(
    centrality1 = factor(centrality1, levels = centr_order),
    centrality2 = factor(centrality2, levels = rev(centr_order))
  )]
  dt_fourway_corr
}))

# Define German names for axes in plots
mapping_axis <- c("degree_norm" = "Grad",
                  "betweenness_unweighted_norm" = "Betweenness",
                  "closeness_unweighted" = "Closeness",
                  "eigenvector_unweighted" = "Eigenvector")

# Correlation heatmap visualisation, faceted by programme
plot_corr_heatmap <-
  ggplot(dt_fourway_long, aes(x = centrality1, y = centrality2, fill = corr)) +
  geom_tile() +
  geom_text(aes(label = round(corr, 2)), color = lmu_colors$white, size = 3) +
  scale_fill_gradientn(colors = RColorBrewer::brewer.pal(11, "RdBu"), limits = c(-1, 1)) +
  scale_x_discrete(labels = mapping_axis) +
  scale_y_discrete(labels = mapping_axis) +
  labs(x = NULL, y = NULL, fill = "Spearman-Korrelationskoeffizient") +
  facet_wrap(~ programme) +
  theme_lmu() +
  theme(legend.position = "bottom",
        legend.direction = "horizontal",
        legend.title = element_text(vjust = 0.8))
save_plot_lmu(plot_corr_heatmap, "centrality_correlation_matrix.png")

# Scatter plot of degree vs. betweenness to investigate the question: Are high-degree
# organisations simultaneously bridges? Take normalised degree and betweenness centrality
# for inter-programme comparison; exclude isolated organisations, as both measures are zero
plot_degree_betweenness <-
  ggplot(dt_centrality[degree != 0],
         aes(x = degree_norm, y = betweenness_unweighted_norm)) +
  geom_point(size = 0.5, alpha = 0.25) +
  scale_x_log10(labels = scales::label_number(drop0trailing = TRUE)) +
  labs(x = "Normierte Grad-Zentralität [log10]",
       y = "Normierte Betweenness-Zentralität") +
  facet_wrap(~ programme) +
  theme_lmu()
save_plot_lmu(plot_degree_betweenness, "centrality_degree_betweenness.png")

# Scatter plot of degree vs. eigenvector to investigate the question: Are high-degree
# organisations simultaneously connected to other important organisations? Take normalised
# degree centrality (eigenvector is automatically scaled to maximum of 1) for inter-programme
# comparison; exclude isolated organisation and restrict to giant component, as eigenvector
# only well-defined for giant component
plot_degree_eigenvector <-
  ggplot(dt_centrality[(degree != 0) & (in_giant_comp == TRUE)],
         aes(x = degree_norm, y = eigenvector_unweighted)) +
  geom_point(size = 0.5, alpha = 0.25) +
  scale_x_log10(labels = scales::label_number(drop0trailing = TRUE)) +
  # Switch from decimal to exponent display for y-axis as small negative exponents could
  # still be represented nicely, but large negative exponents will not
  scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
                labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  labs(x = "Normierte Grad-Zentralität [log10]",
       y = "Normierte Eigenvektor-Zentralität [log10]") +
  facet_wrap(~ programme) +
  theme_lmu()
save_plot_lmu(plot_degree_eigenvector, "centrality_degree_eigenvector.png")
