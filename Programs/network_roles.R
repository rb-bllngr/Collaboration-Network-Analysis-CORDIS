# network_roles.R: Investigate the organizations' roles in projects they participate in.
#                  Look at project coordinators and their relationship to an organisations
#                  centrality/degree for H2020 and HORIZON EUROPE collaboration networks.

# Load networks for each programme (take unweighted version for vertex attributes for
# 'n_coord' and 'n_proj') and the centrality results
dt_centrality <- readRDS(file.path(PATHS$DATA_INT, "centrality.RDS"))
networks <- list(H2020 = readRDS(file.path(PATHS$DATA_INT, "network_h2020.RDS"))$unweighted,
                 HORIZON = readRDS(file.path(PATHS$DATA_INT, "network_horizon.RDS"))$unweighted)
programmes <- names(networks)

# Extract information about coordinator role into programme-specific data.tables
dt_roles <- rbindlist(lapply(programmes, function(prog) {
  data.table(
    programme = prog,
    organisationID = V(networks[[prog]])$name,
    n_projects = V(networks[[prog]])$n_proj,
    n_coordinator = V(networks[[prog]])$n_coord
  )
}))

# Derive new variables from that information:
#   - Has an organisation ever had coordinator status? (binary, Yes = 1/No = 0),
#   - Coordination share = proportion of its projects organisation acted as coordinator on
dt_roles[, ":=" (
  is_coordinator = n_coordinator > 0,
  coordinator_share = n_coordinator / n_projects
)]

# Merge coordination information with the centrality results
dt_roles <- merge(dt_roles, dt_centrality, by = c("programme", "organisationID"))

# Summarize the centrality measures which shall be compared to coordination information in
# addition to flag, whether to use giant component or full graph.
centrality <- list(
  degree = list(name = "degree_norm", giant_comp = FALSE),
  betweenness = list(name = "betweenness_unweighted_norm", giant_comp = FALSE),
  closeness = list(name = "closeness_unweighted", giant_comp = TRUE),
  eigenvector = list(name = "eigenvector_unweighted", giant_comp = TRUE)
)

# Convert to long format data.table for faceting by measure and programme, later on
dt_roles_long <- rbindlist(lapply(names(centrality), function(measure) {
  # Filter to giant component-only if individual measure is restricted to it
  if(centrality[[measure]]$giant_comp == TRUE) {
    dt_measure <- dt_roles[in_giant_comp == TRUE]
  } else {
    dt_measure <- dt_roles
  }

  # Reshape the data.table
  data.table(
    programme = dt_measure$programme,
    n_coordinator = dt_measure$n_coordinator,
    measure = measure,
    value = dt_measure[[centrality[[measure]]$name]]
  )
}))

# Determine order for centrality measures
dt_roles_long[, measure := factor(measure, levels = order_centrality)]

# Scatter plots of each centrality measure vs. coordinator counts, programme-specific 
plot_coordinator_centrality <-
  ggplot(dt_roles_long, aes(x = n_coordinator, y = value)) +
  geom_point(size = 0.5, alpha = 0.2) +
  scale_x_continuous(transform = scales::pseudo_log_trans(base = 10),
                     breaks = c(0, 1, 10, 100, 1000)) +
  scale_y_continuous(labels = scales::label_number(accuracy = 0.01)) +
  labs(x = "Anzahl Organisation in Koordinatorrolle [pseudo-log10]",
       y = "Normierte Zentralität") +
  facet_grid(measure ~ programme, scales = "free_y",
             labeller = labeller(measure = mapping_centrality)) +
  theme_lmu()
save_plot_lmu(plot_coordinator_centrality, "roles_centrality_coordinator.png",
              width = 8, height = 8)

# Alternatively, one plot per centrality measure, each with programme as vertical faceting
# plot_coordinator_centrality <- lapply(order_centrality, function(m) {
#   # Build plot for specific 'measure', where you have a shared y-axis between the H2020
#   # and HORIZON programme within that measure, but free y-axis across measures for scaling
#   ggplot(dt_roles_long[measure == m], aes(x = n_coordinator, y = value)) +
#     geom_point(size = 0.5, alpha = 0.2) +
#     scale_x_continuous(transform = scales::pseudo_log_trans(base = 10),
#                        breaks = c(0, 1, 10, 100, 1000)) +
#     scale_y_continuous(labels = scales::label_number(accuracy = 0.01)) +
#     labs(x = NULL,
#          # Give only the first (leftmost) panel the y-axis label
#          y = if (m == order_centrality[1]) "Zentralität" else NULL,
#          title = mapping_centrality[[m]]) +
#     facet_grid(programme ~ .) +
#     theme_lmu() +
#     theme(plot.title = element_text(hjust = 0.5, size = 12, face = "plain"))
#   # Note: Use pseudo-log scaling to refrain from excluding organisations with zero
#   # coordinator roles to avoid transformation to infinite values due to log-scaling.
#   # Pseudo-log behaves like log10 for larger values but transitions smoothly to linear for
#   # values near zero, i.e. zero organisations plot at their true x = 0 position
# })
# # Combine single measure plots into 2 x 4 grid using 'patchwork'
# plot_coordinator_centrality_complete <-
#   wrap_plots(plot_coordinator_centrality, ncol = 1) +
#   plot_annotation(
#     caption = "Anzahl Organisation in Koordinatorrolle [pseudo-log10]"
#   ) & 
#   theme(plot.caption = element_text(hjust = 0.5, size = 12))
# 
# Alternatively without patchwork and fixed y-axis scale:
# ggplot(dt_roles_long, aes(x = n_coordinator + 1, y = value)) +
# geom_point(size = 0.5, alpha = 0.1) +
# scale_x_log10() +
# labs(x = "Anzahl Organisation in Koordinatorrolle [pseudo-log10]",
#      y = NULL) +
# facet_grid(programme ~ measure, scales = "free_y",
#            labeller = labeller(measure = mapping_centrality)) +
# theme_lmu()

# Spearman correlation between coordination share/coordinator role count and each centrality
# measure, programme-specific (for closeness/eigenvector centrality, giant component-only).
# Answers the question: Does more coordination relate to higher centrality?
dt_roles_corr <- rbindlist(lapply(programmes, function(prog) {
    # Reduce data set to giant component subset within each programme (analogous to same
    # behaviour for the centrality correlation comparison in 'network_centrality.R')
    dt_giant_comp <- dt_roles[(programme == prog) & (in_giant_comp == TRUE)]

    # Compute correlation between centrality measures and coordination information
    rbindlist(lapply(names(centrality), function(measure) {
      data.table(
        programme = prog,
        measure = measure,
        corr_n_coordinator = round(cor(dt_giant_comp$n_coordinator,
                                dt_giant_comp[[centrality[[measure]]$name]],
                                method = "spearman"), digits = 4),
        corr_coordinator_share = round(cor(dt_giant_comp$coordinator_share,
                                           dt_giant_comp[[centrality[[measure]]$name]],
                                           method = "spearman"), digits = 4)
      )
    }))
}))
print(dt_roles_corr)

# Hypothesis Test (cf. Statistical Inference I, Theory Sheet 5 'Testing')
# --> Two samples --> Non-parametric tests --> Wilcoxon Rank Sum Test
# !!! Caution !!!: Exclusively exploratory/descriptive, NOT as inferential, due to network
#                  autocorrelation undermining validity of hypothesis testing!
dt_wilcoxon <- rbindlist(lapply(programmes, function(prog) {
    # Reduce data set to giant component subset within each programme
    dt_giant_comp <- dt_roles[(programme == prog) & (in_giant_comp == TRUE)]

    rbindlist(lapply(names(centrality), function(measure) {
      # Perform Wilcoxon rank sum test on all centrality measures between the two groups
      #     - organisations to at least once coordinate project (is_coordinator == TRUE),
      #     - organisations to never coordinate project (is_coordinator == FALSE)
      test <-wilcox.test(dt_giant_comp[[centrality[[measure]]$name]]
                         ~ dt_giant_comp$is_coordinator,
                         alternative = "two.sided", conf.level = 0.95, conf.int = TRUE)

      # Summarize results into one data.table
      data.table(
        programme = prog,
        measure = measure,
        median_coordinatorYES = median(dt_giant_comp[is_coordinator == TRUE][[centrality[[measure]]$name]]),
        median_corrdinatorNO = median(dt_giant_comp[is_coordinator == FALSE][[centrality[[measure]]$name]]),
        confidence = list(test$conf.int),
        p = test$p.value
      )
    }))
}))
print(dt_wilcoxon)
# Note: Apparent p-values of (almost) zero would imply extreme significance but important
# to state large, non-independent n could work in exact connection to network autocorrelation!
