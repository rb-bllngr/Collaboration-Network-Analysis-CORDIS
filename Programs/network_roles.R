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

# Compute percentile ranks [0, 1] per programme for each centrality measure. Restrict the
# computation on the giant component for all four measures due to comparability reasons.
for (measure in names(centrality)) {
  # Create column for rank
  rank <- paste0("rank_", measure)
  dt_roles[, (rank) := NA_real_]
  # Take subset of data.table that contains only the columns names in .SDcols argument,
  # converting it to a temporary single column data.table of the unweighted and normalised
  # centrality measure in that for-loop iteration which is then ranked
  dt_roles[in_giant_comp == TRUE, (rank) := frank(.SD[[1]], ties.method = "average") / .N,
           by = programme, .SDcols = centrality[[measure]]$name]
}

# Full outer join merge of the two programmes to keep all the organisations that are present
# in at least one network to analyse their role persistence from one to the other and mark
# newly/not anymore participating organisations
dt_persistence <- merge(dt_roles[programme == "H2020"], dt_roles[programme == "HORIZON"],
                        by = "organisationID", all = TRUE, suffixes = c("_h2020", "_horizon"))
# Note: Reminder that these are only the organisations which are part of giant component!

# Check how many organisations originate from each of the networks
message("Organisations only in H2020: ", dt_persistence[is.na(is_coordinator_horizon), .N],
        "\n",
        "Organisations only in HORIZON: ", dt_persistence[is.na(is_coordinator_h2020), .N],
        "\n",
        "Organisations in both: ", dt_persistence[!is.na(is_coordinator_h2020) &
                                                    !is.na(is_coordinator_horizon), .N])

# Determine per programme in what categorical status organisation at hand falls under:
#     - present as coordinator
#     - present as non-coordinator
#     - not present
dt_persistence[, ":=" (
  category_h2020 = fcase(
    # If condition fulfilled ----- then:
    is_coordinator_h2020 == TRUE, "Koordinator",
    is_coordinator_h2020 == FALSE, "Teilnehmer",
    default = "Nicht vorhanden"
  ),
  category_horizon = fcase(
    is_coordinator_horizon == TRUE, "Koordinator",
    is_coordinator_horizon == FALSE, "Teilnehmer",
    default = "Nicht vorhanden"
  )
)]

# Convert status categories into factors for faceting
dt_persistence[, ":=" (
  category_h2020 = factor(category_h2020, levels = c("Nicht vorhanden", "Teilnehmer", "Koordinator")),
  category_horizon = factor(category_horizon, level = c("Nicht vorhanden", "Teilnehmer", "Koordinator"))
)]

# Group further for persistence in coordinating activities IF AND ONLY IF organisation is
# present in both programmes
dt_persistence[, persistence := fcase(
  # Organisation is not present in both progammes
  is.na(is_coordinator_h2020) | is.na(is_coordinator_horizon), NA_character_,
  # Organisation is present and acts as coordinator in both programmes
  is_coordinator_h2020 == TRUE & is_coordinator_horizon == TRUE, "Koordinator in H2020 und HORIZON",
  # Organisation is present in both programmes but acts as coordinator in just one of them
  is_coordinator_h2020 == TRUE & is_coordinator_horizon == FALSE, "Koordinator in H2020",
  is_coordinator_h2020 == FALSE & is_coordinator_horizon == TRUE, "Koordinator in HORIZON",
  # Fallback case, which picks up all organisations present in both programmes but in
  # neither of them the organisation acts as coordinator
  default = "Kein Koordinator"
)]

# Summary of persistence information as contingency table
dt_persistence_summary <- dt_persistence[, .(
  n = .N,
  pct = (.N / nrow(dt_persistence)) * 100
), by = .(category_h2020, category_horizon)]
print(dt_persistence_summary[order(category_h2020, category_horizon)])

# Determine change in degree centrality from H2020 to HORIZON for participating organisations
dt_persistence[, ":=" (
  jitter_h2020 = as.numeric(category_h2020) + runif(.N, -0.4999, 0.4999),
  jitter_horizon = as.numeric(category_horizon) + runif(.N, -0.4999, 0.4999),
  degree_change =
    fifelse(is.na(is_coordinator_h2020) == FALSE & is.na(is_coordinator_horizon) == FALSE,
            degree_horizon - degree_h2020, NA_real_)
)]

# Build mirrored (i.e. symmetric) logarithmic scale for the coloring of degree change
#     - log10(|x|) with the original sign (+/-) of x preserved by multiplying sign(x)
#     - special case to 0 at x = 0 (as its the only remaining value undefined for log10)
dt_persistence[, degree_change_symlog := fifelse(
  is.na(degree_change), yes = NA_real_, no = fifelse(
    degree_change == 0, yes = 0, no = sign(degree_change) * log10(abs(degree_change)))
)]
symlog_limit <- max(abs(dt_persistence$degree_change_symlog), na.rm = TRUE)
degree_change_breaks <- c(-1000, -100, -10, 0, 10, 100, 1000)
symlog_breaks <- fifelse(degree_change_breaks == 0, yes = 0,
                         no = sign(degree_change_breaks) * log10(abs(degree_change_breaks)))

# Quadrant (3 x 3) plot of jittered scatters. color symbolises the change in normalised
# degree centrality from H2020 to HORIZON (no coloring if not present in either programme)
plot_persistence_quadrant <-
  ggplot(dt_persistence, aes(x = jitter_h2020, y = jitter_horizon,
                             fill = degree_change_symlog,
                             alpha = (category_h2020 != "Nicht vorhanden") &
                               (category_horizon != "Nicht vorhanden"))) +
  geom_point(shape = 21, color = lmu_default_color(), size = 1, stroke = 0.15) +
  geom_vline(xintercept = c(1.5, 2.5), linetype = "solid", color = lmu_default_color()) +
  geom_hline(yintercept = c(1.5, 2.5), linetype = "solid", color = lmu_default_color()) +
  scale_x_continuous(breaks = 1:3, labels = levels(dt_persistence$category_h2020),
                     limits = c(0.5, 3.5), expand = c(0, 0)) +
  scale_y_continuous(breaks = 1:3, labels = levels(dt_persistence$category_horizon),
                     limits = c(0.5, 3.5), expand = c(0, 0)) +
  scale_fill_gradientn(colors = RColorBrewer::brewer.pal(11, "RdBu"),
                       limits = c(-symlog_limit, symlog_limit),
                       breaks = symlog_breaks,
                       labels = degree_change_breaks,
                       na.value = lmu_default_color()) +
  scale_alpha_manual(values = c(`TRUE` = 0.75, `FALSE` = 0.1), guide = "none") +
  labs(x = "H2020", y = "HORIZON", fill = "Änderung") +
  theme_lmu()
