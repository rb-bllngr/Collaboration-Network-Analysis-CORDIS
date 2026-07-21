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

# Get access variable saved separately
centrality_names <- setNames(names(centrality), sapply(centrality, "[[", "name"))

# Convert to long format data.table for faceting by measure and programme, later on
dt_roles_long <- melt(
  dt_roles,
  id.vars = c("programme", "n_coordinator"),
  measure.vars = sapply(centrality, FUN = "[[", "name"),
  variable.name = "column_temp",
  value.name = "value",
  variable.factor = FALSE,
  # Filter out all NAs, i.e. nodes outside the giant component
  na.rm = TRUE
)
dt_roles_long[, measure := factor(centrality_names[column_temp], levels = order_centrality)]
dt_roles_long[, column_temp := NULL]

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
  category_horizon = factor(category_horizon, levels = c("Nicht vorhanden", "Teilnehmer", "Koordinator"))
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
dt_persistence[,
  persistence := factor(persistence,
                        levels = c("Kein Koordinator", "Koordinator in H2020",
                                   "Koordinator in HORIZON", "Koordinator in H2020 und HORIZON"))
]

# Summary of persistence information as contingency table
dt_persistence_summary <- dt_persistence[, .(
  n = .N,
  pct = (.N / nrow(dt_persistence)) * 100
), by = .(category_h2020, category_horizon)]
print(dt_persistence_summary[order(category_h2020, category_horizon)])

# Determine change in degree centrality from H2020 to HORIZON for participating organisations
dt_persistence[,
               degree_change := fifelse(is.na(is_coordinator_h2020) == FALSE & is.na(is_coordinator_horizon) == FALSE,
                                        degree_horizon - degree_h2020, NA_real_)
]

# Self-written function to sort scatters by ranking inside quadrant cell
position_quadrant_cell <- function(values, movement = 0.49) {
  n <- length(values)
  n_columns <- ceiling(sqrt(n))
  n_rows <- ceiling(n / n_columns)

  # Rank descending, i.e. highest change in degree is first rank, lowest change in degree
  # last rank (unless NA --> placed at end). Ties ordered by first-come-first-placed.
  ranking <- frankv(values, order = -1, na.last = TRUE, ties.method = "first")
  row <- ceiling(ranking / n_columns)
  column <- ranking - (row - 1) * n_columns

  # Position values according to ranking
  list(
    x_coord = if (n_columns > 1) {
      scales::rescale(column, to = c(-movement, movement), from = c(1, n_columns))
    } else {
      rep(0, n)
    },
    y_coord = if (n_rows > 1) {
      scales::rescale(row, to = c(movement, -movement), from = c(1, n_rows))
    } else {
      rep(0, n)
    }
  )
}

# Variant No. 1: Place scatter points for the quadrant plot using uniformly-shifted jitters
dt_persistence[, ":=" (
  jitter_h2020 = as.numeric(category_h2020) + runif(.N, -0.4999, 0.4999),
  jitter_horizon = as.numeric(category_horizon) + runif(.N, -0.4999, 0.4999)
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

# Quadrant (3 x 3) plot of jittered scatters. Color symbolises the change in degree
# centrality from H2020 to HORIZON (no coloring if not present in one of the programmes)
plot_persistence_quadrant_jittered <-
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
  scale_alpha_manual(values = c(`TRUE` = 0.8, `FALSE` = 0.1), guide = "none") +
  labs(x = "H2020", y = "HORIZON",
       fill = "Änderung in\nGradzentralität\n[sym-log10\n+ lineare Null]") +
  theme_lmu() +
  theme(legend.title = element_text(hjust = 0.5))
save_plot_lmu(plot_persistence_quadrant_jittered, "roles_persistence_jittered.png")

# Variant No. 2: Place scatter points for quadrant plot using ranking-based written function
dt_persistence[, ":=" (
  movement_h2020 = position_quadrant_cell(degree_change)[[1]],
  movement_horizon = position_quadrant_cell(degree_change)[[2]]
), by = .(category_h2020, category_horizon)]
dt_persistence[, rankshift_h2020 := as.numeric(category_h2020) + movement_h2020]
dt_persistence[, rankshift_horizon := as.numeric(category_horizon) + movement_horizon]

# Quadrant (3 x 3) plot of rank-shifted scatters. Color symbolises the change in degree
# centrality from H2020 to HORIZON (no coloring if not present in one of the programmes)
plot_persistence_quadrant_rankshifted <-
  ggplot(dt_persistence, aes(x = rankshift_h2020, y = rankshift_horizon,
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
  scale_alpha_manual(values = c(`TRUE` = 0.8, `FALSE` = 0.1), guide = "none") +
  labs(x = "H2020", y = "HORIZON",
       fill = "Änderung in\nGradzentralität\n[sym-log10\n+ lineare Null]") +
  theme_lmu() +
  theme(legend.title = element_text(hjust = 0.5))
save_plot_lmu(plot_persistence_quadrant_rankshifted, "roles_persistence_rankshifted.png")

# Restrict the data set to fulfill the following conditions for the upcoming persistence
# check in centrality measures:
#     - Organisations that are present in NOT ONLY ONE BUT BOTH programmes
#     - Organisations that are part of giant component for NOT ONLY ONE BUT BOTH programmes
# This ensures comparability among measures and that persistence check is applied upon
# same set of organisations for all different scenarios (programme x centrality measure).
# So first, reduce to all organisations present in both programmes
dt_intersect <- dt_persistence[is.na(is_coordinator_h2020) == FALSE &
                                 is.na(is_coordinator_horizon) == FALSE]
# Then, reduce to all organisations that are part of giant component of both programmes
dt_intersect <- dt_intersect[in_giant_comp_h2020 == TRUE & in_giant_comp_horizon == TRUE]

# Extract the column names to be able to match them for reshaping of data.table
column_value_h2020 <- sapply(centrality, function(x) paste0(x$name, "_h2020"))
column_value_horizon <- sapply(centrality, function(x) paste0(x$name, "_horizon"))
column_rank_h2020 <- paste0("rank_", names(centrality), "_h2020")
column_rank_horizon <- paste0("rank_", names(centrality), "_horizon")

# Reshape centrality measure values in data.table to long format for faceting
dt_intersect_long_value <- melt(
  dt_intersect,
  id.vars = c("organisationID", "persistence"),
  measure.vars = list(value_h2020 = column_value_h2020, value_horizon = column_value_horizon),
  variable.name = "measure_index",
  variable.factor = FALSE
)
dt_intersect_long_value[, measure := factor(names(centrality)[as.integer(measure_index)],
                                      levels = order_centrality)]
dt_intersect_long_value[, measure_index := NULL]

# Analogous reshaping for ranking values
dt_intersect_long_rank <- melt(
  dt_intersect,
  id.vars = c("organisationID", "persistence"),
  measure.vars = list(rank_h2020 = column_rank_h2020, rank_horizon = column_rank_horizon),
  variable.name = "measure_index",
  variable.factor = FALSE
)
dt_intersect_long_rank[, measure := factor(names(centrality)[as.integer(measure_index)],
                                            levels = order_centrality)]
dt_intersect_long_rank[, measure_index := NULL]

# Scatter plot of HORIZON vs. H2020 normalised centrality values, faceted by measure
plot_persistence_centrality <-
  ggplot(dt_intersect_long_value, aes(x = value_h2020, y = value_horizon, color = persistence)) +
  geom_point(size = 0.5, alpha = 0.3) +
  geom_abline(slope = 1, intercept = 0, linetype = "twodash", color = "grey85") +
  scale_color_manual(values = c("#000000", colorblindfriendly())) +
  labs(x = "Normierte Zentralität in H2020", y = "Normierte Zentralität in HORIZON", color = NULL) +
  facet_wrap(~ measure, scales = "free", labeller = labeller(measure = mapping_centrality)) +
  theme_lmu() +
  theme(legend.position = "top") +
  guides(color = guide_legend(override.aes = list(alpha = 1, size = 2)))
save_plot_lmu(plot_persistence_centrality, "roles_persistence_centrality.png")

# Scatter plot of HORIZON vs. H2020 centrality percentile ranks, faceted by measure
plot_persistence_centrality_ranks <-
  ggplot(dt_intersect_long_rank, aes(x = rank_h2020, y = rank_horizon, color = persistence)) +
  geom_point(size = 0.5, alpha = 0.3) +
  geom_abline(slope = 1, intercept = 0, linetype = "twodash", color = "grey85") +
  scale_color_manual(values = c("#000000", colorblindfriendly())) +
  labs(x = "Perzentil H2020", y = "Perzentil HORIZON", color = NULL) +
  facet_wrap(~ measure, scales = "free", labeller = labeller(measure = mapping_centrality)) +
  theme_lmu() +
  theme(legend.position = "top") +
  guides(color = guide_legend(override.aes = list(alpha = 1, size = 2)))
save_plot_lmu(plot_persistence_centrality_ranks, "roles_persistence_centrality_ranks.png")

# Save one individual value plot per centrality measure, each faceted by persistence category
for (name in names(centrality)) {
  # Reduce data set to one specific measure
  dt_plot <- dt_intersect_long_value[measure == name]

  plot_single_measure <-
    ggplot(dt_plot, aes(x = value_h2020, y = value_horizon)) +
    geom_point(size = 0.5, alpha = 0.5) +
    geom_abline(slope = 1, intercept = 0, linetype = "twodash", color = "grey85") +
    facet_wrap(~ persistence) +
    theme_lmu()

  if (name == "betweenness") {
    # Calculate smallest value for normalised betweenness centrality to be sigma threshold
    # for pseudo-logarithmic scale, i.e. the value for which onward towards zero the values
    # are treated with linear not logarithmic scaling (ergo, closest not-zero point to zero)
    sigma <- min(
      dt_intersect_long_value[(measure == name) & (value_h2020 > 0), value_h2020],
      dt_intersect_long_value[(measure == name) & (value_horizon > 0), value_horizon]
    )
    betweenness_breaks <- c(1e-10, 1e-8, 1e-6, 1e-4, 1e-2)
    

    # Add specific scaling to plot
    plot_single_measure <- plot_single_measure +
      scale_x_continuous(transform = scales::pseudo_log_trans(sigma = sigma, base = 10),
                         breaks = betweenness_breaks,
                         labels = scales::trans_format("log10", scales::math_format(10^.x)),
                         name = paste0("Normierte ", mapping_centrality[[name]],
                                       "-Zentralität in H2020 [pseudo-log10]")) +
      scale_y_continuous(transform = scales::pseudo_log_trans(sigma = sigma, base = 10),
                         breaks = betweenness_breaks,
                         labels = scales::trans_format("log10", scales::math_format(10^.x)),
                         name = paste0("Normierte ", mapping_centrality[[name]],
                                       "-Zentralität in HORIZON [pseudo-log10]"))
  } else {
    plot_single_measure <- plot_single_measure +
      scale_x_log10(name = paste0("Normierte ", mapping_centrality[[name]],
                                  "-Zentralität in H2020 [log10]"),
                    labels = scales::label_number(drop0trailing = TRUE)) +
      scale_y_log10(name = paste0("Normierte ", mapping_centrality[[name]],
                                  "-Zentralität in HORIZON [log10]"),
                    labels = scales::label_number(drop0trailing = TRUE))
  }

  # Save resulting plots individually named according to centrality measure
  save_plot_lmu(plot_single_measure, paste0("roles_persistence_centrality_", name, ".png"))
}
##########################################################################################
# TODO: FIND THE BEST WAY TO DETERMINE SCALING AND AXIS-RANGE FOR ALL FOUR MEASURES ######
##########################################################################################