# network_degrees.R: Degree, degree distribution, and degree correlation analysis for the
#                    H2020 and HORIZON EUROPE collaboration networks.

# Load networks for each programme (take weighted version for possibility to extract
# information on graph strength)
h2020 <- readRDS(file.path(PATHS$DATA_INT, "network_h2020.RDS"))$weighted
horizon <- readRDS(file.path(PATHS$DATA_INT, "network_horizon.RDS"))$weighted
networks <- list(H2020 = h2020, HORIZON = horizon)

# Compute degrees and strength for both programme networks and connect into one table
dt_metrics <- rbindlist(list(
  data.table(programme = "H2020",
             degree = degree(h2020),
             strength = strength(h2020),
             knn = knn(h2020)$knn),
  data.table(programme = "HORIZON",
             degree = degree(horizon),
             strength = strength(horizon),
             knn = knn(horizon)$knn)
))

# Compute degree distribution for both programme networks and connect into one table
# Note: 'degree_distribution()' ensures one entry per integer degree value from zero to
# the maximum, including unobserved degrees with zero probability. These entries are
# sorted. That's why subtract 1 to convert from 1-based R index to 0-based degree value.
h2020_degree_distrib <- degree_distribution(h2020)
horizon_degree_distrib <- degree_distribution(horizon)
dt_degree_distrib <- rbindlist(list(
  data.table(programme = "H2020",
             degree = seq_along(h2020_degree_distrib) - 1,
             prob = h2020_degree_distrib),
  data.table(programme = "HORIZON",
             degree = seq_along(horizon_degree_distrib) - 1,
             prob = horizon_degree_distrib)
))

# Filter out unobserved degree values (i.e. probability equals zero)
dt_degree_distrib <- dt_degree_distrib[prob != 0 & degree != 0]

# Fit against power law and compute Hill-estimator
powerlawfit <- lapply(networks, function(net) {fit_power_law(degree(net)[degree(net) > 0])})
powerlaw_summary <- rbindlist(list(
  data.table(programme = "H2020",
             alpha = powerlawfit$H2020$alpha,
             xmin = powerlawfit$H2020$xmin,
             KSstat = powerlawfit$H2020$KS.stat),
  data.table(programme = "HORIZON",
             alpha = powerlawfit$HORIZON$alpha,
             xmin = powerlawfit$HORIZON$xmin,
             KSstat = powerlawfit$HORIZON$KS.stat)
))

# Compute Hill estimator for each k (= number of order statistics used) from 1 to n - 1
dt_hill_estimator <- rbindlist(lapply(networks, function(net) {
  # Sorted degree sequence
  degrees <- sort(degree(net)[degree(net) > 0])
  n <- length(degrees)

  # Direct implementation of formula (4.4) from Kolaczyk (2010) to compute estimated alpha
  alpha_k <- sapply(seq_len(n - 1), function(k) {
    d_Nv_minus_i <- degrees[(n - k + 1):n]
    d_Nv_minus_k <- degrees[n - k]
    gamma_k <- mean(log(d_Nv_minus_i / d_Nv_minus_k))

    # Return alpha value using computed gamma value
    1 + (1 / gamma_k)
  })
  # Return data.table object with alpha for each k (and keep programme-column)
  data.table(k = seq_len(n - 1), alpha_k = alpha_k)}), idcol = "programme")

# Build joint degree distribution according to Kolaczyk (2010), pp. 86-87. 
dt_joint_degrees <- rbindlist(lapply(networks, function(net) {
  # Get degree of all edges for both nodes connected by the edge
  edges_degrees <- as_edgelist(net, names = FALSE)
  degrees <- degree(net)

  # Return data.table with degree of one end and degree of other end of every edge
  data.table(d1 = c(degrees[edges_degrees[, 1]], degrees[edges_degrees[, 2]]),
             d2 = c(degrees[edges_degrees[, 2]], degrees[edges_degrees[, 1]]))
}), idcol = "programme")

# Calculate relative frequency of each degree combination
# Alternatively: Following Kolaczyk (2010)'s description, first to sort the nodes in all
# edges to receive d_start <= d_end
# dt_joint_degrees[, c("d_start", "d_end") := .(pmin(d_start, d_end), pmax(d_start, d_end))]
dt_joint_degrees <- dt_joint_degrees[, .N, by = .(programme, d1, d2)]
dt_joint_degrees[, freq := N / sum(N), by = programme]

# Compute summary statistics (and more) on degree and strength information
dt_summary <- dt_metrics[, .(
  n_nodes = .N,
  n_isolated = sum(degree == 0),
  pct_isolated = (sum(degree == 0) / .N) * 100,

  deg_mean = mean(degree),
  deg_median = median(degree),
  deg_max = max(degree),
  deg_sd = sd(degree),

  str_mean = mean(strength),
  str_median = median(strength),
  str_max = max(strength),
  str_sd = sd(strength)
), by = programme]
print(dt_summary)

# Histogram for degrees
plot_degree_hist <-
  ggplot(dt_metrics[degree != 0], aes(x = degree, fill = programme)) +
  geom_histogram(position = "identity", alpha = 0.75, binwidth = 0.1) +
  # Alternatively:
  # geom_histogram(aes(y = after_stat(density)), position = "identity", alpha = 0.75, binwidth = 0.1) +
  # geom_density(position = "identity", alpha = 0.75) +
  scale_x_log10() +
  scale_fill_manual(values = colorblindfriendly()) +
  labs(x = "Grad [log10]", y = "Anzahl Organisationen", fill = "EU-Förderprogramm") +
  theme_lmu() +
  theme(legend.position = "top")
save_plot_lmu(plot_degree_hist, "degree_histogram.png")

# Histogram for strength
plot_strength_hist <-
  ggplot(dt_metrics[strength != 0], aes(x = strength, fill = programme)) +
  geom_histogram(position = "identity", alpha = 0.75, binwidth = 0.1) +
  scale_x_log10() +
  scale_fill_manual(values = colorblindfriendly()) +
  labs(x = "Stärke [log10]", y = "Anzahl Organisationen", fill = "EU-Förderprogramm") +
  theme_lmu() +
  theme(legend.position = "top")
save_plot_lmu(plot_strength_hist, "strength_histogram.png")
# Note: Looks basically identical to degree histogram. Validate plausibility by checking
# whether the majority of organisations are one-off, i.e. most edge weights are close to 1
# (meaning most pairs of organisations only collaborated on one project together) because
# strength = degree x average edge weight per node.
dt_metrics[, mean(strength / degree, na.rm = TRUE), by = programme]
# Note: Ratio is indeed close to 1, histogram seems plausible!

# Log-log degree distribution
plot_degree_distrib <-
  ggplot(dt_degree_distrib, aes(x = degree, y = prob, color = programme)) +
  geom_point(size = 1, alpha = 0.5) +
  scale_x_log10() +
  scale_y_log10(labels = scales::label_number(drop0trailing = TRUE)) +
  scale_color_manual(values = colorblindfriendly()) +
  labs(x = "Grad [log10]", y = "Relative Häufigkeit [log10]", color = "EU-Förderprogramm") +
  theme_lmu() +
  theme(legend.position = "top") +
  guides(color = guide_legend(override.aes = list(alpha = 1, size = 2)))

# Investigate low-degree fall-off from the log-log distribution expectation looking at the
# role of project size and one-off participation:
cordis <- readRDS(file.path(PATHS$DATA_INT, "cordis_population.RDS"))
dt_project_size <- cordis[, .(project_size = uniqueN(organisationID)),
                          by = .(frameworkProgramme, projectID)]

# Attach project size to every organisation-project row and aggregate to organisation-level
dt_organisations_projects <- merge(cordis[, .(frameworkProgramme, projectID, organisationID)],
                                   dt_project_size, by = c("frameworkProgramme", "projectID"))
dt_organisations_projects <- dt_organisations_projects[, .(
  consortium_mean = mean(project_size),
  consortium_med = median(project_size)
), by = .(frameworkProgramme, organisationID)]

# Rebuild degree information with 'organisationID' retained ('dt_metrics' drops it) and
# merge with consortium information
dt_degree <- rbindlist(list(
  data.table(programme = "H2020", organisationID = V(h2020)$name, degree = degree(h2020),
             n_projects = V(h2020)$n_proj),
  data.table(programme = "HORIZON", organisationID = V(horizon)$name, degree = degree(horizon),
             n_projects = V(horizon)$n_proj)
))
dt_degree <- merge(dt_degree, dt_organisations_projects,
                   by.x = c("programme", "organisationID"),
                   by.y = c("frameworkProgramme", "organisationID"))

# Flag organisations that only participated in exactly one project
dt_degree[, onetime := (n_projects == 1)]

# Bin the degrees for tabular summary focused on the low-degree region
dt_degree[, bin := cut(degree, breaks = c(-1, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 25, 50, 100, Inf),
                       labels = c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10",
                                  "11-25", "26-50", "51-100", "100+"))]
dt_bin_summary <- dt_degree[degree != 0, .(
  n_organisations = .N,
  pct_onetime = mean(onetime) * 100,
  consortium_mean_mean = mean(consortium_mean)
), by = .(programme, bin)]
print(dt_bin_summary[order(programme, bin)])

# Scatter plot of mean consortium size vs. degree
plot_consortium_degree <-
  ggplot(dt_degree[degree != 0], aes(x = degree, y = consortium_mean, color = onetime)) +
  geom_point(size = 1, alpha = 0.15) +
  scale_x_log10() +
  scale_y_log10() +
  scale_color_manual(values = colorblindfriendly(),
                     labels = c("Teilnahme an mehreren Projekten", "Teilnahme an einem Projekt")) +
  labs(color = NULL, x = "Grad [log10]",
       y = "Mittlere Anzahl beteiligter Organisationen pro Projekt [log10]") +
  facet_wrap(~ programme) +
  theme_lmu() +
  theme(legend.position = "top") +
  guides(color = guide_legend(override.aes = list(alpha = 1, size = 2)))

# Compute for each degree value the share of organisations that are one-time participants
dt_crossover <- dt_degree[degree != 0, .(
  pct_onetime = mean(onetime) * 100,
  n_organisations = .N
), by = .(programme, degree)]
setorder(dt_crossover, programme, degree)

# Identify first degree value exceeding (i.e. falling below) the 50% majority threshold
# Note: Starting at 11 to skip past the naturally high one-time shares among low-degree
#       organisations (c. 'dt_bin_summary')
dt_crossover_summary <- dt_crossover[degree > 10 & pct_onetime <= 50][, .SD[1], by = programme]
print(dt_crossover_summary)

# Overlay crossover point on the degree distribution plot
plot_degree_distrib <- plot_degree_distrib +
  geom_vline(data = dt_crossover_summary, aes(xintercept = degree, color = programme),
             linetype = "twodash")
save_plot_lmu(plot_degree_distrib, "degree_distribution.png")

# Add the theoretical one-time identity line to the consortium-degree plot, i.e. for an
# organisation with exactly one project, degree = consortium - 1 by definition, therefore
# consortium = degree + 1
plot_consortium_degree <- plot_consortium_degree +
  geom_function(fun = function(x) x + 1, color = lmu_default_color(), linetype = "twodash")
save_plot_lmu(plot_consortium_degree, "degree_consortiumsize_lowdegree.png")

# Investigate neighbour-degree behaviour among organisations by plotting knn-degree
plot_degree_knn <-
  ggplot(dt_metrics[!is.na(knn) & degree != 0],
         aes(x = degree, y = knn, color = programme)) +
  geom_point(size = 1, alpha = 0.1) +
  scale_x_log10() +
  scale_y_log10() +
  scale_color_manual(values = colorblindfriendly()) +
  labs(x = "Grad [log10]",
       y = "Mittlerer Grad der k-nächsten Nachbarn [log10]",
       color = "EU-Förderprogramm") +
  theme_lmu() +
  theme(legend.position = "top") +
  guides(color = guide_legend(override.aes = list(alpha = 1, size = 2)))
save_plot_lmu(plot_degree_knn, "degree_knn.png")

# Hill plot
plot_hill <-
  ggplot(dt_hill_estimator, aes(x = k, y = alpha_k, color = programme)) +
  geom_point(size = 1, alpha = 0.5) +
  scale_color_manual(values = colorblindfriendly()) +
  labs(x = "k", y = expression(hat(alpha)[k]), color = "EU-Förderprogramm") +
  theme_lmu() +
  theme(legend.position = "top") +
  guides(color = guide_legend(override.aes = list(alpha = 1, size = 2)))
save_plot_lmu(plot_hill, "powerlaw_fit_hill.png")

# Degree correlation matrix as image representation of logarithmically-transformed joint
# degree distribution (c. Kolaczyk (2010), Fig. 4.3)
# Alternatively: Sorting the degrees within each edge (with pmin and pmax) leads to all
# edges having the smaller degree first and the bigger degree second. Therefore, as the
# first edge is shown on the x-axis, the plot would only show the correlation for the
# upper left triangle, i.e. the undirected graph makes the rest superfluous as symmetric.
plot_joint_degree_distrib <-
  ggplot(dt_joint_degrees, aes(x = d1, y = d2, color = log10(freq))) +
  geom_tile() +
  scale_x_log10() +
  scale_y_log10() +
  scale_color_viridis_c(option = "plasma", direction = -1) +
  labs(x = "Grad [log10]", y = "Grad [log10]", color = "Relative Häufigkeit [log10]") +
  facet_wrap(~ programme) +
  theme_lmu() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        legend.position = "bottom",
        legend.direction = "horizontal", 
        legend.title = element_text(vjust = 0.8))
save_plot_lmu(plot_joint_degree_distrib, "degree_correlation_matrix.png")

# Perform some cross-programme distributional comparisons using statistical tests and more:
# Two-sample Kolmogorov-Smirnov test on degree distributions testing whether H2020
# and HORIZON degree sequences follow the same underlying distribution. Exploratory only,
# as degrees within network are not independent draws! Ties in the integer degree values
# prevent exact p-value, hence exact = FALSE.
test_kolmogorov_smirnov_degree <- ks.test(dt_metrics[programme == "H2020", degree],
                                          dt_metrics[programme == "HORIZON", degree],
                                          alternative = "two.sided", exact = FALSE)
print(test_kolmogorov_smirnov_degree)

# Wilcoxon rank-sum test on degrees to test whether one programmes' degrees are
# systematically higher/lower than the other's, in addition to distributional KS-Test
test_wilcoxon_degree <- wilcox.test(degree ~ programme, data = dt_metrics,
                                    alternative = "two.sided", conf.int = TRUE)
print(test_wilcoxon_degree)

# Degree-strength scaling exponent by fitting strength and degree, respectively, in
# logarithmic LM per programme to test near-1 strength/degree ratio observed above
dt_lm_fit_degree_strength <- dt_metrics[(degree != 0) & (strength != 0)]
fit_degree_strength <- lm(log(strength) ~ log(degree) * programme,
                          data = dt_lm_fit_degree_strength)
print(summary(fit_degree_strength))

# Degree-knn scaling exponent by fitting knn and degree, respectively, in logarithmic LM
# per programme to test degree-correlation pattern shown in 'plot_degree_knn'
dt_lm_fit_degree_knn <- dt_metrics[(is.na(knn) == FALSE) & (degree != 0) & (knn != 0)]
fit_degree_knn <- lm(log(knn) ~ log(degree) * programme, data = dt_lm_fit_degree_knn)
print(summary(fit_degree_knn))
