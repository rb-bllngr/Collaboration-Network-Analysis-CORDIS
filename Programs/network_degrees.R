# network_degrees.R: Degree, degree distribution, and degree correlation analysis for the
#                    H2020 and HORIZON EUROPE collaboration networks.

# --- Data Preparation -------------------------------------------------------------------
# Load networks for each programme (take weighted version for possibility to extract
# information on graph strength)
h2020 <- readRDS(file.path(PATHS$DATA_INT, "network_h2020_weighted.RDS"))
horizon <- readRDS(file.path(PATHS$DATA_INT, "network_horizon_weighted.RDS"))
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


# --- Analysis and Visualization ---------------------------------------------------------
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
  set_theme_lmu() +
  theme(legend.position = "top")
### TODO: SAVE THE PLOT TO /PLOTS

# Histogram for strength
plot_strength_hist <-
  ggplot(dt_metrics[strength != 0], aes(x = strength, fill = programme)) +
  geom_histogram(position = "identity", alpha = 0.75, binwidth = 0.1) +
  scale_x_log10() +
  scale_fill_manual(values = colorblindfriendly()) +
  labs(x = "Stärke [log10]", y = "Anzahl Organisationen", fill = "EU-Förderprogramm") +
  set_theme_lmu() +
  theme(legend.position = "top")
### TODO: SAVE THE PLOT TO /PLOTS
# Note: Looks basically identical to degree histogram. Validate plausibility by checking
# whether the majority of organisations are one-off, i.e. most edge weights are close to 1
# (meaning most pairs of organisations only collaborated on one project together) because
# strength = degree x average edge weight per node.
dt_metrics[, mean(strength / degree, na.rm = TRUE), by = programme]
# Note: Ratio is indeed close to 1, histogram seems plausible!

# Log-log degree distribution
plot_degree_distrib <-
  ggplot(dt_degree_distrib, aes(x = degree, y = prob, color = programme)) +
  geom_point(size = 0.8) +
  scale_x_log10() +
  scale_y_log10() +
  scale_color_manual(values = colorblindfriendly()) +
  labs(x = "Grad [log10]", y = "Relative Häufigkeit [log10]", color = "EU-Förderprogramm") +
  set_theme_lmu() +
  theme(legend.position = "top")
### TODO: SAVE THE PLOT TO /PLOTS

# Investigate neighbour-degree behaviour among organisations by plotting knn-degree
plot_degree_knn <-
  ggplot(dt_metrics[!is.na(knn) & degree != 0],
         aes(x = degree, y = knn, color = programme)) +
  geom_point(shape = 4, size = 0.8, alpha = 0.3) +
  scale_x_log10() +
  scale_y_log10() +
  scale_color_manual(values = colorblindfriendly()) +
  labs(x = "Grad [log10]",
       y = "Mittlerer Grad der k-nächsten Nachbarn [log10]",
       color = "EU-Förderprogramm") +
  set_theme_lmu() +
  theme(legend.position = "top") +
  guides(color = guide_legend(override.aes = list(alpha = 1, size = 2)))
### TODO: SAVE THE PLOT TO /PLOTS

# Hill plot
plot_hill <-
  ggplot(dt_hill_estimator, aes(x = k, y = alpha_k, color = programme)) +
  geom_point(shape = 1, size = 0.8, alpha = 0.75) +
  scale_color_manual(values = colorblindfriendly()) +
  labs(x = "k", y = expression(hat(alpha)[k]), color = "Förderprogramm") +
  set_theme_lmu() +
  theme(legend.position = "top") +
  guides(color = guide_legend(override.aes = list(alpha = 1, size = 2)))
### TODO: SAVE THE PLOT TO /PLOTS

# Degree correlation matrix as image representation of logarithmically-transformed joint
# degree distribution (c. Kolaczyk (2010), Fig. 4.3)
### TODO: