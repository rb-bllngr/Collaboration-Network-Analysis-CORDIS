# mds_feasibility_check.R: One-time computation of memory and runtime figures used to
#                          justify restricting MDS to the country level (instead of
#                          applying it to organisation level). Not part of main pipeline,
#                          run manually as needed.

# Load H2020 programme network (i.e. larger of the two), use as relevant/worst case
h2020 <- readRDS(file.path(PATHS$DATA_INT, "network_h2020.RDS"))

# Extract the respective giant component in H2020
giant_comp <- largest_component(h2020$weighted)

# Exact node and edge counts for the giant component
n_nodes <- vcount(giant_comp)
m_edges <- ecount(giant_comp)
message("Nodes (giant component): ", n_nodes, "; Edges (giant component): ", m_edges)

# Estimate the peak memory usage, i.e. two (n x n) double matrices simultaneously managed
# (distance matrix + eigenvalue matrix) by 'cmdscale()' (according to the source code of
# 'stats::cmdscale') with 8 bytes per double entry (IEEE, 2019)
memory_GB <- (2 * (n_nodes * n_nodes * 8) / 1000^3)
# Note: Convert raw bytes into Gigabytes by / (1000 (--> KB) * 1000 (--> MB) * 1000 (--> GB))
message("Estimated peak memory for cmdscale(): ", round(memory_GB, 2), " Gigabytes")

# Full eigendecomposition (DSYTRD --> MRRR --> DORMTR) scales O(n^3) overall. See an
# empirical benchmark below for an actual runtime estimate.
# Empirical runtime benchmark: time 'cmdscale()' on random subsamples of increasing size,
# then fit against theoretically expected n^3 scaling to extrapolate to full giant-component
set.seed(20260916)
subsamples <- c(500, 1000, 2000, 4000, 8000)

dt_benchmark <- rbindlist(lapply(subsamples, function(size) {
  points <- matrix(rnorm(size * 2), ncol = 2)
  distance_matrix <- dist(points)
  time <- system.time(cmdscale(distance_matrix, k = 2))["elapsed"]
  data.table(n = size, seconds = as.numeric(time))
}))
print(dt_benchmark)

# Fit runtime proportional to n^3 without intercept for equivalence to complexity form
lm_runtime <- lm(seconds ~ I(n^3) - 1, data = dt_benchmark)
print(summary(lm_runtime))
message("Fit R-squared: ", round(summary(lm_runtime)$r.squared, 4))

# Extrapolate to full giant_component size
runtime_extrapolated_seconds <- predict(lm_runtime, newdata = data.table(n = n_nodes))
runtime_extrapolated_hours <- as.numeric(runtime_extrapolated_seconds) / 3600
message("Extrapolated cmdscale() runtime at n = ", n_nodes, ": ",
        round(runtime_extrapolated_hours, 1), " hours")
