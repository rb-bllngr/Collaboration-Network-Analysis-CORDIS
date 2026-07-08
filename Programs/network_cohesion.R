# network_cohesion.R: Compute component structure, density, clustering and small-world
#                     coefficient, as well as average path length for each of the H2020
#                     and HORIZON EUROPE collaboration networks.

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