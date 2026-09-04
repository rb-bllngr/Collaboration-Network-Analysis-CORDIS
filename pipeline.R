# pipeline.R: Master script for the project. Run this script to execute the full pipeline.

# Step 01: Set up environment (loads packages, sources functions, defines paths)
message("01/11: Program 'envir_setup.R' is being processed ...")
source("envir_setup.R")

# Step 02: Download and extract raw data from EU Commission portal 'CORDIS' or import frozen
# snapshot from Sync&Share. Load and combine project and organisation data from both programmes.
message("02/11: Program 'download_and_merge_data.R' is being processed ...")
source("Programs/download_and_merge_data.R")

# Step 03: Fix malformed/missing country code entries and introduce population information
# as well as merge with existing CORDIS data
message("03/11: Program 'manage_country_information.R' is being processed ...")
source("Programs/manage_country_information.R")

# Step 04: Build all variants of the networks
message("04/11: Program 'build_networks.R' is being processed ...")
source("Programs/build_networks.R")

# Step 05: Visualize the programme-specific networks for the first time to get an overview
#          of how the data looks like in total
message("05/11: Program 'visualise_basic_networks.R' is being processed ...")
source("Programs/visualise_basic_networks.R")

# Step 06: Perform degree, degree distribution, and degree correlation analysis
message("06/11: Program 'network_degrees.R' is being processed ...")
source("Programs/network_degrees.R")

# Step 07: Perform centrality (degree, betweenness, closeness, eigenvector) analysis
message("07/11: Program 'network_centrality.R' is being processed ...")
source("Programs/network_centrality.R")

# Step 08: Perform cohesion (component structure, density, coreness, clustering coefficient,
#         average path length, small-world coefficient) analysis
message("08/11: Program 'network_cohesion.R' is being processed ...")
source("Programs/network_cohesion.R")

# Step 09: Perform coordinator role analysis
message("09/11: Program 'network_roles.R' is being processed ...")
source("Programs/network_roles.R")

# Step 10: Perform comparable analyses for country-level aggregated data
message("10/11: Program 'network_countries.R' is being processed ...")
source("Programs/network_countries.R")

# Step 11: Perform Multidimensional Scaling (MDS) on country-level
message("11/11: Program 'network_mds.R' is being processed ...")
source("Programs/network_mds.R")