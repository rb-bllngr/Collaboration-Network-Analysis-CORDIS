# pipeline.R: Master script for the project. Run this script to execute the full pipeline.

# Step 01: Set up environment (loads packages, sources functions, defines paths)
source("envir_setup.R")

# Step 02: Download and extract raw data from EU Commission portal 'CORDIS' or import frozen
# snapshot from Sync&Share. Load and combine project and organisation data from both programmes.
source("Programs/download_and_merge_data.R")

# Step 03: Fix malformed/missing country code entries and introduce population information
# as well as merge with existing CORDIS data
source("Programs/manage_country_information.R")

# Step 04: Build all variants of the networks
source("Programs/build_networks.R")

# Step 05: Visualize the programme-specific networks for the first time to get an overview
#          of how the data looks like in total
# source("Programs/visualise_basic_networks.R")

# Step 06: Perform degree, degree distribution, and degree correlation analysis
source("Programs/network_degrees.R")

# Step 07: Perform centrality (degree, betweenness, closeness, eigenvector) analysis
source("Programs/network_centrality.R")

# Step 08: Perform cohesion (component structure, density, coreness, clustering coefficient,
#         average path length, small-world coefficient) analysis
source("Programs/network_cohesion.R")

# Step 09: Perform coordinator role analysis
source("Programs/network_roles.R")

# Step 10: Perform comparable analyses for country-level aggregated data
source("Programs/network_countries.R")

# Step 11: Perform Multidimensional Scaling (MDS) on country-level
source("Programs/network_mds.R")