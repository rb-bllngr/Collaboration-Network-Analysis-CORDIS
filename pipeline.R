# pipeline.R: Master script for the project. Run this script to execute the full pipeline

# Step 1: Set up environment (loads packages, sources functions, defines paths)
source("envir_setup.R")

# Step 2: Download and extract raw data from EU Commission portal 'CORDIS'
source("Programs/download_data.R")

# Step 3: Load and combine project and organisation data from both programs
source("Programs/merge_data_sets.R")

# Step 4: First visualization of network data
source("Programs/visualize_network_map.R")

# Step 5: Perform degree, degree distribution, and degree correlation analysis
source("Programs/network_degrees.R")

# Step 6: Perform centrality (degree, betweenness, closeness, eigenvector) analysis
source("Programs/network_centrality.R")