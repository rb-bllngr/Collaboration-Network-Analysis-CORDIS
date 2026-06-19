# pipeline.R: Master script for the project. Run this script to execute the full pipeline

# Step 1: Set up environment (loads packages, sources functions, defines paths)
source("envir_setup.R")

# Step 2: Download and extract raw data from EU Commission portal 'CORDIS'
source("download_data.R")