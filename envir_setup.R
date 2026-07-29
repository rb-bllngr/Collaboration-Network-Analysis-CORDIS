# envir_setup.R: Determine global settings and prepare needed packages for project work

# Data file paths
PATHS <- list(
  DATA_RAW = "Data/Raw",
  DATA_INT = "Data/Intermediate"
)

# Source helper functions
source("plot_styling.R")
source("functions.R")

# TODO:
# invisible(lapply(paths, dir.create, showWarnings = FALSE, recursive = TRUE))

# Install and load the 'pak' package to access GitHub packages
if (!requireNamespace("pak", quietly = TRUE)) {
  install.packages("pak")
}

# GitHub packages
github_packages <- c("PPgp/wpp2024")

# Increase time for install GitHub package as its quite large
options(timeout = 600)

# Install or update GitHub packages
for (packages in github_packages) {
  # Extract package name from the repository string
  pkg <- sub(".*/", "", packages)
  
  # Check if the package is installed
  if (!requireNamespace(pkg, quietly = TRUE)) {
    message("Installing GitHub package:", pkg)
    pak::pak(paste0("github::", packages))
  }

  library(pkg, character.only = TRUE)
}

# List of CRAN packages. Every package needed for project beside default packages in
# chronological order
packages <- c(
  # Install package for downloading data from EU Commission CORDIS' URLs
  "httr",
  # Install package for validating function input
  "checkmate",
  # Install package for data manipulation at scale
  "data.table",
  # Install package for reading .xlsx files
  "readxl",
  # Install package for handling of network objects
  "igraph",
  # Install package for visualization of networks
  "ggplot2",
  # Install package for visualization if world map
  "maps",
  # Install package for colorblind-friendly coloring scales
  "RColorBrewer",
  # Install package for assembling plots into grids
  "patchwork",
  # Install package for labeling in plots
  "ggrepel",
  # Install package for abstract graph layout visualisations
  "ggraph"
)

# Set CRAN mirror
options(repos = c(CRAN = "https://cloud.r-project.org"))

# Install CRAN packages if missing
for (pkg in packages) {
  if (!require(pkg, character.only = TRUE)) {
    message(paste("Installing CRAN package:", pkg))
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
}

# Inform the user to restart R if needed
message("If you experience any issues with loaded packages,
        please restart R and re-run this script.")
