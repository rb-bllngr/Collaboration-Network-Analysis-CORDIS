# envir_setup.R: Determine global settings and prepare needed packages for project work

# Data file paths
PATHS <- list(
  DATA_DIR = "Data/Raw"
)

# Source helper functions
source("functions.R")

# invisible(lapply(paths, dir.create, showWarnings = FALSE, recursive = TRUE))

# List of CRAN packages - every package needed for project beside default packages
packages <- c(
  # # Install packages
  # "dplyr",
  # "ggcorrplot",
  # "ggplot2",
  # "knitr",
  # "lubridate",
  # "ranger",
  # "RColorBrewer",
  # "readr",
  # "rmarkdown",
  # "scales",
  # "stringr"
  
  # Install package for downloading data from EU Commission CORDIS' URLs
  "httr",
  # Install package for validating function input
  "checkmate",
  # Install package for data manipulation at scale
  "data.table",
  # Install package for reading .xlsx files
  "readxl"
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