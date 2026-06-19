# functions.R: contains all the self-written functions used in the project

# --- List of functions ------------------------------------------------------------------
#' 1. unzip_recursive
#' 2. download_and_unzip
#' 3. load_csv

# --- Function 1 -------------------------------------------------------------------------
#' @description
#' Recursively unzips .zip files to destination directory and removes the .zip afterwards.
#' If any nested .zip files are found among the extracted contents, those are unzipped in
#' the same way until no .zip files remain.
#'
#' Inputs:
#' @param path        Character string. Full path to the .zip file to extract.
#' @param destination Character string. Path to directory where extracted files are placed.
#'
#' Output:
#' @returns No return value. Called for extracting files and removing zips.

unzip_recursive <- function(zip_path, destination) {
  # Check for valid input
  require(checkmate)
  assertString(zip_path)
  assertString(destination)

  # Unzip the original .zip files imported from the portal and remove afterwards
  unzip(zip_path, exdir = destination)
  file.remove(zip_path)

  # Check for any nested zips and unzip those too
  nested_zips <- list.files(
    destination, pattern = "\\.zip$", full.names = TRUE, recursive = TRUE
    )
  for (zip in nested_zips) {
    message("Unzipping nested zip: ", basename(zip))
    unzip_recursive(zip, dirname(zip))
  }
}

# --- Function 2 -------------------------------------------------------------------------
#' @description
#' Downloads a .zip file from given URL to destination directory, then extracts its
#' contents recursively using unzip_recursive().
#'
#' Inputs:
#' @param url         Character string. URL of the .zip file to download.
#' @param destination Character string. Path to the directory where downloaded files are
#'                    saved and extracted into.
#'
#' Output:
#' @returns No return value. Called for downloading and extracting files.

download_and_unzip <- function(url, destination) {
  # Check for valid input
  require(checkmate)
  assertString(url)
  assertString(destination)

  # Create a subdirectory named after the .zip file
  zip_name <- tools::file_path_sans_ext(basename(url))  # e.g. "cordis-h2020projects-csv"
  subdirectory <- file.path(destination, zip_name)
  dir.create(subdirectory, showWarnings = FALSE)

  # Download the files via URLs
  zip_path <- file.path(destination, basename(url))
  message("Downloading: ", basename(url))
  downloaded <- GET(url,
                    config(http_version = 2),  # force HTTP/1.1 for CORDIS compatibility
                    write_disk(zip_path, overwrite = TRUE),
                    # Attention: Switched overwrite from TRUE to FALSE on July __ 2026 to
                    #            use up-to-date version for analysis. If latest version is
                    #            desired, just switch back to TRUE which makes the files
                    #            to be overwritten once new version is available.
                    progress())
  if (http_error(downloaded)) {
    stop("Failed to download: ", url, "\nStatus: ", status_code(downloaded))
  }

  # Recursively repeat unzipping
  message("Unzipping: ", basename(zip_path))
  unzip_recursive(zip_path, subdirectory)
  message("Done: ", basename(url))
}

# --- Function 3 -------------------------------------------------------------------------
#' @description
#' Loads a single .csv file from a CORDIS program subdirectory using 'fread'.
#'
#' @param subdirectory  Character string. Subdirectory name within raw data directory.
#' @param filename      Character string. CSV filename to load.
#'
#' @returns A data.table of the referenced data.

load_csv <- function(subdirectory, filename) {
  # Check for valid input
  require(checkmate)
  assertString(subdirectory)
  assertString(filename)

  # Check for paths to load .csv files from
  path <- file.path(PATHS$DATA_DIR, subdirectory, filename)
  if (!file.exists(path)) {
    stop("File not found: ", path)
  }
  message("Loading: ", path)

  # Fast read .csv files as data.table objects
  dt <- fread(path, encoding = "UTF-8", sep = ";", na.strings = c("", "N/A"))
  return(dt)
}