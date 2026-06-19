# functions.R: contains all the self-written functions used in the project

# --- List of functions ------------------------------------------------------------------
#' 1. unzip_recursive
#' 2. download_and_unzip

#' FUNCTION 1:
#' @description
#' TODO: The function 'unzip_recursive' ...
#'
#' Inputs:
#' @param path TODO
#' @param destination TODO
#'
#' Output:
#' @returns TODO

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

#' FUNCTION 2:
#' @description
#' TODO: The function 'download_and_unzip' ...
#'
#' Inputs:
#' @param url TODO
#' @param destination TODO
#'
#' Output:
#' @returns TODO

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