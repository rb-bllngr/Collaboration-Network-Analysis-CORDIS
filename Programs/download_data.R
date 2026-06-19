# download_data.R: Import data sets needed for project from openly-accessible EU Commission
#                  portal CORDIS by downloading them directly via URLs

# Configure the URLs required to download
URLS <- list(
  Horizon2014to2020 = list(
    deliverables  = "https://cordis.europa.eu/data/cordis-h2020projectDeliverables-csv.zip",
    publications  = "https://cordis.europa.eu/data/cordis-h2020projectPublications-csv.zip",
    reports       = "https://cordis.europa.eu/data/cordis-h2020reports-csv.zip",
    projects      = "https://cordis.europa.eu/data/cordis-h2020projects-csv.zip"
  ),
  Horizon2021to2027 = list(
    deliverables  = "https://cordis.europa.eu/data/cordis-HORIZONprojectDeliverables-csv.zip",
    publications  = "https://cordis.europa.eu/data/cordis-HORIZONprojectPublications-csv.zip",
    reports       = "https://cordis.europa.eu/data/cordis-HORIZONreports-csv.zip",
    projects      = "https://cordis.europa.eu/data/cordis-HORIZONprojects-csv.zip"
  )
)

# Download the respective .zip files and unzip them accordingly
for (programme in names(URLS)) {
  for (dataset in names(URLS[[programme]])) {
    url <- URLS[[programme]][[dataset]]
    download_and_unzip(url, PATHS$DATA_DIR)
  }
}

message("All files downloaded and extracted.")