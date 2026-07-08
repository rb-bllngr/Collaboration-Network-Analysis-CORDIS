# download_data.R: Import data sets needed for project from openly-accessible EU Commission
#                  portal CORDIS by downloading them directly via URLs

# Configure the URLs required to download
URLS <- list(
  Horizon2014to2020 = list(
    deliverables  = "https://cordis.europa.eu/data/cordis-h2020projectDeliverables-xlsx.zip",
    publications  = "https://cordis.europa.eu/data/cordis-h2020projectPublications-xlsx.zip",
    reports       = "https://cordis.europa.eu/data/cordis-h2020reports-xlsx.zip",
    projects      = "https://cordis.europa.eu/data/cordis-h2020projects-xlsx.zip"
  ),
  Horizon2021to2027 = list(
    deliverables  = "https://cordis.europa.eu/data/cordis-HORIZONprojectDeliverables-xlsx.zip",
    publications  = "https://cordis.europa.eu/data/cordis-HORIZONprojectPublications-xlsx.zip",
    reports       = "https://cordis.europa.eu/data/cordis-HORIZONreports-xlsx.zip",
    projects      = "https://cordis.europa.eu/data/cordis-HORIZONprojects-xlsx.zip"
  )
)

# Download the respective .zip files and unzip them accordingly
for (programme in names(URLS)) {
  for (dataset in names(URLS[[programme]])) {
    url <- URLS[[programme]][[dataset]]
    download_and_unzip(url, PATHS$DATA_RAW)
  }
}

message("All files downloaded and extracted.")