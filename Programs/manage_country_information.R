# manage_country_information.R: Fix missing/malformed country entries via nutsCode-prefix
#                               extraction (and geolocation as fallback), compare CORDIS
#                               data ISO2 codes to UN WPP2024 data codes, and fix remaining
#                               deviations by hand.

# Import CORDIS data and exclude missing/malformed ISO2 country codes
cordis <- readRDS(file.path(PATHS$DATA_INT, "cordis.RDS"))

# Create snapshot of all originally malformed entries
malformed_snapshot <- cordis[is.na(country) == TRUE |
                               grepl(pattern = "^[A-Z]{2}$", country) == FALSE,
                             .(organisationID, name, geolocation, nutsCode, city,
                               country_malformed = country)]
message(nrow(malformed_snapshot), " entries with missing/malformed ISO country codes ",
        "identified. Attempting correction before excluding them.")

# Inspect these entries by hand once, to get an overview of possible problem solutions
# View(malformed_snapshot)
malformed_snapshot[, .N, by = country_malformed]

# Creating a log of which fixing tier resolved which organisation (important for checking,
# whether fixes are actually valid)
correction_log <- data.table(organisationID = character(0), tier = character(0))

# Tier 1: nutsCode-prefix extraction. Check validity of nutsCode as fixing method
cordis[!is.na(country) & grepl("^[A-Z]{2}$", country) & !is.na(nutsCode),
       .N, by = .(match = substr(nutsCode, 1, 2) == country)]
# Note: The first two characters of nutsCode agree with the existing (valid) country field
# in 321,814 of 321,826 non-malformed rows (> 99.99%). Deviations for the 12 disagreements
# can be primarily be explained by distinctions in treatment of overseas territories (New
# Caledonia folded into France's prefix) and different Belgian cities and organisations
# with the same Dutch (placeholder?) NUTS prefix --> no systematic weakness!
# View(cordis[!is.na(country) & grepl("^[A-Z]{2}$", country) &
#       !is.na(nutsCode) & substr(nutsCode, 1, 2) != country,
#       .(organisationID, name, country, nutsCode, city, geolocation)])

# Take still malformed entries and impute country code by extracting prefix of nutsCode
malformed_still <- cordis[(is.na(country) == TRUE) | (grepl("^[A-Z]{2}$", country) == FALSE),
                          .(organisationID, name, geolocation, nutsCode, city, country)]

nuts_available <- malformed_still[(is.na(nutsCode) == FALSE)]
nuts_available[, country_nuts := substr(nutsCode, 1, 2)]

# Check whether NUTS' codes are valid (i.e. two uppercase letters) and flag anything that
# is not for manual review
nuts_invalid <- nuts_available[grepl("^[A-Z]{2}$", country_nuts) == FALSE]
if (nrow(nuts_invalid) > 0) {
  message(nrow(nuts_invalid), " entries have malformed nutsCode prefix. ",
          "Fall through to manual review.")
}
nuts_available <- nuts_available[grepl("^[A-Z]{2}$", country_nuts) == TRUE]

# Update correction log with Tier 1 fix
message(nrow(nuts_available), " entries resolved via nutsCode-prefix imputation. ",
        nrow(malformed_snapshot) - nrow(nuts_available), " unresolved and falling through",
        " to next tier.")
correction_log <- rbind(correction_log, data.table(
  organisationID = unique(nuts_available$organisationID),
  tier = "Tier 1: nutsCode"
))

# Merge the CORDIS data with the Tier 1 fixed country code data
cordis[nuts_available, on = "organisationID", country := i.country_nuts]

# Tier 2: Geolocation-based point-in-polygon imputation using 'maps::map.where()'
malformed_still <- cordis[(is.na(country) == TRUE) | (grepl("^[A-Z]{2}$", country) == FALSE),
                          .(organisationID, name, geolocation, nutsCode, city, country)]
malformed_still[,
  c("latitude", "longitude") := tstrsplit(geolocation, ",", type.convert = TRUE)
]
geolocation_available <- malformed_still[(is.na(latitude) == FALSE) &
                                           (is.na(longitude) == FALSE)]

# Look up the country membership via point-in-polygon using 'maps::map.where()'
geolocation_available[, country_name := maps::map.where(database = "world",
                                                        x = longitude, y = latitude)]

# Inspecting the assignment via 'maps::map.where()' done for plausibility based on city
# and organisation names
# View(geolocation_available)
# Note: All entries seem logically assigned!

# Strip country name after colon for subregional descriptions done in 'maps::map.where()'
# and use 'countrycode()' to convert names to ISO2 codes for all remaining entries
geolocation_available[, country_name := sub(":.*$", "", country_name)]
geolocation_available[, country_iso2 := countrycode(
  sourcevar = country_name, origin = "country.name",
  destination = "iso2c", custom_match = c("UK" = "UK")
)]

# Update correction log with Tier 2 fix
unresolved <- geolocation_available[is.na(country_iso2) == TRUE]
message(nrow(geolocation_available) - nrow(unresolved), " entries resolved via ",
        "geolocational imputation. ", nrow(unresolved), " unresolved and falling through ",
        "to manual review.")
correction_log <- rbind(correction_log, data.table(
  organisationID = unique(geolocation_available[is.na(country_iso2) == FALSE, organisationID]),
  tier = "Tier 2: geolocation"
))

# Merge the CORDIS data with the Tier 2 fixed country code data
cordis[geolocation_available[is.na(country_iso2) == FALSE], on = "organisationID",
       country := i.country_iso2]

# Tier 3: Manual fixing
malformed_still <- cordis[(is.na(country) == TRUE) | (grepl("^[A-Z]{2}$", country) == FALSE),
                          .(organisationID, name, geolocation, nutsCode, city, country)]
if (nrow(malformed_still) > 0) {
  message(nrow(malformed_still), " entries remain malformed after Tier 1 and 2; manual ",
          "fix is required.")
  # View(malformed_still)
  # Note: Not needed for this specific data set anymore, as all malformed/missing country
  # code entries have already been corrected but fill in manually and uncomment if required
  # fix_manually <- data.table(organisationID = c(), country = c())
  # correction_log <- rbind(correction_log, data.table(
  #   organisationID = unique(fix_manually$organisationID), tier = "Tier 3: manual"
  # ))
  # cordis[fix_manually, on = "organisationID", country := i.country]
} else {
  message("No entries required manual Tier 3 correction for this dataset.")
}

# Overview over all the changes applied to fix the country code inconsistencies
review_table <- merge(malformed_snapshot, correction_log, by = "organisationID", all.x = TRUE)
review_table[cordis, country_fixed := i.country, on = "organisationID"]
review_table[is.na(tier) == TRUE, tier := "unresolved"]

# Catch accidental duplication from the merge
stopifnot(nrow(review_table) == nrow(malformed_snapshot))

# Sort review table by tiers
setorder(review_table, tier, organisationID)
# View(review_table[, .(organisationID, name, geolocation, city, nutsCode, country_malformed,
#                       tier, country_fixed)])

# Exclude anything still malformed from existing script (especially applied when using the
# latest CORDIS data instead of frozen snapshot)
cordis <- cordis[is.na(country) == FALSE & grepl(pattern = "^[A-Z]{2}$", country) == TRUE]

# Import UN data from World Population Prospect Report's R-package version wpp2024. Restrict
# data to start of each programme's year and extract population
# Note: wpp2024's README mentions data - different to UN Data Portal to be for Dec 31 at
#       midnight of each year, therefore need to take wanted year minus 1 to get population
#       at start of year of interest (2014 - 1 = 2013, 2021 - 1 = 2020).
data(pop1dt)
dt_population <- pop1dt[year %in% c(2013, 2020), .(country_code, name, pop = pop * 1000, year)]

# Check WPP's regional/income-group aggregates to drop
dt_population[country_code >= 900, sort(unique(name))]
dt_population <- dt_population[country_code < 900]

# Encode to ISO2 code
dt_population[, iso2 := countrycode(
  sourcevar = country_code, origin = "un", destination = "iso2c",
  custom_match = c("158" = "TW", "300" = "EL", "412" = "XK", "826" = "UK")
)]

# Compare the ISO2 codes used in CORDIS data with those available in WPP2024
iso2_cordis <- unique(cordis$country)
iso2_wpp2024 <- unique(dt_population$iso2)
sort(setdiff(iso2_cordis, iso2_wpp2024))
# Note: This returns the following countries present with the same ISO2 code in CORDIS but
#       not in UN data from GitHub with explanation of why it deviates and/or respective fix.
#
# ID | CORDIS | WPP2024 | Full Country Name     | Explanation/Fix
# ---|--------|---------|-----------------------|-----------------------------------------
# 01 |   UM   |   xx    | United States Minor   | see below, investigation (A) for more
#    |        |         | Outlying Islands      | information; manually assign to 'US'
# ---|--------|---------|-----------------------|-----------------------------------------
# 02 |   ZZ   |   xx    | custom-made           | see below, investigation (B) for more
#    |        |         |                       | information; manually assign to 'AF'
# ---|--------|---------|-----------------------|-----------------------------------------
# 03 |   VA   |   xx    | The Holy See /        | data missing in version wpp2024::pop1dt,
#    |        |         | Vatican City          | manually assign values based on the UN
#    |        |         |                       | WPP2024 Data Portal; fixed in (C)
# ---|--------|---------|-----------------------|-----------------------------------------
# Note: Copyright ©2024 by United Nations, made available under Creative Commons license CC BY 3.0 IGO
#       (United Nations, Department of Economic and Social Affairs, Population Division (2024).
#       World Population Prospects 2024, Online Edition.)
#
# Investigate (and fix) the deviations in ISO2 codes
# (A) Answer: Cayman Chemical Company in Ann Arbor (Michigan, US) --> re-assign to 'US'!
cordis[country == "UM", .(country, name, city)]
cordis[country == "UM", country := "US"]
# (B) Answer: Afghanistan Research and Evaluation Unit (AREU) in Kabul --> re-assign to 'AF'!
cordis[country == "ZZ", .(country, name, city)]
cordis[country == "ZZ", country := "AF"]
# (C) Answer: Vatican City is missing in R-package version --> add manually based on UN data!
# Note: Unlike wpp2024's package data (Dec 31 midnight, requiring -1), these values are
#       directly for Jan 1 of the target years (2014, 2021) and are correct as-is. They
#       are labeled year = 2013/2020 here purely to align with the join keys used below,
#       not because the same timing adjustment applies to them.
nrow(dt_population[iso2 == "VA"])
dt_population <- rbind(
  dt_population,
  data.table(country_code = NA_integer_,
             name = "Vatican City",
             pop = c(587, 524),
             year = c(2013, 2020),
             iso2 = "VA")
)

# Split population data by programme year and merge population information onto CORDIS data
dt_population_h2020 <- dt_population[year == 2013, .(iso2, population_h2020 = pop)]
dt_population_horizon <- dt_population[year == 2020, .(iso2, population_horizon = pop)]
cordis[dt_population_h2020, population_h2020 := i.population_h2020, on = .(country = iso2)]
cordis[dt_population_horizon, population_horizon := i.population_horizon, on = .(country = iso2)]

# Check for any remaining missing population values
sort(unique(cordis[is.na(population_h2020) == TRUE, country]))
sort(unique(cordis[is.na(population_horizon) == TRUE, country]))

# Save CORDIS data again, now refined by the population data for each country
saveRDS(cordis, file.path(PATHS$DATA_INT, "cordis_population.RDS"))
