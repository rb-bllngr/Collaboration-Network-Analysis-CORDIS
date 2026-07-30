# map_country_information.R: Compare the CORDIS data ISO2 codes to UN WPP2024 data codes,
#                            and fix deviations by hand

# TODO: FIX MALFORMED COUNTRY ENTRIES BY HAND (IF REASONABLE EFFORT REQUIRED)
# TODO: ---> HERE?????

# Import CORDIS data and exclude missing/malformed ISO2 country codes
cordis <- readRDS(file.path(PATHS$DATA_INT, "cordis.RDS"))
message(cordis[is.na(country) == TRUE | grepl(pattern = "^[A-Z]{2}$", country) == FALSE, .N],
        " organisations excluded due to missing/malformed ISO2 country codes")
cordis <- cordis[is.na(country) == FALSE & grepl(pattern = "^[A-Z]{2}$", country) == TRUE]

# Import UN data from World Population Prospect Report's R-package version wpp2024. Restrict
# data to start of each programme's year and extract population
data(pop1dt)
dt_population <- pop1dt[year %in% c(2014, 2021), .(country_code, name, pop = pop * 1000, year)]

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
nrow(dt_population[iso2 == "VA"])
dt_population <- rbind(
  dt_population,
  data.table(country_code = NA_integer_,
             name = "Vatican City",
             pop = c(587, 524),
             year = c(2014, 2021),
             iso2 = "VA")
)

# Split population data by programme year and merge population information onto CORDIS data
dt_population_h2020 <- dt_population[year == 2014, .(iso2, population_h2020 = pop)]
dt_population_horizon <- dt_population[year == 2021, .(iso2, population_horizon = pop)]
cordis[dt_population_h2020, population_h2020 := i.population_h2020, on = .(country = iso2)]
cordis[dt_population_horizon, population_horizon := i.population_horizon, on = .(country = iso2)]

# Check for any remaining missing population values
sort(unique(cordis[is.na(population_h2020) == TRUE, country]))
sort(unique(cordis[is.na(population_horizon) == TRUE, country]))

# Save CORDIS data again, now refined by the population data for each country
saveRDS(cordis, file.path(PATHS$DATA_INT, "cordis_population.RDS"))
