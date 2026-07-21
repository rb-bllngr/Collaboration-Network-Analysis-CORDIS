# network_countries: Investigate country-level aggregated data on coordinator roles and
#                    country-pair collaboration for H2020 and HORIZON EUROPE networks.

# Load networks for each programme (both unweighted and weighted as needed for different
# analyses)
networks <- list(H2020 = readRDS(file.path(PATHS$DATA_INT, "network_h2020.RDS")),
                 HORIZON = readRDS(file.path(PATHS$DATA_INT, "network_horizon.RDS")))
programmes <- names(networks)

# Extract an organisation's role and country information, then aggregate on country-level
# per programme
dt_country <- rbindlist(lapply(programmes, function(prog) {
  g_unweighted <- networks[[prog]]$unweighted

  data.table(
    programme = prog,
    organisationID = V(g_unweighted)$name,
    country = V(g_unweighted)$country,
    n_projects = V(g_unweighted)$n_proj,
    n_coordinator = V(g_unweighted)$n_coord
  )
}))

# TODO: FIX MALFORMED COUNTRY ENTRIES BY HAND (IF REASONABLE EFFORT REQUIRED)

# Flag and exclude missing/malformed ISO2 country codes
message(dt_country[is.na(country) == TRUE | grepl(pattern = "^[A-Z]{2}$", country) == FALSE, .N],
        " organisations excluded due to missing/malformed ISO2 country codes")
dt_country <- dt_country[is.na(country) == FALSE & grepl(pattern = "^[A-Z]{2}$", country) == TRUE]

# Define flag for coordinator status
dt_country[, is_coordinator := n_coordinator > 0]

# Aggregate to country level, per programme
dt_country_summary <- dt_country[, .(
  n_organisations = .N,
  n_organisations_coordinator = sum(is_coordinator),
  n_projects_total = sum(n_projects),
  n_coordinator_total = sum(n_coordinator),
  # Coordinator share 'share_projects' is project-weighted, i.e. share of this country's
  # organisations in coordinator roles in all of this country's projects
  share_projects = sum(n_coordinator) / sum(n_projects),
  # Coordinator share 'share_organisations' is organisation-weighted, i.e. share of this
  # country's organisations, which have at least once acted as coordinator in project
  share_organisations = mean(is_coordinator)
), by = .(programme, country)]
print(dt_country_summary[order(programme, -n_organisations)][programme == "H2020"])
print(dt_country_summary[order(programme, -n_organisations)][programme == "HORIZON"])

# Initialize lists for results
results_edges <- list()
results_density <- list()

# Build the country-pair interaction network for each of the two framework programmes
for (prog in programmes) {
  g_weighted <- networks [[prog]]$weighted
  g_unweighted <- networks[[prog]]$unweighted

  # Extract organisation-level information and exclude those with invalid ISO2 code entries
  dt_organisations <- data.table(
    organisationID = V(g_unweighted)$name,
    country = V(g_unweighted)$country,
    degree = degree(g_unweighted)
  )
  dt_organisations <- dt_organisations[is.na(country) == FALSE &
                                         grepl(pattern = "^[A-Z]{2}$", country) == TRUE]

  # Get the list of all edges from --> to on organisation level
  dt_edges <- as.data.table(as_data_frame(g_weighted, what = "edges"))
  setnames(dt_edges, old = "weight", new = "n_projects_shared")

  # Add the country for each of the edge endpoints into the data.table by merging them, so
  # that only the valid-country-code organisations from 'dt_organisations' remain and all
  # other edges between/from/to organisations with invalid ISO2 codes are dropped
  dt_edges <- merge(dt_edges, dt_organisations[, .(organisationID, country)],
                    by.x = "from", by.y = "organisationID")
  dt_edges <- merge(dt_edges, dt_organisations[, .(organisationID, country)],
                    by.x = "to", by.y = "organisationID")
  setnames(dt_edges, old = c("country.x", "country.y"), new = c("country_from", "country_to"))

  # Order pairs of countries alphabetically in order to avoid pairs appearing in either
  # direction, e.g. (AT, DE) and (DE, AT) shall aggregate together
  dt_edges[, ":=" (
    country_i = pmin(country_from, country_to),
    country_j = pmax(country_from, country_to)
  )]

  # Aggregate to country pairs
  dt_country_pairs <- dt_edges[, .(
    n_organisation_pairs = .N,
    sum_weight = sum(n_projects_shared)
  ), by = .(country_i, country_j)]

  # Collect important country-level values needed for measurement computation further down
  dt_country_info <- dt_organisations[, .(
    n_organisations = .N,
    sum_degree = sum(degree)
  ), by = country]

  # Cross join all country combinations including same-country pairs, so that non-observed
  # collaborations will be valued at zero, not dropped
  countries_all <- sort(unique(dt_organisations$country))
  dt_country_combos <- CJ(country_i = countries_all,
                          country_j = countries_all)[country_i <= country_j]

  # Merge all country combinations with the data on actual appearance of country pairs
  dt_density <- merge(dt_country_combos, dt_country_pairs,
                      by = c("country_i", "country_j"), all.x = TRUE)

  # Fill the non-existent country pairs (NAs) with zero value
  dt_density[is.na(n_organisation_pairs), ":=" (
    n_organisation_pairs = 0,
    sum_weight = 0
  )]

  # Merge the current data.table with information from 'dt_country_info' collected above,
  # for both the first and second country inside the pair ('country_i', 'country_j')
  dt_density <- merge(dt_density, dt_country_info, by.x = "country_i", by.y = "country")
  dt_density <- merge(dt_density, dt_country_info, by.x = "country_j", by.y = "country")
  setnames(dt_density,
           old = c("n_organisations.x", "sum_degree.x", "n_organisations.y", "sum_degree.y"),
           new = c("n_organisations_i", "sum_degree_i", "n_organisations_j", "sum_degree_j"))

  # Compute block-density, i.e. observed organisation-pairs relative to maximum possible
  #     - Cross country (C_i != C_j): n_organisations_i x n_organisations_j
  #     - Same country (C_i == C_j): use binomial coefficient to calculate the amount of
  #       combinations possible for 'choose(n, k)', meaning choose k elements out of a set
  #       of n elements where order does not matter
  dt_density[, pairs_max := fifelse(country_i == country_j,
                                    choose(n_organisations_i, 2),
                                    n_organisations_i * n_organisations_j)]

  # Handle the case of same-country, only one organisation from that country, i.e. the
  # binomial coefficient ends up being zero (as 'choose(1, 2) = 0') --> NA, as different
  # from an actual density of zero
  dt_density[, density_observed := fifelse(pairs_max == 0,
                                           NA_real_,
                                           n_organisation_pairs / pairs_max)]
  
  # Compute configuration model density, i.e. the expected number of edges between pair of
  # countries under the configuration model
  #     - Cross country (C_i != C_j): sum of degrees for country i multiplied by sum of
  #       degrees for country j, divided by twice the number of all edges (cf. Newman, 2006)
  #     - Same country (C_i == C_j): sum of degree for country i multiplied by sum of
  #       degrees for country i (= squared), divided by twice the doubled number of all
  #       edges to prevent double-counting of pairs
  dt_density[, edges_expected := fifelse(country_i == country_j,
                                         (sum_degree_i * sum_degree_i) / (4 * nrow(dt_edges)),
                                         (sum_degree_i * sum_degree_j) / (2 * nrow(dt_edges)))]
  
  # Handle case of same-country, only one organisation from that country (analogous to above)
  dt_density[, density_config := fifelse(pairs_max == 0,
                                         NA_real_,
                                         edges_expected / pairs_max)]

  # TODO: DO I WANT TO INCLUDE ANOTHER MEASUREMENT HERE?

  # Assemble results into pre-defined lists
  results_edges[[prog]] <- dt_country_pairs[, programme := prog]
  results_density[[prog]] <- dt_density
}

# Combine programme-specific computations into one data.table each
dt_country_pairs <- rbindlist(results_edges)
dt_country_density <- rbindlist(results_density)
