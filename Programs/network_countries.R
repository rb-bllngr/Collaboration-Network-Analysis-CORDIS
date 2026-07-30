# network_countries.R: Investigate country-level aggregated data on coordinator roles and
#                      country-pair collaboration for H2020 and HORIZON EUROPE networks.

# Load networks for each programme (both unweighted and weighted as needed for different
# analyses)
networks <- list(H2020 = readRDS(file.path(PATHS$DATA_INT, "network_h2020.RDS")),
                 HORIZON = readRDS(file.path(PATHS$DATA_INT, "network_horizon.RDS")))
programmes <- names(networks)

# Load CORDIS data refined by population information for countries participating in the
# programmes.
cordis_population <- readRDS(file.path(PATHS$DATA_INT, "cordis_population.RDS"))
dt_population <- unique(rbindlist(list(
  cordis_population[, .(programme = "H2020", country, population = population_h2020)],
  cordis_population[, .(programme = "HORIZON", country, population = population_horizon)]
)))
dt_population <- dt_population[(is.na(country) == FALSE) & (is.na(population) == FALSE)]

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

# Attach population data for inspection alongside organisation counts
dt_country_summary <- merge(dt_country_summary, dt_population,
                            by = c("programme", "country"), all.x = TRUE)
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
  #     - cross-country (C_i != C_j): n_organisations_i x n_organisations_j
  #     - within-country (C_i == C_j): use binomial coefficient to calculate the amount of
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
  #     - cross-country (C_i != C_j): sum of degrees for country i multiplied by sum of
  #       degrees for country j, divided by twice the number of all edges (cf. Newman, 2006)
  #     - within-country (C_i == C_j): sum of degree for country i multiplied by sum of
  #       degrees for country i (= squared), divided by twice the doubled number of all
  #       edges to prevent double-counting of pairs
  dt_density[, edges_expected := fifelse(country_i == country_j,
                                         (sum_degree_i * sum_degree_i) / (4 * nrow(dt_edges)),
                                         (sum_degree_i * sum_degree_j) / (2 * nrow(dt_edges)))]
  
  # Handle case of same-country, only one organisation from that country (analogous to above)
  dt_density[, density_config := fifelse(pairs_max == 0,
                                         NA_real_,
                                         edges_expected / pairs_max)]

  # Compute collaboration preference ratio
  dt_density[, collab_preference := fifelse(density_config == 0,
                                            NA_real_,
                                            density_observed / density_config)]

  # Interactions between countries as the observed cross-country density, normalized by the
  # arithmetic mean density of the respective countries' internal (within-country) density.
  # NA where within-country density is NA itself (i.e. country with only one organisation).
  dt_density_within <- dt_density[country_i == country_j, .(country_i, density_observed)]
  setnames(dt_density_within,
           old = c("country_i", "density_observed"), new = c("country", "density_within"))

  dt_density <- merge(dt_density, dt_density_within, by.x = "country_i", by.y = "country")
  dt_density <- merge(dt_density, dt_density_within, by.x = "country_j", by.y = "country")
  setnames(dt_density,
           old = c("density_within.x", "density_within.y"),
           new = c("density_within_i", "density_within_j"))

  # Calculate relative density
  dt_density[,
    density_relative := density_observed / (1/2 * (density_within_i + density_within_j))
  ]
  # Note: Over 1/3 of the entries end up being NAs...

  # Assemble results into pre-defined lists
  results_edges[[prog]] <- dt_country_pairs[, programme := prog]
  results_density[[prog]] <- dt_density
}

# Combine programme-specific computations into one data.table each
dt_country_pairs <- rbindlist(results_edges)
dt_country_density <- rbindlist(results_density)

# Rank the most common country combinations once by collaborating organisation pairs and
# once by shared-project weight (constrained on cross-country) per programme
top_ranks_pairs <- dt_country_pairs[country_i != country_j][
  order(programme, -n_organisation_pairs)
][, .SD[1:10], by = programme]
print(top_ranks_pairs)

top_ranks_weight <- dt_country_pairs[country_i != country_j][
  order(programme, -sum_weight)
][, .SD[1:10], by = programme]
print(top_ranks_weight)


# Now, including centrality into country analysis. Starting by merging country data on an
# organisational level with centrality data computed and saved in 'network_centrality.R'
dt_centrality <- readRDS(file.path(PATHS$DATA_INT, "centrality.RDS"))

dt_centrality_organisations <- rbindlist(lapply(programmes, function(prog) {
  # Take needed organisational country information ...
  dt_temp <- data.table(
    programme = prog,
    organisationID = V(networks[[prog]]$unweighted)$name,
    country = V(networks[[prog]]$unweighted)$country
  )
  dt_temp <- dt_temp[is.na(country) == FALSE & grepl(pattern = "^[A-Z]{2}$", country)]

  # ... and merge with the from other script imported centrality data
  merge(dt_temp, dt_centrality[programme == prog], by = c("programme", "organisationID"))
}))

# Recall centrality overview from 'network_roles.R'
centrality <- list(
  degree = list(name = "degree_norm", giant_comp = FALSE),
  betweenness = list(name = "betweenness_unweighted_norm", giant_comp = FALSE),
  closeness = list(name = "closeness_unweighted", giant_comp = TRUE),
  eigenvector = list(name = "eigenvector_unweighted", giant_comp = TRUE)
)

# Aggregate the centrality measure on country level
dt_centrality_country <- rbindlist(lapply(programmes, function(prog) {
  rbindlist(lapply(names(centrality), function(measure) {
    dt_prog <- dt_centrality_organisations[programme == prog]

    # If centrality measure requires it, restrict to giant component
    if (centrality[[measure]]$giant_comp == TRUE) {
      dt_prog <- dt_prog[in_giant_comp == TRUE]
    }

    # Compute aggregated centrality measures
    dt_aggregate <- dt_prog[, .(
      n_organisations = .N,
      centr_sum = sum(.SD[[1]], na.rm = TRUE),
      centr_mean = mean(.SD[[1]], na.rm = TRUE)
    ), by = country, .SDcols = centrality[[measure]]$name]

    dt_aggregate[, centr_share := centr_sum / sum(centr_sum)]
    dt_aggregate[, ":=" (programme = prog, measure = measure)]
    dt_aggregate
  }))
}))

# Convert measures to factor again and add EU-country flag for visualization purposes
dt_centrality_country[, measure := factor(measure, levels = order_centrality)]
dt_centrality_country[, isEU := fifelse(programme == "H2020",
                                        country %in% names(eu28), country %in% names(eu27))]

# Attach population and derive population-normalised (per capita) centrality alongside the
# existing organisation count-normalised version
dt_centrality_country <- merge(dt_centrality_country, dt_population,
                               by = c("programme", "country"), all.x = TRUE)
dt_centrality_country[, centr_percapita := centr_sum / population]
print(dt_centrality_country[order(programme, measure, -centr_sum)])

# Visualize ranking of the top-n countries by size-normalized centrality measures
top_ranks_n <- 20
for (prog in programmes) {
  plots_ranking <- list()

  for(name in names(centrality)) {
  dt_ranking <- dt_centrality_country[(programme == prog) & (measure == name)][
    order(-centr_mean)][1:min(top_ranks_n, .N)]
  
  plots_ranking[[name]] <-
    ggplot(dt_ranking, aes(x = reorder(country, centr_mean), y = centr_mean, fill = isEU)) +
    geom_col() +
    coord_flip() +
    scale_y_continuous(labels = scales::label_number(accuracy = 0.00001, drop0trailing = TRUE),
                       n.breaks = 3) +
    scale_fill_manual(values = colorblindfriendly(),
                      labels = c("Kein Mitglied der Europäischen Union",
                                 "Mitglied der Europäischen Union")) +
    labs(x = NULL, fill = NULL,
         y = paste0("Mittlere normierte Zentralität je Organisation"),
         title = mapping_centrality[[name]]) +
    theme_lmu() +
    theme(plot.title = element_text(face = "plain", hjust = 0.5, size = 12))
  }

  # Assemble programme-plot with all four centrality measures
  plot_combined_ranks <- wrap_plots(plots_ranking, ncol = 4, guides = "collect", axis_titles = "collect") &
    theme(legend.position = "bottom")
  save_plot_lmu(plot_combined_ranks, paste0("countries_ranking_", tolower(prog), ".png"))
}

# Visualize the ranking evolution from H2020 to HORIZON, per measure (restricted to EU
# countries, as many countries in top ranking positions are non-EU but programmes are directed
# at collaboration opportunities for EU countries)
# Note: As the United Kingdom has left the European Union in 2020 due to BREXIT, the UK
#       would not be featured as EU member in HORIZON like in H2020. For comparability
#       reasons within the ranking evolution plot, the UK is going to be treated as EU
dt_rank_EU <- dt_centrality_country[(isEU == TRUE) | (country == "UK")]
dt_rank_EU[, programme := factor(programme, levels = names(networks))]
dt_rank_EU[, country_name := fifelse(country == "UK", eu28[["UK"]], eu27[country])]

# Plot the four centrality measure ranking evolution from H2020 to HORIZON and build all
# of them together
plot_combined_evolution_mean <- build_ranking_evolution_plot(
  dt_rank_EU, ranked_by = "centr_mean", y_label = "Rang (nach mittlerer normierter Zentralität)"
)
save_plot_lmu(plot_combined_evolution_mean, "countries_ranking_evolution_mean.png",
              width = 20, height = 8)

# Plot the same four centrality measure ranking evolution from H2020 to HORIZON, but for
# summed not mean centrality
plot_combined_evolution_sum <- build_ranking_evolution_plot(
  dt_rank_EU, ranked_by = "centr_sum", y_label = "Rang (nach summierter normierter Zentralität)"
)
save_plot_lmu(plot_combined_evolution_sum, "countries_ranking_evolution_sum.png",
              width = 20, height = 8)

# Plot the same four centrality measures ranking evolution from H2020 to HORIZON, but for
# the population-normalised (per capita) centrality
plot_combined_evolution_percapita <- build_ranking_evolution_plot(
  dt_rank_EU, ranked_by = "centr_percapita",
  y_label = "Rang (nach bevölkerungsnormierter Zentralität)"
)
save_plot_lmu(plot_combined_evolution_percapita, "countries_ranking_evolution_percapita.png",
              width = 20, height = 8)

# Scatter plot of centrality share vs. organisation count/country size, looped over both
# as the size measure
size_variable <- c(n_organisations = "Anzahl an Organisationen [log10]",
                   population = "Landesbevölkerung [log10]")
for (var in names(size_variable)) {
  plot_share_size <-
    ggplot(dt_centrality_country,
           aes(x = .data[[var]], y = centr_share * 100, color = isEU)) +
    geom_point(size = 2, alpha = 0.5) +
    scale_x_log10(labels = scales::label_number(drop0trailing = TRUE)) +
    scale_color_manual(values = colorblindfriendly(),
                       labels = c("Kein Mitglied der Europäischen Union",
                                  "Mitglied der Europäischen Union")) +
    labs(x = size_variable[[var]], y = "Anteil an Gesamtzentralität [%]",
         color = NULL) +
    facet_grid(programme ~ measure, labeller = labeller(measure = mapping_centrality)) +
    theme_lmu() +
    theme(legend.position = "bottom")

  if (var == "n_organisations") {
    identifier <- "organisations"
  } else {
    identifier <- "population"
  }
  save_plot_lmu(plot_share_size,
                paste0("countries_share_", identifier,".png"), width = 14, height = 6)
}

# Visualize network geographically (as map) with built in country-level information for
# node appearance
# Import CORDIS' geodata to access geolocation for organisations
dt_geo <- readRDS(file.path(PATHS$DATA_INT, "geodata.RDS"))

# Calculate the country centroids of mean latitude and longitude across a country's
# organisations with their geolocations
dt_centroid <- merge(dt_country[, .(programme, organisationID, country)], dt_geo,
                              by = "organisationID")
message(nrow(dt_country) - nrow(dt_centroid), " organisation-programme ",
        "combinations have been excluded due to missing geolocation for organisation")

dt_centroid <- dt_centroid[, .(
  latitude = mean(latitude, na.rm = TRUE),
  longitude = mean(longitude, na.rm = TRUE)
), by = .(programme, country)]

# Compute country-level total connections (summed up organisation-level degree; raw) and
# add to centroid data
dt_country_connections <- dt_centrality_organisations[,
  .(n_connections = sum(degree, na.rm = TRUE)), by = .(programme, country)
]
dt_centroid <- merge(dt_centroid, dt_country_connections, by = c("programme", "country"))

# Plot the geolocal representation of the countries in the programmes collaborating with
# each other, while only showing the above selected most 'important' edges
world <- map_data("world")

for (prog in programmes) {
  for (scope in c("world", "EU")) {
    data <- build_graph_fundamentals(dt_nodes = dt_centroid,
                                     dt_edges = dt_country_pairs,
                                     prog = prog,
                                     eu = (scope == "EU"),
                                     associated = names(associated_countries[[prog]]))

    # Inform the user about what percentage of the possible country-pairs is retained using
    # the current way (MST chooses N-1 edges and manually take overall N-1 strongest links
    # --> at most 2(N-1), fewer if they overlap)
    message("The foundational edge data (scope: ", scope, ") retains ", nrow(data$edges),
            " of ", choose(n = nrow(data$nodes), k = 2), " possible country pairs (",
            round(nrow(data$edges) / choose(nrow(data$nodes), 2) * 100, 1), "%)")

    # Create all different versions of country-map plotting scheme and save relevant ones
    plot_geographic <- build_country_plot_map(data$nodes, data$edges, world)
    if (scope == "world") {
      save_plot_lmu(plot_geographic,
                    paste0("countries_map_geographic_", tolower(prog), "_", scope, ".png"))
    }

    if (scope == "world") {
      plot_geographic_zoom <- plot_geographic +
        coord_cartesian(xlim = c(-22, 48), ylim = c(30, 65))
    } else {
      plot_geographic_zoom <- plot_geographic +
        coord_cartesian(xlim = c(-25, 45), ylim = c(30, 65)) +
        guides(size = "none")
    }
    save_plot_lmu(plot_geographic_zoom,
                  paste0("countries_map_geographic_", tolower(prog), "_", scope, "_zoomed.png"))

    plot_abstract <- build_country_plot_abstract(data$nodes, data$edges)
    if (scope == "EU") {
      labels_prog <- country_labels_EU_plus_associated[[prog]]

      plot_abstract <- plot_abstract +
        geom_node_text(aes(label = fifelse(name %in% names(labels_prog), labels_prog[name], name)),
                       repel = TRUE, size = 3, color = lmu_default_color(), segment.color = NA)+
        guides(size = "none")
    }
    save_plot_lmu(plot_abstract,
                  paste0("countries_map_abstract_", tolower(prog), "_", scope, ".png"))
  }
}
