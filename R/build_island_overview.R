# One row per island, for the Overview tab's map and table.
#
# The three metadata sources are shaped for the modules rather than for a
# table: island_metadata is one row per island, metric_metadata one row per
# island-metric, intervention_metadata one row per island-intervention. This
# folds the last two up so every island is a single row with its metrics and
# interventions collapsed into readable strings.
#
# The marker point is st_point_on_surface(), not st_centroid(). Eleven of the
# seventy-five islands are atolls, crescents or multi-part -- Bikar, Bokak,
# Temoe, Tenarunga, Vahanga and Anacapa among them -- and their centroid falls
# in open water, which would put their dot in the sea beside the island rather
# than on it. point_on_surface is guaranteed to land inside the polygon.
#
# islands:       island_metadata (sf)
# metrics:       metric_metadata
# interventions: intervention_metadata

build_island_overview <- function(islands, metrics, interventions) {

  metric_labels <- c(
    tcc = "Tree Cover",
    ndvi = "NDVI",
    evi = "EVI",
    ndmi = "NDMI",
    msavi2 = "MSAVI2"
  )

  # s2 will not do point_on_surface on lon/lat. These are marker positions on a
  # world map, so the planar answer is well within a pixel of the spherical one.
  # The setting is global, so it is put back before returning.
  was_s2 <- sf::sf_use_s2()
  sf::sf_use_s2(FALSE)
  on.exit(sf::sf_use_s2(was_s2), add = TRUE)

  xy <- st_coordinates(suppressWarnings(
    st_point_on_surface(st_geometry(islands))
  ))

  metrics_by_island <- metrics %>%
    mutate(label = unname(metric_labels[metric])) %>%
    group_by(gid2) %>%
    summarise(
      metrics_available = paste(sort(label), collapse = ", "),
      n_metrics = dplyr::n(),
      .groups = "drop"
    )

  interventions_by_island <- interventions %>%
    group_by(GID2) %>%
    summarise(
      intervention_types = paste(sort(unique(RemTyp)), collapse = ", "),
      # one line per removal for the map popup, where the species and year are
      # worth seeing; the table only shows the types
      intervention_detail = paste(
        sort(unique(paste0(RemTyp, ": ", Genus, " ", Species, " (", IntYr, ")"))),
        collapse = "<br>"
      ),
      first_intervention = suppressWarnings(min(IntYr, na.rm = TRUE)),
      .groups = "drop"
    ) %>%
    mutate(first_intervention = ifelse(
      is.finite(first_intervention), first_intervention, NA_real_
    ))

  islands %>%
    st_drop_geometry() %>%
    transmute(
      gid2 = GID2_ID,
      island = Island,
      short_name = str_replace_all(Isl, "_", " "),
      country = COUNTRY,
      area_km2 = AreaKm2,
      resolution_m = Res,
      productivity_metric = unname(metric_labels[Prod_Met])
    ) %>%
    mutate(
      lon = xy[, 1],
      lat = xy[, 2],
      # Longitude again, shifted into 0..360 for the Overview map only.
      #
      # That map is centred on the antimeridian, and leaflet draws a marker at
      # its literal longitude rather than wrapping it into the view. With the
      # centre at 180 the rendered span runs from about -111 eastward, so the
      # 22 islands west of that -- the whole of French Polynesia, Hawaii, Samoa,
      # Tonga, the Channel Islands, and Alofi at -178 -- were placed off the
      # left-hand edge and simply never appeared, while the table still counted
      # them. Adding 360 to the negative ones puts every island in one
      # continuous band around the Pacific. lon keeps the true value.
      lon_pacific = ifelse(lon < 0, lon + 360, lon)
    ) %>%
    left_join(metrics_by_island, by = "gid2") %>%
    left_join(interventions_by_island, by = c("gid2" = "GID2")) %>%
    mutate(
      # an island with no row in the metric or intervention table has none of
      # that thing, rather than an unknown amount of it
      metrics_available = coalesce(metrics_available, "None"),
      n_metrics = coalesce(n_metrics, 0L),
      intervention_types = coalesce(intervention_types, "None modeled"),
      intervention_detail = coalesce(intervention_detail, "None modeled"),
      has_intervention = intervention_types != "None modeled"
    ) %>%
    arrange(island)
}
