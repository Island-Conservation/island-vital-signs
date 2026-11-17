# The Overview tab's world map: one marker per island in the tool.
#
# Only the basemaps and the view are built here. The markers are drawn by a
# leafletProxy in server.R so that changing a filter redraws the dots without
# resetting the pan and zoom the reader had set.
#
# The view is centred on the antimeridian rather than on Greenwich. Most of
# these islands are Pacific, and a map centred on 0 cuts the Pacific down the
# middle, scattering them to both edges. Zoom 1 is wide enough that the
# Atlantic, Caribbean and Mediterranean islands are still on screen.

create_overview_map <- function() {

  # The basemap is left to wrap. noWrap looks tidier in principle, but a world
  # is 512px wide at zoom 1 and this box is wider than that, so it leaves a
  # dead grey panel beside the map and clips the islands east of the
  # antimeridian. A repeated basemap at the edges is the lesser problem.
  leaflet(options = leafletOptions(worldCopyJump = TRUE, minZoom = 1)) %>%
    addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
    add_carto_basemap(group = "Light", style = "light") %>%
    # dark is added last so it is the one on top, and so the default
    add_carto_basemap(group = "Dark", style = "dark") %>%
    addLayersControl(
      baseGroups = c("Dark", "Light", "Satellite"),
      position = "topright"
    ) %>%
    setView(lng = 180, lat = 10, zoom = 1)
}

# Markers for whatever set of islands the filters currently leave.
# The two marker shapes, as inline SVG.
#
# These are icons rather than addCircleMarkers because leaflet has no second
# vector shape to offer, and a second shape is the point: colour alone is the
# one cue a colour-blind reader may not get, and the brand colours here --
# coral and surf blue -- are close in lightness as well as adjacent in hue.
#
# A data URI rather than a file in www/: makeIcon() treats a path that exists
# on disk as something to copy into the widget's own dependencies, which is a
# different thing from the URL the browser would resolve. Inlining the shape
# sidesteps the question.
overview_marker_svg <- function(shape, fill) {
  svg <- if (shape == "triangle") {
    paste0(
      "<svg xmlns='http://www.w3.org/2000/svg' width='20' height='18' ",
      "viewBox='0 0 20 18'><path d='M10 2.2 L18.3 16.2 L1.7 16.2 Z' fill='",
      fill, "' fill-opacity='0.9' stroke='#ffffff' stroke-width='1.6' ",
      "stroke-linejoin='round'/></svg>"
    )
  } else {
    paste0(
      "<svg xmlns='http://www.w3.org/2000/svg' width='18' height='18' ",
      "viewBox='0 0 18 18'><circle cx='9' cy='9' r='6.6' fill='",
      fill, "' fill-opacity='0.9' stroke='#ffffff' stroke-width='1.6'/></svg>"
    )
  }

  paste0(
    "data:image/svg+xml;charset=UTF-8,",
    utils::URLencode(svg, reserved = TRUE)
  )
}

# The anchor is the shape's visual centre rather than its top-left corner, so
# the marker sits on the island instead of below and to the right of it. For
# the triangle that centre is its centroid, a third of the way up from the
# base.
overview_marker <- list(
  intervention = list(
    url = overview_marker_svg("circle", coral),
    width = 18, height = 18, anchor_x = 9, anchor_y = 9,
    label = "Intervention modeled"
  ),
  none = list(
    url = overview_marker_svg("triangle", surf_blue),
    width = 20, height = 18, anchor_x = 10, anchor_y = 11,
    label = "No intervention modeled"
  )
)

# Markers for whatever set of islands the filters currently leave.
#
# Colour and shape together carry whether an intervention is modeled, which is
# the split that decides whether an island can be read before-and-after at
# all. Size is held constant: island area spans 0.2 to 627 km2, so a radius
# scaled to it would make most of the tool's islands too small to click.
#
# map:      a leafletProxy for the overview map
# islands:  rows of island_overview to draw

draw_overview_markers <- function(map, islands) {

  # An empty result is an ordinary outcome of the filters, but it cannot be
  # passed on to addMarkers: leaflet's safeLabel() does
  # sum(sapply(label, ...)), and sapply over a zero-length label vector hands
  # sum() an empty list, which it refuses. That error is raised inside an
  # observer, so Shiny tears the whole session down and the page freezes --
  # not just the map. Clearing and returning leaves an empty map, which is the
  # honest picture anyway.
  if (nrow(islands) == 0) {
    return(map %>% clearGroup("islands") %>% clearControls())
  }

  # The popup carries a button per module. Clicking one hands the island to
  # Shiny, which selects it everywhere and switches tab -- so the map is a way
  # into the modules rather than only a picture of them.
  #
  # The island and tab ride in data- attributes and a delegated listener in
  # www/overview-map.js reads them, rather than each button carrying its own
  # inline onclick. Island names contain brackets and spaces ("Alofi (Wallis
  # and Futuna)"), and building JavaScript string literals around those by
  # pasting means one unescaped quote away from a silently dead button.
  esc <- function(x) htmltools::htmlEscape(x, attribute = TRUE)

  open_buttons <- vapply(islands$island, function(nm) {
    paste0(
      "<div class='popup-open-row'>",
      "<span class='popup-open-label'>Open this island in:</span>",
      paste0(
        "<button type='button' class='popup-open-btn' data-island='",
        esc(nm), "' data-tab='", esc(overview_module_tabs), "'>",
        htmltools::htmlEscape(overview_module_tabs), "</button>",
        collapse = ""
      ),
      "</div>"
    )
  }, character(1), USE.NAMES = FALSE)

  popups <- paste0(
    "<strong>", islands$short_name, "</strong><br>",
    islands$country, "<br>",
    "<em>", round(islands$area_km2, 1), " km&sup2; &middot; ",
    islands$resolution_m, " m pixels</em><br><br>",
    "<strong>Metrics:</strong> ", islands$metrics_available, "<br>",
    "<strong>Intervention:</strong><br>", islands$intervention_detail,
    open_buttons
  )

  shape <- ifelse(islands$has_intervention, "intervention", "none")
  field <- function(nm) {
    unlist(lapply(overview_marker[shape], `[[`, nm), use.names = FALSE)
  }

  # Every island is drawn three times: one world to the west, one here, one to
  # the east.
  #
  # lon_pacific gathers the islands into a single band from 0 to 360 so that a
  # map centred on the antimeridian shows them together (see
  # build_island_overview()). But leaflet draws a marker only in the copy of
  # the world it was handed, and the map does not stay in one copy: panning
  # past an edge has worldCopyJump return the centre to the canonical
  # -180..180, and a zoom then renders against wherever the centre now is.
  # Everything the band had placed east of 180 -- Hawaii, French Polynesia,
  # Samoa, Tonga, the Channel Islands -- was left in a copy that was no longer
  # on screen, so those dots disappeared partway into a zoom while the table
  # still counted them. Repeating the band at -360, 0 and +360 means whichever
  # copy the map has settled in holds the full set.
  #
  # The repeats match the basemap, which already wraps, and each carries the
  # same label and popup, so landing on one is the same as landing on another.
  copies <- function(x) rep(x, 3)
  offsets <- rep(c(-360, 0, 360), each = nrow(islands))

  map %>%
    clearGroup("islands") %>%
    clearControls() %>%
    addMarkers(
      # lon_pacific, not lon: see build_island_overview() for why the islands
      # west of the antimeridian need shifting to be drawn at all
      lng = copies(islands$lon_pacific) + offsets,
      lat = copies(islands$lat),
      icon = icons(
        iconUrl = copies(field("url")),
        iconWidth = copies(field("width")),
        iconHeight = copies(field("height")),
        iconAnchorX = copies(field("anchor_x")),
        iconAnchorY = copies(field("anchor_y"))
      ),
      label = copies(islands$short_name),
      popup = copies(popups),
      group = "islands"
    ) %>%
    # addLegend draws colour swatches only, and a legend that cannot show the
    # shapes would undo the reason for having them.
    addControl(
      position = "bottomright",
      className = "info legend overview-legend",
      html = paste0(
        vapply(overview_marker, function(m) paste0(
          "<div class='overview-legend-row'>",
          "<img src='", m$url, "' width='", m$width,
          "' height='", m$height, "' alt=''>",
          "<span>", m$label, "</span></div>"
        ), character(1), USE.NAMES = FALSE),
        collapse = ""
      )
    )
}
