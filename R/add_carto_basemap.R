# Function for adding the dark or light basemap to a leaflet map
#
# CARTO began watermarking anonymous basemap requests in August 2026: without a
# key their endpoint returns an "API KEY REQUIRED" tile for every coordinate.
# global.R resolves the CARTO_API_KEY environment variable once and sets the
# basemap_* globals to either the CARTO templates or the key-free Esri gray
# canvas fallback, so every map in the app requests tiles the same way and no
# map ever ends up with the watermark tile under it.
#
# map:     leaflet map to add the basemap to
# group:   layer control group the basemap belongs to
# layerId: id for the tile layer; the side-by-side control references these
# options: tile options, e.g. pathOptions(pane = "left") for a split map
# style:   "dark" or "light" raster style

add_carto_basemap <- function(map, group = "Dark", layerId = NULL,
                              options = tileOptions(),
                              style = c("dark", "light")) {

  style <- match.arg(style)
  tile_url <- if (style == "light") basemap_light_url else basemap_dark_url

  addTiles(map,
           urlTemplate = tile_url,
           attribution = basemap_attribution,
           group = group,
           layerId = layerId,
           options = options)

}
