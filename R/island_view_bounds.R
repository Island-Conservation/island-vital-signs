# The lon/lat window a classified map should open on.
#
# Modules 3 and 4 draw only the pixels that answer a question, and leaflet
# frames whatever it is handed. When a question comes back with three pixels
# that meant opening at street level on an otherwise empty rectangle, with
# nothing on screen to say where on the island those three pixels were -- the
# resist-change question on Mona is the case that prompted this. Framing the
# island instead keeps the answer in context, and holds the view still while
# the reader changes question or probability level and the pixel count moves
# underneath them.
#
# The pixels are unioned in rather than replaced, so the window can only ever
# grow: a classified pixel that sits outside the mapped coastline still gets
# drawn inside the frame, and the view never closes in tighter than the
# island.
#
# Returns NULL when there is nothing usable to frame, which leaves leaflet to
# fit the drawn pixels the way it did before. An island whose extent wraps the
# antimeridian is one of those: its bounding box comes back spanning most of
# the globe, and fitting that would open the map on the whole world.
#
# island: one row of island_metadata (sf)
# sp:     the classified pixels being drawn (sf), or NULL
# pad:    fraction of the framed span added on every side, so coastal pixels
#         are not flush against the edge

island_view_bounds <- function(island, sp = NULL, pad = 0.05) {

  # the lon/lat bounding box of one layer, or NULL if it has nothing to give
  latlon_bbox <- function(x) {

    if (is.null(x) || nrow(x) == 0) return(NULL)

    geom <- sf::st_geometry(x)

    if (all(sf::st_is_empty(geom))) return(NULL)

    crs <- sf::st_crs(geom)

    # leaflet works in lon/lat whatever the layer arrived in
    if (!is.na(crs) && crs != sf::st_crs(4326)) {
      geom <- sf::st_transform(geom, 4326)
    }

    bb <- as.numeric(sf::st_bbox(geom))

    if (!all(is.finite(bb))) NULL else bb
  }

  boxes <- Filter(Negate(is.null), list(latlon_bbox(island), latlon_bbox(sp)))

  if (length(boxes) == 0) return(NULL)

  xmin <- min(vapply(boxes, `[`, numeric(1), 1))
  ymin <- min(vapply(boxes, `[`, numeric(1), 2))
  xmax <- max(vapply(boxes, `[`, numeric(1), 3))
  ymax <- max(vapply(boxes, `[`, numeric(1), 4))

  width <- xmax - xmin
  height <- ymax - ymin

  # an island drawn on both sides of the antimeridian, rather than an island
  # that really is half the planet across
  if (width > 180) return(NULL)

  # a degenerate extent still needs a window to open on
  if (width <= 0) width <- 0.01
  if (height <= 0) height <- 0.01

  c(
    lng1 = xmin - width * pad,
    lat1 = ymin - height * pad,
    lng2 = xmax + width * pad,
    lat2 = ymax + height * pad
  )
}
