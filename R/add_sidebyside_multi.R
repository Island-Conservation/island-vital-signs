# Function for adding a side-by-side control that knows about every basemap
#
# leaflet.extras2::addSidebyside() binds the divider to exactly one tile layer
# per side, so as soon as the user picked a different basemap in the layers
# control the divider was clipping a layer that was no longer on the map and
# the split stopped working. That is a limit of the R wrapper, not of the
# underlying control: leaflet-side-by-side accepts an array of layers per side
# and re-picks whichever one is currently on the map, because it listens to
# "layeradd layerremove".
#
# This passes all of them through, using the method defined in
# www/splitmap.js, and pulls in the same JS dependency the package uses.
#
# map:      leaflet map to add the control to
# layerId:  id for the control itself
# leftIds:  layerIds of every basemap sitting in the left pane
# rightIds: layerIds of every basemap sitting in the right pane

add_sidebyside_multi <- function(map, layerId, leftIds, rightIds,
                                 options = list(thumbSize = 42, padding = 0)) {
  
  map$dependencies <- c(map$dependencies,
                        leaflet.extras2:::sidebysideDependencies())
  
  leaflet::invokeMethod(map, NULL, "addSidebysideMulti",
                        layerId, as.list(leftIds), as.list(rightIds),
                        leaflet::filterNULL(options))
  
}
