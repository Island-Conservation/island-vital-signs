/* Multi-basemap support for the side-by-side (split) maps.
 *
 * leaflet.extras2's addSidebyside() binds the control to exactly one tile
 * layer per side, so switching basemaps in the layers control left the
 * divider clipping a layer that was no longer on the map. The underlying
 * leaflet-side-by-side control already accepts an ARRAY of layers per side
 * and re-picks whichever one is currently on the map (it listens to
 * "layeradd layerremove"), so this method just passes all of them through.
 */
LeafletWidget.methods.addSidebysideMulti = function(layerId, leftIds, rightIds, options) {
  var map = this;

  var resolve = function(ids) {
    if (!ids) return [];
    if (!Array.isArray(ids)) ids = [ids];
    return ids
      .map(function(id) { return map.layerManager.getLayer("tile", id); })
      .filter(function(layer) { return layer; });
  };

  var control = L.control.sideBySide(resolve(leftIds), resolve(rightIds), options);

  map.controls.add(control, layerId);
};
