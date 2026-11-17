// Behaviour for the Overview tab's island map: keeping it correctly sized
// across tab switches, and the "open this island in" buttons in its popups.

// Leaflet measures its container once, when the map is created, and a map
// inside a hidden tab measures 0x0. The Overview map is only hidden once the
// reader leaves the tab -- which the "open this island in" buttons now make
// them do -- and on the way back it is still working from that stale size:
// tiles stop halfway across, and popups open in the top-left corner. There is
// no invalidateSize() in leaflet's R API, so the nudge happens here, on
// Bootstrap's own "this tab is now visible" event.
$(document).on("shown.bs.tab", function () {
  var widget = typeof HTMLWidgets === "undefined"
    ? null
    : HTMLWidgets.find("#overview_map");

  if (widget && widget.getMap) {
    widget.getMap().invalidateSize();
  }
});

// The listener is on document rather than on the buttons themselves. Leaflet
// builds a popup's DOM when it opens and throws it away when it closes, so
// there is nothing to bind to at startup and anything bound to one popup dies
// with it. Delegation means the handler outlives every popup.
document.addEventListener("click", function (event) {
  // every click on the page reaches this, and not all of them land on an
  // element -- closest() does not exist on a text node or on document itself
  var target = event.target;

  if (!target || typeof target.closest !== "function") {
    return;
  }

  var btn = target.closest(".popup-open-btn, .module-card");

  if (!btn) {
    return;
  }

  // Shiny is not on the page while the UI is still being written out, and a
  // click cannot arrive before then -- but a stale popup in a disconnected
  // session can still be clicked, and that should do nothing rather than throw.
  if (typeof Shiny === "undefined" || !Shiny.setInputValue) {
    return;
  }

  Shiny.setInputValue(
    "ov_open_module",
    {
      // the shortcut cards carry no island and leave this null, which the
      // server reads as "just change tab, leave the selection alone"
      island: btn.getAttribute("data-island"),
      tab: btn.getAttribute("data-tab"),
      // the same island and tab twice in a row is a repeat request to go
      // there, not a no-op, so the value has to differ each time
      nonce: Math.random()
    },
    { priority: "event" }
  );
});
