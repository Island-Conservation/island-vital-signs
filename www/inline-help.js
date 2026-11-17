// Toggles the short help note that sits under a control.
//
// One delegated listener rather than a handler per button, so notes added to
// other tabs later work without registering anything. The toggle is pure
// client side -- it never round-trips to R, so opening a definition cannot
// invalidate a reactive or redraw a map.
(function () {
  document.addEventListener("click", function (event) {
    var button = event.target.closest(".inline-help-btn");
    if (!button) return;

    event.preventDefault();

    var note = document.getElementById(button.getAttribute("aria-controls"));
    if (!note) return;

    var opening = note.hasAttribute("hidden");

    if (opening) {
      note.removeAttribute("hidden");
    } else {
      note.setAttribute("hidden", "");
    }

    button.setAttribute("aria-expanded", opening ? "true" : "false");
  });

  // Escape closes whichever note the reader is in or focused on.
  document.addEventListener("keydown", function (event) {
    if (event.key !== "Escape") return;

    document.querySelectorAll(".inline-help-note:not([hidden])").forEach(function (note) {
      note.setAttribute("hidden", "");

      var button = document.querySelector(
        '.inline-help-btn[aria-controls="' + note.id + '"]'
      );
      if (button) button.setAttribute("aria-expanded", "false");
    });
  });
})();
