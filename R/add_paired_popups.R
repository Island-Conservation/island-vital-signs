# Function for pairing the two sides of a split map in one popup

# The left and right layers are prepped independently, so each one only carries
# its own value and a click reports a single year. Matching the two on cell_id
# lets a click on either side report both years at once.
add_paired_popups <- function(sp_left, sp_right, label_left, label_right,
                              metric_selection) {

  needed <- c("cell_id", "value", "X", "Y")

  # The cell_id layer is prepped without a cell_id column of its own, so there
  # is nothing to pair on; leave those layers exactly as they came.
  if (!all(needed %in% names(sp_left)) || !all(needed %in% names(sp_right))) {
    return(list(left = sp_left, right = sp_right))
  }

  lookup_left <- setNames(sp_left$value, as.character(sp_left$cell_id))
  lookup_right <- setNames(sp_right$value, as.character(sp_right$cell_id))

  format_value <- function(x) {
    ifelse(is.na(x), "not available", as.character(round(x, 6)))
  }

  build_popup <- function(sp) {
    ids <- as.character(sp$cell_id)

    paste0(
      "Latitude: ", sp$Y,
      "<br>Longitude: ", sp$X,
      "<br>", metric_selection, " (", label_left, "): ",
      format_value(unname(lookup_left[ids])),
      "<br>", metric_selection, " (", label_right, "): ",
      format_value(unname(lookup_right[ids]))
    )
  }

  sp_left$popup_text <- build_popup(sp_left)
  sp_right$popup_text <- build_popup(sp_right)

  list(left = sp_left, right = sp_right)
}
