# Helpers for the raster downloads
#
# A downloaded file leaves the app and is opened somewhere else, often months
# later and by someone who never saw the controls that produced it. Band names
# carry some of what they need; the rest -- which island, which probability
# level, what the values encode -- cannot fit in a band name and goes in a
# README written beside the raster.
#
# Renaming means the download no longer matches the band names in the modeling
# outputs, so each README says what the bands were called there.

# ---- band names --------------------------------------------------------

# Annual stack: cell_id, then one band per year.
#   1994 -> tcc_1994
rename_annual_bands <- function(stack, metric_code) {
  is_year <- names(stack) != "cell_id"

  names(stack)[is_year] <- paste0(metric_code, "_", names(stack)[is_year])

  stack
}

# Change stack: drop the x/y bands, which only restate the raster's own
# geometry, fold the per-period cell_id bands into one, and spell out the
# rest.
#   posterior.mean_fulltime        -> change_fulltime
#   posterior.mean_hpdi95_fulltime -> change_fulltime_prob95
prep_change_bands <- function(stack) {
  stack <- stack[[!grepl("^[xy]_", names(stack))]]

  # One cell id per pixel, not one per time period. The per-period bands
  # carry the same ids as each other, so this is a rename rather than a
  # choice -- but take the cell-wise maximum rather than simply the first
  # band, so a period that ever did cover different pixels is still
  # carried through rather than silently dropped.
  id_bands <- grep("^cell_id", names(stack), value = TRUE)

  cell_id <- if (length(id_bands) > 1) {
    max(stack[[id_bands]], na.rm = TRUE)
  } else {
    stack[[id_bands]]
  }
  names(cell_id) <- "cell_id"

  stack <- stack[[!grepl("^cell_id", names(stack))]]

  nms <- names(stack)
  nms <- sub("^posterior\\.mean_hpdi([0-9]+)_(.*)$", "change_\\2_prob\\1", nms)
  nms <- sub("^posterior\\.mean_(.*)$", "change_\\1", nms)
  names(stack) <- nms

  c(cell_id, stack)
}

# Classified result: a single band, named for the metric and the question that
# produced it.
#   baseline.high -> tcc_high_condition
rename_classified_band <- function(r, metric_code, question_fn) {
  names(r) <- paste0(metric_code, "_", question_fn)

  r
}

# ---- README ------------------------------------------------------------

# "EPSG:4326 (WGS 84)", or a fallback when the raster cannot name its own CRS
crs_label <- function(r) {
  described <- try(terra::crs(r, describe = TRUE), silent = TRUE)

  if (
    inherits(described, "try-error") ||
      is.null(described$code) ||
      is.na(described$code)
  ) {
    return("see the file's own CRS metadata")
  }

  paste0(described$authority, ":", described$code, " (", described$name, ")")
}

readme_header <- function(tif_name, island, metric_label, r, res_m) {
  c(
    "Island Vital Signs -- downloaded data",
    "=====================================",
    "",
    paste0("File:       ", tif_name),
    paste0("Downloaded: ", format(Sys.Date(), "%Y-%m-%d")),
    paste0("Island:     ", island),
    paste0("Metric:     ", metric_label),
    paste0("Resolution: ", res_m, " m"),
    paste0("CRS:        ", crs_label(r)),
    ""
  )
}

readme_annual <- function(tif_name, island, metric_label, metric_code,
                          stack, res_m) {
  # the bands are already renamed by this point, so read the years back out
  # of them rather than reporting "tcc_1994" as the first year
  years <- sub(
    paste0("^", metric_code, "_"), "",
    names(stack)[names(stack) != "cell_id"]
  )

  band_label <- paste0(metric_code, "_<year>")
  pad <- function(x) formatC(x, width = max(nchar(band_label), 7), flag = "-")

  c(
    readme_header(tif_name, island, metric_label, stack, res_m),
    "WHAT THIS HOLDS",
    paste0(
      "Modeled annual values, one band per year from ",
      min(years), " to ", max(years), ". Each value is the"
    ),
    "model's posterior mean for that pixel in that year -- a prediction, not a",
    "direct observation. Pixels outside the modeled area are NA.",
    "",
    "BANDS",
    paste0("  ", pad("cell_id"), "  pixel identifier, the same in every band"),
    paste0(
      "  ", pad(band_label), "  modeled ", metric_label, " for that year (",
      length(years), " bands)"
    ),
    "",
    "ORIGINAL BAND NAMES",
    "In the modeling outputs the year bands are named by the year alone",
    "(\"1994\"); cell_id is unchanged."
  )
}

readme_change <- function(tif_name, island, metric_label, metric_code,
                          stack, res_m, change_type, viewed_period,
                          viewed_probability) {
  periods <- sub("^change_", "",
                 grep("_prob[0-9]+$", names(stack), value = TRUE, invert = TRUE))
  periods <- periods[periods != "cell_id"]

  c(
    readme_header(tif_name, island, metric_label, stack, res_m),
    paste0("Change type: ", change_type),
    "",
    "WHAT THIS HOLDS",
    if (identical(tolower(change_type), "annualized")) {
      "Average change per year over each time period, in metric units per year."
    } else {
      "Total change across each time period, in metric units."
    },
    "",
    "This file holds EVERY time period and probability level modeled for this",
    paste0(
      "island, not only the one that was on screen (", viewed_period,
      ", ", viewed_probability, ")."
    ),
    "",
    "BANDS",
    "  cell_id                   pixel identifier, one band for the whole file",
    "",
    "then, for each time period:",
    "  change_<period>           modeled change, all pixels",
    "  change_<period>_prob75    the same, keeping only pixels whose change is",
    "  change_<period>_prob85      clear at that probability level; pixels that",
    "  change_<period>_prob95      do not reach it are NA",
    "",
    paste0("Periods in this file: ", paste(periods, collapse = ", ")),
    "",
    "ORIGINAL BAND NAMES",
    "In the modeling outputs these are posterior.mean_<period> and",
    "posterior.mean_hpdi<level>_<period>, and cell_id is repeated once per",
    "period as cell_id_<period>. Bands named x_<period> and y_<period> are",
    "dropped here: they repeat the pixel coordinates the raster already",
    "carries."
  )
}

readme_classified <- function(tif_name, island, metric_label, metric_code,
                              r, res_m, question, question_fn, categories,
                              probability) {
  category_lines <- if (is.null(categories)) {
    "  1   this pixel answers the question"
  } else {
    paste0(
      "  ", format(categories$value), "   ", categories$label
    )
  }

  c(
    readme_header(tif_name, island, metric_label, r, res_m),
    paste0("Question:   ", question),
    if (!is.null(probability)) paste0("Probability level: ", probability),
    "",
    "WHAT THIS HOLDS",
    "One band. Only the pixels that answer the question carry a value; every",
    "other modeled pixel is NA. The map in the app draws exactly these pixels.",
    "",
    "VALUES",
    category_lines,
    "  NA  modeled, but does not answer the question (or outside the model)",
    "",
    "BANDS",
    paste0("  ", metric_code, "_", question_fn),
    "",
    "ORIGINAL BAND NAME",
    "In the modeling outputs this band is named after the function that built",
    "it, without the metric."
  )
}

# ---- the download itself -----------------------------------------------

# Write the raster and its README into the zip the browser receives.
write_download_zip <- function(file, layers, tif_name, readme_lines) {
  staging <- file.path(
    tempdir(),
    paste0("ivs_download_", format(Sys.time(), "%Y%m%d%H%M%S"), "_", Sys.getpid())
  )

  dir.create(staging, showWarnings = FALSE, recursive = TRUE)
  on.exit(unlink(staging, recursive = TRUE), add = TRUE)

  writeRaster(
    layers,
    file.path(staging, tif_name),
    filetype = "GTiff",
    overwrite = TRUE
  )

  writeLines(readme_lines, file.path(staging, "README.txt"))

  # zip from inside the staging directory so the archive holds two files
  # rather than a chain of temp folders
  zip::zip(
    zipfile = file,
    files = c(tif_name, "README.txt"),
    root = staging
  )
}
