# Pick one period's layer out of an accumulated change stack.
#
# The accumulated rasters do not all cover the same time periods. Of the 198
# in data/, 72 carry the full set -- fulltime, pre, post1 and the deltas --
# and 9 carry that plus post2, but 90 carry pre alone and 27 carry post1
# alone. Which ones an island has depends on when it was modeled relative to
# its intervention, and time-periods-metadata.csv records the same thing.
#
# The question functions were written for the first case and the pre-only
# case: they looked for _fulltime, then fell back to _pre. On the 27
# post1-only rasters neither name is there, so terra raised
# "[subset] invalid name(s)". That happens inside an observer, which takes the
# whole Shiny session down rather than just failing the one map -- the page
# freezes and every other module stops responding too.
#
# So: take the whole series where there is one, and otherwise take whichever
# single period the island was actually modeled over. For 171 of the 198 this
# picks exactly what the old code picked; for the other 27 it picks the only
# trend that exists instead of failing.
#
# The deltas are deliberately not in the order. post1pre_delta and
# post2post1_delta are differences between two periods rather than a trend
# through one, so they do not answer "is this pixel increasing".
#
# x:      accumulated change SpatRaster
# prefix: the layer stem to look for, e.g. "posterior.mean" or
#         "posterior.mean_hpdi85"

accumulated_period_order <- c("fulltime", "pre", "post1", "post2")

accumulated_layer <- function(x, prefix) {

  candidates <- paste0(prefix, "_", accumulated_period_order)
  found <- candidates[candidates %in% names(x)]

  # Every raster in the current data has one of these, so this is a guard
  # rather than an expected path -- but it is the difference between a
  # readable message and taking the session down, which is what the bug it
  # replaces actually did.
  validate(need(
    length(found) > 0,
    paste(
      "This island and metric have no modeled trend that can answer this",
      "question. Try another metric, or another island."
    )
  ))

  x[[found[1]]]
}

# Which period accumulated_layer() will have used, for saying so on screen.
#
# Answering "is this pixel increasing" over the post-intervention period is
# not the same claim as answering it over the full satellite record, and
# which one a reader gets depends on the island. Without this the two look
# identical on the map.
#
# This reads the plain posterior.mean names rather than the interval ones.
# The two always agree -- checked across all 198 accumulated rasters at all
# three probability levels -- and this way the label does not need to know
# which level is selected.

accumulated_period <- function(x) {
  found <- accumulated_period_order[
    paste0("posterior.mean_", accumulated_period_order) %in% names(x)
  ]

  if (length(found) == 0) NA_character_ else found[1]
}

# A period as a reader would say it, or NULL if there is nothing to say.
# fulltime returns NULL deliberately: the whole record is what someone would
# assume they were getting, so naming it is noise. The others are the cases
# worth flagging.

accumulated_period_phrase <- function(period) {
  if (is.na(period)) {
    return(NULL)
  }

  switch(
    period,
    fulltime = NULL,
    pre = "the pre-intervention period",
    post1 = "the post-intervention period",
    post2 = "the second post-intervention period",
    NULL
  )
}
