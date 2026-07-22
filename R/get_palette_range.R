get_palette_range <- function(sf_obj, col = "value") {
  # Extract the numeric vector safely
  x <- sf_obj[[col]]
  
  # Compute the largest absolute value, ignoring NA
  A <- max(abs(x), na.rm = TRUE)
  
  # Handle the case where all values are NA (A becomes -Inf)
  if (!is.finite(A)) {
    warning("get_palette_range(): All values are NA or non-finite; returning c(-0, 0).")
    A <- 0
  }
  
  c(-A, A)
}
