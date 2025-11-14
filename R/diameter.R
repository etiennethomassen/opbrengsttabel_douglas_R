#' Calculate breast height diameter (d_bt) based on top height and stocking
#'
#' This function computes the breast height diameter d_bt (at breast height) for a given
#' top height `h_top` and initial stocking `N0`, using a biologically-informed
#' nonlinear adjustment formula.
#' formula 46
#'
#' The formula includes a reference diameter `d7` adjusted for tree growth ratio
#' differences (`Tgr_difference`), and a height-dependent adjustment if `h_top <= 7`.
#'
#' @param h_top Top height (m). Numeric scalar.
#' @param N0 Initial number of trees per hectare. Numeric scalar.
#' @param c11, c12, c13, c14 Coefficients (numeric scalars).
#'
#' @return Estimated breast height diameter (numeric scalar).
#'
#' @examples
#' calculate_d_bt(h_top = 6.5, N0 = 6000, c11 = 1.5, c12 = 0.3, c13 = 12, c14 = 0.05)
#' calculate_d_bt(h_top = 8, N0 = 6000, c11 = 1.5, c12 = 0.3, c13 = 12, c14 = 0.05)
#'
#' @export
calculate_d_bt <- function(h_top, N0, c11, c12, c13, c14) {
  # Validate inputs
  if (!is.numeric(h_top) || !is.numeric(N0)) stop("h_top and N0 must be numeric")
  if (N0 <= 0) stop("N0 must be positive")

  # Step 1: Calculate Tgr_difference
  Tgr_diff <- (10746 / sqrt(N0) - 10746 / sqrt(5000)) / 3.7

  # Step 2: Compute d7 (reference diameter at h_top = 7)
  d7 <- c13 * (1 + c14 * Tgr_diff)

  # Step 3: Apply piecewise formula for d_bt
  if (h_top <= 7) {
    numerator <- exp(-c11 * exp(-c12 * (h_top - 1.3)))
    denominator <- exp(-c11 * exp(-c12 * (7 - 1.3)))
    d_bt <- d7 * (numerator / denominator)
  } else {
    d_bt <- d7  # no adjustment needed
  }

  return(d_bt)
}
