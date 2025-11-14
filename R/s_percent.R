#' Calculate the S-Percent (Hart-Becking Spacing Index)
#'
#' This function computes the S-percent (S%), also known as the Hart-Becking Spacing Index,
#' which quantifies the degree of thinning in a forest stand. It is based on the average
#' top height of trees (`h_top`) and the number of trees per hectare (`N_at`).
#'
#' The approximate formula used is:
#' \deqn{S\% = 10745.7 / (h_top * sqrt(N_at))}
#'
#' @param h_top Numeric. The top height (dominant height) of the stand in meters.
#' @param N_at Numeric. The number of trees per hectare (stand density).
#'
#' @return Numeric. The calculated S-percent (spacing percentage), representing the relative spacing in the stand.
#'
#' @details
#' The S-percent was introduced by Hart (1928) and approximates the ratio between spacing and tree height.
#' A higher S% implies a more widely spaced (or thinned) stand. It is a key index in silviculture and thinning studies.
#'
#' The approximation 10745.7 is derived from:
#' \deqn{100 * sqrt((10000 * 2) / (sqrt(3))) ≈ 10745.7}
#'
#' @examples
#' # Example: top height = 25 meters, 600 trees per hectare
#' calculate_s_percent(h_top = 25, N_at = 600)
#'
#' @export
calculate_s_percent <- function(h_top, N_at) {
  # Input validation
  if (!is.numeric(h_top) || any(h_top <= 0)) stop("h_top must be a positive numeric value.")
  if (!is.numeric(N_at) || any(N_at <= 0)) stop("N_at must be a positive numeric value.")

  # Constant derived from Hart formula approximation
  constant <- 10745.7

  # Calculate S%
  S_percent <- constant / (h_top * sqrt(N_at))

  return(S_percent)
}


#' Calculate S% based on time and the initial thinning grade
#' Formula 74, p87
#'
#' Computes the S% depending on the stand age `t`, the initial thinning grade
#' `Tgr0`, and a coefficient `c19`. The function implements
#' a piecewise formula:
#'
#' - For t <= 50: S% = 13 + 3 * (Tgr0 - 1)
#' - For t >  50: S% = 13 + 3 * (Tgr0 - 1) + c19 * (t - 50)
#'
#' @param t Stand age (numeric scalar or vector).
#' @param Tgr0 Initial thinning grade (numeric scalar).
#' @param c19 Growth extension coefficient beyond age 50 (numeric scalar).
#'
#' @return Numeric value (or vector) of S% corresponding to each `t`.
#'
#' @examples
#' calculate_S_percent(t = 45, Tgr0 = 1.2, c19 = 0.05)
#' calculate_S_percent(t = c(30, 55, 80), Tgr0 = 1.1, c19 = 0.02)
#'
#' @export
calculate_tgr <- function(t, tgr_0, c19) {
  # Ensure t is numeric
  if (!is.numeric(t)) stop("t must be numeric")

  # Base term (applies to both conditions)
  base <- 13 + 3 * (tgr_0 - 1)

  # Initialize S% with base
  s_percent <- base

  # Apply conditional logic for vectorized inputs
  s_percent <- ifelse(
    t <= 50,
    base,
    base + c19 * (t - 50)
  )

  return(s_percent)
}

