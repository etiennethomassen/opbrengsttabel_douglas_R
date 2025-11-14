#' Calculate t7
#'
#' Calculates the age at which the height is 7m
#' Inverse of formula 28, printed on p33
#'
#' @param S_i Site index (numeric scalar).
#' @param c1, c2, c3, Model coefficients (numeric scalars).
#' @return Top height h_top at age t (numeric scalar).
#'
calculate_t7 <- function(S_i, c1, c2, c3) {

  # Calculate exponent b
  b <- c2 - c3 * S_i

  # Calculate t7 where both f1 and f2 intersect (i.e. f1(t7) = f2(t7) = 7)
  t7 <- (1 / c1) * log(1 / (1 - (7 / S_i)^(1 / b)))

  return(t7)
}


#' Calculate top height (h_top) for a specific stand age (t)
#'
#' Computes h_top for a single time point t, based on site index and model parameters.
#' Formula 31 in Jansen et al 2016
#'
#' @param t Stand age (numeric scalar).
#' @param S_i Site index (numeric scalar).
#' @param c1, c2, c3, c4 Model coefficients (numeric scalars).
#' @return Top height h_top at age t (numeric scalar).
#' @examples
#' calculate_htop(t = 20, S_i = 30, c1 = 0.5, c2 = 0.3, c3 = 0.01, c4 = 1.2)
calculate_h_top <- function(t, S_i, c1, c2, c3, c4) {

  # Calculate exponent b
  b <- c2 - c3 * S_i

  # Calculate t7 where both f1 and f2 intersect (i.e. f1(t7) = f2(t7) = 7)
  # TODO remove t7 here and use formula above
  t7 <- (1 / c1) * log(1 / (1 - (7 / S_i)^(1 / b)))

  # Calculate a_k constant based on t7
  numerator <- c1 * b * (S_i / 7) * exp(-c1 * t7) * (1 - exp(-c1 * t7))^(b - 1)
  denominator <- c4 * t7^(-c4 - 1)
  a_k <- numerator / denominator

  # Evaluate the appropriate function depending on t
  if (t <= t7) {
    # Use f1(t)
    h_top <- 7 * exp(-a_k * t^(-c4)) / exp(-a_k * t7^(-c4))
  } else {
    # Use f2(t)
    h_top <- S_i * (1 - exp(-c1 * t))^b
  }

  return(h_top)
}


#' Calculate dominant height (h_dom) from top height and stand density
#'
#' Computes the dominant height `h_dom` based on top height (`h_top`),
#' stand density (`N_at`), and model parameters `c24` and `c53`. The function
#' uses a piecewise formula:
#'
#' - If N_at > 250:
#'     \deqn{h_dom = h_top - c24 * h_top^c25}
#' - If 100 < N_at ≤ 250:
#'     \deqn{h_dom = ((N_at - 100)/150) * (h_top - c24 * h_top^c25) + ((250 - N_at)/150) * h_top}
#' - If N_at ≤ 100:
#'     \deqn{h_dom = h_top}
#'
#' @param h_top Top height in meters. Numeric scalar.
#' @param N_at Stand density (trees per hectare). Numeric scalar.
#' @param c24 Model coefficient c24. Numeric scalar.
#' @param c25 Model exponent c25. Numeric scalar.
#'
#' @return Dominant height `h_dom` in meters. Numeric scalar.
#'
#' @examples
#' calculate_h_dom(h_top = 30, N_at = 300, c24 = 0.5, c25 = 1.2)
#' calculate_h_dom(h_top = 30, N_at = 180, c24 = 0.5, c25 = 1.2)
#' calculate_h_dom(h_top = 30, N_at = 80, c24 = 0.5, c25 = 1.2)
#'
#' @export
calculate_h_dom <- function(h_top, N_at, c24, c25) {
  # Input validation
  if (!is.numeric(h_top) || h_top <= 0) stop("h_top must be a positive number.")
  if (!is.numeric(N_at) || N_at < 0) stop("N_at must be a non-negative number.")

  # Case 1: N_at > 250
  if (N_at > 250) {
    h_dom <- h_top - c24 * h_top^c25
  }

  # Case 2: 100 < N_at <= 250
  else if (N_at > 100) {
    weight_adj <- (N_at - 100) / 150
    weight_top <- (250 - N_at) / 150
    h_dom <- weight_adj * (h_top - c24 * h_top^c25) + weight_top * h_top
  }

  # Case 3: N_at <= 100
  else {
    h_dom <- h_top
  }

  return(h_dom)
}






#' Calculate average height after thinning(ha) from top height (htop) using thinning and growth modifiers
#' Formula 89, p104
#'
#' This function computes the adjusted tree height (`ha`) based on top height (`htop`),
#' thinning type, and tree growth ratio. It applies a correction based on the
#' provided coefficients and the thinning flag (`x_HD`).
#'
#' @param htop Top height (m). Numeric scalar.
#' @param h70 Height at base age 70 (m). Numeric scalar.
#' @param tgr Tree growth ratio. Numeric scalar.
#' @param x_HD Thinning type indicator: 1 for thinning from above, 0 otherwise. Integer (0 or 1).
#' @param c32L,c32H,c33,c34,c53,c56 Model coefficients (numeric scalars).
#'
#' @return Adjusted height `ha` (numeric scalar).
#'
#' @examples
#' calculate_adjusted_height(htop = 30, h70 = 25, tgr = 6.0, x_HD = 1,
#'                           c32L = 0.5, c32H = 0.8, c33 = 0.6, c34 = 0.4, c53 = 1.2, c56 = 0.2)
#'
#' @export
calculate_hg_at<- function(htop, h70, tgr, x_HD,
                                      c32L, c32H, c33, c34, c53, c56) {
  # Input checks
  if (!x_HD %in% c(0, 1)) stop("x_HD must be 0 (no thinning) or 1 (thinning from above)")
  if (htop <= 0 || h70 <= 0) stop("htop and h70 must be positive")

  # Compute tgr_add
  tgr_add <- tgr + c56 * x_HD

  # Compute Term_tgr
  Term_tgr <- if (tgr_add >= 6.5) 0 else (6.5 - tgr_add)^2

  # Compute numerator of h_diff
  numerator <- (c32L * (1 - x_HD)) + (c32H * x_HD) + (c33 * Term_tgr) + (c34 * h70)

  # Compute h_diff
  h_diff <- numerator / (htop ^ c53)

  # Final adjusted height
  hg_at <- htop - h_diff

  return(hg_at)
}

#' Calculate hg_bt (h) from h_at and thinning status
#'
#' This function computes hg_bt using hg_at and a thinning correction factor
#' that linearly blends between two coefficients (`c36L` and `c36H`) depending on the thinning type.
#'
#' Formula:
#' \deqn{h_bt = ((c36L * (1 - x_HD)) + (c36H * x_HD)) * h_at}
#'
#' @param h_at Adjusted height (numeric scalar). Typically from `calculate_adjusted_height()`.
#' @param x_HD Integer (0 or 1). Thinning indicator: 1 = thinning from above, 0 = otherwise.
#' @param c36L Coefficient for no thinning or thinning from below.
#' @param c36H Coefficient for thinning from above.
#'
#' @return Breast-height-adjusted height `h_bt` (numeric scalar).
#'
#' @examples
#' calculate_h_bt(h_at = 28.5, x_HD = 0, c36L = 0.9885, c36H = 0.9746)
#' calculate_h_bt(h_at = 28.5, x_HD = 1, c36L = 0.9885, c36H = 0.9746)
#'
#' @export
calculate_hg_bt <- function(h_at, x_HD, c36L = 0.9885, c36H = 0.9746) {
  # Input checks
  if (!x_HD %in% c(0, 1)) stop("x_HD must be 0 (no thinning) or 1 (thinning from above)")
  if (!is.numeric(h_at) || h_at < 0) stop("h_at must be a non-negative number")

  # Compute weighted coefficient
  adjustment_coef <- (c36L * (1 - x_HD)) + (c36H * x_HD)

  # Compute h_bt
  h_bt <- adjustment_coef * h_at

  return(hg_bt)
}


