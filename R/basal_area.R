#' Calculate basal area per hectare (G)
#'
#' Formula 103, p110
#' This function computes the total basal area (G, in m²/ha) given the number of trees per hectare
#' and the average tree diameter at breast height (in cm). The formula used is:
#'
#' \deqn{G = N * π * (d / 200)^2}
#'
#' where:
#' - N = number of trees per hectare
#' - d = diameter at breast height in cm
#'
#' @param N Number of trees per hectare (numeric scalar or vector)
#' @param d Diameter at breast height (cm) (numeric scalar or vector)
#'
#' @return Basal area in square meters per hectare (numeric)
#'
#' @examples
#' calculate_basal_area(N = 500, d = 30)
#' calculate_basal_area(N = c(500, 600), d = c(25, 30))
#'
#' @export
calculate_g <- function(N, d) {
  if (!is.numeric(N) || !is.numeric(d)) stop("Both N and d must be numeric.")
  if (any(N < 0) || any(d < 0)) stop("N and d must be non-negative.")

  G <- N * pi * (d / 200)^2
  return(G)
}


#' Calculate basal area increment for the year after passing t7
#'
#' Formula from FORTRAN programma
#'
#' @param N Number of trees per hectare (numeric scalar or vector)
#' @param d Diameter at breast height (cm) (numeric scalar or vector)
#'
#' @return Basal area in square meters per hectare (numeric)
#'
#' @examples
#' calculate_basal_area(N = 500, d = 30)
#' calculate_basal_area(N = c(500, 600), d = c(25, 30))
#'
#' @export
calculate_ic_g_t7 <- function(t, h_top_t1, h_top_t2, tgr_0, c) {

  with(c, {
  cf80 = c52
  hmin1 = c53

  # Adjusted TGR based on thinning
  x_HD=0
  tgr_adj <- tgr_0 + c56 * x_HD
  # TODO duplicate met formule hieronder
  cor_tgr <- if (h_top_t1 <= c55) {
    1
  } else if (tgr_adj <= c54) {
    1
  } else {
    1 - c6 * (tgr_adj - c54)^c7
  }

  hmadj = (hmin1 - h_top_t1)
  if (hmadj <= 0) hmadj <- 0.0
  c8 = c8 + c9 * hmadj

  t2 = t+1
  delta_t7 = t2 - t7

  termh = ((h_top_t2-1.3) ** c8-(7-1.3) ** c8) / delta_t7
  deltaG = delta_t7 * cf80 * cor_tgr * (c10+c5 * termh)
  iG=deltaG

  # TODO TERM H veel te hoog
  cat("t ", t, " term_h =", termh, "\n")

  return(iG)
  })
}


#' Calculate basal area increment (i_G) with updated cor_tgr
#' Formula 71 p81
#'
#' Computes the annual height increment i_G between two time points,
#' adjusted for thinning, growth ratio, and recording year.
#' Incorporates updated logic for the cor_tgr correction as per Equation 70 variant.
#'
#' @param h_top_t1 Top height at time t1 (in meters)
#' @param h_top_t2 Top height at time t2 (in meters)
#' @param dt Time interval (in years)
#' @param tgr0 Tree growth ratio (numeric)
#' @param x_HD Thinning indicator: 1 = thinning from above, 0 otherwise
#' @param x80 Indicator to identify if projection is for before(0) or after 1980(1)
#' @param c Named list of model parameters:
#'   c5, c6, c7, c8, c9, c10, c51, c52, c53, c54, c55, c56, c57
#'
#' @return Annual increment i_G (numeric scalar, m/year)
#'
#' @export
calculate_ic_g <- function(h_top_t1, h_top_t2, dt, tgr_0, x80, x_HD, c) {

  #cat("htop\t", h_top_t1, "(calculate_ic_g) \n")

  if (h_top_t1 <= 7 || h_top_t2 <= 7) stop("This function is only valid for h_top > 7.")
  if (!x_HD %in% c(0, 1)) stop("x_HD must be 0 or 1.")
  if (dt <= 0) stop("dt must be positive.")

  with(c, {
    # Adjusted TGR based on thinning
    tgr_adj <- tgr_0 + c56 * x_HD

    # Exponent b (based on h_top_t1)
    b <- if (h_top_t1 > c53) {
      c8
    } else {
      c8 + c9 * sqrt(c53 - h_top_t1)
    }

    # TODO check of hier op juiste plek of dat dit dubbelt
    # Updated cor_tgr logic (new definition)
    cor_tgr <- if (h_top_t1 <= c55) {
      1
    } else if (tgr_adj <= c54) {
      1
    } else {
      1 - c6 * (tgr_adj - c54)^c7
    }

    # Year-based correction cf80
    cf80 <- c51 * (1 - x80) + c52 * x80

    # Thinning correction
    cor_HD <- 1 + c57 * x_HD

    # Main increment formula
    delta_h <- ((h_top_t2 - 1.3)^b - (h_top_t1 - 1.3)^b) / dt
    iG <- cor_tgr * (c10 + c5 * delta_h) * cf80 * cor_HD

    #cat("t ", year, " delta_h=", delta_h, "\n")

    return(iG)
  })
}
