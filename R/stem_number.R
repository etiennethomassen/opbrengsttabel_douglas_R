#' Calculate number of trees per hectare (N_at) from relative spacing and dominant height
#'
#' This function computes the stand density (N_at) using the relative spacing percentage (S_percent)
#' and dominant height (h_dom), based on the spacing-density rule:
#'
#' \deqn{N_at = (10746 / (S_percent * h_dom))^2}
#'
#' @param S_percent Relative spacing (%) as a decimal (e.g. 0.25 for 25%)
#' @param h_dom Dominant height in meters (numeric)
#'
#' @return Number of trees per hectare (N_at), numeric
#'
#' @examples
#' calculate_N_at(S_percent = 0.25, h_dom = 28)
#' calculate_N_at(S_percent = 0.30, h_dom = 22)
#'
#' @export
calculate_n_at <- function(s_percent, h_top) {
  # Input validation
  if (!is.numeric(s_percent) || any(s_percent <= 0)) {
    stop("S_percent must be a positive numeric value (e.g., 0.25 for 25%)")
  }
  if (!is.numeric(h_dom) || any(h_dom <= 0)) {
    stop("h_dom must be a positive numeric value (dominant height in meters)")
  }

  # TODO staat h_dom in de handleiding
  n_at <- (10746 / (s_percent * h_top))^2
  return(n_at)
}

