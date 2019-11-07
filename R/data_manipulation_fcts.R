#' Replaces outliers from a vector and returns a new vector
#'
#' @param col_in The vector
#' @param my_prob Probability of quantiles (will remove quantiles at p and 1-p)
#'
#' @return A vector without the outliers
#' @export
#'
#' @examples
#' x <- afedR_replace_outliers(runif(100))
afedR_replace_outliers <- function(col_in, my_prob = 0.05) {
  #
  #
  # INPUTS: col_in
  #         my_prob
  #
  # OUTPUT:

  # return if class is other than numeric
  if (!(class(col_in) %in% c('numeric', 'integer'))) return(col_in)

  my_outliers <- stats::quantile(x = col_in,
                                 probs = c(my_prob, 1-my_prob),
                                 na.rm = TRUE)

  idx <- (col_in <= my_outliers[1])|(col_in >= my_outliers[2])
  col_in[idx] <- NA

  return(col_in)

}
